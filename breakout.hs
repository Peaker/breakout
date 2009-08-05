{-# OPTIONS -Wall -O2 #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where
    import qualified Graphics.UI.HaskGame as HaskGame
    import qualified Graphics.UI.HaskGame.Vector2 as Vector2
    import Graphics.UI.HaskGame.Vector2(Vector2(..))
    import Graphics.UI.HaskGame.Color(Color(..))
    import qualified Graphics.UI.SDL as SDL
    import Graphics.UI.SDL(Rect(..))
    import qualified IO
    import qualified Control.Exception as Exception
    import Control.Exception(Exception)
    import Data.Typeable(Typeable)
    import Control.Monad(forM_, forever, when)
    import Control.Monad.State.Strict(evalStateT, modify, get)
    import Control.Monad.Trans(liftIO)
    import Control.Applicative((<$>))

    data QuitException = QuitException
      deriving (Show, Typeable)
    instance Exception QuitException where
      -- Nothing

    capRange :: Ord a => a -> a -> a -> a
    capRange bottom top x = (x `max` bottom) `min` top

    brickColor :: Color
    brickColor = Color 255 0 0
    brickWidth :: Int
    brickWidth = 72
    brickHeight :: Int
    brickHeight = 30
    spaceWidth :: Int
    spaceWidth = 8
    spaceHeight :: Int
    spaceHeight = 5
    ballRadius :: Int
    ballRadius = 8

    playerColor :: Color
    playerColor = Color 0 0 255

    playerHeight :: Int
    playerHeight = 15
    initialPlayerWidth :: Int
    initialPlayerWidth = 80
    initialBallSpeed :: Vector2 Double
    initialBallSpeed = Vector2 1 (-2)

    playerRectAt :: Int -> Int -> Int -> Rect
    playerRectAt height x width =
        Rect (x - hwidth) (height - playerHeight)
             width height
        where
          hwidth = width `div` 2

    capPlayerRange :: Int -> Int -> Int -> Int
    capPlayerRange displayWidth curPlayerWidth = capRange halfWidth (displayWidth - halfWidth)
        where halfWidth = curPlayerWidth `div` 2

    data GameState = GameState {
              gsPlayerPos :: Int
            , gsPlayerWidth :: Int
            , gsBallPosSpeed :: Maybe (Vector2 Int, Vector2 Double)
            , gsBrickPositions :: [Vector2 Int]
            }

    type Endo a = (a -> a)
    type Inside whole part = Endo part -> Endo whole
    atGsPlayerPos :: Inside GameState Int
    atGsPlayerPos f gs = gs{gsPlayerPos = f (gsPlayerPos gs)}
    atGsPlayerWidth :: Inside GameState Int
    atGsPlayerWidth f gs = gs{gsPlayerWidth = f (gsPlayerWidth gs)}
    atGsBallPosSpeed :: Inside GameState (Maybe (Vector2 Int, Vector2 Double))
    atGsBallPosSpeed f gs = gs{gsBallPosSpeed = f (gsBallPosSpeed gs)}
    atGsBrickPositions :: Inside GameState [Vector2 Int]
    atGsBrickPositions f gs = gs{gsBrickPositions = f (gsBrickPositions gs)}

    -- Pure part
    brickPositionsIn :: Vector2 Int -> [Vector2 Int]
    brickPositionsIn (Vector2 width height) =
        [Vector2 x y
         | x <- [0, brickWidth+spaceWidth..width-brickWidth]
        , y <- [0, brickHeight+spaceHeight..height-brickHeight]]


    brickArea :: Vector2 Int -> Vector2 Int
    brickArea (Vector2 width height) = Vector2 width (height * 2 `div` 5)

    initBrickPositions :: Vector2 Int -> [Vector2 Int]
    initBrickPositions = brickPositionsIn . brickArea

    brickRect :: Vector2 Int -> Rect
    brickRect (Vector2 x y) = Rect x y brickWidth brickHeight

    -- Monadic part
    handleEvents :: [SDL.Event] -> IO ()
    handleEvents events = do
      forM_ events $ \event -> case event of 
                                       SDL.Quit -> Exception.throwIO QuitException
                                       _ -> return ()

    draw :: SDL.Surface -> Vector2 Int -> Rect -> [Vector2 Int] -> IO ()
    draw display (Vector2 ballx bally) playerRect brickPositions = do
        forM_ brickPositions $ \pos ->
            HaskGame.fillRect display (brickRect pos) brickColor
        HaskGame.fillRect display playerRect playerColor
        let ballRect = Rect (ballx - ballRadius) (bally - ballRadius) (ballRadius*2) (ballRadius*2)
        HaskGame.fillRect display ballRect playerColor

    inRange :: Ord a => a -> a -> a -> Bool
    inRange bottom top x = x >= bottom && x < top

    nextGameStateIn :: Vector2 Int -> GameState -> GameState
    nextGameStateIn (Vector2 width height) gs = maybe gs newState $ gsBallPosSpeed gs
      where
        brickPoss = gsBrickPositions gs
        playerWidth = gsPlayerWidth gs
        Rect px _ pw _ = playerRectAt height (gsPlayerPos gs) playerWidth
        newState (ballPos, ballSpeed@(Vector2 sx sy)) =
            gs{gsBallPosSpeed=if offScreen
                              then Nothing
                              else Just (ballPos', ballSpeed')
              ,gsBrickPositions=filter noCollision brickPoss }
          where
            fi = fromIntegral
            noCollision = not . collide ballPos' . brickRect
            ballPos'@(Vector2 bpx bpy) = ballPos + (truncate <$> ballSpeed)
            hitBottom = bpy >= height
            offScreen = hitBottom && not (bpx >= px && bpx <= px+pw)
            factor = if hitBottom
                     then (+) $ (8::Double) * (fi bpx - fi px - fi pw/2) / fi pw
                     else id

            ballSpeed' =
                Vector2
                (factor $ if inRange 0 width bpx
                          then sx
                          else -sx)
                (if inRange 0 height bpy
                 then sy
                 else -sy)

    collide :: Vector2 Int -> Rect -> Bool
    collide (Vector2 px py) (Rect x y w h) = not $
        (px + ballRadius) < x ||   -- ball is entirely left of rect
        (px - ballRadius) > x+w || -- ball is entirely right of rect
        (py + ballRadius) < y ||   -- ball is entirely up of rect
        (py - ballRadius) > y+h    -- ball is entirely below rect

    mainLoop :: SDL.Surface -> IO ()
    mainLoop display = do
      black <- SDL.mapRGB (SDL.surfaceGetPixelFormat display) 0 0 0

      let displaySize@(Vector2 displayWidth displayHeight) = HaskGame.surfaceSize display
          capPlayerRange' = capPlayerRange displayWidth
          playerRectAt' = playerRectAt displayHeight
          nextGameState = nextGameStateIn displaySize

      (`evalStateT` (GameState 0 initialPlayerWidth Nothing (initBrickPositions displaySize))) . forever $ do
        liftIO $ SDL.fillRect display Nothing black

        (mouseX, _, buttons) <- liftIO $ SDL.getMouseState
        let leftPressed = SDL.ButtonLeft `elem` buttons

        curPlayerWidth <- gsPlayerWidth <$> get
        modify . atGsPlayerPos . const $ capPlayerRange' curPlayerWidth mouseX
        curPlayerPos <- gsPlayerPos <$> get

        ballPos <- (maybe (Vector2 mouseX (displayHeight - playerHeight - ballRadius)) fst . gsBallPosSpeed)
                   `fmap` get
        modify nextGameState
        when leftPressed $
             modify . atGsBallPosSpeed $ const (Just (ballPos, initialBallSpeed))
        brickPositions <- gsBrickPositions `fmap` get
        liftIO $ draw display ballPos (playerRectAt' curPlayerPos curPlayerWidth) brickPositions

        liftIO $ SDL.flip display
        events <- liftIO $ HaskGame.getEvents
        liftIO $ handleEvents events
        ticks <- liftIO $ SDL.getTicks
        liftIO $ SDL.delay (10 - (ticks `mod` 10))

    xres :: Int
    xres = 640
    yres :: Int
    yres = 480
    colordepth :: Int
    colordepth = 32

    main :: IO ()
    main = do
      HaskGame.withInit $ do
        display <- HaskGame.setVideoMode xres yres colordepth
        mainLoop display
          `Exception.catch`
          \QuitException -> return ()
