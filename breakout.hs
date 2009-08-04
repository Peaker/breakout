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
    import Control.Monad(forM_, forever)

    data QuitException = QuitException
      deriving (Show, Typeable)
    instance Exception QuitException where

    brickColor :: Color
    brickColor = Color 255 0 0
    brickWidth :: Int
    brickWidth = 42
    brickHeight :: Int
    brickHeight = 23

    playerColor :: Color
    playerColor = Color 0 0 255

    playerHeight :: Int
    playerHeight = 15

    data Player = Player { playerPos :: Int
                         , playerWidth :: Int }
    playerRectAt :: Int -> Player -> Rect
    playerRectAt height (Player x width) =
        Rect (x - hwidth) (height - playerHeight)
             width height
        where
          hwidth = width `div` 2

    spaceWidth :: Int
    spaceWidth = 4
    spaceHeight :: Int
    spaceHeight = 3

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

    draw :: SDL.Surface -> Rect -> [Vector2 Int] -> IO ()
    draw display playerRect brickPositions =
        forM_ brickPositions $ \pos -> do
            HaskGame.fillRect display (brickRect pos) brickColor
            HaskGame.fillRect display playerRect playerColor

    mainLoop :: SDL.Surface -> IO ()
    mainLoop display = do
      black <- SDL.mapRGB (SDL.surfaceGetPixelFormat display) 0 0 0

      let displaySize@(Vector2 _ displayHeight) = HaskGame.surfaceSize display
          brickPositions = initBrickPositions displaySize

      forever $ do
        SDL.fillRect display Nothing black

        (mouseX, _, _) <- SDL.getMouseState

        let player = Player mouseX 50
        draw display (playerRectAt displayHeight player) brickPositions

        SDL.flip display
        events <- HaskGame.getEvents
        handleEvents events
        ticks <- SDL.getTicks
        SDL.delay (10 - (ticks `mod` 10))

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
