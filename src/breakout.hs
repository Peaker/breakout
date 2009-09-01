{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified Graphics.UI.HaskGame as HaskGame
import qualified Graphics.UI.HaskGame.Vector2 as Vector2
import qualified Graphics.UI.HaskGame.Rect as Rect
import Graphics.UI.HaskGame.Vector2(Vector2(..))
import Graphics.UI.HaskGame.Color(Color(..))
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Video as SDL.Video
import Graphics.UI.SDL(Rect(..))
import qualified System.IO as IO
import qualified Control.Exception as Exception
import Control.Exception(Exception)
import Data.Typeable(Typeable)
import Data.Maybe(listToMaybe, isNothing, mapMaybe)
import Control.Monad(forM_, forever, when)
import Control.Monad.State.Strict(evalStateT, modify, get)
import Control.Monad.Trans(liftIO)
import Control.Applicative((<$>))

data Direction = X | Y
  deriving (Eq, Ord, Show, Read)

data QuitException = QuitException
  deriving (Show, Typeable)
instance Exception QuitException

displayWidth :: Int
displayHeight :: Int
displaySize :: Vector2 Int
displaySize@(Vector2 displayWidth displayHeight) = Vector2 640 480
colordepth :: Int
colordepth = 32

capRange :: Ord a => a -> a -> a -> a
capRange bottom top x = (x `max` bottom) `min` top

bgColor :: Color
bgColor = Color 0 0 0

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
initialBallSpeed = Vector2 3 (-2)

data GameState = GameState {
          gsPlayerPos :: Int
        , gsPlayerWidth :: Int
        , gsBall :: Maybe (Vector2 Double, Vector2 Double)
        , gsBrickPositions :: [Vector2 Int]
        }

playerRect :: GameState -> Rect
playerRect GameState{gsPlayerPos=playerPos
                    ,gsPlayerWidth=playerWidth} =
    Rect (playerPos - halfWidth) (displayHeight - playerHeight)
         playerWidth displayHeight
    where
      halfWidth = playerWidth `div` 2

capPlayerRange :: Int -> Int -> Int
capPlayerRange curPlayerWidth = capRange halfWidth (displayWidth - halfWidth)
    where halfWidth = curPlayerWidth `div` 2

initGameState :: GameState
initGameState = GameState 0 initialPlayerWidth Nothing initBrickPositions

-- This boilerplate should really be automatic from records :-(
type Endo a = (a -> a)
type Inside whole part = Endo part -> Endo whole
atGsPlayerPos :: Inside GameState Int
atGsPlayerPos f gs = gs{gsPlayerPos = f (gsPlayerPos gs)}
atGsPlayerWidth :: Inside GameState Int
atGsPlayerWidth f gs = gs{gsPlayerWidth = f (gsPlayerWidth gs)}
atGsBall :: Inside GameState (Maybe (Vector2 Double, Vector2 Double))
atGsBall f gs = gs{gsBall = f (gsBall gs)}
atGsBrickPositions :: Inside GameState [Vector2 Int]
atGsBrickPositions f gs = gs{gsBrickPositions = f (gsBrickPositions gs)}

initBrickPositions :: [Vector2 Int]
initBrickPositions =
    let height = displayHeight * 2 `div` 5
    in [Vector2 x y
       | x <- [0, brickWidth+spaceWidth..displayWidth-brickWidth]
       , y <- [0, brickHeight+spaceHeight..height-brickHeight]]

brickRect :: Vector2 Int -> Rect
brickRect (Vector2 x y) = Rect x y brickWidth brickHeight

handleEvents :: [SDL.Event] -> IO ()
handleEvents events = do
  forM_ events $
    \event -> case event of
       SDL.Quit -> Exception.throwIO QuitException
       _ -> return ()

draw :: SDL.Surface -> GameState -> IO ()
draw display gs = do
    forM_ (gsBrickPositions gs) $ \pos ->
        HaskGame.fillRect display (brickRect pos) brickColor
    let Vector2 ballx bally = ballPosition gs
    HaskGame.fillRect display (playerRect gs) playerColor
    let ballRect = Rect (truncate ballx - ballRadius) (truncate bally - ballRadius) (ballRadius*2) (ballRadius*2)
    HaskGame.fillRect display ballRect playerColor

nextGameState :: GameState -> GameState
nextGameState gs =
    maybe gs newState $ gsBall gs
  where
    Rect px _ pw _ = playerRect gs
    brickPositions = gsBrickPositions gs
    fi = fromIntegral
    newState (ballPos, ballSpeed@(Vector2 speedX speedY)) =
        gs{gsBall=if ballY - fi ballRadius >= fi displayHeight
                          then Nothing
                          else Just (ballPos', ballSpeed')
          ,gsBrickPositions=filter (isNothing . collision) $ brickPositions }
      where
        ballPos'@(Vector2 ballX ballY) = ballPos + ballSpeed
        collision = collideBall (truncate <$> ballPos') . brickRect
        firstCollision = listToMaybe $ mapMaybe collision brickPositions

        hitPlayer = ballY + fi ballRadius >= fi (displayHeight - playerHeight) &&
                    ballX+fi ballRadius >= fi px && ballX-fi ballRadius <= fi (px+pw)
        factor = if hitPlayer
                 then (+) $ 8 * (ballX - fi px - fi pw/2) / fi pw
                 else id
        collideX = maybe False (==X) firstCollision
        collideY = maybe False (==Y) firstCollision

        ballSpeed' =
            Vector2
            (factor $
             if ballX - fi ballRadius > 0 && ballX + fi ballRadius < fi displayWidth && not collideX
             then speedX
             else -speedX)
            (if ballY - fi ballRadius > 0 && not hitPlayer && not collideY
             then speedY
             else -speedY)

collideBall :: Vector2 Int -> Rect -> Maybe Direction
collideBall (Vector2 ballX ballY) r =
    let intersect = r `Rect.intersect` (Rect (ballX - ballRadius) (ballY - ballRadius)
                                         (ballRadius*2) (ballRadius*2))
        Vector2 iwidth iheight = Rect.getSize intersect
    in if iwidth <= 0 || iheight <= 0
       then Nothing
       else Just $
            if iwidth < iheight
            then X
            else Y

ballPosition :: GameState -> Vector2 Double
ballPosition GameState{gsBall=ballPosSpeed
                      ,gsPlayerPos=playerPos} =
    maybe (fromIntegral <$> Vector2 playerPos (displayHeight - playerHeight - ballRadius))
          fst $
    ballPosSpeed

sdlIteration :: SDL.Surface -> GameState -> IO ()
sdlIteration display gs =
  do
    HaskGame.fillSurface display bgColor
    draw display gs
    SDL.flip display
    HaskGame.getEvents >>= handleEvents
    ticks <- SDL.getTicks
    SDL.delay (10 - (ticks `mod` 10))

mainLoop :: SDL.Surface -> IO ()
mainLoop display =
  (`evalStateT` initGameState) . forever $ do
    (mouseX, _, buttons) <- liftIO $ SDL.getMouseState
    let leftPressed = SDL.ButtonLeft `elem` buttons

    gameState <- get
    liftIO $ sdlIteration display gameState
    modify . atGsPlayerPos . const . capPlayerRange (gsPlayerWidth gameState) $ mouseX
    ball <- gsBall `fmap` get
    when (leftPressed && isNothing ball) $ do
      ballPos <- ballPosition `fmap` get
      modify . atGsBall . const $ Just (ballPos, initialBallSpeed)
    modify nextGameState

main :: IO ()
main = do
  HaskGame.withInit $ do
    SDL.Video.showCursor False
    display <- HaskGame.setVideoMode displaySize colordepth
    mainLoop display
      `Exception.catch`
      \QuitException -> return ()
