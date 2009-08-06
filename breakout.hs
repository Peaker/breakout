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
import Data.Maybe(listToMaybe, isNothing, mapMaybe)
import Control.Monad(forM_, forever, when)
import Control.Monad.State.Strict(evalStateT, modify, get)
import Control.Monad.Trans(liftIO)
import Control.Applicative((<$>))

data Direction = X | Y
  deriving (Eq, Ord, Show, Read)

data QuitException = QuitException
  deriving (Show, Typeable)
instance Exception QuitException where
  -- Nothing

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
initialBallSpeed = Vector2 1 (-2)

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

data GameState = GameState {
          gsPlayerPos :: Int
        , gsPlayerWidth :: Int
        , gsBallPosSpeed :: Maybe (Vector2 Double, Vector2 Double)
        , gsBrickPositions :: [Vector2 Int]
        }

type Endo a = (a -> a)
type Inside whole part = Endo part -> Endo whole
atGsPlayerPos :: Inside GameState Int
atGsPlayerPos f gs = gs{gsPlayerPos = f (gsPlayerPos gs)}
atGsPlayerWidth :: Inside GameState Int
atGsPlayerWidth f gs = gs{gsPlayerWidth = f (gsPlayerWidth gs)}
atGsBallPosSpeed :: Inside GameState (Maybe (Vector2 Double, Vector2 Double))
atGsBallPosSpeed f gs = gs{gsBallPosSpeed = f (gsBallPosSpeed gs)}
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
  forM_ events $ \event -> case event of 
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

inRange :: Ord a => a -> a -> a -> Bool
inRange bottom top x = x >= bottom && x < top

nextGameState :: GameState -> GameState
nextGameState gs =
    maybe gs newState $ gsBallPosSpeed gs
  where
    Rect px _ pw _ = playerRect gs
    brickPositions = gsBrickPositions gs
    newState (ballPos, ballSpeed@(Vector2 speedX speedY)) =
        gs{gsBallPosSpeed=if ballY >= fi displayHeight
                          then Nothing
                          else Just (ballPos', ballSpeed')
          ,gsBrickPositions=filter (isNothing . collision) $ brickPositions }
      where
        ballPos'@(Vector2 ballX ballY) = ballPos + ballSpeed
        collision = collideBall (truncate <$> ballPos') . brickRect
        firstCollision = listToMaybe $ mapMaybe collision brickPositions

        fi = fromIntegral
        hitPlayer = ballY >= fi (displayHeight - playerHeight) &&
                    ballX >= fi px && ballX <= fi (px+pw)
        factor = if hitPlayer
                 then (+) $ 8 * (ballX - fi px - fi pw/2) / fi pw
                 else id
        collideX = maybe False (==X) firstCollision
        collideY = maybe False (==Y) firstCollision

        ballSpeed' =
            Vector2
            (factor $
             if inRange 0 (fi displayWidth) ballX && not collideX
             then speedX
             else -speedX)
            (if ballY >= 0 && not hitPlayer && not collideY
             then speedY
             else -speedY)

collideBall :: Vector2 Int -> Rect -> Maybe Direction
collideBall (Vector2 ballx bally) (Rect x y w h) =
    let left  = ballx + ballRadius < x
        right = ballx - ballRadius > x+w
        up    = bally + ballRadius < y
        down  = bally - ballRadius > y+h
    in if left || right || up || down
    then Nothing
    else Just $
         if ballx < x || ballx > x+w
         then X
         else Y

ballPosition :: GameState -> Vector2 Double
ballPosition GameState{gsBallPosSpeed=ballPosSpeed
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
mainLoop display = do
  (`evalStateT` (GameState 0 initialPlayerWidth Nothing initBrickPositions)) . forever $ do
    (mouseX, _, buttons) <- liftIO $ SDL.getMouseState
    let leftPressed = SDL.ButtonLeft `elem` buttons

    gameState <- get
    liftIO $ sdlIteration display gameState
    let curPlayerWidth = gsPlayerWidth gameState
        curPlayerPos = capPlayerRange curPlayerWidth mouseX
    modify . atGsPlayerPos . const $ curPlayerPos
    ballPos <- ballPosition `fmap` get
    when leftPressed $
         modify . atGsBallPosSpeed . const $ Just (ballPos, initialBallSpeed)

    modify nextGameState

main :: IO ()
main = do
  HaskGame.withInit $ do
    display <- HaskGame.setVideoMode displaySize colordepth
    mainLoop display
      `Exception.catch`
      \QuitException -> return ()
