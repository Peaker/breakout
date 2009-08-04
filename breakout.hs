{-# OPTIONS -Wall -O2 #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where
    import qualified Graphics.UI.HaskGame as HaskGame
    import qualified Graphics.UI.HaskGame.Font as Font
    import qualified Graphics.UI.HaskGame.Vector2 as Vector2
    import Graphics.UI.HaskGame.Vector2(Vector2(..))
    import Graphics.UI.HaskGame.Color(Color(..))
    import qualified Graphics.UI.SDL as SDL
    import qualified IO
    import qualified System
    import qualified Control.Monad as Monad
    import qualified Control.Exception as Exception
    import Control.Exception(Exception)
    import Data.Typeable(Typeable)
    import Control.Applicative(liftA3)

    data QuitException = QuitException
      deriving (Show, Typeable)
    instance Exception QuitException where

    -- Pure part
    inRange :: (Ord a) => a -> a -> a -> Bool
    inRange start stop x = (start <= x && x < stop)

    positions :: Vector2 Int -> [Vector2 Int]
    positions size = positions'
        where
          positions' = initPos : zipWith (+) positions' speeds
          speeds = initSpeed : zipWith (liftA3 nextSpeed size) (tail positions') speeds
          initPos = Vector2 0 0
          initSpeed = Vector2 1 2

    nextSpeed :: Int -> Int -> Int -> Int
    nextSpeed size position = if inRange 0 size position then id else negate

    -- Monadic part

    handleEvents :: [SDL.Event] -> IO ()
    handleEvents events = do
      Monad.forM_ events $ \event -> case event of 
                                       SDL.Quit -> Exception.throwIO QuitException
                                       _ -> return ()

    mainLoop :: String -> SDL.Surface -> IO ()
    mainLoop text display = do
      myFont <- Font.defaultFont 40
      black <- SDL.mapRGB (SDL.surfaceGetPixelFormat display) 0 0 0

      textSurface <- Font.renderText myFont text (Color 255 0 0)
      let displaySize = HaskGame.surfaceSize display
          textSize = HaskGame.surfaceSize textSurface
          size = displaySize - textSize

      Monad.forM_ (positions size) $ \pos -> do
        SDL.fillRect display Nothing black
        HaskGame.blit display pos textSurface
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
      args <- System.getArgs
      let text = case args of
                   [] -> "Not enough arguments"
                   (_:_:_) -> "Too many arguments"
                   (x:[]) -> x
      HaskGame.withInit $ do
        display <- HaskGame.setVideoMode xres yres colordepth
        mainLoop text display
        return ()
