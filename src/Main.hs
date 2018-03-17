{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (finally)
import Control.Monad (void)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (StateT, execStateT)
import Linear (V2(V2), V3(V3))
import qualified SDL
import System.Environment (getArgs)

import AppState (AppState(..), ViewState(..))
import Event (eventLoop, loadPaths, reloadShader)
import Render.Types (initRenderState)

main :: IO ()
main = getArgs >>= runWithFiles

runWithFiles :: [FilePath] -> IO ()
runWithFiles files =
  withWindow $ \win -> do
    startTime <- SDL.time
    runAppStack (initState win startTime) $ do
      loadPaths files
      reloadShader
      eventLoop

runAppStack :: AppState -> ExceptT () (StateT AppState IO) () -> IO ()
runAppStack startState app = void (execStateT (runExceptT app) startState)

initState :: SDL.Window -> Double -> AppState
initState appWindow startTime =
  AppState
  { appWindow
  , appWinSize = V2 0 0
  , appPaused = False
  , appFrame = 0
  , appFrameRateInterp = 0
  , appFrameRate = 0
  , appFrameRateFactor = 1
  , appTimePrev = startTime
  , appViewState =
      ViewState
      { viewCamAngle = V2 0 0
      , viewCamAngleVel = V2 0 0
      , viewCamDistance = 10
      , viewCamDistanceVel = 0
      , viewCamPos = V3 0 0 2
      , viewSamplePos = V3 0 0 2
      }
  , appRenderState = initRenderState
  }

withWindow :: (SDL.Window -> IO ()) -> IO ()
withWindow callback = run `finally` SDL.quit
  where
    windowTitle = "ScaleScope"
    run = do
      SDL.initialize [SDL.InitVideo]
      win <-
        SDL.createWindow
          windowTitle
          SDL.defaultWindow
          { SDL.windowOpenGL =
              Just
                SDL.defaultOpenGL
                { SDL.glProfile = SDL.Core SDL.Normal 3 3
                , SDL.glMultisampleSamples = 4
                }
          , SDL.windowResizable = True
          }
      _ <- SDL.glCreateContext win
      SDL.swapInterval SDL.$= SDL.SynchronizedUpdates
      callback win
