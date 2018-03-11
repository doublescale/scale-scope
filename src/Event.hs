module Event
  ( loadPaths
  , reloadShader
  , eventLoop
  ) where

import qualified Codec.Compression.Zlib as Zlib
import Control.Lens ((%=), (*=), (+=), (.=), (<~), (^.))
import Control.Monad (forever)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, get, gets, modify)
import Data.Bool (bool)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (mapMaybe)
import qualified Data.Store as Store
import Linear (V2(V2), (*^), (^/), _y)
import qualified SDL

import Action (AppAction(..))
import AppState
import Mesh (AnimationData(..))
import Render (RenderState(..), render)
import Render.Mesh (deleteMeshSequence, loadMeshSequence)
import Render.Shader (loadShader)
import Render.Types
       (initRenderState, renderStateMeshesL, renderStateShaderL)
import Util (fromCString)

loadPaths :: (MonadIO m, MonadState AppState m) => [FilePath] -> m ()
loadPaths [] = do
  deleteMeshSequence =<< gets (renderStateMeshes . appRenderState)
  appRenderStateL .= initRenderState
loadPaths [path] = do
  deleteMeshSequence =<< gets (renderStateMeshes . appRenderState)
  animData <- loadAnimationFile path
  newMeshSequence <- loadMeshSequence animData
  appRenderStateL . renderStateMeshesL .= newMeshSequence
  appFrameRateL .= animationFramerate animData
  where
    loadAnimationFile file =
      liftIO $ do
        blob <- BSL.readFile file
        case Store.decode (BSL.toStrict (Zlib.decompress blob)) of
          Right animData -> return animData
          Left err -> error (show err)
loadPaths _ = liftIO (putStrLn "Need exactly one file argument.")

reloadShader :: (MonadIO m, MonadState AppState m) => m ()
reloadShader = do
  oldStateShader <- gets (renderStateShader . appRenderState)
  newStateShader <- loadShader oldStateShader
  appRenderStateL . renderStateShaderL .= newStateShader

eventLoop ::
     (MonadIO m, MonadState AppState m, MonadError () m) => SDL.Window -> m ()
eventLoop win =
  forever $ do
    actions <- mapMaybe eventToAction <$> SDL.pollEvents
    mapM_ handleAction actions
    handleMouseMode
    appWinSizeL <~ fmap fromIntegral <$> SDL.glGetDrawableSize win
    modify . integrateState =<< SDL.time
    render =<< get
    SDL.glSwapWindow win

handleMouseMode :: MonadIO m => m ()
handleMouseMode =
  liftIO $ do
    pressedButtons <- SDL.getMouseButtons
    _ <-
      SDL.setMouseLocationMode
        (bool
           SDL.AbsoluteLocation
           SDL.RelativeLocation
           (any pressedButtons [SDL.ButtonLeft, SDL.ButtonMiddle]))
    return ()

eventToAction :: SDL.Event -> Maybe AppAction
eventToAction SDL.Event
  { SDL.eventPayload = SDL.MouseMotionEvent SDL.MouseMotionEventData
    { SDL.mouseMotionEventRelMotion = delta@(V2 _ dy)
    , SDL.mouseMotionEventState = buttons
    }
  } = case buttons of
    [SDL.ButtonLeft] -> Just (CamRotate (0.5 * fmap fromIntegral delta))
    [SDL.ButtonMiddle] -> Just (CamDistance (0.01 * fromIntegral dy))
    _ -> Nothing
eventToAction SDL.Event
  { SDL.eventPayload = SDL.MouseWheelEvent SDL.MouseWheelEventData
    { SDL.mouseWheelEventPos = V2 _ dy }
  } = Just (CamDistance (-fromIntegral dy))
eventToAction SDL.Event
  { SDL.eventPayload = SDL.KeyboardEvent SDL.KeyboardEventData
    { SDL.keyboardEventKeysym = SDL.Keysym {SDL.keysymKeycode}
    , SDL.keyboardEventKeyMotion = SDL.Pressed
    , SDL.keyboardEventRepeat
    }
  } = case (keysymKeycode, keyboardEventRepeat) of
    (SDL.KeycodeP, False) -> Just PauseToggle
    (SDL.KeycodeF5, False) -> Just ShaderReload
    (SDL.KeycodeF11, False) -> Just FullscreenToggle
    (SDL.KeycodePeriod, _) -> Just (FrameSkip 1)
    (SDL.KeycodeComma, _) -> Just (FrameSkip (-1))
    (SDL.KeycodeBackspace, _) -> Just SpeedReset
    (SDL.KeycodeLeftBracket, _) -> Just (SpeedMultiply (recip 1.125))
    (SDL.KeycodeRightBracket, _) -> Just (SpeedMultiply 1.125)
    (SDL.KeycodeBackslash, _) -> Just (SpeedMultiply (-1))
    (SDL.KeycodeEscape, _) -> Just Quit
    _ -> Nothing
eventToAction SDL.Event
  { SDL.eventPayload = SDL.DropEvent SDL.DropEventData
    { SDL.dropEventFile = dropData }
  } = Just (FileLoad (fromCString dropData))
eventToAction SDL.Event {SDL.eventPayload = SDL.QuitEvent} = Just Quit
eventToAction _ = Nothing

handleAction ::
     (MonadIO m, MonadState AppState m, MonadError () m) => AppAction -> m ()
handleAction (CamDistance d) = appViewStateL . viewCamDistanceVelL += d
handleAction (CamRotate d) = appViewStateL . viewCamAngleVelL += d
handleAction PauseToggle = appPausedL %= not
handleAction ShaderReload = reloadShader
handleAction FullscreenToggle = do
  win <- gets appWindow
  liftIO $ do
    SDL.WindowConfig {windowMode} <- SDL.getWindowConfig win
    case windowMode of
      SDL.Windowed -> SDL.setWindowMode win SDL.FullscreenDesktop
      _ -> SDL.setWindowMode win SDL.Windowed
handleAction (FrameSkip d) = appFrameL += d
handleAction SpeedReset = appFrameRateFactorL .= 1
handleAction (SpeedMultiply d) = appFrameRateFactorL *= d
handleAction Quit = throwError ()
handleAction (FileLoad path) = loadPaths [path]

integrateState :: Double -> AppState -> AppState
integrateState currentTime st@AppState {appViewState = vs@ViewState {..}, ..} =
  st
  { appViewState =
      vs
      { viewCamAngle =
          viewCamAngle +
          angleFactor * dt *^ (newCamAngleVel + 2 * V2 0 pitchRestitution)
      , viewCamDistance =
          viewCamDistance * (1 + distanceFactor * dt * newCamDistanceVel)
      , viewCamAngleVel = newCamAngleVel
      , viewCamDistanceVel = newCamDistanceVel
      , viewCamPos =
          (viewCamPos + dt * posFactor *^ viewSamplePos) ^/ (1 + dt * posFactor)
      }
  , appTimePrev = currentTime
  , appFrameRateInterp =
      (appFrameRateInterp + dt * rateFactor * effectiveFrameRate) /
      (1 + dt * rateFactor)
  , appFrame = appFrame + dt * appFrameRateInterp
  }
  where
    dt = currentTime - appTimePrev
    newCamAngleVel = viewCamAngleVel ^/ (1 + angleDamp * dt)
    newCamDistanceVel = viewCamDistanceVel / (1 + distanceDamp * dt)
    angleDamp = 10
    distanceDamp = 20
    angleFactor = 5
    distanceFactor = 2
    posFactor = 10
    pitchRestitution =
      let p = viewCamAngle ^. _y
      in min 0 (90 - p) - min 0 (p + 90)
    rateFactor = 10
    effectiveFrameRate =
      if appPaused
        then 0
        else appFrameRate * appFrameRateFactor
