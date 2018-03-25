module Event
  ( loadPaths
  , eventLoop
  ) where

import qualified Codec.Compression.Zlib as Zlib
import Control.Lens ((%=), (*=), (+=), (.=), (<~), (^.))
import Control.Monad (forever, guard)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, get, gets, modify)
import Data.Bool (bool)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Store as Store
import Linear (V2(V2), V3(V3), (*^), (^/), _y)
import qualified SDL

import Action (AppAction(..), isRepeating)
import AppState
import Event.ModState (ModState(ModState), fromKeyModifier)
import InputMap (InputMap(..))
import Mesh (AnimationData(..))
import Render (RenderState(..), render)
import Render.Mesh (deleteMeshSequence, loadMeshSequence)
import Render.Shader (reloadShader)
import Render.Types (initRenderState, renderStateMeshesL, renderStateShaderL)
import Util (fromCString, modifyingM)

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

eventLoop :: (MonadIO m, MonadState AppState m, MonadError () m) => m ()
eventLoop =
  forever $ do
    inputMap <- gets appInputMap
    eventActions <- mapMaybe (eventToAction inputMap) <$> SDL.pollEvents
    modState <- fromKeyModifier <$> SDL.getModState
    continuousActions <-
      keysToActions (1 / 60) modState <$> SDL.getKeyboardState
    mapM_ handleAction (eventActions ++ continuousActions)
    handleMouseMode
    win <- gets appWindow
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

eventToAction :: InputMap -> SDL.Event -> Maybe AppAction
eventToAction _ SDL.Event
  { SDL.eventPayload = SDL.MouseMotionEvent SDL.MouseMotionEventData
    { SDL.mouseMotionEventRelMotion = delta@(V2 _ dy)
    , SDL.mouseMotionEventState = buttons
    }
  } = case buttons of
    [SDL.ButtonLeft] -> Just (CamRotate (0.5 * fmap fromIntegral delta))
    [SDL.ButtonMiddle] -> Just (CamDistance (0.01 * fromIntegral dy))
    _ -> Nothing
eventToAction _ SDL.Event
  { SDL.eventPayload = SDL.MouseWheelEvent SDL.MouseWheelEventData
    { SDL.mouseWheelEventPos = V2 _ dy }
  } = Just (CamDistance (-fromIntegral dy))
eventToAction InputMap {keyboardMap} SDL.Event
  { SDL.eventPayload = SDL.KeyboardEvent SDL.KeyboardEventData
    { SDL.keyboardEventKeysym = SDL.Keysym {SDL.keysymScancode}
    , SDL.keyboardEventKeyMotion = SDL.Pressed
    , SDL.keyboardEventRepeat
    }
  } = do
    action <- Map.lookup keysymScancode keyboardMap
    guard (isRepeating action || not keyboardEventRepeat)
    return action
eventToAction _ SDL.Event
  { SDL.eventPayload = SDL.DropEvent SDL.DropEventData
    { SDL.dropEventFile = dropData }
  } = Just (FileLoad (fromCString dropData))
eventToAction _ SDL.Event {SDL.eventPayload = SDL.QuitEvent} = Just Quit
eventToAction _ _ = Nothing

-- TODO: Probably integrate with dt somewhere else.
keysToActions :: Double -> ModState -> (SDL.Scancode -> Bool) -> [AppAction]
keysToActions dt (ModState False False False) keyState =
  concat
    [ [CamRotate (dt *^ V2 200 0) | keyState SDL.ScancodeLeft]
    , [CamRotate (dt *^ V2 (-200) 0) | keyState SDL.ScancodeRight]
    , [CamRotate (dt *^ V2 0 200) | keyState SDL.ScancodeUp]
    , [CamRotate (dt *^ V2 0 (-200)) | keyState SDL.ScancodeDown]
    , [CamMove (dt *^ V3 0 0 (-5)) | keyState SDL.ScancodePageDown]
    , [CamMove (dt *^ V3 0 0 5) | keyState SDL.ScancodePageUp]
    ]
keysToActions _ _ _ = []

handleAction ::
     (MonadIO m, MonadState AppState m, MonadError () m) => AppAction -> m ()
handleAction (CamDistance d) = appViewStateL . viewCamDistanceVelL += d
handleAction (CamRotate d) = appViewStateL . viewCamAngleVelL += d
handleAction (CamMove d) = appViewStateL . viewSamplePosL += d
handleAction PauseToggle = appPausedL %= not
handleAction ShaderReload =
  modifyingM (appRenderStateL . renderStateShaderL) reloadShader
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
