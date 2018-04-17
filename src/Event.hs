{-# LANGUAGE ViewPatterns #-}

module Event
  ( eventLoop
  , loadPaths
  ) where

import qualified Codec.Compression.Zlib as Zlib
import Control.Lens ((%=), (*=), (+=), (.=), (<~), (^.), assign)
import Control.Monad (forever, guard)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, get, gets, modify)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Store as Store
import Linear (V2(V2), (*^), (^/), _y)
import qualified SDL

import Action (AppAction(..), isContinuous, isRepeating, scaleAction)
import AppState
import Event.ModState (ModState, fromKeyModifier)
import InputMap (InputMap(..), Modified(..), readInputMap)
import Mesh (AnimationData(..))
import Render (RenderState(..), render)
import Render.Mesh (deleteMeshSequence, loadMeshSequence)
import Render.Shader (reloadShader)
import Render.Types (initRenderState, renderStateMeshesL, renderStateShaderL)
import Util (fromCString, modifyingM)

eventLoop :: (MonadIO m, MonadState AppState m, MonadError () m) => m ()
eventLoop =
  forever $ do
    inputMap <- gets appInputMap
    modState <- fromKeyModifier <$> SDL.getModState
    eventActions <-
      concatMap (eventToActions inputMap modState) <$> SDL.pollEvents
    currentTime <- SDL.time
    previousTime <- gets appTimePrev
    let dt = currentTime - previousTime
    continuousActions <- map (scaleAction dt) .
      keysToActions inputMap modState <$> SDL.getKeyboardState
    mapM_ handleAction (eventActions ++ continuousActions)
    handleMouseMode inputMap modState
    win <- gets appWindow
    appWinSizeL <~ fmap fromIntegral <$> SDL.glGetDrawableSize win
    modify (integrateState dt)
    render =<< get
    SDL.glSwapWindow win

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

handleMouseMode :: MonadIO m => InputMap -> ModState -> m ()
handleMouseMode InputMap {mouseMotionMap} mods =
  liftIO $ do
    isPressed <- SDL.getMouseButtons
    _ <-
      SDL.setMouseLocationMode
        (if any isPressed mappedButtonsForCurrentMods
           then SDL.RelativeLocation
           else SDL.AbsoluteLocation)
    return ()
  where
    mappedButtonsForCurrentMods =
      map modKey . filter ((== mods) . modState) $ Map.keys mouseMotionMap

eventToActions :: InputMap -> ModState -> SDL.Event -> [AppAction]
eventToActions InputMap {mouseMotionMap} mods SDL.Event
  { SDL.eventPayload = SDL.MouseMotionEvent SDL.MouseMotionEventData
    { SDL.mouseMotionEventRelMotion = fmap fromIntegral -> vec
    , SDL.mouseMotionEventState = [mouseButton]
    }
  } = concat $ do
    xyMaybeActions <- Map.lookup (Modified mods mouseButton) mouseMotionMap
    return (catMaybes (toList (fmap . scaleAction <$> vec <*> xyMaybeActions)))
eventToActions InputMap {mouseClickMap} mods SDL.Event
  { SDL.eventPayload = SDL.MouseButtonEvent SDL.MouseButtonEventData
    { SDL.mouseButtonEventButton = mouseButton
    , SDL.mouseButtonEventMotion = SDL.Pressed
    }
  } = toList (Map.lookup (Modified mods mouseButton) mouseClickMap)
eventToActions InputMap {mouseWheelMap} _ SDL.Event
  { SDL.eventPayload = SDL.MouseWheelEvent SDL.MouseWheelEventData
    { SDL.mouseWheelEventPos = fmap fromIntegral -> vec }
  } = catMaybes (toList (fmap . scaleAction <$> vec <*> mouseWheelMap))
eventToActions InputMap {keyboardMap} mods SDL.Event
  { SDL.eventPayload = SDL.KeyboardEvent SDL.KeyboardEventData
    { SDL.keyboardEventKeysym = SDL.Keysym {SDL.keysymScancode}
    , SDL.keyboardEventKeyMotion = SDL.Pressed
    , SDL.keyboardEventRepeat
    }
  } = toList $ do
    action <- Map.lookup (Modified mods keysymScancode) keyboardMap
    guard (not (isContinuous action))
    guard (isRepeating action || not keyboardEventRepeat)
    return action
eventToActions _ _ SDL.Event
  { SDL.eventPayload = SDL.DropEvent SDL.DropEventData
    { SDL.dropEventFile = dropData }
  } = [FileLoad (fromCString dropData)]
eventToActions _ _ SDL.Event {SDL.eventPayload = SDL.QuitEvent} = [Quit]
eventToActions _ _ _ = []

keysToActions :: InputMap -> ModState -> (SDL.Scancode -> Bool) -> [AppAction]
keysToActions InputMap {keyboardMap} modState keyState =
  Map.elems (Map.filterWithKey isContinuousAndActive keyboardMap)
  where
    isContinuousAndActive (Modified mods key) action =
      isContinuous action && mods == modState && keyState key

handleAction ::
     (MonadIO m, MonadState AppState m, MonadError () m) => AppAction -> m ()
handleAction (CamDistance d) = appViewStateL . viewCamDistanceVelL += d
handleAction (CamRotate d) = appViewStateL . viewCamAngleVelL += d
handleAction (CamMove d) = appViewStateL . viewSamplePosL += d
handleAction PauseToggle = appPausedL %= not
handleAction ShaderReload =
  modifyingM (appRenderStateL . renderStateShaderL) reloadShader
handleAction InputMapReload = liftIO readInputMap >>= assign appInputMapL
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
handleAction (FileLoad path) = loadPaths [path]
handleAction Quit = throwError ()

integrateState :: Double -> AppState -> AppState
integrateState dt st@AppState {appViewState = vs@ViewState {..}, ..} =
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
  , appTimePrev = appTimePrev + dt
  , appFrameRateInterp =
      (appFrameRateInterp + dt * rateFactor * effectiveFrameRate) /
      (1 + dt * rateFactor)
  , appFrame = appFrame + dt * appFrameRateInterp
  }
  where
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
