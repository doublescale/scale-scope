{-# LANGUAGE ViewPatterns #-}

module Event
  ( eventLoop
  , loadPath
  ) where

import qualified Codec.Compression.Zlib as Zlib
import Control.Lens ((%=), (*=), (+=), (.=), (<~), (^.), assign, view)
import Control.Monad (forever, guard, unless)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, get, gets, modify)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Store as Store
import Foreign (alloca, peek)
import GHC.Float (float2Double)
import qualified Graphics.Rendering.OpenGL.GL as GL
import Linear
  ( V2(V2)
  , V4(V4)
  , (!*)
  , (!*!)
  , (*!)
  , (*^)
  , (^/)
  , _xyz
  , _y
  , inv44
  , normalizePoint
  , vector
  )
import qualified SDL

import Action (AppAction(..), isContinuous, isRepeating, scaleAction)
import AppState
import Event.ModState (ModState, fromKeyModifier)
import InputMap (InputMap(..), Modified(..), Scroll(..), readInputMap)
import Mesh (AnimationData(..))
import Render (RenderState(..), buildModelMatrix, buildViewMatrix, render)
import Render.Mesh (deleteMeshSequence, loadMeshSequence)
import Render.Shader (reloadShader)
import Render.Types (renderStateMeshesL, renderStateShaderL)
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

loadPath :: (MonadIO m, MonadState AppState m) => FilePath -> m ()
loadPath path = do
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
eventToActions InputMap {mouseWheelMap} mods SDL.Event
  { SDL.eventPayload = SDL.MouseWheelEvent SDL.MouseWheelEventData
    { SDL.mouseWheelEventPos = fmap fromIntegral -> vec }
  } = concat $ do
    xyMaybeActions <- Map.lookup (Modified mods Scroll) mouseWheelMap
    return (catMaybes (toList (fmap . scaleAction <$> vec <*> xyMaybeActions)))
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
handleAction (CamMoveCam d) = do
  mat <- buildModelMatrix <$> gets appViewState
  appViewStateL . viewSamplePosL += view _xyz (vector d *! mat)
handleAction (CamMoveWorld d) = appViewStateL . viewSamplePosL += d
handleAction CamFocusCursor = do
  winSize <- gets appWinSize
  let V2 w h = fromIntegral <$> winSize
  SDL.P (V2 x y) <- fmap fromIntegral <$> liftIO SDL.getAbsoluteMouseLocation
  depth :: GL.GLfloat <-
    liftIO $
    alloca
      (\ptr -> do
         GL.readPixels
           (GL.Position x (h - y))
           (GL.Size 1 1)
           (GL.PixelData GL.DepthComponent GL.Float ptr)
         peek ptr)
  unless (depth == 1) $ do
    modelMat <- buildModelMatrix <$> gets appViewState
    let viewMat = buildViewMatrix winSize
    let screenSpace :: V4 Double =
          V4
            (2 * fromIntegral x / fromIntegral w - 1)
            (1 - 2 * fromIntegral y / fromIntegral h)
            (2 * float2Double depth - 1)
            1
    let worldSpace = inv44 (viewMat !*! modelMat) !* screenSpace
    appViewStateL . viewSamplePosL .= normalizePoint worldSpace
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
handleAction (SpeedSet s) = appFrameRateFactorL .= s
handleAction (SpeedMultiply d) = appFrameRateFactorL *= d
handleAction (FileLoad path) = loadPath path
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
    posFactor = 20
    pitchRestitution =
      let p = viewCamAngle ^. _y
      in min 0 (90 - p) - min 0 (p + 90)
    rateFactor = 10
    effectiveFrameRate =
      if appPaused
        then 0
        else appFrameRate * appFrameRateFactor
