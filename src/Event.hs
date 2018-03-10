module Event
  ( loadPaths
  , reloadShader
  , eventLoop
  ) where

import qualified Codec.Compression.Zlib as Zlib
import Control.Lens ((%=), (*=), (+=), (-=), (.=), (//=), (^.))
import Control.Monad (forever)
import Control.Monad.Except (ExceptT, MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, StateT, get, gets, modify)
import Data.Bool (bool)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Serialize as Cereal
import Foreign.C.String (peekCString)
import Linear (V2(V2), (*^), (^/), _y)
import qualified SDL

import AppState
import Mesh (AnimationData(..))
import Render (RenderState(..), render)
import Render.Mesh (deleteMeshSequence, loadMeshSequence)
import Render.Shader (loadShader)
import Render.Types (initRenderState)
import SerializeHalf ()

loadPaths ::
     (MonadIO m, MonadState (AppState RenderState) m) => [FilePath] -> m ()
loadPaths [] = do
  deleteMeshSequence =<< gets (renderStateMeshes . appRenderState)
  modify (\st -> st {appRenderState = initRenderState})
loadPaths [path] = do
  deleteMeshSequence =<< gets (renderStateMeshes . appRenderState)
  animData <- loadAnimationFile path
  newStateMesh <- loadMeshSequence animData
  modify
    (\st ->
       st
       { appRenderState = (appRenderState st) {renderStateMeshes = newStateMesh}
       , appFrameRate = animationFramerate animData
       })
  where
    loadAnimationFile file =
      liftIO $ do
        blob <- BSL.readFile file
        let Right ad = Cereal.decodeLazy (Zlib.decompress blob)
        return ad
loadPaths _ = liftIO (putStrLn "Need exactly one file argument.")

reloadShader :: (MonadIO m, MonadState (AppState RenderState) m) => m ()
reloadShader = do
  oldStateShader <- gets (renderStateShader . appRenderState)
  newStateShader <- loadShader oldStateShader
  modify
    (\st ->
       st
       { appRenderState =
           (appRenderState st) {renderStateShader = newStateShader}
       })

eventLoop :: SDL.Window -> ExceptT () (StateT (AppState RenderState) IO) ()
eventLoop win = forever $ do
  events <- SDL.pollEvents
  mapM_ (handleEvent win) events
  winSize <- fmap fromIntegral <$> SDL.glGetDrawableSize win
  appWinSizeL .= winSize
  time <- SDL.time
  timePrev <- gets appTimePrev
  let dt = time - timePrev
  frameRateInterp <- gets appFrameRateInterp
  appFrameL += dt * frameRateInterp
  modify (integrateState dt)
  pressedButtons <- SDL.getMouseButtons
  _ <- SDL.setMouseLocationMode
    (bool SDL.AbsoluteLocation SDL.RelativeLocation
     (any pressedButtons [SDL.ButtonLeft, SDL.ButtonMiddle]))
  render =<< get
  SDL.glSwapWindow win

-- TODO: Put Window in AppState
handleEvent ::
     (MonadIO m, MonadState (AppState RenderState) m, MonadError () m)
  => SDL.Window
  -> SDL.Event
  -> m ()
handleEvent _ SDL.Event
  { SDL.eventPayload = SDL.MouseMotionEvent
    SDL.MouseMotionEventData
    { SDL.mouseMotionEventRelMotion = delta @ (V2 _ dy)
    , SDL.mouseMotionEventState = buttons
    }
  } = case buttons of
    [SDL.ButtonLeft] ->
      appViewStateL . viewCamAngleVelL += 0.5 * fmap fromIntegral delta
    [SDL.ButtonMiddle] ->
      appViewStateL . viewCamDistanceVelL += 0.01 * fromIntegral dy
    _ -> return ()
handleEvent _ SDL.Event
  { SDL.eventPayload = SDL.MouseWheelEvent
    SDL.MouseWheelEventData {SDL.mouseWheelEventPos = V2 _ dy}
  } = appViewStateL . viewCamDistanceVelL -= fromIntegral dy
handleEvent win SDL.Event
  { SDL.eventPayload = SDL.KeyboardEvent SDL.KeyboardEventData
    { SDL.keyboardEventKeysym = SDL.Keysym {SDL.keysymKeycode}
    , SDL.keyboardEventKeyMotion = SDL.Pressed
    , SDL.keyboardEventRepeat
    }
  } = case (keysymKeycode, keyboardEventRepeat) of
    (SDL.KeycodeP, False) -> appPausedL %= not
    (SDL.KeycodeF5, False) -> reloadShader
    (SDL.KeycodeF11, False) -> do
      SDL.WindowConfig {windowMode} <- SDL.getWindowConfig win
      case windowMode of
        SDL.Windowed -> SDL.setWindowMode win SDL.FullscreenDesktop
        _ -> SDL.setWindowMode win SDL.Windowed
    (SDL.KeycodePeriod, _) -> appFrameL += 1
    (SDL.KeycodeComma, _) -> appFrameL -= 1
    (SDL.KeycodeBackspace, _) -> appFrameRateFactorL .= 1
    (SDL.KeycodeLeftBracket, _) -> appFrameRateFactorL //= 1.125
    (SDL.KeycodeRightBracket, _) -> appFrameRateFactorL *= 1.125
    (SDL.KeycodeBackslash, _) -> appFrameRateFactorL %= negate
    (SDL.KeycodeEscape, _) -> throwError ()
    _ -> return ()
handleEvent _
  SDL.Event
  { SDL.eventPayload =
    SDL.DropEvent SDL.DropEventData {SDL.dropEventFile = dropData}
  }
  = liftIO (peekCString dropData) >>= \path -> loadPaths [path]
handleEvent _ SDL.Event {SDL.eventPayload = SDL.QuitEvent} = throwError ()
handleEvent _ _ = return ()

integrateState :: Double -> AppState rs -> AppState rs
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
