module Action
  ( AppAction(..)
  ) where

import Linear (V2)

type Scalar = Double

data AppAction
  = CamDistance Scalar
  | CamRotate (V2 Scalar)
  | FullscreenToggle
  | PauseToggle
  | FrameSkip Scalar
  | SpeedMultiply Scalar
  | SpeedReset
  | FileLoad FilePath
  | ShaderReload
  | Quit
