module Action
  ( AppAction(..)
  ) where

import Data.Yaml (FromJSON, parseJSON)
import Linear (V2, V3)

type Scalar = Double

data AppAction
  = CamDistance Scalar
  | CamRotate (V2 Scalar)
  | CamMove (V3 Scalar)
  | FullscreenToggle
  | PauseToggle
  | FrameSkip Scalar
  | SpeedMultiply Scalar
  | SpeedReset
  | FileLoad FilePath
  | ShaderReload
  | Quit
  deriving (Read, Show)

instance FromJSON AppAction where
  parseJSON = fmap read . parseJSON
