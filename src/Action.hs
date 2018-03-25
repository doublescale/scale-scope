module Action
  ( AppAction(..)
  , isRepeating
  , isContinuous
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

isRepeating :: AppAction -> Bool
isRepeating FullscreenToggle = False
isRepeating (FileLoad _) = False
isRepeating ShaderReload = False
isRepeating Quit = False
isRepeating _ = True

isContinuous :: AppAction -> Bool
isContinuous (CamDistance _) = True
isContinuous (CamRotate _) = True
isContinuous (CamMove _) = True
isContinuous _ = False

-- scaleAction?  Like fmap but only for continuous actions?
