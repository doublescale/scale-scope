module Action
  ( AppAction(..)
  , isRepeating
  , isContinuous
  , scaleAction
  ) where

import Data.Yaml (FromJSON, parseJSON)
import Linear (V2, V3, (*^))

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

scaleAction :: Scalar -> AppAction -> AppAction
scaleAction s (CamDistance x) = CamDistance (s * x)
scaleAction s (CamRotate x) = CamRotate (s *^ x)
scaleAction s (CamMove x) = CamMove (s *^ x)
scaleAction s (FrameSkip x) = FrameSkip (s * x)
scaleAction s (SpeedMultiply x) = SpeedMultiply (x ** s)
scaleAction _ a = a
