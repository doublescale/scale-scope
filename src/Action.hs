module Action
  ( AppAction(..)
  , isRepeating
  , isContinuous
  , scaleAction
  ) where

import Data.Text (unpack)
import Data.Yaml (FromJSON, Parser, parseJSON, withText)
import Linear (V2, V3, (*^))
import Text.Read (readMaybe)

type Scalar = Double

data AppAction
  = CamDistance Scalar
  | CamRotate (V2 Scalar)
  | CamMoveCam (V3 Scalar)
  | CamMoveWorld (V3 Scalar)
  | CamFocusCursor
  | FullscreenToggle
  | PauseToggle
  | FrameSkip Scalar
  | SpeedMultiply Scalar
  | SpeedSet Scalar
  | FileLoad FilePath
  | ShaderReload
  | InputMapReload
  | Quit
  deriving (Read, Show)

instance FromJSON AppAction where
  parseJSON = withText "AppAction" (parseAction . unpack)

parseAction :: String -> Parser AppAction
parseAction s =
  case readMaybe s of
    Nothing -> fail ("Invalid AppAction: " ++ s)
    Just a -> return a

isRepeating :: AppAction -> Bool
isRepeating PauseToggle = True
isRepeating (FrameSkip _) = True
isRepeating (SpeedMultiply _) = True
isRepeating _ = False

isContinuous :: AppAction -> Bool
isContinuous (CamDistance _) = True
isContinuous (CamRotate _) = True
isContinuous (CamMoveCam _) = True
isContinuous (CamMoveWorld _) = True
isContinuous _ = False

scaleAction :: Scalar -> AppAction -> AppAction
scaleAction s (CamDistance x) = CamDistance (s * x)
scaleAction s (CamRotate x) = CamRotate (s *^ x)
scaleAction s (CamMoveCam x) = CamMoveCam (s *^ x)
scaleAction s (CamMoveWorld x) = CamMoveWorld (s *^ x)
scaleAction s (FrameSkip x) = FrameSkip (s * x)
scaleAction s (SpeedMultiply x) = SpeedMultiply (x ** s)
scaleAction _ a = a
