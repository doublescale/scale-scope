module InputMap
  ( InputMap(..)
  , defaultInputMap
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import qualified SDL

import Action

data InputMap = InputMap
  { keyboardMap :: Map SDL.Scancode AppAction
  } deriving (Generic, Show)

defaultInputMap :: InputMap
defaultInputMap =
  InputMap
  { keyboardMap =
      Map.fromList [(SDL.ScancodeP, PauseToggle), (SDL.ScancodeQ, Quit)]
  }
