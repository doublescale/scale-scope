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
      Map.fromList
        [ (SDL.ScancodeF5, ShaderReload)
        , (SDL.ScancodeF11, FullscreenToggle)
        , (SDL.ScancodeJ, FrameSkip (-1))
        , (SDL.ScancodeComma, FrameSkip (-1))
        , (SDL.ScancodeK, FrameSkip 1)
        , (SDL.ScancodePeriod, FrameSkip 1)
        , (SDL.ScancodeBackspace, SpeedReset)
        , (SDL.ScancodeLeftBracket, SpeedMultiply (recip 1.125))
        , (SDL.ScancodeU, SpeedMultiply (recip 1.125))
        , (SDL.ScancodeRightBracket, SpeedMultiply 1.125)
        , (SDL.ScancodeI, SpeedMultiply 1.125)
        , (SDL.ScancodeBackslash, SpeedMultiply (-1))
        , (SDL.ScancodeO, SpeedMultiply (-1))
        , (SDL.ScancodeSpace, PauseToggle)
        , (SDL.ScancodeP, PauseToggle)
        , (SDL.ScancodeQ, Quit)
        , (SDL.ScancodeEscape, Quit)
        ]
  }