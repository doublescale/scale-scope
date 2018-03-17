module Event.ModState
  ( ModState(..)
  , fromKeyModifier
  ) where

import qualified SDL

data ModState = ModState
  { modShift, modCtrl, modAlt :: Bool
  } deriving (Eq, Show)

fromKeyModifier :: SDL.KeyModifier -> ModState
fromKeyModifier SDL.KeyModifier {..} =
  ModState
  { modShift = keyModifierLeftShift || keyModifierRightShift
  , modCtrl = keyModifierLeftCtrl || keyModifierRightCtrl
  , modAlt = keyModifierLeftAlt || keyModifierRightAlt
  }
