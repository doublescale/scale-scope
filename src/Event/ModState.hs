module Event.ModState
  ( ModState(..)
  , emptyModState
  , orModState
  , fromKeyModifier
  ) where

import qualified SDL

data ModState = ModState
  { modShift, modCtrl, modAlt :: Bool
  } deriving (Eq, Ord, Show)

emptyModState :: ModState
emptyModState = ModState False False False

orModState :: ModState -> ModState -> ModState
orModState (ModState a1 b1 c1) (ModState a2 b2 c2) =
  ModState (a1 || a2) (b1 || b2) (c1 || c2)

fromKeyModifier :: SDL.KeyModifier -> ModState
fromKeyModifier SDL.KeyModifier {..} =
  ModState
  { modShift = keyModifierLeftShift || keyModifierRightShift
  , modCtrl = keyModifierLeftCtrl || keyModifierRightCtrl
  , modAlt = keyModifierLeftAlt || keyModifierRightAlt
  }
