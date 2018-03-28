{-# LANGUAGE OverloadedStrings #-}

module InputMap
  ( InputMap(..)
  , defaultInputMap
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Yaml (FromJSON, Object, Parser, (.:), (.:?), parseJSON, withObject)
import GHC.Generics (Generic)
import Linear (V2(V2))
import qualified SDL

import Action
import ReadScancode (readScancode)

data InputMap = InputMap
  { mouseMotionMap :: Map SDL.MouseButton (V2 (Maybe AppAction))
  , keyboardMap :: Map SDL.Scancode AppAction
  } deriving (Generic, Show)

instance FromJSON InputMap where
  parseJSON =
    withObject "InputMap" $ \o -> do
      mouseDragObject <- o .: "MouseDrag"
      mouseMotionMap <-
        Map.traverseWithKey
          (const parse2DAction)
          (Map.mapKeys read mouseDragObject)
      -- TODO: Handle Nothing, failed read.
      keyboardMap <- Map.mapKeys (fromJust . readScancode) <$> o .: "Keyboard"
      return InputMap {mouseMotionMap, keyboardMap}

parse2DAction :: Object -> Parser (V2 (Maybe AppAction))
parse2DAction o = V2 <$> o .:? "x" <*> o .:? "y"

defaultInputMap :: InputMap
defaultInputMap =
  InputMap
  { mouseMotionMap =
      Map.fromList
        [ ( SDL.ButtonLeft
          , V2 (Just (CamRotate (V2 0.5 0))) (Just (CamRotate (V2 0 0.5))))
        , (SDL.ButtonMiddle, V2 Nothing (Just (CamDistance 0.01)))
        ]
  , keyboardMap =
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
