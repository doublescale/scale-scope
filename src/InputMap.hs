{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module InputMap
  ( InputMap(..)
  , Modified(..)
  , Scroll(..)
  , defaultInputMap
  , readInputMap
  ) where

import Data.Aeson
  ( FromJSON
  , FromJSONKey
  , FromJSONKeyFunction(FromJSONKeyTextParser)
  , (.:)
  , (.:?)
  , fromJSONKey
  , fromJSONKeyList
  , parseJSON
  , withObject
  )
import qualified Data.Attoparsec.Text as P
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Semigroup ((<>))
import Data.Text (Text, unpack)
import Data.Yaml (Parser, decodeFileEither)
import GHC.Generics (Generic)
import Linear (V2(V2), V3(V3))
import qualified SDL
import Text.Read (readMaybe)

import Action
import Event.ModState (ModState, emptyModState)
import ParseModState (parseModifiers)
import ReadScancode (readScancode)

data Modified a = Modified
  { modState :: ModState
  , modKey :: a
  } deriving (Eq, Ord, Show)

data InputMap = InputMap
  { mouseMotionMap :: Map (Modified SDL.MouseButton) (V2 (Maybe AppAction))
  , mouseClickMap :: Map (Modified SDL.MouseButton) AppAction
  , mouseWheelMap :: Map (Modified Scroll) (V2 (Maybe AppAction))
  , keyboardMap :: Map (Modified SDL.Scancode) AppAction
  } deriving (Generic, Show)

instance FromJSON InputMap where
  parseJSON =
    withObject "InputMap" $ \o -> do
      mouseMotionMap <- Map.map unXYMaybe <$> o .: "MouseDrag"
      mouseClickMap <- o .: "MouseClick"
      mouseWheelMap <- Map.map unXYMaybe <$> o .: "MouseWheel"
      keyboardMap <- o .: "Keyboard"
      return InputMap {..}

newtype XYMaybe a = XYMaybe { unXYMaybe :: V2 (Maybe a) }

instance FromJSON a => FromJSON (XYMaybe a) where
  parseJSON =
    withObject "2D action" (\o -> XYMaybe <$> (V2 <$> o .:? "x" <*> o .:? "y"))

parseModified :: (Text -> Parser a) -> (Text -> Parser (Modified a))
parseModified parser input =
  case P.parseOnly ((,) <$> parseModifiers <*> P.takeText) input of
    Left err -> fail err
    Right (mods, rest) -> Modified mods <$> parser rest

class Parsable a where
  parsinate :: Text -> Parser a

instance Parsable a => FromJSONKey (Modified a) where
  fromJSONKey = FromJSONKeyTextParser (parseModified parsinate)
  fromJSONKeyList = FromJSONKeyTextParser (const (fail "unexpected key list"))

instance Parsable SDL.MouseButton where
  parsinate (unpack -> s) =
    case readMaybe s of
      Nothing -> fail ("Invalid mouse button: " ++ s)
      Just mb -> return mb

instance Parsable SDL.Scancode where
  parsinate s =
    case readScancode s of
      Nothing -> fail (unpack ("Invalid key: " <> s))
      Just sc -> return sc

data Scroll = Scroll deriving (Eq, Ord, Show)

instance Parsable Scroll where
  parsinate "Scroll" = return Scroll
  parsinate _ = fail "Expected \"Scroll\""

defaultInputMap :: InputMap
defaultInputMap =
  InputMap
  { mouseMotionMap =
      Map.fromList
        [ ( unmodified SDL.ButtonLeft
          , V2 (Just (CamRotate (V2 0.5 0))) (Just (CamRotate (V2 0 0.5))))
        , (unmodified SDL.ButtonMiddle, V2 Nothing (Just (CamDistance 0.01)))
        ]
  , mouseClickMap = Map.empty
  , mouseWheelMap =
      Map.singleton (unmodified Scroll) (V2 Nothing (Just (CamDistance (-1))))
  , keyboardMap =
      Map.fromList
        [ (unmodified SDL.ScancodeQ, Quit)
        , (unmodified SDL.ScancodeEscape, Quit)
        , (unmodified SDL.ScancodePageUp, CamMove (V3 0 0 5))
        , (unmodified SDL.ScancodePageDown, CamMove (V3 0 0 (-5)))
        , (unmodified SDL.ScancodeP, PauseToggle)
        , (unmodified SDL.ScancodeSpace, PauseToggle)
        , (unmodified SDL.ScancodeJ, FrameSkip (-1))
        , (unmodified SDL.ScancodeK, FrameSkip 1)
        , (unmodified SDL.ScancodeU, SpeedMultiply (recip 1.125))
        , (unmodified SDL.ScancodeI, SpeedMultiply 1.125)
        , (unmodified SDL.ScancodeBackspace, SpeedSet 1)
        , (unmodified SDL.ScancodeO, SpeedMultiply (-1))
        , (unmodified SDL.ScancodeF5, ShaderReload)
        , (unmodified SDL.ScancodeF11, FullscreenToggle)
        ]
  }
  where
    unmodified k = Modified emptyModState k

readInputMap :: IO InputMap
readInputMap =
  decodeFileEither filepath >>= \case
    Left err -> do
      print err
      putStrLn "Using default input map."
      return defaultInputMap
    Right inputMap -> do
      putStrLn ("Loading " ++ show filepath)
      return inputMap
  where
    filepath = "inputmap.yaml"
