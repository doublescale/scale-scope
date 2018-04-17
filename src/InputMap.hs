{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module InputMap
  ( InputMap(..)
  , Modified(..)
  , defaultInputMap
  , readInputMap
  ) where

import Data.Aeson
  ( FromJSONKey
  , FromJSONKeyFunction(FromJSONKeyTextParser)
  , fromJSONKey
  )
import qualified Data.Attoparsec.Text as P
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Semigroup ((<>))
import Data.Text (Text, unpack)
import Data.Yaml
  ( FromJSON
  , Object
  , Parser
  , (.:)
  , (.:?)
  , decodeFileEither
  , parseJSON
  , withObject
  , withText
  )
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
  , mouseWheelMap :: V2 (Maybe AppAction)
  , keyboardMap :: Map (Modified SDL.Scancode) AppAction
  } deriving (Generic, Show)

instance FromJSON InputMap where
  parseJSON =
    withObject "InputMap" $ \o -> do
      mouseDragObject <- Map.mapKeys unwrapMouseButton <$> o .: "MouseDrag"
      mouseMotionMap <- traverse parse2DAction mouseDragObject
      mouseClickMap <- Map.mapKeys unwrapMouseButton <$> o .: "MouseClick"
      mouseWheelMap <- parse2DAction =<< o .: "MouseWheel"
      keyboardMap <- Map.mapKeys unwrapScancode <$> o .: "Keyboard"
      return InputMap {..}

parseModified :: (Text -> Parser a) -> (Text -> Parser (Modified a))
parseModified parser input =
  case P.parseOnly ((,) <$> parseModifiers <*> P.takeText) input of
    Left err -> fail err
    Right (mods, rest) -> do
      key <- parser rest
      return (Modified mods key)

parse2DAction :: Object -> Parser (V2 (Maybe AppAction))
parse2DAction o = V2 <$> o .:? "x" <*> o .:? "y"

newtype WrapMouseButton = WrapMouseButton
  { unwrapMouseButton :: Modified SDL.MouseButton
  } deriving (Eq, Ord)

instance FromJSON WrapMouseButton where
  parseJSON =
    fmap WrapMouseButton .
    withText "SDL.MouseButton" (parseModified parseSdlMouseButton)

instance FromJSONKey WrapMouseButton where
  fromJSONKey =
    FromJSONKeyTextParser
      (fmap WrapMouseButton . parseModified parseSdlMouseButton)

parseSdlMouseButton :: Text -> Parser SDL.MouseButton
parseSdlMouseButton (unpack -> s) =
  case readMaybe s of
    Nothing -> fail ("Invalid mouse button: " ++ s)
    Just mb -> return mb

newtype WrapScancode = WrapScancode
  { unwrapScancode :: Modified SDL.Scancode
  } deriving (Eq, Ord)

instance FromJSON WrapScancode where
  parseJSON =
    fmap WrapScancode . withText "SDL.Scancode" (parseModified parseSdlScancode)

instance FromJSONKey WrapScancode where
  fromJSONKey =
    FromJSONKeyTextParser (fmap WrapScancode . parseModified parseSdlScancode)

parseSdlScancode :: Text -> Parser SDL.Scancode
parseSdlScancode s =
  case readScancode s of
    Nothing -> fail (unpack ("Invalid key: " <> s))
    Just sc -> return sc

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
  , mouseWheelMap = V2 Nothing (Just (CamDistance (-1)))
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
        , (unmodified SDL.ScancodeBackspace, SpeedReset)
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
