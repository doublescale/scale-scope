{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module InputMap
  ( InputMap(..)
  , defaultInputMap
  , readInputMap
  ) where

import Data.Aeson
  ( FromJSONKey
  , FromJSONKeyFunction(FromJSONKeyTextParser)
  , fromJSONKey
  )
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
import ReadScancode (readScancode)

data InputMap = InputMap
  { mouseMotionMap :: Map SDL.MouseButton (V2 (Maybe AppAction))
  , mouseWheelMap :: V2 (Maybe AppAction)
  , keyboardMap :: Map SDL.Scancode AppAction
  } deriving (Generic, Show)

instance FromJSON InputMap where
  parseJSON =
    withObject "InputMap" $ \o -> do
      mouseDragObject <- Map.mapKeys unwrapMouseButton <$> o .: "MouseDrag"
      mouseMotionMap <- traverse parse2DAction mouseDragObject
      mouseWheelMap <- parse2DAction =<< o .: "MouseWheel"
      keyboardMap <- Map.mapKeys unwrapScancode <$> o .: "Keyboard"
      return InputMap {mouseMotionMap, mouseWheelMap, keyboardMap}

parse2DAction :: Object -> Parser (V2 (Maybe AppAction))
parse2DAction o = V2 <$> o .:? "x" <*> o .:? "y"

newtype WrapMouseButton = WrapMouseButton
  { unwrapMouseButton :: SDL.MouseButton
  } deriving (Eq, Ord)

instance FromJSON WrapMouseButton where
  parseJSON =
    fmap WrapMouseButton . withText "SDL.MouseButton" parseSdlMouseButton

instance FromJSONKey WrapMouseButton where
  fromJSONKey =
    FromJSONKeyTextParser (fmap WrapMouseButton . parseSdlMouseButton)

parseSdlMouseButton :: Text -> Parser SDL.MouseButton
parseSdlMouseButton (unpack -> s) =
  case readMaybe s of
    Nothing -> fail ("Invalid mouse button: " ++ s)
    Just mb -> return mb

newtype WrapScancode = WrapScancode
  { unwrapScancode :: SDL.Scancode
  } deriving (Eq, Ord)

instance FromJSON WrapScancode where
  parseJSON = fmap WrapScancode . withText "SDL.Scancode" parseSdlScancode

instance FromJSONKey WrapScancode where
  fromJSONKey = FromJSONKeyTextParser (fmap WrapScancode . parseSdlScancode)

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
        [ ( SDL.ButtonLeft
          , V2 (Just (CamRotate (V2 0.5 0))) (Just (CamRotate (V2 0 0.5))))
        , (SDL.ButtonMiddle, V2 Nothing (Just (CamDistance 0.01)))
        ]
  , mouseWheelMap = V2 Nothing (Just (CamDistance (-1)))
  , keyboardMap =
      Map.fromList
        [ (SDL.ScancodeQ, Quit)
        , (SDL.ScancodeEscape, Quit)
        , (SDL.ScancodePageUp, CamMove (V3 0 0 5))
        , (SDL.ScancodePageDown, CamMove (V3 0 0 (-5)))
        , (SDL.ScancodeP, PauseToggle)
        , (SDL.ScancodeSpace, PauseToggle)
        , (SDL.ScancodeJ, FrameSkip (-1))
        , (SDL.ScancodeK, FrameSkip 1)
        , (SDL.ScancodeU, SpeedMultiply (recip 1.125))
        , (SDL.ScancodeI, SpeedMultiply 1.125)
        , (SDL.ScancodeBackspace, SpeedReset)
        , (SDL.ScancodeO, SpeedMultiply (-1))
        , (SDL.ScancodeF5, ShaderReload)
        , (SDL.ScancodeF11, FullscreenToggle)
        ]
  }

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
