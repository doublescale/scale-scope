{-# LANGUAGE OverloadedStrings #-}

module OBJParser
  ( parseOBJ
  , OBJLine(..)
  , FaceEntry(..)
  ) where

import Control.Applicative ((<|>), liftA2, liftA3)
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Either (rights)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Linear (V2(V2), V3(V3))

data OBJLine
  = PosLine (V3 Double)
  | UvLine (V2 Double)
  | FaceLine (V.Vector FaceEntry)
  deriving (Generic, Show)

data FaceEntry = FaceEntry
  { posIdx :: Int
  , uvIdx :: Maybe Int
  } deriving (Generic, Show)

parseOBJ :: BSL.ByteString -> [OBJLine]
parseOBJ = rights . map (P.parseOnly objLine . BSL.toStrict) . BSL.lines

objLine :: P.Parser OBJLine
objLine = P.skipSpace *> P.choice [posLine, uvLine, faceLine]

posLine :: P.Parser OBJLine
posLine = "v" *> (PosLine <$> v3 (tok P.double))

uvLine :: P.Parser OBJLine
uvLine = "vt" *> (UvLine <$> v2 (tok P.double))

faceLine :: P.Parser OBJLine
faceLine = "f" *> (FaceLine <$> vector1 (tok faceEntry))

v2 :: P.Parser a -> P.Parser (V2 a)
v2 p = liftA2 V2 p p

v3 :: P.Parser a -> P.Parser (V3 a)
v3 p = liftA3 V3 p p p

vector1 :: P.Parser a -> P.Parser (V.Vector a)
vector1 p = V.fromList <$> P.many1' p

tok :: P.Parser a -> P.Parser a
tok p = P.skipSpace *> p

faceEntry :: P.Parser FaceEntry
faceEntry =
  liftA2 FaceEntry P.decimal another <* another <|>
  liftA2 FaceEntry P.decimal another <|>
  liftA2 FaceEntry P.decimal (pure Nothing)
  where
    another = P.char '/' *> ((Just <$> P.decimal) <|> pure Nothing)
