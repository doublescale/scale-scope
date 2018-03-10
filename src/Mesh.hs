module Mesh
  ( AnimationData(..)
  , RGBGrid(..)
  , MeshSequence(..)
  , MeshConstant(..)
  , MeshFrame(..)
  , PolyFaces
  , TriFaces
  ) where

import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import qualified Data.Vector as V
import Data.Vector.Serialize ()
import qualified Data.Vector.Storable as VS
import Data.Word (Word8, Word32)
import GHC.Generics (Generic)
import Linear (V2, V3)

data AnimationData scalar fs = AnimationData
  { animationFramerate :: Double
  , textureRGBData :: Maybe RGBGrid
  , meshSequence :: MeshSequence scalar fs
  } deriving (Generic, NFData, Serialize, Show)

data RGBGrid = RGBGrid
  { gridWidth :: Int
  , gridHeight :: Int
  , gridPixels :: VS.Vector Word8
  } deriving (Generic, NFData, Serialize, Show)

data MeshSequence scalar fs = MeshSequence
  { meshConstant :: MeshConstant scalar fs
  , meshFrames :: [MeshFrame scalar]
  } deriving (Generic, NFData, Serialize, Show)

data MeshConstant scalar fs = MeshConstant
  { faces :: fs
  , vertexUVs :: VS.Vector (V2 scalar)
  } deriving (Generic, NFData, Serialize, Show)

data MeshFrame scalar = MeshFrame
  { vertexPositions :: VS.Vector (V3 scalar)
  , vertexNormals :: VS.Vector (V3 scalar)
  } deriving (Generic, NFData, Serialize, Show)

type PolyFaces = V.Vector (VS.Vector Word32)

type TriFaces = VS.Vector (V3 Word32)
