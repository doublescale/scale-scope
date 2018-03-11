module Mesh
  ( AnimationData(..)
  , RGBGrid(..)
  , MeshSequence(..)
  , MeshConstant(..)
  , MeshFrame(..)
  , PolyFaces
  , TriFaces
  ) where

import Data.Store (Store)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word (Word32, Word8)
import GHC.Generics (Generic)
import Linear (V2, V3)

data AnimationData scalar fs = AnimationData
  { animationFramerate :: Double
  , textureRGBData :: Maybe RGBGrid
  , meshSequence :: MeshSequence scalar fs
  } deriving (Generic, Show, Store)

data RGBGrid = RGBGrid
  { gridWidth :: Int
  , gridHeight :: Int
  , gridPixels :: VS.Vector Word8
  } deriving (Generic, Show, Store)

data MeshSequence scalar fs = MeshSequence
  { meshConstant :: MeshConstant scalar fs
  , meshFrames :: [MeshFrame scalar]
  } deriving (Generic, Show, Store)

data MeshConstant scalar fs = MeshConstant
  { faces :: fs
  , vertexUVs :: VS.Vector (V2 scalar)
  } deriving (Generic, Show, Store)

data MeshFrame scalar = MeshFrame
  { vertexPositions :: VS.Vector (V3 scalar)
  , vertexNormals :: VS.Vector (V3 scalar)
  } deriving (Generic, Show, Store)

type PolyFaces = V.Vector (VS.Vector Word32)

type TriFaces = VS.Vector (V3 Word32)
