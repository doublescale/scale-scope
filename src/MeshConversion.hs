module MeshConversion
  ( objToPolyMeshSequence
  , triangulateMeshSequence
  ) where

import Data.Bifunctor (bimap)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import GHC.Float (double2Float)
import Linear (Epsilon, V2(V2), V3(V3), cross, normalize)
import Numeric.Half (Half, toHalf)

import Mesh
  ( MeshConstant(..)
  , MeshFrame(..)
  , MeshSequence(..)
  , PolyFaces
  , TriFaces
  )
import OBJParser (FaceEntry(..), OBJLine(..))

objToPolyMeshSequence :: [[OBJLine]] -> MeshSequence Double PolyFaces
objToPolyMeshSequence [] =
  MeshSequence
  { meshConstant = MeshConstant {faces = V.empty, vertexUVs = VS.empty}
  , meshFrames = []
  }
objToPolyMeshSequence objFiles@(firstFile:_) =
  MeshSequence {meshConstant, meshFrames = map convertFrame objFiles}
  where
    (meshConstant, convertFrame) = objToPolyMesh firstFile

objToPolyMesh ::
     [OBJLine] -> (MeshConstant Double PolyFaces, [OBJLine] -> MeshFrame Double)
objToPolyMesh objLines =
  ( MeshConstant
    { faces =
        VG.fromList
          (map
             (VG.fromList . map (\x -> Map.findWithDefault 0 x splitFaceIdxMap))
             origFacesAndUvs)
    , vertexUVs = VG.backpermute origUvs uvBackpermutation
    }
  , objToPolyFrame origFaces vertBackpermutation)
  where
    faceLines = [entries | FaceLine entries <- objLines]
    origFaces = V.map (VG.map (pred . posIdx)) (VG.fromList faceLines)
    origFacesAndUvs =
      [ [ (posIdx - 1, fromMaybe 0 uvIdx)
        | FaceEntry {posIdx, uvIdx} <- VG.toList entries
        ]
      | entries <- faceLines
      ]
    origUvs = VG.fromList (V2 0 0 : [uv | UvLine uv <- objLines])
    uniqueVertUvPairs =
      Set.toList
        (Set.fromList
           [ (posIdx - 1, fromMaybe 0 uvIdx)
           | entries <- faceLines
           , FaceEntry {posIdx, uvIdx} <- VG.toList entries
           ])
    splitFaceIdxMap = Map.fromList (zip uniqueVertUvPairs [0 ..])
    (vertBackpermutation, uvBackpermutation) =
      bimap VG.fromList VG.fromList (unzip uniqueVertUvPairs)

objToPolyFrame ::
     V.Vector (V.Vector Int) -> VS.Vector Int -> [OBJLine] -> MeshFrame Double
objToPolyFrame faces vertBackpermutation objLines =
  MeshFrame
  { vertexPositions = VG.backpermute positions vertBackpermutation
  , vertexNormals = VG.backpermute normals vertBackpermutation
  }
  where
    normals = buildNormals positions faces
    positions = VG.fromList [pos | PosLine pos <- objLines]

buildNormals ::
     ( Epsilon x
     , Floating x
     , VG.Vector u (V3 x)
     , VG.Vector u Int
     , VG.Vector e Int
     , VG.Vector v (e Int)
     , VG.Vector w (V3 x)
     )
  => u (V3 x)
  -> v (e Int)
  -> w (V3 x)
buildNormals positions faces = VG.accum (+) initialNormals posIdxFaceNormals
  where
    initialNormals = VG.replicate (VG.length positions) (V3 0 0 0)
    posIdxFaceNormals =
      [ (i, faceNormal entries)
      | entries <- VG.toList faces
      , i <- VG.toList entries
      ]
    faceNormal entries =
      let corners = VG.take 2 entries `VG.snoc` VG.last entries
          [a, b, c] = VG.toList (VG.backpermute positions (VG.convert corners))
      in normalize (cross (b - a) (c - a))

triangulateMeshSequence ::
     MeshSequence Double PolyFaces -> MeshSequence Half TriFaces
triangulateMeshSequence MeshSequence {meshConstant = MeshConstant {..}, ..} =
  MeshSequence
  { meshConstant =
      MeshConstant
      { faces = VG.convert (VG.concatMap triangulatePoly faces)
      , vertexUVs = VG.map (fmap double2Half) vertexUVs
      }
  , meshFrames = map frameToFloat meshFrames
  }
  where
    frameToFloat MeshFrame {..} =
      MeshFrame
      { vertexPositions = VG.map (fmap double2Half) vertexPositions
      , vertexNormals = VG.map (fmap double2Half) vertexNormals
      }
    double2Half = toHalf . double2Float

-- Fan triangulation with corner indices:
-- 0, 1, 2
-- 0, 2, 3
-- 0, 3, 4
-- 0, 4, 5
-- ...
triangulatePoly :: (VG.Vector v a, VG.Vector w (V3 a)) => v a -> w (V3 a)
triangulatePoly idxs =
  VG.generate nTriangles (\i -> lookupIdx <$> V3 0 (i + 1) (i + 2))
  where
    nTriangles = VG.length idxs - 2
    lookupIdx j = idxs VG.! j
