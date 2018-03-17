{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}

module Render.Mesh
  ( loadMeshSequence
  , deleteMeshSequence
  ) where

import Control.Monad (zipWithM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy (Proxy(Proxy))
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import Foreign (Storable, nullPtr, sizeOf)
import GHC.Types (Nat)
import Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import Linear.V (Dim, V, dim, reflectDim, toV)
import Numeric.Half (Half)

import Mesh
  ( AnimationData(..)
  , MeshConstant(..)
  , MeshFrame(..)
  , MeshSequence(..)
  , TriFaces
  )
import Render.Texture (loadTexture)
import Render.Types (MeshDescriptor(..))

loadMeshSequence ::
     MonadIO io => AnimationData Half TriFaces -> io MeshDescriptor
loadMeshSequence
  AnimationData
  {meshSequence = MeshSequence {meshConstant = MeshConstant {..}, ..}, ..}
  =
  liftIO $ do
    bindUVs <- loadVbo (VVector (VS.map toV vertexUVs))
    binders <-
      V.fromList <$>
      zipWithM
        (\i MeshFrame {..} -> do
           putStrLn ("Loading " ++ show i ++ "/" ++ show (length meshFrames))
           bindPositions <- loadVbo (VVector (VS.map toV vertexPositions))
           bindNormals <- loadVbo (VVector (VS.map toV vertexNormals))
           return (bindPositions, bindNormals))
        [1 :: Int ..]
        meshFrames
    vaos <-
      zipWithM
        (\i MeshFrame {} -> do
           vao :: GL.VertexArrayObject <- GL.genObjectName
           GL.bindVertexArrayObject $= Just vao
           _ <- loadBuffer GL.ElementArrayBuffer faces
           let (bindPositions0, bindNormals0) = binders V.! i
           let (bindPositions1, bindNormals1) =
                 binders V.! ((i + 1) `mod` length meshFrames)
           bindPositions0 0
           bindPositions1 1
           bindNormals0 2
           bindNormals1 3
           bindUVs 4
           return vao)
        [0 :: Int ..]
        meshFrames
    meshTexObj <- loadTexture textureRGBData
    return
      MeshDescriptor
      { meshVaos = VG.fromList vaos
      , meshTexObj
      , meshNIdxs = fromIntegral (dim (toV (VS.head faces)) * VS.length faces)
      }

deleteMeshSequence :: MonadIO io => MeshDescriptor -> io ()
deleteMeshSequence MeshDescriptor {meshVaos, meshTexObj} =
  liftIO $ do
    GL.deleteObjectNames (VG.toList meshVaos)
    GL.deleteObjectName meshTexObj

loadBuffer :: Storable a => GL.BufferTarget -> VS.Vector a -> IO GL.BufferObject
loadBuffer bufferTarget srcData = do
  bufObj :: GL.BufferObject <- GL.genObjectName
  GL.bindBuffer bufferTarget $= Just bufObj
  let dataSize = sizeOfVec srcData
  VS.unsafeWith srcData $ \ptr ->
    GL.bufferData bufferTarget $= (dataSize, ptr, GL.StaticDraw)
  return bufObj
  where
    sizeOfVec xs = fromIntegral (sizeOf (VS.head xs) * VS.length xs)

data VVector = forall (n :: Nat) a. (Dim n, ToGLType a, Storable a) =>
  VVector (VS.Vector (V n a))

class ToGLType a where toGLType :: Proxy a -> GL.DataType
instance ToGLType Half where toGLType _ = GL.HalfFloat
instance ToGLType Float where toGLType _ = GL.Float
instance ToGLType Double where toGLType _ = GL.Double

loadVbo :: VVector -> IO (GL.GLuint -> IO ())
loadVbo (VVector (vvec :: VS.Vector (V n a))) = do
  vbo <- loadBuffer GL.ArrayBuffer vvec
  return $ \attribIdx -> do
    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    GL.vertexAttribArray (GL.AttribLocation attribIdx) $= GL.Enabled
    GL.vertexAttribPointer (GL.AttribLocation attribIdx) $=
      ( GL.ToFloat
      , GL.VertexArrayDescriptor
          (fromIntegral (reflectDim (Proxy :: Proxy n)))
          (toGLType (Proxy :: Proxy a))
          0
          nullPtr)
