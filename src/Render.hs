module Render
  ( render
  , RenderState(..)
  ) where

import Control.Applicative (liftA2)
import Control.Lens ((%~), (&), (.~), (^.))
import Control.Monad (zipWithM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Fixed (mod')
import qualified Data.Foldable as Foldable
import qualified Data.Vector as V
import Foreign (nullPtr, pokeElemOff)
import GHC.Float (double2Float)
import Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import Linear
       (M44, V2(V2), V3(V3), (!*!), _x, _y, _z, axisAngle, identity,
        mkTransformation, perspective, translation)

import AppState (AppState(..), ViewState(..))
import Render.Types
       (MeshDescriptor(..), RenderState(..), ShaderDescriptor(..))

render :: MonadIO io => AppState RenderState -> io ()
render AppState {appRenderState = RenderState {renderStateMeshes = MeshDescriptor{..}, ..}, ..} =
  liftIO $ do
    GL.viewport $=
      (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.depthFunc $= Just GL.Lequal
    sequence_
      (liftA2
         (renderMeshWithShader frameTime modelMat viewMat meshNIdxs)
         currentVao
         renderStateShader)
  where
    V2 width height = appWinSize
    aspect = fromIntegral width / fromIntegral height
    viewMat = perspective (pi / 4) aspect 0.01 100
    modelMat = buildModelMatrix appViewState
    frameTime = appFrame `mod'` 1
    currentVao = pickFrame meshVaos appFrame

renderMeshWithShader ::
     Double
  -> M44 Double
  -> M44 Double
  -> GL.NumArrayIndices
  -> GL.VertexArrayObject
  -> ShaderDescriptor
  -> IO ()
renderMeshWithShader
  frameT modelMat viewMat meshNIdxs meshVao ShaderDescriptor {..}
  = do
  glModelMat <- toGlMatrix modelMat
  glViewMat <- toGlMatrix viewMat
  GL.currentProgram $= Just shaderProgram
  GL.uniform shaderFrameTime $= double2Float frameT
  GL.uniform shaderModelMat $= glModelMat
  GL.uniform shaderViewMat $= glViewMat
  GL.bindVertexArrayObject $= Just meshVao
  GL.drawElements GL.Triangles (fromIntegral meshNIdxs) GL.UnsignedInt nullPtr

buildModelMatrix :: ViewState -> M44 Double
buildModelMatrix ViewState {viewCamAngle, viewCamDistance, viewCamPos} =
  (camRotationMat !*! camTranslationMat) & translation . _z %~
  subtract viewCamDistance
  where
    camTranslationMat = identity & translation .~ negate viewCamPos
    camRotationMat = mkTransformation (quatPitch * quatYaw) 0
    quatPitch = axisAngle (V3 1 0 0) (pi / 180 * (viewCamAngle ^. _y - 90))
    quatYaw = axisAngle (V3 0 0 1) (pi / 180 * (viewCamAngle ^. _x))

toGlMatrix :: M44 Double -> IO (GL.GLmatrix GL.GLfloat)
toGlMatrix mat =
  GL.withNewMatrix GL.RowMajor $ \glPtr ->
    zipWithM_
      (pokeElemOff glPtr)
      [0 ..]
      (concat $ Foldable.toList <$> Foldable.toList (fmap double2Float <$> mat))

pickFrame :: V.Vector a -> Double -> Maybe a
pickFrame xs t
  | V.null xs = Nothing
  | otherwise = Just (xs V.! (floor t `mod` length xs))
