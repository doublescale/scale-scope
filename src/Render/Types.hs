module Render.Types
  ( RenderState(..)
  , MeshDescriptor(..)
  , ShaderDescriptor(..)
  , initRenderState
  ) where

import qualified Data.Vector as V
import qualified Graphics.Rendering.OpenGL.GL as GL

data RenderState = RenderState
  { renderStateMeshes :: MeshDescriptor
  , renderStateShader :: Maybe ShaderDescriptor
  }

data MeshDescriptor = MeshDescriptor
  { meshVaos :: V.Vector GL.VertexArrayObject
  , meshTexObj :: GL.TextureObject
  , meshNIdxs :: GL.NumArrayIndices
  }

data ShaderDescriptor = ShaderDescriptor
  { shaderProgram :: GL.Program
  , shaderFrameTime :: GL.UniformLocation
  , shaderModelMat :: GL.UniformLocation
  , shaderViewMat :: GL.UniformLocation
  }

initRenderState :: RenderState
initRenderState =
  RenderState
  { renderStateMeshes =
      MeshDescriptor
      {meshVaos = V.empty, meshTexObj = GL.TextureObject 0, meshNIdxs = 0}
  , renderStateShader = Nothing
  }
