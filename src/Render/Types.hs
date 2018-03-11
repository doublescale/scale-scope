{-# LANGUAGE TemplateHaskell #-}

module Render.Types
  ( RenderState(..)
  , renderStateMeshesL
  , renderStateShaderL
  , MeshDescriptor(..)
  , ShaderDescriptor(..)
  , initRenderState
  ) where

import Control.Lens (makeLensesWith)
import qualified Data.Vector as V
import qualified Graphics.Rendering.OpenGL.GL as GL

import Util (suffixedLRule)

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

makeLensesWith suffixedLRule ''RenderState

initRenderState :: RenderState
initRenderState =
  RenderState
  { renderStateMeshes =
      MeshDescriptor
      {meshVaos = V.empty, meshTexObj = GL.TextureObject 0, meshNIdxs = 0}
  , renderStateShader = Nothing
  }
