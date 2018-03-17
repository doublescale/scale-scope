module Render.Shader
  ( loadShader
  ) where

import Control.Monad (forM_, unless)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Data.ByteString.Lazy as BSL

import Render.Types (ShaderDescriptor(..))

loadShader ::
     MonadIO io => Maybe ShaderDescriptor -> io (Maybe ShaderDescriptor)
loadShader maybeShaderDescriptor =
  liftIO $ do
    vertexShaderSource <- BSL.readFile "shader/simple.vs"
    fragmentShaderSource <- BSL.readFile "shader/simple.fs"
    errorOrProgram <-
      compileShader
        [ (GL.VertexShader, vertexShaderSource)
        , (GL.FragmentShader, fragmentShaderSource)
        ]
    case errorOrProgram of
      Left err -> putStrLn err >> return maybeShaderDescriptor
      Right program -> do
        mapM_ (GL.deleteObjectName . shaderProgram) maybeShaderDescriptor
        shaderFrameTime <- GL.get $ GL.uniformLocation program "frame_t"
        shaderModelMat <- GL.get $ GL.uniformLocation program "model_mat"
        shaderViewMat <- GL.get $ GL.uniformLocation program "view_mat"
        return
          (Just
             ShaderDescriptor
             { shaderProgram = program
             , shaderFrameTime
             , shaderModelMat
             , shaderViewMat
             })

compileShader ::
     [(GL.ShaderType, BSL.ByteString)] -> IO (Either String GL.Program)
compileShader shaderDescriptors =
  runExceptT $ do
    program <- liftIO GL.createProgram
    forM_ shaderDescriptors $ \(shaderType, shaderSource) -> do
      shader <- liftIO (GL.createShader shaderType)
      liftIO (GL.shaderSourceBS shader $= BSL.toStrict shaderSource)
      liftIO (GL.compileShader shader)
      check shader GL.compileStatus GL.shaderInfoLog (show shaderType)
      liftIO (GL.attachShader program shader)
    liftIO (GL.linkProgram program)
    check program GL.linkStatus GL.programInfoLog "linking"
    return program
  where
    check object status info msg = do
      ok <- GL.get (status object)
      unless ok $ do
        infoLog <- GL.get (info object)
        throwError (msg ++ ": " ++ infoLog)
