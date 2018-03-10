module Render.Shader
  ( loadShader
  ) where

import Control.Monad (forM_, unless)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL

import Render.Types (ShaderDescriptor(..))

loadShader ::
     MonadIO io => Maybe ShaderDescriptor -> io (Maybe ShaderDescriptor)
loadShader maybeShaderDescriptor =
  liftIO $ do
    errorOrProgram <-
      compileShader
        [ (GL.VertexShader, "shader/simple.vs")
        , (GL.FragmentShader, "shader/simple.fs")
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

-- TODO: Get file contents, not file paths as parameters.
compileShader :: [(GL.ShaderType, FilePath)] -> IO (Either String GL.Program)
compileShader shaderDescs = runExceptT $ do
  program <- liftIO GL.createProgram
  forM_ shaderDescs $ \(typ, path) -> do
    shader <- liftIO (GL.createShader typ)
    source <- liftIO (readFile path)
    liftIO (GL.shaderSourceBS shader $= GL.packUtf8 source)
    liftIO (GL.compileShader shader)
    check shader GL.compileStatus GL.shaderInfoLog path
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
