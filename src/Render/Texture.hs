module Render.Texture
  ( loadTexture
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as VS
import Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL

import Mesh (RGBGrid(..))

loadTexture :: MonadIO io => Maybe RGBGrid -> io GL.TextureObject
loadTexture textureRGBData =
  liftIO (load (fromMaybe whiteGrid textureRGBData))
  where
    whiteGrid =
      RGBGrid {gridWidth = 1, gridHeight = 1, gridPixels = VS.replicate 3 255}

load :: RGBGrid -> IO GL.TextureObject
load RGBGrid {gridWidth, gridHeight, gridPixels} = do
  texObj :: GL.TextureObject <- GL.genObjectName
  GL.textureBinding GL.Texture2D $= Just texObj
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  VS.unsafeWith gridPixels $ \ptr ->
    GL.texImage2D
      GL.Texture2D
      GL.NoProxy
      0
      GL.RGB'
      (GL.TextureSize2D (fromIntegral gridWidth) (fromIntegral gridHeight))
      0
      (GL.PixelData GL.RGB GL.UnsignedByte ptr)
  return texObj
