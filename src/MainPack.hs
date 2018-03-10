{-# LANGUAGE ApplicativeDo #-}

module Main where

import qualified Codec.Compression.Zlib as Zlib
import qualified Codec.Picture as JP
import Control.Applicative ((<**>), optional, some)
import Control.Monad (forM)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Semigroup ((<>))
import qualified Data.Serialize as Cereal
import qualified Options.Applicative as O

import Mesh (AnimationData(..), RGBGrid(..), RGBGrid)
import MeshConversion
       (objToPolyMeshSequence, triangulateMeshSequence)
import OBJParser (parseOBJ)
import SerializeHalf ()

data Options = Options
  { optFramerate :: Double
  , optTextureFile :: Maybe FilePath
  , optOutFile :: FilePath
  , optInFiles :: [FilePath]
  }

optionParser :: O.Parser Options
optionParser = do
  optOutFile <-
    O.strOption
      (O.short 'o' <> O.long "out" <> O.metavar "OUTFILE" <>
       O.help "Output file")
  optFramerate <-
    O.option
      O.auto
      (O.short 'r' <> O.long "rate" <> O.metavar "FPS" <>
       O.help "Framerate in 1/s" <>
       O.value 30 <>
       O.showDefault)
  optTextureFile <-
    optional
      (O.strOption
         (O.short 't' <> O.long "texture" <> O.metavar "TEXFILE" <>
          O.help "Texture file to pack"))
  optInFiles <- some (O.strArgument (O.metavar "OBJFILES..."))
  return Options {..}

parserInfo :: O.ParserInfo Options
parserInfo =
  O.info
    (optionParser <**> O.helper)
    (O.fullDesc <>
     O.header "Pack a sequence of OBJ files for the viewer program.")

main :: IO ()
main = do
  options <- O.execParser parserInfo
  packObjFiles options

packObjFiles :: Options -> IO ()
packObjFiles Options {..} = do
  objFiles <-
    forM optInFiles $ \file -> do
      putStrLn ("Loading " ++ show file)
      parseOBJ <$> BSL.readFile file
  textureRGBData <- case optTextureFile of
    Nothing -> return Nothing
    Just textureFile -> do
      -- TODO: Handle Left
      Right rgbData <- readTextureFile textureFile
      return (Just rgbData)
  let animationData =
        AnimationData
        { animationFramerate = optFramerate
        , textureRGBData
        , meshSequence =
            triangulateMeshSequence (objToPolyMeshSequence objFiles)
        }
  putStrLn ("Writing " ++ show optOutFile)
  BSL.writeFile optOutFile (Zlib.compress (Cereal.encodeLazy animationData))

readTextureFile :: FilePath -> IO (Either String RGBGrid)
readTextureFile path = do
  eitherDynImg <- JP.readImage path
  return (toPixelGrid . JP.convertRGB8 <$> eitherDynImg)
  where
    toPixelGrid :: JP.Image JP.PixelRGB8 -> RGBGrid
    toPixelGrid JP.Image {JP.imageWidth, JP.imageHeight, JP.imageData} =
      RGBGrid
      { gridWidth = imageWidth
      , gridHeight = imageHeight
      , gridPixels = imageData
      }
