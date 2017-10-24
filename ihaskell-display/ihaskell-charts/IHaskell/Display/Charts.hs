{-# LANGUAGE CPP #-}

module IHaskell.Display.Charts () where

import           System.Directory
import           Data.Default.Class
import           Graphics.Rendering.Chart.Renderable
import           Graphics.Rendering.Chart.Backend.Diagrams
import qualified Data.ByteString.Char8 as Char
import           System.IO.Unsafe

import           IHaskell.Display

width :: Width
width = 450

height :: Height
height = 300

instance IHaskellDisplay (Renderable a) where
  display renderable = do
    svgDisp <- chartData renderable SVG

    return $ Display [svgDisp]

chartData :: Renderable a -> FileFormat -> IO DisplayData
chartData renderable format = do
  switchToTmpDir

  -- Write the SVG image.
  let filename = ".ihaskell-chart.svg"
      opts = def { _fo_format = format, _fo_size = (fromIntegral width :: Double, fromIntegral height :: Double) }
  mkFile opts filename renderable

  -- Convert to base64.
  imgData <- Char.readFile filename
  return $
    case format of
      SVG -> svg $ Char.unpack imgData
mkFile opts filename renderable = renderableToFile opts filename renderable
