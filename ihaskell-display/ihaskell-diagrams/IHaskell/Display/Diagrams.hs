{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module IHaskell.Display.Diagrams (rasterDiagram, 
                                  svgDiagram,
                                  diagram,
                                  -- animation
                                 ) where

import qualified Data.ByteString.Char8 as Char
import           System.Directory
import           System.IO.Unsafe
import           Diagrams.Prelude
import qualified Diagrams.Backend.Rasterific as RASTA
import qualified Diagrams.Backend.SVG as SVG
import           IHaskell.Display
-- import           IHaskell.Display.Diagrams.Animation

-- Rasterific

instance IHaskellDisplay (QDiagram RASTA.Rasterific V2 Double Any) where
  display renderable = do
    png <- diagramDataRasta renderable
    return $ Display [png]

diagramDataRasta :: Diagram RASTA.Rasterific -> IO DisplayData
diagramDataRasta renderable = do
  switchToTmpDir

  -- Compute width and height.
  let w = width renderable
      h = height renderable
      aspect = w / h
      imgHeight = 300
      imgWidth = aspect * imgHeight

  -- Write the image.
  let filename = ".ihaskell-diagram.png"
  RASTA.renderRasterific filename (mkSizeSpec2D (Just imgWidth) (Just imgHeight)) renderable

  -- Convert to base64.
  imgData <- Char.readFile filename
  let value = png (floor imgWidth) (floor imgHeight) $ base64 imgData

  return value


-- Rendering hint.
rasterDiagram :: Diagram RASTA.Rasterific -> Diagram RASTA.Rasterific
rasterDiagram = id

--  SVG

instance IHaskellDisplay (QDiagram SVG.SVG V2 Double Any) where
  display renderable = do
    svg <- diagramDataSVG renderable
    return $ Display [svg]

diagramDataSVG :: Diagram SVG.SVG -> IO DisplayData
diagramDataSVG renderable = do
  switchToTmpDir

  -- Compute width and height.
  let w = width renderable
      h = height renderable
      aspect = w / h
      imgHeight = 300
      imgWidth = aspect * imgHeight

  -- Write the image.
  let filename = ".ihaskell-diagram.svg"
  SVG.renderSVG filename (mkSizeSpec2D (Just imgWidth) (Just imgHeight)) renderable

  -- Convert to base64.
  imgData <- readFile filename
  let value = svg $ imgData

  return value


-- Rendering hint.
svgDiagram :: Diagram SVG.SVG -> Diagram SVG.SVG
svgDiagram = id

diagram :: Diagram SVG.SVG -> Diagram SVG.SVG
diagram = svgDiagram

