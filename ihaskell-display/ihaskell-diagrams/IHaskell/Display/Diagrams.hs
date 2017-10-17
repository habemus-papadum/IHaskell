{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module IHaskell.Display.Diagrams (diagram) where

import qualified Data.ByteString.Char8 as Char
import           System.Directory
import           System.IO.Unsafe
import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           IHaskell.Display
import           IHaskell.Display.Diagrams.Animation

instance IHaskellDisplay (QDiagram SVG V2 Double Any) where
  display renderable = do
    svg <- diagramData renderable
    return $ Display [svg]

diagramData :: Diagram SVG -> IO DisplayData
diagramData renderable  = do
  switchToTmpDir

  -- Compute width and height.
  let w = width renderable
      h = height renderable
      aspect = w / h
      imgHeight = 300
      imgWidth = aspect * imgHeight

  -- Write the image.
  let filename = ".ihaskell-diagram.svg"
  renderSVG filename (mkSizeSpec2D (Just imgWidth) (Just imgHeight)) renderable

  -- Convert to base64.
  imgData <- Char.readFile filename
  let value = svg (Char.unpack imgData)

  return value


-- Rendering hint.
diagram :: Diagram SVG -> Diagram SVG
diagram = id
