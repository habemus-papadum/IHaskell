{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module IHaskell.Display.Diagrams (diagram, animation) where

import qualified Data.ByteString.Char8 as Char
import           System.Directory
import           System.IO.Unsafe
import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude
import           IHaskell.Display
import           IHaskell.Display.Diagrams.Animation

instance IHaskellDisplay (QDiagram Rasterific V2 Double Any) where
  display renderable = do
    png <- diagramData renderable
    return $ Display [png]

diagramData :: Diagram Rasterific -> IO DisplayData
diagramData renderable = do
  switchToTmpDir

  -- Compute width and height.
  let w = width renderable
      h = height renderable
      aspect = w / h
      imgHeight = 300
      imgWidth = aspect * imgHeight

  -- Write the image.
  let filename = ".ihaskell-diagram.png"
  renderRasterific filename (mkSizeSpec2D (Just imgWidth) (Just imgHeight)) renderable

  -- Convert to base64.
  imgData <- Char.readFile filename
  let value = png (floor imgWidth) (floor imgHeight) $ base64 imgData

  return value


-- Rendering hint.
diagram :: Diagram Rasterific -> Diagram Rasterific
diagram = id
