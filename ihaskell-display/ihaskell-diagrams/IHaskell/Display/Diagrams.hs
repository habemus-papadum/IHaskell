{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module IHaskell.Display.Diagrams (rasterDiagram, 
                                  -- animation
                                 ) where

import qualified Data.ByteString.Char8 as Char
import           System.Directory
import           System.IO.Unsafe
import qualified Diagrams.Backend.Rasterific as RASTA
import           Diagrams.Prelude
import           IHaskell.Display
-- import           IHaskell.Display.Diagrams.Animation

instance IHaskellDisplay (QDiagram RASTA.Rasterific V2 Double Any) where
  display renderable = do
    png <- diagramData renderable
    return $ Display [png]

diagramData :: Diagram RASTA.Rasterific -> IO DisplayData
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
  RASTA.renderRasterific filename (mkSizeSpec2D (Just imgWidth) (Just imgHeight)) renderable

  -- Convert to base64.
  imgData <- Char.readFile filename
  let value = png (floor imgWidth) (floor imgHeight) $ base64 imgData

  return value


-- Rendering hint.
rasterDiagram :: Diagram RASTA.Rasterific -> Diagram RASTA.Rasterific
rasterDiagram = id
