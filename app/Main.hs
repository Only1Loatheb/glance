{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  (main
  , CmdLineOptions(..)) where

import Prelude hiding (return)

-- Note: (#) and (&) are hidden in all Glance source files, since they would
-- require a special case when translating when Glance is run on its own source
-- code.
import qualified Diagrams.Prelude as Dia hiding ((#), (&))

-- Options.Applicative does not seem to work qualified

import Util(customRenderSVG)

import           Data.Text (Text)
import Diagrams.Backend.Canvas as CV
-- import           Control.Concurrent
import qualified Graphics.Blank as BC hiding (rotate, scale, ( # ))
import           Types (
  SpecialDiagram
  ,SpecialQDiagram
  , SpecialBackend
  , NameQuery
  )

import IconToSymbolDiagram(ColorStyle(..), colorScheme, multilineComment)

import ModuleToDiagram(getModuleGraphs, diagramFromModule)

import ParseCmdLineArgs as CMD
-- {-# ANN module "HLint: ignore Unnecessary hiding" #-}

  
getBlankCanvasOpts :: Int -> BC.Options
getBlankCanvasOpts  portNumber =  BC.Options {
  BC.port = portNumber
  , BC.events = ["mousedown"] 
  , BC.debug = False
  , BC.root = "."
  , BC.middleware = [BC.local_only]
  , BC.weak = False
  }

diagramForBlankCanvas ::  SpecialBackend b Double 
  => SpecialQDiagram b Double
  -> Double
  -> (SpecialQDiagram b Double, (Double, Double) -> Dia.Point Dia.V2 Double,  Dia.SizeSpec Dia.V2 Double)
diagramForBlankCanvas moduleDiagram imageScale = (moduleDiagramAligned, pointToDiaPoint, sizeSpec) where
  moduleDiagramAligned = Dia.alignTL moduleDiagram
  pointToDiaPoint _point@(x,y) = (1.0/imageScale) Dia.*^ Dia.p2 (x,-y)
  sizeSpec =  Dia.dims2D (imageScale * Dia.width moduleDiagram) (imageScale * Dia.height moduleDiagram)

renderFile :: CMD.CmdLineOptions -> IO ()
renderFile (CMD.CmdLineOptions
             inputFilename
             outputFilename
             portNumber
             imageScale
             includeComments)
  = do
  putStrLn $ "Translating file " ++ inputFilename ++ " into a Glance image."
  moduleGraphs <- getModuleGraphs inputFilename
  moduleDiagram <- diagramFromModule moduleGraphs includeComments
  let (moduleDiagramAligned, pointToDiaPoint, sizeSpec) = diagramForBlankCanvas moduleDiagram imageScale
  let blankCanvasOpts  = getBlankCanvasOpts portNumber

  BC.blankCanvas blankCanvasOpts $ \ context -> loop context sizeSpec moduleDiagramAligned pointToDiaPoint
  -- customRenderSVG outputFilename (Dia.mkWidth imageWidth) moduleDiagram
  putStrLn $ "Successfully wrote " ++ outputFilename

loop :: (Show a, Monoid a) =>
          BC.DeviceContext
          -> Dia.SizeSpec Dia.V2 Double
          -> Dia.QDiagram Canvas Dia.V2 Double a
          -> ((Double, Double) -> Dia.Point Dia.V2 Double)
          -> IO b
loop context sizeSpec moduleDiagram pointToDiaPoint = do
  let moduleDrawing = Dia.bg (backgroundC colorScheme) $ Dia.clearValue moduleDiagram
  BC.send context $ Dia.renderDia CV.Canvas (CanvasOptions sizeSpec) moduleDrawing 

  event <- BC.wait context
  case BC.ePageXY event of
    -- if no mouse location, ignore, and redraw
    Nothing -> loop context sizeSpec moduleDiagram pointToDiaPoint
    Just point -> do
      let scaledPoint = pointToDiaPoint point
      print $ Dia.sample  moduleDiagram scaledPoint
      loop context sizeSpec moduleDiagram pointToDiaPoint


translateFileMain :: IO ()
translateFileMain = CMD.customExecParser CMD.parserPrefs  CMD.opts >>= renderFile

main :: IO ()
main = translateFileMain
