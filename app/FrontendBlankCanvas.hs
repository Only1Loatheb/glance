{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-} -- for "mousedown"
module FrontendBlankCanvas
  ( blankCanvasLoop
  )
where
    
import qualified Diagrams.Prelude as Dia hiding ((#), (&))
import Diagrams.Backend.Canvas as CV
import qualified Graphics.Blank as BC hiding (rotate, scale, ( # ))
import Data.Text (Text)

import Types (
  SpecialDiagram
  ,SpecialQDiagram
  , SpecialBackend
  , DiaQuery
  , ModuleGraphs
  )

import IconToSymbolDiagram(ColorStyle(..), colorScheme, multilineComment)

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

bcDrawDiagram :: BC.DeviceContext
  -> Dia.SizeSpec Dia.V2 Double
  -> Dia.QDiagram Canvas Dia.V2 Double m
  -> IO ()
bcDrawDiagram context sizeSpec moduleDiagram = do
  BC.send context $ BC.clearRect (0,0,BC.width context, BC.height context)
  let moduleDrawing = Dia.bg (backgroundC colorScheme) $ Dia.clearValue moduleDiagram
  BC.send context $ Dia.renderDia CV.Canvas (CanvasOptions sizeSpec) moduleDrawing   
  
blankCanvasLoop moduleGraphs portNumber loopControl imageScale = do
  let blankCanvasOpts  = getBlankCanvasOpts portNumber
  BC.blankCanvas blankCanvasOpts $ \ context -> loop context (moduleGraphs, Nothing) loopControl imageScale

-- loop ::
--   BC.DeviceContext
--   -> (ModuleGraphs, Maybe ModuleGraphs)
--   -> Double
--   -> IO b
loop 
  context 
  (moduleGraphs, maybeView) 
  loopControl@(chooseFullOrView, sampleDiagram, createView) 
  imageScale
  = do
  moduleDiagram <- chooseFullOrView moduleGraphs maybeView
  -- print maybeView
  let (moduleDiagramAligned, pointToDiaPoint, sizeSpec) = diagramForBlankCanvas moduleDiagram imageScale
  bcDrawDiagram context sizeSpec moduleDiagramAligned
  event <- BC.wait context
  case BC.ePageXY event of
    -- if no mouse location, ignore, and redraw
    Nothing -> loop context (moduleGraphs, Nothing) loopControl imageScale
    Just point -> do
      let scaledPoint = pointToDiaPoint point
      let clicked = sampleDiagram moduleDiagram scaledPoint
      if not $ null clicked
      then do
        let view = createView clicked moduleGraphs
        loop context (moduleGraphs, view) loopControl imageScale
      else loop context (moduleGraphs, Nothing) loopControl imageScale