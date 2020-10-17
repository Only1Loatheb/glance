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
  , DiaQuery(..)
  , ModuleGraphs
  , SrcRef
  , ViewGraphs
  , CreateView
  , SourceCode
  , View
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
  
blankCanvasLoop moduleDiagram portNumber loopControl imageScale = do
  let blankCanvasOpts  = getBlankCanvasOpts portNumber
  BC.blankCanvas blankCanvasOpts $ \ context -> loop context (moduleDiagram, (Nothing, Nothing)) loopControl imageScale

loop ::
  BC.DeviceContext
  -> (SpecialQDiagram Canvas Double, View)
  -> (
    SpecialQDiagram Canvas Double -> View -> IO (SpecialQDiagram Canvas Double)
    , SpecialQDiagram Canvas Double -> Dia.Point Dia.V2 Double -> DiaQuery
    , CreateView
    , (View -> View)
  )
  -> Double
  -> IO b
loop 
  context 
  (moduleDiagram, view) 
  loopControl@(chooseFullOrView, sampleDiagram, progressView, withdrawView) 
  imageScale
  = do
  diagram <- chooseFullOrView moduleDiagram view
  -- print view
  let (moduleDiagramAligned, pointToDiaPoint, sizeSpec) = diagramForBlankCanvas diagram imageScale
  bcDrawDiagram context sizeSpec moduleDiagramAligned
  event <- BC.wait context
  case BC.ePageXY event of
    -- if no mouse location, ignore, and redraw
    Nothing -> loop context (moduleDiagram, withdrawView view) loopControl imageScale
    Just point -> do
      let scaledPoint = pointToDiaPoint point
      let clicked = sampleDiagram moduleDiagramAligned scaledPoint
      let 
        newView = if not $ null clicked 
          then progressView clicked view 
          else withdrawView view
      putStrLn $ show newView
      loop context (moduleDiagram, newView) loopControl imageScale