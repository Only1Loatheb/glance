{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-} -- for "mousedown"
module FrontendBlankCanvas
  ( blankCanvasLoop
  )
where
    
import qualified Diagrams.Prelude as Dia hiding ((#), (&))
import Diagrams.Backend.Canvas as CV
import qualified Graphics.Blank as BC hiding (rotate, scale, ( # ))

import Types (
  SpecialQDiagram
  , SpecialBackend
  , DiaQuery
  , CreateView
  , View
  , NumericType
  , PointType
  )

import DrawingColors(ColorStyle(..))

getBlankCanvasOpts :: Int -> BC.Options
getBlankCanvasOpts  portNumber =  BC.Options {
  BC.port = portNumber
  , BC.events = ["mousedown"] 
  , BC.debug = False
  , BC.root = "."
  , BC.middleware = [BC.local_only]
  , BC.weak = False
  }

diagramForBlankCanvas :: SpecialBackend b
  => SpecialQDiagram b
  -> Double
  -> (SpecialQDiagram b, (Double, Double) -> PointType,  Dia.SizeSpec Dia.V2 Double)
diagramForBlankCanvas moduleDiagram imageScale = (moduleDiagramAligned, pointToDiaPoint, sizeSpec) where
  moduleDiagramAligned = Dia.alignTL moduleDiagram
  pointToDiaPoint _point@(x,y) = (1.0/imageScale) Dia.*^ Dia.p2 (x,-y)
  sizeSpec =  Dia.dims2D (imageScale * Dia.width moduleDiagram) (imageScale * Dia.height moduleDiagram)

bcDrawDiagram :: BC.DeviceContext
  -> Dia.SizeSpec Dia.V2 Double
  -> Dia.QDiagram Canvas Dia.V2 Double m
  -> ColorStyle Double
  -> IO ()
bcDrawDiagram context sizeSpec moduleDiagram colorStyle = do
  BC.send context $ BC.clearRect (0,0,BC.width context, BC.height context)
  let moduleDrawing = Dia.bg (backgroundC colorStyle) $ Dia.clearValue moduleDiagram
  BC.send context $ Dia.renderDia CV.Canvas (CanvasOptions sizeSpec) moduleDrawing   
  
blankCanvasLoop :: SpecialQDiagram Canvas 
  -> Int 
  -> (SpecialQDiagram Canvas -> View -> IO (SpecialQDiagram Canvas)
    , SpecialQDiagram Canvas -> PointType  -> DiaQuery
    , CreateView, View -> View) 
  -> Double 
  -> ColorStyle Double 
  -> IO ()
blankCanvasLoop moduleDiagram portNumber loopControl imageScale colorStyle = do
  let blankCanvasOpts  = getBlankCanvasOpts portNumber
  BC.blankCanvas blankCanvasOpts $ \ context -> loop context (moduleDiagram, (Nothing, Nothing)) loopControl imageScale colorStyle

loop ::
  BC.DeviceContext
  -> (SpecialQDiagram Canvas, View)
  -> (SpecialQDiagram Canvas -> View -> IO (SpecialQDiagram Canvas)
    , SpecialQDiagram Canvas -> PointType -> DiaQuery
    , CreateView
    , View -> View
  )
  -> Double
  -> ColorStyle Double
  -> IO b
loop 
  context 
  (moduleDiagram, view) 
  loopControl@(selectViewWithSourceCode, sampleDiagram, progressView, withdrawView) 
  imageScale
  colorStyle
  = do
  diagram <- selectViewWithSourceCode moduleDiagram view
  -- print view
  let (moduleDiagramAligned, pointToDiaPoint, sizeSpec) = diagramForBlankCanvas diagram imageScale
  bcDrawDiagram context sizeSpec moduleDiagramAligned colorStyle
  event <- BC.wait context
  case BC.ePageXY event of
    -- if no mouse location, ignore, and redraw
    Nothing -> loop context (moduleDiagram, withdrawView view) loopControl imageScale colorStyle
    Just point -> do
      let scaledPoint = pointToDiaPoint point
      let clicked = sampleDiagram moduleDiagramAligned scaledPoint
      let 
        newView = if not $ null clicked 
          then progressView clicked view 
          else withdrawView view
      loop context (moduleDiagram, newView) loopControl imageScale colorStyle