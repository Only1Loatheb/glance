{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-} -- for "mousedown"
{-# LANGUAGE PatternSynonyms #-}

module FrontendBlankCanvas
  ( blankCanvasLoop
  )
where
    
import Data.String(IsString)
import qualified Diagrams.Prelude as Dia hiding ((#), (&))
import Diagrams.Backend.Canvas as CV
import qualified Graphics.Blank as BC hiding (rotate, scale, ( # ))

import Types (
  QueryableDrawing
  , DrawingBackend
  , DiaQuery
  , CreateView
  , View
  , PointType
  , ColorStyle
  , ColorStyle'(..)
  )

pattern LMBkeyCode :: Int
pattern LMBkeyCode = 1
pattern RMBkeyCode :: Int
pattern RMBkeyCode = 3

pattern MouseDownEvent :: (Eq a, IsString a) => a
pattern MouseDownEvent = "mousedown"
pattern KeyPressEvent :: (Eq a, IsString a) => a
pattern KeyPressEvent = "keypress"

getBlankCanvasOpts :: Int -> BC.Options
getBlankCanvasOpts  portNumber =  BC.Options {
  BC.port = portNumber
  , BC.events = [MouseDownEvent,KeyPressEvent] 
  , BC.debug = False
  , BC.root = "."
  , BC.middleware = [BC.local_only]
  , BC.weak = False
  }

diagramForBlankCanvas ::
  QueryableDrawing Canvas
  -> Double
  -> (QueryableDrawing Canvas, (Double, Double) -> PointType,  Dia.SizeSpec Dia.V2 Double)
diagramForBlankCanvas moduleDiagram imageScale = (moduleDiagramAligned, pointToDiaPoint, sizeSpec) where
  moduleDiagramAligned = Dia.alignTL moduleDiagram
  pointToDiaPoint _point@(x,y) = (1.0/imageScale) Dia.*^ Dia.p2 (x,-y)
  sizeSpec =  Dia.dims2D (imageScale * Dia.width moduleDiagram) (imageScale * Dia.height moduleDiagram)

bcDrawDiagram :: BC.DeviceContext
  -> Dia.SizeSpec Dia.V2 Double
  -> Dia.QDiagram Canvas Dia.V2 Double m
  -> ColorStyle
  -> IO ()
bcDrawDiagram context sizeSpec moduleDiagram colorStyle = do
  BC.send context $ BC.clearRect (0,0,BC.width context, BC.height context)
  let moduleDrawing = Dia.bg (backgroundC colorStyle) $ Dia.clearValue moduleDiagram
  BC.send context $ Dia.renderDia CV.Canvas (CanvasOptions sizeSpec) moduleDrawing   
  
blankCanvasLoop :: QueryableDrawing Canvas 
  -> Int 
  -> (QueryableDrawing Canvas -> View -> IO (QueryableDrawing Canvas)
    , QueryableDrawing Canvas -> PointType  -> DiaQuery
    , CreateView
    , View -> View
    ) 
  -> Double 
  -> ColorStyle 
  -> IO ()
blankCanvasLoop moduleDiagram portNumber loopControl imageScale colorStyle = do
  let blankCanvasOpts  = getBlankCanvasOpts portNumber
  BC.blankCanvas blankCanvasOpts $ \ context -> loop context (moduleDiagram, (Nothing, Nothing)) loopControl imageScale colorStyle

loop ::
  BC.DeviceContext
  -> (QueryableDrawing Canvas, View)
  -> (QueryableDrawing Canvas -> View -> IO (QueryableDrawing Canvas)
    , QueryableDrawing Canvas -> PointType -> DiaQuery
    , CreateView
    , View -> View
    )
  -> Double
  -> ColorStyle
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
  
  case (BC.eType event, BC.ePageXY event, BC.eWhich event) of
    (MouseDownEvent, Just point, Just keyCode) -> do
      let 
        scaledPoint = pointToDiaPoint point
        clicked = sampleDiagram moduleDiagramAligned scaledPoint 
        newView = case keyCode of
          LMBkeyCode | not $ null clicked ->  progressView clicked view 
          RMBkeyCode -> withdrawView view
          _ -> view
      loop context (moduleDiagram, newView) loopControl imageScale colorStyle
    _ -> loop context (moduleDiagram, view) loopControl imageScale colorStyle
-- https://github.com/ku-fpg/blank-canvas/blob/master/examples/keyread/Main.hs