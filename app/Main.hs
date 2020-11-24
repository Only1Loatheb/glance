{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Main
  (main
  , CMD.CmdLineOptions(..)) where

import Prelude hiding (return)

-- Note: (#) and (&) are hidden in all Glance source files, since they would
-- require a special case when translating when Glance is run on its own source
-- code.
import qualified Diagrams.Prelude as Dia hiding ((#), (&))

import SVGrender(customRenderSVG')

import Types (
  SpecialQDiagram
  , SpecialBackend
  , DiaQuery
  , SourceCode
  , SrcRef
  , QueryValue(..)
  , CreateView
  , View
  )
import DrawingColors (getColorStyle,ColorStyle)

import ModuleToGraphs(getModuleGraphs)

import ModuleGraphsToDiagram(
  diagramFromModule
  , staticDiagramFromModule
  , declDiagram
  , nodeDiagram
  , addSourceCodeDiagram
  )

import qualified CmdLineArgs as CMD

import FrontendBlankCanvas( blankCanvasLoop )

import SrcRefToSourceCode (srcRefToSourceCode) 

import Util(showSrcInfo, hasSrcRef, getSrcRef)

main :: IO ()
main = passCmdArgs

passCmdArgs :: IO ()
passCmdArgs = CMD.customExecParser CMD.parserPrefs  CMD.opts >>= prepareDiagram

prepareDiagram :: CMD.CmdLineOptions -> IO ()
prepareDiagram (CMD.CmdLineOptions 
    mode 
    (CMD.BasicOptions 
      inputFilename
      portNumber
      imageScale
      doIncludeComments
    )
    colorStyleType
    )
  = do
  putStrLn $ "Opening file " ++ inputFilename ++ " for visualisation."
  moduleGraphs <- getModuleGraphs inputFilename
  let colorStyle = getColorStyle colorStyleType
  if CMD.isBatch mode 
  then do
    let outputFilename = CMD.getFilename mode
    diagram <- staticDiagramFromModule doIncludeComments moduleGraphs colorStyle
    customRenderSVG' outputFilename imageScale diagram colorStyle
    putStrLn $ "Saving file: " ++ outputFilename
  else pure ()
  if CMD.isInteractive mode
  then do
    source <- readFile inputFilename
    let
      getCodeFragment = srcRefToSourceCode source
      moduleDiagram = diagramFromModule getCodeFragment colorStyle doIncludeComments moduleGraphs
      selectViewWithSourceCode' = selectViewWithSourceCode getCodeFragment colorStyle
      loopControl = (selectViewWithSourceCode', sampleDiagram, progressView, withdrawView)
    blankCanvasLoop moduleDiagram portNumber loopControl imageScale colorStyle
  else pure ()

selectViewWithSourceCode :: SpecialBackend b Double =>
  (SrcRef -> SourceCode) -> ColorStyle Double -> SpecialQDiagram b Double -> View -> IO(SpecialQDiagram b Double)
selectViewWithSourceCode getCodeFragment colorStyle moduleDiagram view = do
  diagram <- selectView colorStyle moduleDiagram view
  if hasSrcRef view
  then do
    let srcRef = getSrcRef view
    putStrLn $ showSrcInfo srcRef
    let diagramWithSourceCode = addSourceCodeDiagram colorStyle diagram $ getCodeFragment srcRef
    pure diagramWithSourceCode
  else pure diagram 


selectView :: SpecialBackend b Double => ColorStyle Double -> SpecialQDiagram b Double -> View -> IO (SpecialQDiagram b Double)
selectView colorStyle moduleDiagram (maybeDeclQV, maybeNodeQV) = do
  case maybeDeclQV of
    Nothing -> pure moduleDiagram
    Just declQueryValue -> do
      case maybeNodeQV of
        Nothing -> declDiagram colorStyle declQueryValue
        Just nodeQueryValue -> nodeDiagram colorStyle declQueryValue nodeQueryValue

sampleDiagram :: SpecialBackend b Double =>
  SpecialQDiagram b Double -> Dia.Point Dia.V2 Double -> DiaQuery
sampleDiagram = Dia.sample

progressView :: CreateView
progressView [] _ = error "got empty DiaQuery"
progressView _ (Nothing, Just _) = error "got incorrect view"

progressView (clicked:_) (Nothing, _ ) = case clicked of
  NodeQv {}         -> (Nothing, Nothing)
  (DeclQv declQV) -> (Just declQV, Nothing)

progressView (clicked:_) (oldDeclQV@(Just _), _ ) = case clicked of
  (NodeQv nodeQV) -> (oldDeclQV, Just nodeQV)
  (DeclQv declQV) -> (Just declQV, Nothing)

withdrawView :: View -> View
withdrawView (oldDeclQV@(Just {}), Just _) = (oldDeclQV, Nothing)
withdrawView _ = (Nothing, Nothing)

