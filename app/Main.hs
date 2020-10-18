{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Main
  (main
  , CmdLineOptions(..)) where

import Prelude hiding (return)
import Data.Maybe

-- Note: (#) and (&) are hidden in all Glance source files, since they would
-- require a special case when translating when Glance is run on its own source
-- code.
import qualified Diagrams.Prelude as Dia hiding ((#), (&))

import SVGrender(customRenderSVG')


import Types (
  SpecialDiagram
  ,SpecialQDiagram
  , SpecialBackend
  , DiaQuery(..)
  , ModuleGraphs
  , NodeQueryValue(..)
  , SourceCode
  , SrcRef
  , ViewGraphs
  , QueryValue(..)
  , CreateView
  , View(..)
  )

import ModuleToGraphs(getModuleGraphs)

import ModuleGraphsToDiagram(
  diagramFromModule
  , staticDiagramFromModule
  , declDiagram
  , nodeDiagram
  , addSourceCodeDiagram
  )

import CmdLineArgs as CMD

import FrontendBlankCanvas( blankCanvasLoop )

import SrcRefToSourceCode (srcRefToSourceCode) 

import Util(showSrcInfo, hasSrcRef, getSrcRef)

-- {-# ANN module "HLint: ignore Unnecessary hiding" #-}
main :: IO ()
main = passCmdArgs

passCmdArgs :: IO ()
passCmdArgs = CMD.customExecParser CMD.parserPrefs  CMD.opts >>= prepareDiagram

prepareDiagram :: CMD.CmdLineOptions -> IO ()
prepareDiagram (CMD.CmdLineOptions
             inputFilename
             outputFilename
             portNumber
             imageScale
             doIncludeComments
             isInteractive)
  = do
  putStrLn $ "Opening file " ++ inputFilename ++ " for visualisation."
  moduleGraphs <- getModuleGraphs inputFilename
  if isInteractive
  then do
    source <- readFile inputFilename
    let moduleDiagram = diagramFromModule doIncludeComments moduleGraphs
    let getCodeFragment = srcRefToSourceCode source
    let selectViewWithSourceCode' = selectViewWithSourceCode getCodeFragment
    let loopControl = (selectViewWithSourceCode', sampleDiagram, progressView, withdrawView)
    blankCanvasLoop moduleDiagram portNumber loopControl imageScale
  else do
    diagram <- staticDiagramFromModule doIncludeComments moduleGraphs
    customRenderSVG' outputFilename (Dia.mkWidth 500) diagram
    putStrLn $ "Saving file: " ++ outputFilename

selectViewWithSourceCode :: SpecialBackend b Double =>
  (SrcRef -> SourceCode) -> SpecialQDiagram b Double -> View -> IO(SpecialQDiagram b Double)
selectViewWithSourceCode getCodeFragment moduleDiagram view = do
  diagram <- selectView moduleDiagram view
  if hasSrcRef view
  then do
    let srcRef = getSrcRef view
    putStrLn $ showSrcInfo srcRef
    let diagramWithSourceCode = addSourceCodeDiagram diagram $ getCodeFragment srcRef
    pure (diagramWithSourceCode)
  else pure(diagram) 


selectView :: SpecialBackend b Double => SpecialQDiagram b Double -> View -> IO (SpecialQDiagram b Double)
selectView moduleDiagram (maybeDeclQV, maybeNodeQV) = do
  case maybeDeclQV of
    Nothing -> pure(moduleDiagram)
    Just declQueryValue -> do
      case maybeNodeQV of
        Nothing -> declDiagram declQueryValue
        Just nodeQueryValue -> nodeDiagram declQueryValue nodeQueryValue

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
progressView _ _ = error "progressView"

withdrawView :: View -> View
withdrawView (oldDeclQV@(Just {}), Just _) = (oldDeclQV, Nothing)
withdrawView _ = (Nothing, Nothing)

