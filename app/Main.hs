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
  , QueryValue(..)
  , SourceCode
  , SrcRef
  , ViewGraphs
  )

import ModuleToDiagram(
  getModuleGraphs
  , diagramFromModule
  , selectView
  , staticDiagramFromModule
  , moduleGraphsToViewGraphs
  )

import CmdLineArgs as CMD

import FrontendBlankCanvas( blankCanvasLoop )

import SrcRefToSourceCode (srcRefToSourceCode) 

import Util(showSrcInfo)

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
    let getCodeFragment = srcRefToSourceCode source
    let chooseFullOrView' = chooseFullOrView doIncludeComments getCodeFragment
    let loopControl = (chooseFullOrView', sampleDiagram, createView)
    blankCanvasLoop moduleGraphs portNumber loopControl imageScale
  else do
    diagram <- staticDiagramFromModule doIncludeComments moduleGraphs
    customRenderSVG' outputFilename (Dia.mkWidth 500) diagram
    putStrLn $ "Saving file: " ++ outputFilename

chooseFullOrView :: SpecialBackend b Double =>
  Bool -> (SrcRef -> SourceCode) -> ModuleGraphs -> Maybe (ViewGraphs, SrcRef) -> IO (SpecialQDiagram b Double)
chooseFullOrView doIncludeComments getCodeFragment moduleGraphs maybeView = do
  let diagramFromModule' = diagramFromModule doIncludeComments
  case maybeView of 
    Just (viewGraphs, srcRef) -> do 
      let codeString = getCodeFragment srcRef
      putStrLn $ showSrcInfo srcRef
      let 
      moduleDiagram <- diagramFromModule' viewGraphs (Just codeString)
      pure (moduleDiagram)
    _ -> do
      moduleDiagram <- diagramFromModule' (moduleGraphsToViewGraphs moduleGraphs) Nothing-- (createOverview moduleGraphs) Nothing
      pure (moduleDiagram)


sampleDiagram :: SpecialBackend b Double =>
  SpecialQDiagram b Double -> Dia.Point Dia.V2 Double -> DiaQuery
sampleDiagram = Dia.sample

createView :: DiaQuery -> ModuleGraphs -> Maybe (ViewGraphs, SrcRef)
createView clicked moduleGraphs = Just (viewGraphs, srcRef) where
  firstClickedValue = head clicked
  srcRef =  nodeSrcRef firstClickedValue
  viewGraphs = selectView firstClickedValue moduleGraphs

-- createOverview moduleGraphs = 