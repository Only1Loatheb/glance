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
  , DiaQuery
  , ModuleGraphs
  , QueryValue(..)
  )

import ModuleToDiagram(getModuleGraphs, diagramFromModule, selectView)

import CmdLineArgs as CMD

import FrontendBlankCanvas( blankCanvasLoop )

import SrcRefToSourceCode (srcRefToSourceCode) 

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
    let createView' = createView getCodeFragment
    let loopControl = (chooseFullOrView doIncludeComments, sampleDiagram, createView')
    blankCanvasLoop moduleGraphs portNumber loopControl imageScale
  else do
    diagram <- chooseFullOrView doIncludeComments moduleGraphs Nothing
    customRenderSVG' outputFilename (Dia.mkWidth 500) diagram
    putStrLn $ "Saving file: " ++ outputFilename

chooseFullOrView doIncludeComments moduleGraphs maybeView = do
  let displayedView = fromMaybe (moduleGraphs,"") maybeView
  let diagramFromModule' = diagramFromModule doIncludeComments
  moduleDiagram <- diagramFromModule' displayedView
  pure (moduleDiagram)

sampleDiagram = Dia.sample

createView getCodeFragment clicked moduleGraphs = Just (viewGraphs, codeString) where
  firstClickedValue = head clicked
  codeString = (getCodeFragment . nodeSrcRef ) firstClickedValue
  viewGraphs = selectView firstClickedValue moduleGraphs