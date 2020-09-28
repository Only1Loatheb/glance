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
  )

import ModuleToDiagram(getModuleGraphs, diagramFromModule, selectView)

import CmdLineArgs as CMD

import FrontendBlankCanvas( blankCanvasLoop )
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
    let loopControl = (chooseFullOrView doIncludeComments, sampleDiagram, createView)
    blankCanvasLoop moduleGraphs portNumber loopControl imageScale
  else do
    diagram <- chooseFullOrView doIncludeComments moduleGraphs Nothing
    customRenderSVG' outputFilename (Dia.mkWidth 500) diagram
    putStrLn $ "Saving file: " ++ outputFilename

chooseFullOrView doIncludeComments moduleGraphs maybeViewGraphs = do
  let displayedGraphs = fromMaybe moduleGraphs maybeViewGraphs
  let diagramFromModule' = diagramFromModule doIncludeComments
  moduleDiagram <- diagramFromModule' displayedGraphs
  pure (moduleDiagram)

sampleDiagram = Dia.sample

createView clicked moduleGraphs = viewGraphs where
  firstClickedValue = head clicked
  viewGraphs = Just $ selectView firstClickedValue moduleGraphs