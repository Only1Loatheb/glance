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

import Util(customRenderSVG, showSrcInfo)

import Types (
  SpecialDiagram
  ,SpecialQDiagram
  , SpecialBackend
  , DiaQuery
  , ModuleGraphs
  )

import ModuleToDiagram(getModuleGraphs, diagramFromModule, selectView)

import ParseCmdLineArgs as CMD

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
             includeComments)
  = do
  putStrLn $ "Opening file " ++ inputFilename ++ " for visualisation."
  moduleGraphs <- getModuleGraphs inputFilename
  let loopControl = (chooseFullOrView includeComments, sampleDiagram, createView)
  blankCanvasLoop moduleGraphs portNumber loopControl imageScale
  -- customRenderSVG outputFilename (Dia.mkWidth imageWidth) moduleDiagram
  putStrLn $ "Exiting." ++ outputFilename

chooseFullOrView includeComments moduleGraphs maybeViewGraphs = do
  let displayedGraphs = fromMaybe moduleGraphs maybeViewGraphs
  let diagramFromModule' = diagramFromModule includeComments
  moduleDiagram <- diagramFromModule' displayedGraphs
  pure (moduleDiagram)

sampleDiagram = Dia.sample

createView clicked moduleGraphs = viewGraphs where
  firstClickedValue = head clicked
  viewGraphs = Just $ selectView firstClickedValue moduleGraphs