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
    diagram <- diagramFromModule doIncludeComments moduleGraphs Nothing
    customRenderSVG' outputFilename (Dia.mkWidth 500) diagram
    putStrLn $ "Saving file: " ++ outputFilename

chooseFullOrView doIncludeComments getCodeFragment moduleGraphs maybeView = do
  let diagramFromModule' = diagramFromModule doIncludeComments
  case maybeView of 
    Just (view, srcRef) -> do 
      let codeString = getCodeFragment srcRef
      putStrLn $ showSrcInfo srcRef
      moduleDiagram <- diagramFromModule' view (Just codeString)
      pure (moduleDiagram)
    _ -> do
      moduleDiagram <- diagramFromModule' moduleGraphs Nothing
      pure (moduleDiagram)



sampleDiagram = Dia.sample

createView clicked moduleGraphs = Just (viewGraphs, codeRef) where
  firstClickedValue = head clicked
  codeRef =  nodeSrcRef firstClickedValue
  viewGraphs = selectView firstClickedValue moduleGraphs