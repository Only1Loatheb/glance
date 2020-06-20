{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  (main
  , CmdLineOptions(..)) where

import Prelude hiding (return)

-- Note: (#) and (&) are hidden in all Glance source files, since they would
-- require a special case when translating when Glance is run on its own source
-- code.
import qualified Diagrams.Prelude as Dia hiding ((#), (&))

-- Options.Applicative does not seem to work qualified
import Options.Applicative(header, progDesc, fullDesc, helper, info
                          , defaultPrefs, customExecParser, help, short, switch
                          , metavar, auto, argument, str, prefShowHelpOnError
                          , Parser)
--------------------------------------
import Util(customRenderSVG)
--------------------------------------
import           Data.Text (Text)
import Diagrams.Backend.Canvas as CV
-- import           Control.Concurrent
import qualified Graphics.Blank as BC hiding (rotate, scale, ( # ))
import           Types  ( SpecialQDiagram
                        , SpecialBackend
                        )
---------------------------------------

import ModuleToDiagram(diagramFromModule)


-- {-# ANN module "HLint: ignore Unnecessary hiding" #-}

data CmdLineOptions = CmdLineOptions {
  cmdInputFilename :: String,
  cmdOutputFilename :: String,
  cmdImageWidth :: Double,
  cmdIncludeComments :: Bool
  }

optionParser :: Parser CmdLineOptions
optionParser = CmdLineOptions
  <$> argument str (metavar "INPUT_FILE" Dia.<> help "Input .hs filename")
  <*> argument str (metavar "OUTPUT_FILE" Dia.<> help "Output .svg filename")
  <*> argument auto (metavar "IMAGE_WIDTH" Dia.<> help "Output image width")
  <*> switch
  (short 'c' Dia.<> help "Include comments between top level declarations.")
  -- TODO add port option
  -- TODO add which function are detiled option
renderFile :: CmdLineOptions -> IO ()
renderFile (CmdLineOptions
             inputFilename
             outputFilename
             imageScale
             includeComments)
  = do
  putStrLn $ "Translating file " ++ inputFilename ++ " into a Glance image."
  moduleDiagramAndPointToIcon <- diagramFromModule inputFilename includeComments
  BC.blankCanvas 3000 { BC.events = ["mousedown"] } $ \ context -> loop context moduleDiagramAndPointToIcon imageScale
  -- customRenderSVG outputFilename (Dia.mkWidth imageWidth) moduleDiagram
  putStrLn $ "Successfully wrote " ++ outputFilename

-- loop :: SpecialBackend b Double => BC.DeviceContext ->  IO (SpecialQDiagram b Double) -> IO ()
loop context moduleDiagramAndPointToIcon imageScale = do
  let (moduleDiagram, pointToIcon) = moduleDiagramAndPointToIcon
  let sizeSpec =  Dia.dims2D (imageScale * Dia.width moduleDiagram) (imageScale * Dia.height moduleDiagram)
  BC.send context $ Dia.renderDia CV.Canvas (CanvasOptions sizeSpec) moduleDiagram

  event <- BC.wait context
  case BC.ePageXY event of
    -- if no mouse location, ignore, and redraw
    Nothing -> loop context moduleDiagramAndPointToIcon imageScale
    Just point -> do
      let scaledPoint = (1.0/imageScale) Dia.*^ Dia.p2 point
      let maybeClickedIcon =  pointToIcon scaledPoint
      print point
      print maybeClickedIcon
      loop context moduleDiagramAndPointToIcon imageScale

    -- [0.0,11.4,42.98,54.379999999999995,88.47999999999999,99.88,153.62333319266665]

translateFileMain :: IO ()
translateFileMain = customExecParser parserPrefs  opts >>= renderFile where

  parserPrefs = defaultPrefs{
    prefShowHelpOnError = True
    }

  opts = info (helper <*> optionParser)
    (fullDesc
    Dia.<> progDesc "SimpSyntaxToSyntaxGraph a Haskell source file (.hs) into an SVG image."
    Dia.<> header "Glance - a visual representation of Haskell")

main :: IO ()
main = translateFileMain
