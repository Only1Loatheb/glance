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
import           Types  ( SpecialDiagram
                        , SpecialBackend
                        , NameQuery
                        )
---------------------------------------
import IconToSymbolDiagram(ColorStyle(..), colorScheme, multilineComment)

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
  moduleDiagram <- diagramFromModule inputFilename includeComments
  let moduleDiagramAligned = Dia.alignTL moduleDiagram
  BC.blankCanvas 3000 { BC.events = ["mousedown"] } $ \ context -> loop context moduleDiagramAligned imageScale
  -- customRenderSVG outputFilename (Dia.mkWidth imageWidth) moduleDiagram
  putStrLn $ "Successfully wrote " ++ outputFilename

-- loop :: SpecialBackend b Double => BC.DeviceContext ->  IO (SpecialDiagram b Double) -> IO ()
loop :: BC.DeviceContext
  -> Dia.QDiagram Canvas Dia.V2 Double NameQuery -> Double -> IO b
loop context moduleDiagram imageScale = do
  let sizeSpec =  Dia.dims2D (imageScale * Dia.width moduleDiagram) (imageScale * Dia.height moduleDiagram)
  let moduleDrawing = Dia.bg (backgroundC colorScheme) $ Dia.clearValue moduleDiagram
  BC.send context $ Dia.renderDia CV.Canvas (CanvasOptions sizeSpec) moduleDrawing 

  event <- BC.wait context
  case BC.ePageXY event of
    -- if no mouse location, ignore, and redraw
    Nothing -> loop context moduleDiagram imageScale
    Just point@(x, y) -> do
      let scaledPoint = (1.0/imageScale) Dia.*^ Dia.p2 (x,-y)
      print scaledPoint

      print $ Dia.sample  moduleDiagram scaledPoint
      -- let maybeClickedIcon =  pointToIcon scaledPoint
      -- print maybeClickedIcon
      loop context moduleDiagram imageScale

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
