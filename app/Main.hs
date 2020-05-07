{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
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

import Util(customRenderSVG)

import ModuleToDiagram(diagramFromModule)


{-# ANN module "HLint: ignore Unnecessary hiding" #-}

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

renderFile :: CmdLineOptions -> IO ()
renderFile (CmdLineOptions
             inputFilename
             outputFilename
             imageWidth
             includeComments)
  = do
  putStrLn $ "Translating file " ++ inputFilename ++ " into a Glance image."
  moduleDiagram <- diagramFromModule inputFilename includeComments
  customRenderSVG outputFilename (Dia.mkWidth imageWidth) moduleDiagram
  putStrLn $ "Successfully wrote " ++ outputFilename

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
