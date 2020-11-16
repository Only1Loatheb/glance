module CmdLineArgs(
  CmdLineOptions(..)
  , parserPrefs
  , opts
  , customExecParser
  ) where

import qualified Diagrams.Prelude as Dia hiding ((#), (&))

import           Options.Applicative (
  header
  , progDesc
  , fullDesc
  , helper
  , info
  , defaultPrefs
  , customExecParser
  , help
  , short
  , switch
  , metavar
  , auto
  , argument
  , str
  , prefShowHelpOnError
  , long
  , value
  , option
  , Parser
  )

import Options.Applicative.Types(ParserPrefs, ParserInfo)


data CmdLineOptions = CmdLineOptions {
  inputFilenameOpt :: String
  , outputFilenameOpt :: String
  , portNumberOpt :: Int
  , imageWidthOpt :: Double
  , doIncludeCommentsOpt :: Bool
  , isInteractiveOpt :: Bool
  }

optionParser :: Parser CmdLineOptions
optionParser = CmdLineOptions
  <$> argument str (metavar "INPUT_FILE" Dia.<> help "Input .hs filename")
  <*> argument str (metavar "OUTPUT_FILE" Dia.<> help "Output .svg filename")
  <*> option auto (long "port" Dia.<> short 'p' Dia.<> value 3000 Dia.<> metavar "PORT_NUMBER" Dia.<> help "Go to http://localhost:PORT_NUMBER/")
  <*> option auto (long "scale" Dia.<> short 's' Dia.<> value 1 Dia.<> metavar "IMAGE_SCALE" Dia.<> help "Output image scale")
  <*> switch (long "comments" Dia.<> short 'c' Dia.<> help "Include comments between top level declarations.")
  <*> switch (long "interactive" Dia.<> short 'i' Dia.<> help "Save to file or start interactive session")
  -- TODO add port option
  -- TODO add which function are detiled option

parserPrefs :: ParserPrefs
parserPrefs = defaultPrefs{
  prefShowHelpOnError = True
  }

opts :: ParserInfo CmdLineOptions
opts = info (helper <*> optionParser)
  (fullDesc
  Dia.<> progDesc "SimpSyntaxToSyntaxGraph a Haskell source file (.hs) into an SVG image."
  Dia.<> header "Glance - a visual representation of Haskell")