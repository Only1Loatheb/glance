module CmdLineArgs(
  CmdLineOptions(..)
  , BasicOptions(..)
  , Mode(..)
  , parserPrefs
  , opts
  , customExecParser
  , isInteractive
  , isBatch
  , getFilename
  ) where
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
  , strOption
  , optional
  , command
  , subparser
  , (<|>)
  )

import Options.Applicative.Types(ParserPrefs, ParserInfo)

import DrawingColors (ColorStyleType(..))

parserPrefs :: ParserPrefs
parserPrefs = defaultPrefs{prefShowHelpOnError = True}

data CmdLineOptions = CmdLineOptions Mode BasicOptions ColorStyleType
data BasicOptions = BasicOptions {
  inputFilenameOpt :: String
  , portNumberOpt :: Int
  , imageWidthOpt :: Double
  , doIncludeCommentsOpt :: Bool
  }

data Mode = Mode (Maybe String) Bool

isInteractive :: Mode -> Bool
isInteractive (Mode _ True) = True
isInteractive (Mode Nothing False) = True
isInteractive _ = False

isBatch :: Mode -> Bool
isBatch (Mode (Just _) _) = True
isBatch _ = False

getFilename :: Mode -> String
getFilename (Mode (Just fname) _) = fname
getFilename _ = error "Use isBatch"

mainParser :: Parser CmdLineOptions
mainParser = CmdLineOptions <$> modeParser <*> optionParser <*> (colorParser <|> pure ColorsOnBlack)

opts :: ParserInfo CmdLineOptions
opts = info (helper <*> mainParser)
  (fullDesc
  <> progDesc "A visual representation of Haskell."
  <> header "Available options:")

optionParser :: Parser BasicOptions
optionParser = BasicOptions
  <$> argument str (metavar "INPUT_FILE" <> help "Input .hs filename")
  <*> option auto (long "port" <> short 'p' <> value 3000 <> metavar "PORT_NUMBER" <> help "Go to http://localhost:PORT_NUMBER/")
  <*> option auto (long "scale" <> short 's' <> value 1 <> metavar "IMAGE_SCALE" <> help "Output image scale")
  <*> switch (long "comments" <> short 'c' <> help "Include comments between top level declarations.")
  
colorParser :: Parser ColorStyleType
colorParser = subparser (
  command "ColorsOnBlack" (info (pure ColorsOnBlack) (fullDesc <> progDesc "Good for screen")) 
  <> command "ColorsOnWhite" (info (pure ColorsOnWhite) (fullDesc <> progDesc "Good for digital documents"))
  <> command "WhiteOnBlack" (info (pure WhiteOnBlack) (fullDesc <> progDesc "Good for presentation"))
  <> command "BlackOnWhite" (info (pure BlackOnWhite) (fullDesc <> progDesc "Good for printing"))
  )
-- https://stackoverflow.com/questions/57168091/parsing-user-options-into-custom-data-types-with-optparse-applicative

modeParser :: Parser Mode
modeParser = Mode
  <$> optional (strOption (long "output" <> short 'o' <> metavar "OUTPUT_FILE" <> help "Output .svg filename"))
  <*> switch (long "interactive" <> short 'i' <> help "Save to file or start interactive session")