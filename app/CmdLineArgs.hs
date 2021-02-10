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
  , isHorizontalLayout :: Bool
  }

data Mode = Mode (Maybe String) Bool

defaultPort :: Int
defaultPort = 3000

defaultScale :: Double
defaultScale = 1

defaultColorStyle :: ColorStyleType
defaultColorStyle = ColorsOnBlack

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
mainParser = CmdLineOptions <$> modeParser <*> basicOptsParser <*> (colorParser <|> pure defaultColorStyle)

opts :: ParserInfo CmdLineOptions
opts = info (helper <*> mainParser)
  (fullDesc
  <> header "A visual representation of Haskell."
  <> progDesc 
    ("Create visual representation of haskell program from source code. \
    \INPUT_FILE is required. \
    \You can start interactive session and save to file with one command. \
    \If there is no mode flag specified interactive session is lounched. \
    \Dafault color palette is " ++ show defaultColorStyle)
  )

basicOptsParser :: Parser BasicOptions
basicOptsParser = BasicOptions
  <$> argument str (metavar "INPUT_FILE" 
    <> help "Input filename with .hs extension")
  <*> option auto (long "port" <> short 'p' <> value defaultPort <> metavar "PORT_NUMBER" 
    <> help ( "Port number for interactive session. Address defaults to http://localhost:"++ show defaultPort ++"/"))
  <*> option auto (long "scale" <> short 's' <> value defaultScale <> metavar "IMAGE_SCALE" 
    <> help ("Output image scale. Defaults to " ++ show defaultScale))
  <*> switch (long "comments" <> short 'c' 
    <> help "Include comments between top level declarations.")
  <*> switch (long "flip" <> short 'f' 
    <> help "Arrange diagrams horizontally.")
  
colorParser :: Parser ColorStyleType
colorParser = subparser (
  command "ColorsOnBlack" (info (pure ColorsOnBlack) (fullDesc <>    progDesc "Should look good on a screen.")) 
  <> command "ColorsOnWhite" (info (pure ColorsOnWhite) (fullDesc <> progDesc "Should look good in a digital document."))
  <> command "WhiteOnBlack" (info (pure WhiteOnBlack) (fullDesc <>   progDesc "Should look good in a presentation."))
  <> command "BlackOnWhite" (info (pure BlackOnWhite) (fullDesc <>   progDesc "Should look good on a document printed in monochrome."))
  )
-- https://stackoverflow.com/questions/57168091/parsing-user-options-into-custom-data-types-with-optparse-applicative

modeParser :: Parser Mode
modeParser = Mode
  <$> optional (strOption (long "output" <> short 'o' <> metavar "OUTPUT_FILE" 
    <> help "Output filename with .svg extension"))
  <*> switch (long "interactive" <> short 'i'
    <> help "Start interactive session")