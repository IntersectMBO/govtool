module VVA.CommandLine
    ( Command (..)
    , CommandLineConfig (..)
    , cmdParser
    ) where

import           Options.Applicative

data Command = StartApp | ShowConfig deriving (Show)

data CommandLineConfig
  = CommandLineConfig
      { clcConfigPath :: Maybe FilePath
      , clcCommand    :: Command
      }
  deriving (Show)

cmdParser :: ParserInfo CommandLineConfig
cmdParser =
  info
    (optParser <**> helper)
    ( fullDesc
        <> progDesc "Start VVA Rest API"
        <> header "VVA - Voltaire Voting App"
    )

optParser :: Parser CommandLineConfig
optParser = CommandLineConfig <$> configParser <*> commandParser

configParser :: Parser (Maybe FilePath)
configParser =
  option
    (Just <$> str)
    ( long "config"
        <> metavar "CONFIG"
        <> value Nothing
        <> short 'c'
        <> help "Path to the configuration file."
    )

commandParser :: Parser Command
commandParser =
  hsubparser $
    mconcat
      [ hparser "start-app" "Start VVA" StartApp,
        hparser "show-config" "Show config" ShowConfig
      ]
  where
    hparser name desc cmd =
      command name $ info (pure cmd) (fullDesc <> progDesc desc)
