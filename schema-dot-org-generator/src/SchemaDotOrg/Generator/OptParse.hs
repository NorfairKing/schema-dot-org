{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module SchemaDotOrg.Generator.OptParse where

import Autodocodec
import Autodocodec.Yaml
import Control.Applicative
import Data.Maybe
import qualified Data.Text as T
import Data.Yaml (FromJSON, ToJSON)
import qualified Env
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO

data Instructions
  = Instructions !Dispatch !Settings
  deriving (Show, Eq, Generic)

getInstructions :: IO Instructions
getInstructions = do
  args@(Arguments _ flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions args env config

-- | A product type for the settings that are common across commands
data Settings = Settings
  { settingSchemasFile :: !(Path Abs File)
  }
  deriving (Show, Eq, Generic)

-- | A sum type for the commands and their specific settings
data Dispatch
  = DispatchGenerate
  | DispatchGraph
  deriving (Show, Eq, Generic)

combineToInstructions :: Arguments -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (Arguments cmd Flags {..}) Environment {..} mConf = do
  settingSchemasFile <- resolveFile' $ fromMaybe flagSchemasFile $ envSchemasFile <|> mc configSchemasFile
  let sets = Settings {..}
  disp <-
    case cmd of
      CommandGenerate -> pure DispatchGenerate
      CommandGraph -> pure DispatchGraph
  pure $ Instructions disp sets
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc f = mConf >>= f

data Configuration = Configuration
  { configSchemasFile :: !(Maybe FilePath)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

-- | We use @autodocodec@ for parsing a YAML config.
instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalField "schemas-file" "Path to the schemas file" .= configSchemasFile

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> pure Nothing
    Just cf -> do
      afp <- resolveFile' cf
      readYamlConfigFile afp

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envSchemasFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "SCHEMA_DOT_ORG_GENERATOR_" $
    Environment
      <$> optional (Env.var Env.str "CONFIG_FILE" (Env.help "Config file"))
      <*> optional (Env.var Env.str "SCHEMAS_FILE" (Env.help "Path to the schemas file"))

data Arguments
  = Arguments !Command !Flags
  deriving (Show, Eq, Generic)

getArguments :: IO Arguments
getArguments = customExecParser prefs_ argParser

prefs_ :: OptParse.ParserPrefs
prefs_ =
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

argParser :: OptParse.ParserInfo Arguments
argParser =
  OptParse.info
    (OptParse.helper <*> parseArgs)
    (OptParse.fullDesc <> OptParse.footerDoc (Just $ OptParse.string footerStr))
  where
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (renderColouredSchemaViaCodec @Configuration)
        ]

parseArgs :: OptParse.Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

data Command
  = CommandGenerate
  | CommandGraph
  deriving (Show, Eq, Generic)

parseCommand :: OptParse.Parser Command
parseCommand =
  OptParse.hsubparser $
    mconcat
      [ OptParse.command "generate" $ CommandGenerate <$ parseCommandGenerate,
        OptParse.command "graph" $ CommandGraph <$ parseCommandGraph
      ]

parseCommandGenerate :: OptParse.ParserInfo ()
parseCommandGenerate = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Generate the schema.org library code"
    parser = pure ()

parseCommandGraph :: OptParse.ParserInfo ()
parseCommandGraph = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Generate a graph of schema.org schemas"
    parser = pure ()

data Flags = Flags
  { flagSchemasFile :: !FilePath,
    flagConfigFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic)

-- | The 'optparse-applicative' parser for the 'Flags'.
parseFlags :: OptParse.Parser Flags
parseFlags =
  Flags
    <$> strArgument
      ( mconcat
          [ help "Path to a schemas file",
            metavar "FILEPATH"
          ]
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                help "Path to a config file",
                metavar "FILEPATH"
              ]
          )
      )
