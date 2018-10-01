module TestRunner.Opts
  ( Opts(..)
  , rootDir
  , readOpts
  , compilerPath
  , runEntrypointPath
  )
where

import           Intentio.Prelude        hiding ( argument
                                                , option
                                                )

import           Options.Applicative            ( Parser
                                                , ParserInfo
                                                , execParser
                                                , fullDesc
                                                , help
                                                , helper
                                                , info
                                                , long
                                                , metavar
                                                , optional
                                                , strOption
                                                )

data Opts = Opts
  { _rootDir            :: FilePath
  , _compilerPath       :: FilePath
  , _runEntrypointPath  :: Maybe FilePath
  }
  deriving (Show)

makeLenses ''Opts

options :: Parser Opts
options = Opts <$> rootDirO <*> compilerPathO <*> runEntrypointPathO
 where
  rootDirO = strOption $ long "root" <> metavar "path" <> help
    "Test suite root directory"

  compilerPathO = strOption $ long "compiler" <> metavar "path" <> help
    "Path to the tested Intentio Compiler executable"

  runEntrypointPathO =
    optional . strOption $ long "run-entrypoint" <> metavar "path" <> help
      "Path to the executable that wraps RUN commands"

opts :: ParserInfo Opts
opts = info (helper <*> options) fullDesc

readOpts :: IO Opts
readOpts = execParser opts
