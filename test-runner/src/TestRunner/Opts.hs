module TestRunner.Opts
  ( Opts(..)
  , rootDir
  , readOpts
  , compilerPath
  , runEntrypointPath
  , noCleanup
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
                                                , switch
                                                )
import           System.Directory               ( makeAbsolute )

data Opts = Opts
  { _rootDir            :: FilePath
  , _compilerPath       :: FilePath
  , _runEntrypointPath  :: Maybe FilePath
  , _noCleanup       :: Bool
  }
  deriving (Show)

makeLenses ''Opts

options :: Parser Opts
options =
  Opts <$> rootDirO <*> compilerPathO <*> runEntrypointPathO <*> noCleanupO
 where
  rootDirO = strOption $ long "root" <> metavar "path" <> help
    "Test suite root directory"

  compilerPathO = strOption $ long "compiler" <> metavar "path" <> help
    "Path to the tested Intentio Compiler executable"

  runEntrypointPathO =
    optional . strOption $ long "run-entrypoint" <> metavar "path" <> help
      "Path to the executable that wraps RUN commands"

  noCleanupO = switch $ long "no-cleanup" <> help
    "Do not remove temporary working directories"

opts :: ParserInfo Opts
opts = info (helper <*> options) fullDesc

readOpts :: IO Opts
readOpts = execParser opts >>= absPaths

absPaths :: Opts -> IO Opts
absPaths o = do
  rootDir'           <- makeAbsolute $ o ^. rootDir
  compilerPath'      <- makeAbsolute $ o ^. compilerPath
  runEntrypointPath' <- case o ^. runEntrypointPath of
    Nothing -> return Nothing
    Just p  -> Just <$> makeAbsolute p
  return o { _rootDir           = rootDir'
           , _compilerPath      = compilerPath'
           , _runEntrypointPath = runEntrypointPath'
           }
