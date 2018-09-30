module TestRunner.Opts
  ( Opts(..)
  , rootDir
  , readOpts
  , compilerPath
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
                                                , short
                                                , strOption
                                                )

data Opts = Opts { _rootDir :: FilePath, _compilerPath :: FilePath }
  deriving (Show)

makeLenses ''Opts

options :: Parser Opts
options = Opts <$> rootDirO <*> compilerPathO
 where
  rootDirO = strOption $ long "root" <> short 'd' <> metavar "path" <> help
    "Test suite root directory"

  compilerPathO =
    strOption $ long "compiler" <> short 'c' <> metavar "path" <> help
      "Path to the tested Intentio Compiler executable"

opts :: ParserInfo Opts
opts = info (helper <*> options) fullDesc

readOpts :: IO Opts
readOpts = execParser opts
