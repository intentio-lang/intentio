module Intentio.Driver.Opts
  ( buildInputAssembly
  )
where

import           Intentio.Prelude        hiding ( argument
                                                , option
                                                )

import           Options.Applicative            ( Parser
                                                , ParserInfo
                                                , argument
                                                , execParser
                                                , fullDesc
                                                , help
                                                , helper
                                                , info
                                                , long
                                                , maybeReader
                                                , metavar
                                                , option
                                                , short
                                                , showDefault
                                                , showDefaultWith
                                                , str
                                                , strOption
                                                , value
                                                )
import           System.FilePath                ( takeBaseName )

import           Intentio.Compiler              ( Assembly
                                                , AssemblyType(..)
                                                , showAssemblyTypeAbbr
                                                , fromAssemblyTypeAbbr
                                                , mkAssembly
                                                )
import           Language.Intentio.Compiler     ( SourceFile(..) )

data Opts = Opts
  { assemblyName :: Maybe Text
  , assemblyType :: AssemblyType
  , outputPath :: FilePath
  , mainModule :: FilePath
  , otherModules :: [FilePath]
  }
  deriving (Show)

options :: Parser Opts
options =
  Opts
    <$> assemblyName
    <*> assemblyType
    <*> outputPath
    <*> mainModule
    <*> otherModules
 where
  assemblyName =
    (optional <$> strOption) $ long "assembly-name" <> metavar "name" <> help
      "Output assembly name (default: same as first input module name)"

  assemblyType =
    option (maybeReader fromAssemblyTypeAbbr)
      $  long "assembly-type"
      <> short 'T'
      <> metavar "bin|lib"
      <> value Program
      <> showDefaultWith showAssemblyTypeAbbr
      <> help "Output assembly type"

  outputPath =
    strOption
      $  long "output"
      <> short 'o'
      <> metavar "filename"
      <> value "a.out"
      <> showDefault
      <> help "Write output to <filename>"

  mainModule =
    argument str
      $  metavar "main"
      <> help
           "Main module file in case of program assembly, or the first module in the library assembly"

  otherModules = many $ argument str $ metavar "filename..." <> help
    "Other input modules/files to be included in output assembly"

opts :: ParserInfo Opts
opts = info (helper <*> options) fullDesc

mkAssemblyFromOpts :: Opts -> Assembly SourceFile
mkAssemblyFromOpts o = mkAssembly (assemblyType o) name (outputPath o) modlist
 where
  name    = fromMaybe (toS . takeBaseName . mainModule $ o) (assemblyName o)
  modlist = SourceFile <$> mainModule o :| otherModules o

buildInputAssembly :: IO (Assembly SourceFile)
buildInputAssembly = mkAssemblyFromOpts <$> execParser opts
