module Intentioc.Opts
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

import           Intentio.Cache.WorkDir         ( WorkDirComponent(..) )
import           Intentio.Codegen.GCC           ( GCCOptionsComponent(..) )
import           Intentio.Compiler              ( Compile
                                                , Assembly
                                                , AssemblyName(..)
                                                , AssemblyType(..)
                                                , showAssemblyTypeAbbr
                                                , fromAssemblyTypeAbbr
                                                , mkAssembly
                                                , component
                                                )
import           Language.Intentio.Compiler     ( SourceFile(..) )

data Opts = Opts
  { assemblyName :: Maybe Text
  , assemblyType :: AssemblyType
  , outputPath   :: FilePath
  , mainModule   :: FilePath
  , otherModules :: [FilePath]
  , workDir      :: FilePath
  , includeDirs  :: [FilePath]
  , libraryDirs  :: [FilePath]
  }
  deriving (Show)

options :: Parser Opts
options =
  Opts
    <$> assemblyNameO
    <*> assemblyTypeO
    <*> outputPathO
    <*> mainModuleO
    <*> otherModulesO
    <*> workDirO
    <*> includeDirsO
    <*> libraryDirsO
 where
  assemblyNameO =
    (optional <$> strOption)
      $  long "assembly-name"
      <> metavar "name"
      <> help assemblyNameHelp
  assemblyNameHelp =
    "Output assembly name (default: same as first input module name)"

  assemblyTypeO =
    option (maybeReader fromAssemblyTypeAbbr)
      $  long "assembly-type"
      <> short 'T'
      <> metavar "bin|lib"
      <> value Program
      <> showDefaultWith showAssemblyTypeAbbr
      <> help "Output assembly type."

  outputPathO =
    strOption
      $  long "output"
      <> short 'o'
      <> metavar "filename"
      <> value "a.out"
      <> showDefault
      <> help "Write output to <filename>."

  mainModuleO = argument str $ metavar "main" <> help mainModuleHelp
  mainModuleHelp =
    "Main module file in case of program assembly, or the first"
      <> " module in the library assembly."

  otherModulesO =
    many $ argument str $ metavar "filename..." <> help otherModulesHelp
  otherModulesHelp =
    "Other input modules/files to be included in output assembly."

  workDirO =
    strOption
      $  long "workdir"
      <> metavar "dir"
      <> value ".intentio-work"
      <> showDefault
      <> help "Path to the compiler working directory"

  includeDirsO =
    many . strOption $ short 'I' <> metavar "dir" <> help includeDirsHelp
  includeDirsHelp =
    "Add the directory dir to the list of directories to be searched "
      <> "for C header files during compilation. This also affects searching "
      <> "for Intentio Runtime library."

  libraryDirsO =
    many . strOption $ short 'L' <> metavar "dir" <> help libraryDirsHelp
  libraryDirsHelp =
    "Add directory dir to the list of directories to be searched for "
      <> "linked libraries. This also affects searching for "
      <> "the Intentio Runtime library."

opts :: ParserInfo Opts
opts = info (helper <*> options) fullDesc

buildInputAssembly :: Compile (Assembly SourceFile)
buildInputAssembly = do
  o <- liftIO $ execParser opts

  let defName = toS . takeBaseName . mainModule $ o
  let name    = AssemblyName $ fromMaybe defName (assemblyName o)
  let modlist = SourceFile <$> mainModule o :| otherModules o
  let asm     = mkAssembly (assemblyType o) name (outputPath o) modlist

  component .= Just (WorkDirComponent (workDir o))

  let _gccIncludeDirs = includeDirs o
  let _gccLibraryDirs = libraryDirs o
  component .= Just GCCOptionsComponent { .. }

  return asm
