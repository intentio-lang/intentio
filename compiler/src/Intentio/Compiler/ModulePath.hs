module Intentio.Compiler.ModulePath
  ( ModulePath(..)
  , resolveModuleFile
  , resolveModuleFiles
  , injectStd
  )
where

import           Intentio.Prelude

import           System.Directory               ( doesFileExist
                                                , makeAbsolute
                                                )
import           System.FilePath                ( (</>) )

import           Intentio.Compiler              ( Assembly
                                                , Compile
                                                , assemblyModules
                                                , mapModulesM
                                                , mkModuleMap
                                                , pushErrorFor
                                                , requireComponent
                                                )
import           Language.Intentio.Compiler     ( SourceFile(..) )

newtype ModulePath = ModulePath [FilePath]
  deriving (Show, Eq)

resolveModuleFile :: SourceFile -> Compile SourceFile
resolveModuleFile (SourceFile sf) = do
  ModulePath modulePath <- requireComponent
  let possibilities = (</> sf) <$> modulePath
  go possibilities
 where
  go []       = pushErrorFor (SourceFile sf) $ "Cannot find module: " <> toS sf
  go (p : ps) = lift (doesFileExist p) >>= \case
    True  -> liftIO $ SourceFile <$> makeAbsolute p
    False -> go ps


resolveModuleFiles :: Assembly SourceFile -> Compile (Assembly SourceFile)
resolveModuleFiles = mapModulesM resolveModuleFile

stdModules :: NonEmpty SourceFile
stdModules = SourceFile <$> fromList ["prelude.ieo", "simpleparsing.ieo"]

injectStd :: Assembly SourceFile -> Assembly SourceFile
injectStd = assemblyModules <>~ mkModuleMap stdModules
