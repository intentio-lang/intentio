module Intentio.Cache.WorkDir where

import           Intentio.Prelude

import           System.Directory               ( getCurrentDirectory
                                                , createDirectoryIfMissing
                                                )
import           System.FilePath                ( (</>) )

import           Intentio.Compiler              ( Compile
                                                , liftIOE
                                                )

workDirRootName :: FilePath
workDirRootName = ".intentio-work"

getWorkDirRoot :: Compile FilePath
getWorkDirRoot = do
  cwd <- liftIOE getCurrentDirectory
  let wd = cwd </> workDirRootName
  liftIOE $ createDirectoryIfMissing True wd
  return wd

getWorkDir :: FilePath -> Compile FilePath
getWorkDir name = do
  rwd <- getWorkDirRoot
  let wd = rwd </> name
  liftIOE $ createDirectoryIfMissing True wd
  return wd
