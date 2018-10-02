module Intentio.Cache.WorkDir
  ( WorkDirComponent(..)
  , getWorkDirRoot
  , getWorkDir
  )
where

import           Intentio.Prelude

import           System.FilePath                ( (</>) )
import           System.Directory               ( createDirectoryIfMissing
                                                , makeAbsolute
                                                )

import           Intentio.Compiler              ( Compile
                                                , getComponent
                                                , liftIOE
                                                )

newtype WorkDirComponent = WorkDirComponent { unWorkDirComponent :: FilePath }
  deriving (Show, Eq)

getWorkDirRoot :: Compile FilePath
getWorkDirRoot = do
  wdc <- getComponent @WorkDirComponent
  wd  <- liftIO . makeAbsolute . unWorkDirComponent $ wdc
  liftIOE $ createDirectoryIfMissing True wd
  return wd

getWorkDir :: FilePath -> Compile FilePath
getWorkDir name = do
  rwd <- getWorkDirRoot
  let wd = rwd </> name
  liftIOE $ createDirectoryIfMissing True wd
  return wd
