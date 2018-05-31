module Intentio.TestUtil where

import           Intentio.Prelude

import System.Directory
import System.FilePath

--------------------------------------------------------------------------------
-- Tools for fixture based tests.

data FixtureInfo = FixtureInfo {
      name :: String,
      inputPath :: FilePath,
      expectedPath :: FilePath
    }
    deriving (Eq, Show)

mkFixtureInfo :: FilePath -> FixtureInfo
mkFixtureInfo inputPath = FixtureInfo {
    name = takeBaseName inputPath,
    inputPath = inputPath,
    expectedPath = addExtension inputPath ".expected"
  }

-- | Returns list of fixture infos with given prefix.
-- The fixture directory is @$PROJECT_ROOT/fixtures/$PREFIX/@.
-- Creates prefix directory if it does not exist.
getFixtures
  :: String -- ^ Prefix directory of fixtures
  -> IO [FixtureInfo]
getFixtures prefix = do
  cwd <- getCurrentDirectory
  let path = cwd </> "fixtures" </> prefix
  createDirectoryIfMissing True path
  inputs <- listDirectory path
  return $ (mkFixtureInfo . (path </>)) `fmap` inputs
