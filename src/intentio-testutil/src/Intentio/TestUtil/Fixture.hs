module Intentio.TestUtil.Fixture
  ( Fixture(..)
  , FixtureMaterializable(..)
  , runFixture
  , runFixtures
  , runFileFixtures
  )
where

import           Intentio.Prelude

import           System.Directory
import           System.FilePath

import           Test.Hspec

--------------------------------------------------------------------------------
-- Fixture type class.

class Fixture f where
  fixtureName          :: f -> String
  getFixtureInput      :: f -> IO Text
  getFixtureExpected   :: f -> IO (Maybe Text)
  writeFixtureExpected :: f -> Text -> IO ()

class FixtureMaterializable a where
  fixtureMaterialize :: a -> Text

--------------------------------------------------------------------------------
-- Fixture-based test framework.

-- | Build specification for given fixture.
-- The specification is names as the fixture.
runFixture
  :: (Fixture f, FixtureMaterializable m)
  => (Text -> m)
  -> f
  -> SpecWith ()
runFixture s f =
  it (fixtureName f) $ do
    input <- getFixtureInput f
    let actual = fixtureMaterialize . s $ input
    getFixtureExpected f >>= \case
      Nothing -> do
        putStrLn $ "WARN: generating test case for fixture " ++ fixtureName f
        writeFixtureExpected f actual
        True `shouldBe` True
      Just expected -> actual `shouldBe` expected

-- | Build multiple specifications for given list of fixtures.
runFixtures
  :: (Fixture f, FixtureMaterializable m)
  => (Text -> m)
  -> [f]
  -> SpecWith ()
runFixtures s = mapM_ (runFixture s)

--------------------------------------------------------------------------------
-- File fixtures.

data FileFixture = FileFixture {
      name :: String,
      inputPath :: FilePath,
      expectedPath :: FilePath
    }
    deriving (Eq, Show)

instance Fixture FileFixture where
  fixtureName = name

  getFixtureInput = readFile . inputPath

  getFixtureExpected f = do
    exists <- doesFileExist $ expectedPath f
    if exists
    then Just <$> readFile (expectedPath f)
    else return Nothing

  writeFixtureExpected = writeFile . expectedPath

getFileFixtures :: String -> IO [FileFixture]
getFileFixtures prefix = do
  cwd <- getCurrentDirectory
  let path = cwd </> "fixtures" </> prefix
  createDirectoryIfMissing True path
  path
    &   listDirectory
    <&> filter notExpected
    <&> map (path </>)
    <&> map mkFileFixtureFromInputPath
 where
  expectedExt = ".expected"

  notExpected p = takeExtension p /= expectedExt

  mkFileFixtureFromInputPath inputPath = FileFixture
    { name         = takeBaseName inputPath
    , inputPath    = inputPath
    , expectedPath = addExtension inputPath expectedExt
    }

-- | Run file fixtures from specified fixture directory
-- The fixture directory is @$PROJECT_ROOT/fixtures/$PREFIX/@.
-- Creates prefix directory if it does not exist.
runFileFixtures
  :: FixtureMaterializable m
  => String                  -- ^ Prefix directory of fixtures
  -> (Text -> m)             -- ^ Test function
  -> SpecWith ()
runFileFixtures prefix s = runIO (getFileFixtures prefix) >>= runFixtures s
