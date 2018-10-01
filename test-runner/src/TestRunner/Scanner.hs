module TestRunner.Scanner
  ( scanDirectory
  )
where

import           Intentio.Prelude

import           System.Directory               ( listDirectory
                                                , makeAbsolute
                                                , doesDirectoryExist
                                                )
import           System.FilePath                ( (</>)
                                                , dropExtensions
                                                , makeRelative
                                                , takeDirectory
                                                , takeExtensions
                                                )

import           TestRunner.Model               ( TestSuite(..)
                                                , TestCaseType(..)
                                                , TestCase(..)
                                                )

testYaml :: String
testYaml = "test.yaml"

scanDirectory :: FilePath -> IO TestSuite
scanDirectory root = do
  rootAbs <- makeAbsolute root
  TestSuite <$> scanDirectory' rootAbs rootAbs

scanDirectory' :: FilePath -> FilePath -> IO [TestCase]
scanDirectory' root cwd = do
  files <- listDirectory cwd
  if testYaml `elem` files
    then processTestFile (cwd </> testYaml)
    else mconcat <$> mapM (processFile . (cwd </>)) files
 where
  processFile :: FilePath -> IO [TestCase]
  processFile file = if takeExtensions file == ".ieo"
    then processSingleFile file
    else do
      isDir <- doesDirectoryExist file
      if isDir then scanDirectory' root file else return []

  processTestFile :: FilePath -> IO [TestCase]
  processTestFile testFilePath = do
    let _testCaseType = MultiFile
    let _testCaseName = makeRelative root . takeDirectory $ testFilePath
    let _testCasePath = testFilePath
    return [TestCase {..}]

  processSingleFile :: FilePath -> IO [TestCase]
  processSingleFile filePath = do
    let _testCaseType = SingleFile
    let _testCaseName = makeRelative root . dropExtensions $ filePath
    let _testCasePath = filePath
    return [TestCase {..}]
