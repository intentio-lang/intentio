module Intentio.Foo
  ( someFunc
  )
where

import           Intentio.Prelude

someFunc :: IO ()
someFunc = putStrLn "{{cookiecutter.package_name}}"
