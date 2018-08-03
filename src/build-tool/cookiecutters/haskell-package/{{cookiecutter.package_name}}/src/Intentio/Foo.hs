module Intentio.Foo
  ( someFunc
  )
where

import           Intentio.Prelude

someFunc :: IO ()
someFunc = putText "{{cookiecutter.package_name}}"
