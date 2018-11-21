-- | This module re-exports the backbone of Intentio compiler:
--
--   * the 'Assembly' data structure which describes the state of the compilation.
--   * the 'Compile' monad for performing compilation in 'IO' domain,
--     along with 'CompilePure' monad for pure compilations and
--     'CompileT' monad transformer.
module Intentio.Compiler
  ( module Intentio.Compiler.Monad
  , module Language.Intentio.Assembly
  , module Intentio.Compiler.AssemblyMonad
  )
where

import           Intentio.Compiler.Monad
import           Language.Intentio.Assembly
import           Intentio.Compiler.AssemblyMonad
