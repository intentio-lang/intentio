-- | This module re-exports the backbone of Intentio compiler:
--
--   * the 'Assembly' data structure which describes the state of the compilation.
--   * the 'Compile' monad for performing compilation in 'IO' domain,
--     along with 'CompilePure' monad for pure compilations and
--     'CompileT' monad transformer.
--   * the 'CompileCtx' data structure which describes context of the compilation.
module Intentio.Compiler
  ( module Intentio.Compiler.Assembly
  , module Intentio.Compiler.Context
  , module Intentio.Compiler.Monad
  )
where

import           Intentio.Compiler.Assembly
import           Intentio.Compiler.Context      ( CompileCtx )
import           Intentio.Compiler.Monad
