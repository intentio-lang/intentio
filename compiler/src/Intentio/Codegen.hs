module Intentio.Codegen
  ( emitCAssembly
  , printCAssembly
  , printCAssemblyToDirectory
  , printCAssemblyToWorkDir
  , runGCC
  )
where

import           Intentio.Codegen.Emitter       ( emitCAssembly
                                                , printCAssembly
                                                , printCAssemblyToDirectory
                                                , printCAssemblyToWorkDir
                                                )
import           Intentio.Codegen.GCC           ( runGCC )
