module Intentio.Codegen.Emitter.Types
  ( CModuleHeader
  , CModuleSource
  , CModuleDef(..)
  , cModuleDefSourcePos
  , cModuleDefName
  , cModuleDefDefinitions
  )
where

import           Intentio.Prelude

import qualified Language.C.Quote              as C

import           Intentio.Compiler              ( ModuleName
                                                , Module(..)
                                                )
import           Intentio.Diagnostics           ( SourcePos
                                                , HasSourcePos(..)
                                                )

--------------------------------------------------------------------------------
-- CModule data structure

-- | Type tag representing a C header (@*.h@) file.
data CModuleHeader

-- | Type tag representing a C source (@*.c@) file.
data CModuleSource

-- | A particular Intentio to C translation unit.
--
-- Type parameter @t@ describes whether this is header or source module.
-- @t@ is either 'CModuleHeader' or 'CModuleSource'.
data CModuleDef t = CModuleDef {
    -- | Intentio source file of this module
    _cModuleDefSourcePos    :: SourcePos,
    -- | Intentio module name
    _cModuleDefName         :: ModuleName,
    -- | List of top level C definitions
    _cModuleDefDefinitions  :: [C.Definition]
  }
  deriving (Show, Eq, Generic)

makeLenses ''CModuleDef

instance HasSourcePos (CModuleDef t) where
  _sourcePos = _cModuleDefSourcePos

instance Module (CModuleDef t) where
  type ItemTy (CModuleDef t) = Void
  _moduleName = _cModuleDefName
  _moduleItems = const []
