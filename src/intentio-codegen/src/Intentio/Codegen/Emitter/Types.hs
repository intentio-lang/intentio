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

data CModuleHeader
data CModuleSource

data CModuleDef t = CModuleDef
  { _cModuleDefSourcePos    :: SourcePos
  , _cModuleDefName         :: ModuleName
  , _cModuleDefDefinitions  :: [C.Definition]
  }
  deriving (Show, Eq, Generic)

makeLenses ''CModuleDef

instance HasSourcePos (CModuleDef t) where
  _sourcePos = _cModuleDefSourcePos

instance Module (CModuleDef t) where
  type ItemTy (CModuleDef t) = Void
  _moduleName = _cModuleDefName
  _moduleItems = const []
