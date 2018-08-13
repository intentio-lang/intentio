module Intentio.Prelude
  ( module X
  , unreachable
  )
where

import           Prelude                       as X
                                                ( String
                                                , error
                                                , fail
                                                , id
                                                )

import           Protolude                     as X
                                         hiding ( uncons
                                                , unsnoc
                                                , Lenient
                                                , Strict
                                                , (<&>)
                                                , (<.>)
                                                , from
                                                , to
                                                , identity
                                                , moduleName
                                                )

import           Data.Convertible              as X
                                                ( Convertible
                                                , convert
                                                , safeConvert
                                                )

import           Control.Lens                  as X

-- | A marker for unreachable code paths, throws error when reached.
unreachable :: HasCallStack => a
unreachable = error "unreachable code"
