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

import           Control.Arrow                 as X
                                                ( (>>>)
                                                , (<<<)
                                                )

import           Control.Lens                  as X

import           Control.Monad.Reader          as X
                                                ( Reader
                                                , ReaderT
                                                , withReader
                                                , withReaderT
                                                , mapReader
                                                , mapReaderT
                                                )
import           Control.Monad.Writer          as X
                                                ( Writer
                                                , WriterT
                                                , execWriter
                                                , execWriterT
                                                , mapWriter
                                                , mapWriterT
                                                )

import           Data.Aeson                    as X
                                                ( ToJSON
                                                , FromJSON
                                                , ToJSONKey
                                                , FromJSONKey
                                                )

import           Data.Convertible              as X
                                                ( Convertible
                                                , convert
                                                , safeConvert
                                                )

import           GHC.Exts                      as X
                                                ( IsList(fromList, fromListN) )

-- | A marker for unreachable code paths, throws error when reached.
unreachable :: HasCallStack => a
unreachable = error "unreachable code"
