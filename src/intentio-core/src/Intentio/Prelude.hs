module Intentio.Prelude
  ( module X
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
                                                )

import           Data.Convertible              as X
                                                ( Convertible
                                                , convert
                                                , safeConvert
                                                )

import           Control.Lens                  as X
