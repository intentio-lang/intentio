module Intentio.Prelude
  ( module X
  )
where

import           Prelude                       as X
                                                ( String
                                                , error
                                                , fail
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
                                                )

import           Data.Convertible              as X
                                                ( Convertible
                                                , convert
                                                , safeConvert
                                                )

import           Control.Lens                  as X
