module Intentio.Prelude
  ( module X
  )
where

import           Prelude                       as X
                                                ( String
                                                , fail
                                                )

import           Protolude                     as X
                                         hiding ( MonadState
                                                , State
                                                , StateT(StateT)
                                                , put
                                                , get
                                                , gets
                                                , modify
                                                , state
                                                , withState
                                                , runState
                                                , execState
                                                , evalState
                                                , runStateT
                                                , execStateT
                                                , evalStateT
                                                )

import           Control.Monad.State.Strict    as X
                                                ( MonadState
                                                , State
                                                , StateT(StateT)
                                                , put
                                                , get
                                                , gets
                                                , modify
                                                , modify'
                                                , state
                                                , withState
                                                , runState
                                                , execState
                                                , evalState
                                                , runStateT
                                                , execStateT
                                                , evalStateT
                                                )

import           Data.Convertible              as X
                                                ( Convertible
                                                , convert
                                                , safeConvert
                                                )
