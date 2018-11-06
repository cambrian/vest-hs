module TezosChainWatcher.Api
  ( module TezosChainWatcher.Api
  ) where

import qualified Tezos
import TezosChainWatcher.Internal
import qualified Transport.Amqp as Amqp
import Vest

type CycleEvents = Event T Amqp.T "cycles" Tezos.CycleEvent

type BlockEvents = Event T Amqp.T "blocks" Tezos.BlockEvent
