module Monitor
  ( main
  ) where

import qualified Amqp
import TezosChainWatcher
import Vest

bridgeConfig :: ResourceConfig Amqp.T
bridgeConfig =
  Amqp.Config
    { hostname = "localhost"
    , virtualHost = "/"
    , username = "guest"
    , password = "guest"
    }

main :: IO ()
main = do
  opHash <- getArgs >>- last >>= fromJustUnsafe BugException >>- Tagged . pack
  with bridgeConfig $ \amqp -> withMonitor amqp opHash print
