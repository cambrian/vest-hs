import qualified Bridge.Transports.Amqp as Amqp
import TezosDelegationCore
import qualified TezosDelegationCore.Db as Db
import VestPrelude

config :: Config
config = Config {}

dbPoolConfig :: PoolConfig
dbPoolConfig = PoolConfig {idleTime = sec 30, numResources = 5}

main :: IO Void
main =
  withForever
    Amqp.localConfig
    (\amqp ->
       withPoolForever
         dbPoolConfig
         Db.localConfig
         (\dbPool -> withForever (config, amqp, dbPool) (start amqp)))
