import qualified Bridge.Transports.Amqp as Amqp
import Core
import qualified Core.Db as Db
import VestPrelude

data Core =
  XTZ
  deriving (Show, Data)

config :: Config
config = Config {}

dbPoolConfig :: PoolConfig
dbPoolConfig = PoolConfig {idleTime = sec 30, numResources = 5}

run ::
     forall c u. Stakeable c u
  => IO Void
run =
  withForever
    Amqp.localConfig
    (\amqp ->
       withPoolForever
         dbPoolConfig
         Db.localConfig
         (\dbPool -> make @c @u config amqp dbPool >>= start amqp))

main :: IO Void
main =
  cmdArgs (modes [XTZ]) >>= \case
    XTZ -> run @"XTZ" @"mutez"
