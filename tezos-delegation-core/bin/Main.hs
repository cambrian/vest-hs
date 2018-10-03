import qualified Bridge.Transports.Amqp as Amqp
import TezosDelegationCore
import qualified TezosDelegationCore.Db as Db
import VestPrelude

data TezosDelegationCore =
  XTZ
  deriving (Show, Data)

config :: Config c u
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
         (\dbPool -> withForever (config @c @u, amqp, dbPool) (start amqp)))

main :: IO Void
main =
  cmdArgs (modes [XTZ]) >>= \case
    XTZ -> run @"XTZ" @"mutez"
