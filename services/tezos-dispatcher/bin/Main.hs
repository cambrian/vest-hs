import TezosDispatcher
import TezosDispatcher.Api
import VestPrelude

main :: IO Void
main = start @T (const blockForever)
