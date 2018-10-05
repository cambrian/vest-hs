import TezosDelegationCore
import VestPrelude

main :: IO Void
main = start @T (const blockForever)
