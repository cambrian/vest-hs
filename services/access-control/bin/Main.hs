import AccessControl
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Vest

pubKeyHandler :: T -> () -> IO PublicKey
pubKeyHandler T {keyPair} () = return $ toPublicKey keyPair

accessTokenHandler :: T -> PublicKey -> IO (SignedText' "AccessToken")
accessTokenHandler T {access, keyPair} pubKey = do
  let Access {roles, subjects} = access
      Subject {roles = subjectRoles, name} =
        fromMaybe Subject {roles = HashSet.empty, name = "unknown"} $
        HashMap.lookup pubKey subjects
      permissions =
        concatMap
          (\role -> fromMaybe HashSet.empty $ HashMap.lookup roles role)
          subjectRoles
  return ""

main :: IO Void
main = start @T (const $ return ()) handlePubKey
