import AccessControl
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Vest

pubKeyHandler :: T -> () -> IO PublicKey
pubKeyHandler T {keyPair} () = return $ toPublicKey keyPair

accessTokenHandler :: T -> PublicKey -> IO (SignedText' "AccessToken")
accessTokenHandler T {access, keyPair, tokenTTL} publicKey = do
  time <- now
  let Access {roles, subjects} = access
      Subject {roles = subjectRoles, name} =
        fromMaybe Subject {roles = HashSet.empty, name = "unknown"} $
        HashMap.lookup publicKey subjects
      permissions =
        HashSet.foldl'
          (\permissions roleName ->
             HashSet.union permissions $
             fromMaybe HashSet.empty $ HashMap.lookup roleName roles)
          (HashSet.empty @Permission)
          subjectRoles
      token =
        AccessToken
          {publicKey, name, permissions, expiration = timeAdd tokenTTL time}
      tokenText' = show' token
  sign' (toPrivateKey keyPair) tokenText'

main :: IO Void
main = start @T (const $ return ()) (pubKeyHandler :<|> accessTokenHandler)
