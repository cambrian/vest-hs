import AccessControl
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Vest

pubKeyHandler :: T -> () -> IO PublicKey
pubKeyHandler T {publicKey} () = return publicKey

accessTokenHandler :: T -> PublicKey -> IO (SignedText' "AccessToken")
accessTokenHandler T {access, secretKey, tokenTTL} publicKey = do
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
          HashSet.empty
          subjectRoles
      token =
        AccessToken
          {publicKey, name, permissions, expiration = timeAdd tokenTTL time}
      signedToken = sign' secretKey (show' token)
  return signedToken

main :: IO Void
main = start @T (const $ return ()) (pubKeyHandler :<|> accessTokenHandler)
