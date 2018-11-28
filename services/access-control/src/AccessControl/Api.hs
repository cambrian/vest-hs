module AccessControl.Api
  ( module AccessControl.Api
  ) where

import qualified AccessControl.Auth as Auth
import qualified AccessControl.Internal as AccessControl
import qualified AccessControl.Permission as Permission
import qualified Amqp
import Vest

type TokenEndpoint
   = Endpoint 'NoAuth AccessControl.T Amqp.T "token" PublicKey ('Direct AccessControl.SignedToken)

type InvalidateAllExistingTokensEndpoint
   = Endpoint ('Auth (Auth.T 'Permission.InvalidateAuthTokens)) AccessControl.T Amqp.T "invalidateAllExistingTokens" () ('Direct ())

type TokenVersionValue
   = ValueTopic AccessControl.T Amqp.T "minValidTokenTime" Time
