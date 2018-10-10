module AccessControl.Token
  ( module AccessControl.Token
  ) where

import Vest

data AccessToken = AccessToken
  { pubKey :: PublicKey
  }
