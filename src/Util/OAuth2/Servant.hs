{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Util.OAuth2.Servant where

import Data.Text
import Servant
import Util.Misc
import Api (DN)

{-

# OAuth2 Refresher:

1. Client will call <your api>/authorize

2. Say your Handler for `authorize` redirects to the auth uri, then
they will be asked by the OAuth2 provider whether or not to give you
permissions.

3. The provider redirects to your  `callback` which can then extract a
token, and persist it,  and use it to make requests  on behalf of your
user.


-}

type Authorize a = DN :> Get '[JSON] a

type Callback a = QueryParam "code" String
                  :> QueryParam "email" String
                  :> Get '[JSON] a

-- | The API consumer can provide the handler for starting the OAuth2
-- process
authorize :: (Maybe Text -> Handler a) -> Server (Authorize a)
authorize f = f

-- | The OAuth2 Provider will callback to the Server created here. The
-- API consumer can provide a function that takes a Maybe Code and
-- returns a Handler.
callback :: (Maybe String -- ^ temporary code
          -> Maybe String -- ^ email
          -> Handler a)
          -> Server (Callback a)
callback f = (\code email -> f code email >>= return)

-- | If your servant endpoints use this type, you've probably
-- implemented OAuth2 properly.
type ProviderAPI = "authorize" :> Authorize String
                   :<|> "authorized" :> Callback String
