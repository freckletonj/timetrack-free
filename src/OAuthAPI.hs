{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module OAuthAPI where

import Servant
import OAuth2

-- | Type of OAuth2's endpoints
type AuthAPI = "authorize" :> Get '[HTML] HTML
               :<|> "authorized" :> QueryParam "code" String :> Get '[HTML] HTML

-- | Construct endpoints for a given OAuth2
authEndpoint :: OAuth2 -> Server AuthAPI
authEndpoint oa = (getAuthorize oa)
                   :<|> (getAuthorized oa)



