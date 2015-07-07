{-# LANGUAGE OverloadedStrings #-}
module Keys (sphereKeys) where

import           Network.OAuth.OAuth2

sphereKeys :: OAuth2
sphereKeys =
  OAuth2 {oauthClientId = "XXXXXXXXXXXXXXXXXXXXXXXX"
         ,oauthClientSecret = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
         ,oauthCallback =
            Just "http://127.0.0.1:8080/githubCallback"
         ,oauthOAuthorizeEndpoint = "https://auth.sphere.io/oauth/authorize"
         ,oauthAccessTokenEndpoint = "https://auth.sphere.io/oauth/token"}
