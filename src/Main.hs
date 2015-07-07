{-# LANGUAGE OverloadedStrings #-}
module Main where

import           API
import           Control.Monad.Catch
import           Control.Monad.Trans.Either
import           Data.Monoid                hiding (Product)
import           Keys
import           Network.OAuth.OAuth2       hiding (fetchAccessToken)
import           Pipes
import qualified Pipes.ByteString           as PB
import           Pipes.HTTP
import           System.Console.GetOpt
import           System.Environment
import           Webserver

productSearchRequest :: MonadThrow m => AccessToken -> m Request
productSearchRequest token =
  do request <-
       parseUrl "https://api.sphere.io/cape-town-84/product-projections/search"
     return $
       request {requestHeaders =
                  [("Content-type","application/json")
                  ,("Authorization",mappend "Bearer " (accessToken token))]}

fetchAccessToken :: IO (OAuth2Result AccessToken)
fetchAccessToken =
  withManager tlsManagerSettings $
  \manager ->
    do eitherToken <-
         doJSONPostRequest
           manager
           sphereKeys
           (oauthAccessTokenEndpoint sphereKeys)
           [("grant_type","client_credentials")
           ,("scope","manage_project:cape-town-84")]
       return eitherToken

runRequest :: Request -> IO ()
runRequest request =
  withManager tlsManagerSettings $
  \manager ->
    withHTTP request manager $
    \response -> runEffect $ responseBody response >-> PB.stdout

searchProducts :: IO ()
searchProducts =
  do r <- fetchAccessToken
     case r of
       Left e -> print e
       Right token ->
         do req <- (productSearchRequest token)
            runRequest req

------------------------------------------------------------

data Flag
  = ParseProducts String
  | DownloadProducts
  deriving (Show)

handleCommand :: ([Flag],[String],[String]) -> IO ()
handleCommand ([],[],[]) = webserver
handleCommand ([DownloadProducts],[],[]) = searchProducts
handleCommand ([(ParseProducts filename)],[],[]) = parseProducts filename >>=
                                                   print
handleCommand (_,_,errors) =
  ioError (userError (concat errors ++
                      usageInfo "USAGE: " options))

options :: [OptDescr Flag]
options =
  [Option "d"
          ["download"]
          (NoArg DownloadProducts)
          "Download product data."
  ,Option "p"
          ["parser"]
          (ReqArg ParseProducts "filename")
          "Parse a product file and print it."]

main :: IO ()
main = getArgs >>= handleCommand . getOpt Permute options
