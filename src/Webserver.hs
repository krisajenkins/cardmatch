{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Webserver where

import           API
import           Control.Lens              hiding (from, (^.))
import           Control.Monad.IO.Class
import           Control.Monad.State.Class
import           Data.Aeson
import           Data.ByteString
import           Data.Set
import qualified Data.Set                  as Set
import           Schema
import           Snap
import           Snap.Util.FileServe
import           Snap.Util.GZip

mimeTypeJson :: ByteString
mimeTypeJson = "application/json"

jsonResponse :: MonadSnap m => m ()
jsonResponse = modifyResponse $ setContentType mimeTypeJson

writeJSON :: (ToJSON a,MonadSnap m) => a -> m ()
writeJSON x =
  do jsonResponse
     (writeLBS . encode) x

compressableMimeTypes :: Set ByteString
compressableMimeTypes =
  Set.fromList [mimeTypeJson, "text/javascript","text/css"]

data Sphere = Sphere {products :: [Product]}

data App = App {_sphere :: Snaplet Sphere}
makeLenses ''App

initSphere :: SnapletInit a Sphere
initSphere =
  makeSnaplet "sphere" "Code for connecting to sphere." Nothing $
  do parsed <-
       liftIO $
       parseProducts "products.json"
     case parsed of
       Left e -> fail e
       Right response ->
         return (Sphere (results response))

initApp :: SnapletInit App App
initApp =
  makeSnaplet "my app" "my application" Nothing $
  do sphereSnaplet <-
       nestSnaplet "sphere" sphere initSphere
     addRoutes [(""
                ,withCompression' compressableMimeTypes $
                 serveDirectory "ui/dist")
               ,("/products.json"
                ,withCompression' compressableMimeTypes $
                 with sphere $
                 do jsonResponse
                    ps <- gets products
                    writeJSON ps)]
     return $
       App sphereSnaplet

webserver :: IO ()
webserver =
  serveSnaplet (setPort 8080 defaultConfig)
               initApp
