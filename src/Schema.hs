{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Schema where

import           Control.Applicative
import           Data.Aeson
import           Data.Map
import           Data.Text
import           GHC.Generics

-- TODO I don't like 'w' and 'h' as identifiers, but it makes the parsing easier for now.
data Dimensions =
  Dimensions {w :: Int
             ,h :: Int}
  deriving (Eq,Show,Read,Generic)

data Image =
  Image {dimensions :: Dimensions
        ,url        :: Text}
  deriving (Eq,Show,Read,Generic)

data Locale = English | German | Unknown
  deriving (Eq,Show,Read,Generic,Ord)

toIsoCode :: Locale -> String
toIsoCode English = "en"
toIsoCode German = "de"
toIsoCode Unknown = ""

fromIsoCode :: String -> Locale
fromIsoCode "en" = English
fromIsoCode "de" = German
fromIsoCode _ = Unknown

data Variant =
  Variant {images :: [Image]}
  deriving (Eq,Show,Read,Generic)

data Product =
  Product {id            :: Text
          ,name          :: Map Locale String
          ,masterVariant :: Variant}
  deriving (Eq,Show,Read,Generic)

data SearchResponse =
  SearchResponse {total   :: Integer
                 ,results :: [Product]}
  deriving (Eq,Show,Read,Generic)

instance ToJSON Dimensions
instance FromJSON Dimensions
instance ToJSON Image
instance FromJSON Image
instance ToJSON Variant
instance FromJSON Variant
instance ToJSON Locale
instance FromJSON Locale
instance ToJSON Product
instance FromJSON Product
instance ToJSON SearchResponse
instance FromJSON SearchResponse

instance (ToJSON v) => ToJSON (Map Locale v) where
  toJSON = toJSON . mapKeys toIsoCode

instance (FromJSON v) => FromJSON (Map Locale v) where
     parseJSON = fmap (mapKeys fromIsoCode) <$> parseJSON
