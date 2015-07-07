module API where

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Schema

parseProducts :: FilePath -> IO (Either String SearchResponse)
parseProducts filename = eitherDecode <$> BL.readFile filename
