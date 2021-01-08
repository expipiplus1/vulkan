module Data.ByteString.Extra where

import           Data.ByteString               as BS
import           Data.ByteString.Internal       ( ByteString(..)
                                                , w2c
                                                )
import           Data.Char                      ( isSpace )
import           Data.Word
import           Prelude

dropPrefix :: ByteString -> ByteString -> Maybe ByteString
dropPrefix prefix s = if prefix `BS.isPrefixOf` s
  then Just (BS.drop (BS.length prefix) s)
  else Nothing
