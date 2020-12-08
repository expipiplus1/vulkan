module Data.ByteString.Extra where

import           Data.ByteString               as BS
import           Prelude

dropPrefix :: ByteString -> ByteString -> Maybe ByteString
dropPrefix prefix s = if prefix `BS.isPrefixOf` s
                        then Just (BS.drop (BS.length prefix) s)
                        else Nothing

