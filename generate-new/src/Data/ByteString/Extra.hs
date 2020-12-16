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

strip :: ByteString -> ByteString
strip = BS.dropWhile (isSpace . w2c) . dropWhileEnd (isSpace . w2c)

dropWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhileEnd p ps = BS.take (findFromEndUntil (not . p) ps) ps

findFromEndUntil :: (Word8 -> Bool) -> ByteString -> Int
findFromEndUntil f ps@(PS _ _ l) = case unsnoc ps of
  Nothing     -> 0
  Just (b, c) -> if f c then l else findFromEndUntil f b
