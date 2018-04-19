{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Text.Extra
  ( readMaybe
  , tShow
  , pattern Cons
  , upperCaseFirst
  , dropPrefix
  , module Data.Text
  ) where

import           Data.Char
import qualified Text.Read as R
import           Data.Text
import qualified Data.Text as T

-- | Read some text into a value
readMaybe :: Read a => Text -> Maybe a
readMaybe = R.readMaybe . unpack

-- | Show a value as text
tShow :: Show a => a -> Text
tShow = pack . show

upperCaseFirst :: Text -> Text
upperCaseFirst = onFirst Data.Char.toUpper

onFirst :: (Char -> Char) -> Text -> Text
onFirst f = \case
  Cons c cs -> Cons (f c) cs
  t         -> t

pattern Cons :: Char -> Text -> Text
pattern Cons c cs <- (uncons -> Just (c, cs))
  where Cons c cs = cons c cs

dropPrefix :: Text -> Text -> Maybe Text
dropPrefix prefix s = if prefix `T.isPrefixOf` s
                        then Just (T.drop (T.length prefix) s)
                        else Nothing
