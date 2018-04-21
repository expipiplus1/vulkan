{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}

module Data.Text.Extra
  ( readMaybe
  , tShow
  , pattern Cons
  , upperCaseFirst
  , lowerCaseFirst
  , dropPrefix
  , dropPrefix'
  , (<+>)
  , module Data.Text
  ) where

import           Data.Char
import           Data.Maybe
import           Data.Semigroup
import           Data.String    (IsString)
import           Data.Text
import qualified Data.Text      as T
import qualified Text.Read      as R

-- | Read some text into a value
readMaybe :: Read a => Text -> Maybe a
readMaybe = R.readMaybe . unpack

-- | Show a value as text
tShow :: Show a => a -> Text
tShow = pack . show

upperCaseFirst :: Text -> Text
upperCaseFirst = onFirst Data.Char.toUpper

lowerCaseFirst :: Text -> Text
lowerCaseFirst = onFirst Data.Char.toLower

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

dropPrefix' :: Text -> Text -> Text
dropPrefix' prefix s = fromMaybe s (dropPrefix prefix s)

(<+>) :: (IsString a, Semigroup a) => a -> a -> a
a <+> b = a <> " " <> b
