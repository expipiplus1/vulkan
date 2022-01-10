module Data.Text.Extra
  ( upperCaseFirst
  , lowerCaseFirst
  , dropPrefix
  , dropSuffix
  , (<+>)
  , module T
  )
where

import           Data.String                    ( IsString )
import           Data.Text as T
import           Data.Semigroup
import           Data.Char
import           Prelude

dropPrefix :: Text -> Text -> Maybe Text
dropPrefix prefix s = if prefix `isPrefixOf` s
                        then Just (T.drop (T.length prefix) s)
                        else Nothing

dropSuffix :: Text -> Text -> Maybe Text
dropSuffix suffix s = if suffix `isSuffixOf` s
  then Just (T.dropEnd (T.length suffix) s)
  else Nothing

(<+>) :: (IsString a, Semigroup a) => a -> a -> a
a <+> b = a <> " " <> b

lowerCaseFirst :: Text -> Text
lowerCaseFirst = onFirst Data.Char.toLower

upperCaseFirst :: Text -> Text
upperCaseFirst = onFirst Data.Char.toUpper . T.dropWhile (not . isAlpha)

onFirst :: (Char -> Char) -> Text -> Text
onFirst f = \case
  Cons c cs -> Cons (f c) cs
  t         -> t

pattern Cons :: Char -> Text -> Text
pattern Cons c cs <- (uncons -> Just (c, cs))
  where Cons c cs = cons c cs
