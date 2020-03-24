module Data.Text.Extra
  (
  -- readMaybe
  -- , tShow
  -- , pattern Cons
    upperCaseFirst
  , lowerCaseFirst
  , dropPrefix
  -- , dropPrefix'
  , (<+>)
  , module T
  )
where

import           Data.String                    ( IsString )
import           Data.Text as T
import           Data.Semigroup
import           Data.Char
import           Prelude

-- -- | Read some text into a value
-- readMaybe :: Read a => Text -> Maybe a
-- readMaybe = R.readMaybe . unpack

-- -- | Show a value as text
-- tShow :: Show a => a -> Text
-- tShow = pack . show

dropPrefix :: Text -> Text -> Maybe Text
dropPrefix prefix s = if prefix `isPrefixOf` s
                        then Just (T.drop (T.length prefix) s)
                        else Nothing

-- dropPrefix' :: Text -> Text -> Text
-- dropPrefix' prefix s = fromMaybe s (dropPrefix prefix s)

(<+>) :: (IsString a, Semigroup a) => a -> a -> a
a <+> b = a <> " " <> b

lowerCaseFirst :: Text -> Text
lowerCaseFirst = onFirst Data.Char.toLower

upperCaseFirst :: Text -> Text
upperCaseFirst = onFirst Data.Char.toUpper

onFirst :: (Char -> Char) -> Text -> Text
onFirst f = \case
  Cons c cs -> Cons (f c) cs
  t         -> t

pattern Cons :: Char -> Text -> Text
pattern Cons c cs <- (uncons -> Just (c, cs))
  where Cons c cs = cons c cs
