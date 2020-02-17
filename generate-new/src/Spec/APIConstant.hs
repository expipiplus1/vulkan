module Spec.APIConstant
  ( ConstantValue(..)
  , parseConstant
  , decode
  ) where

import           Relude
import           Polysemy
import           Data.Char
import           Data.Bits
import           Text.Regex.Applicative
import qualified Data.ByteString.Char8         as BS
import qualified Data.Text                     as T

import           Error

data ConstantValue
  = StrValue Text
  | -- | An integral value with no specific size
    IntegralValue Word
  | -- | A value ending in 'f'
    FloatValue Float
  | -- | A value sized with 'U'
    Word32Value Word32
  | -- | A value sized with 'ULL'
    Word64Value Word64
  deriving (Show)

parseConstant :: HasErr r => ByteString -> Sem r ConstantValue
parseConstant s = fromMaybe
  (throw ("unable to read constant" <> decodeUtf8 s))
  (match parse (BS.unpack s))

parse :: HasErr r => RE Char (Sem r ConstantValue)
parse =
  parens
    $   (pure . StrValue . T.pack <$> ("&quot;" *> many anySym <* "&quot;"))
    <|> (fmap IntegralValue . readSpec <$> digits)
    <|> (fmap Word32Value <$> subtracted (inverted word32))
    <|> (fmap Word64Value <$> subtracted (inverted word64))
    <|> (fmap FloatValue <$> float)

-- | Oops, Applicative
subtracted :: (HasErr r, Num a) => RE Char (Sem r a) -> RE Char (Sem r a)
subtracted x =
  (fmap (subtract 1) <$> x <* "-1")
    <|> (fmap (subtract 2) <$> x <* "-2")
    <|> (x *> "-" *> (throw . T.pack <$> digits))
    <|> x

inverted :: (Bits a, Functor f) => RE Char (f a) -> RE Char (f a)
inverted x = ("~" *> (fmap complement <$> x)) <|> x

word32 :: HasErr r => RE Char (Sem r Word32)
word32 = readSpec <$> digits <* "U"

word64 :: HasErr r => RE Char (Sem r Word64)
word64 = readSpec <$> digits <* "ULL"

float :: HasErr r => RE Char (Sem r Float)
float = readSpec <$> (fmap concat . sequenceA $ [digits, ".", digits]) <* "f"

digits :: RE Char String
digits = many (psym isDigit)

parens :: RE Char a -> RE Char a
parens a = "(" *> a <* ")" <|> a

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

readSpec :: (HasErr r, Read a) => String -> Sem r a
readSpec s = case readMaybe s of
  Nothing -> throw (T.pack s)
  Just x  -> pure x

decode :: HasErr r => ByteString -> Sem r Text
decode bs = case decodeUtf8' bs of
  Left  e -> throw $ show e
  Right t -> pure t
