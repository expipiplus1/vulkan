module Spec.APIConstant
  ( ConstantValue(..)
  , parseConstant
  , decode
  , decodeName
  ) where

import           Data.Bits
import qualified Data.ByteString.Char8         as BS
import           Data.Char
import qualified Data.Text                     as T
import           Polysemy
import           Relude
import           Text.Regex.Applicative

import           Error
import           Spec.Name

data ConstantValue
  = StrValue Text
  | -- | An integral value with no specific size
    IntegralValue Integer
  | -- | A value ending in 'f'
    FloatValue Float
  | -- | A value sized with 'U'
    Word32Value Word32
  | -- | A value sized with 'ULL'
    Word64Value Word64
  | -- | A value sized with 'LL'
    Int64Value Int64
  | -- | size of a type
    SizeOfValue CName
  deriving (Show)

parseConstant :: HasErr r => ByteString -> Sem r ConstantValue
parseConstant s = fromMaybe (throw ("unable to read constant: " <> show s))
                            (match parse (BS.unpack s))

parse :: HasErr r => RE Char (Sem r ConstantValue)
parse =
  parens
    $   (pure . StrValue . T.pack <$> ("&quot;" *> many anySym <* "&quot;"))
    <|> (fmap IntegralValue <$> inverted (readSpec <$> digits))
    <|> (fmap Word32Value <$> subtracted (inverted word32))
    <|> (fmap Word64Value <$> subtracted (inverted word64))
    <|> (fmap Int64Value <$> subtracted (inverted int64))
    <|> (fmap FloatValue <$> float)
    <|> (pure . SizeOfValue <$> sizeof)

-- | Oops, Applicative
subtracted :: (HasErr r, Num a) => RE Char (Sem r a) -> RE Char (Sem r a)
subtracted x =
  (fmap (subtract 1) <$> x <* "-1")
    <|> (fmap (subtract 2) <$> x <* "-2")
    <|> (x *> "-" *> (throw . T.pack <$> digits))
    <|> x

inverted :: (Bits a, Functor f, Num a) => RE Char (f a) -> RE Char (f a)
inverted x =
  asum ["~" *> (fmap complement <$> x), "-" *> (fmap negate <$> x), x]

word32 :: HasErr r => RE Char (Sem r Word32)
word32 = readSpec <$> digits <* "U"

word64 :: HasErr r => RE Char (Sem r Word64)
word64 = readSpec <$> digits <* "ULL"

int64 :: HasErr r => RE Char (Sem r Int64)
int64 = readSpec <$> digits <* "LL"

float :: HasErr r => RE Char (Sem r Float)
float = readSpec <$> (fmap concat . sequenceA $ [digits, ".", digits]) <* ("F" <|> "f")

digits :: RE Char String
digits = many (psym isDigit) <|> ((<>) <$> "0x" <*> many (psym isHexDigit))

sizeof :: RE Char CName
sizeof =
  CName
    .   T.pack
    <$> ("sizeof" *> parens (many (psym (isAlphaNum <||> (== '_')))))

parens :: RE Char a -> RE Char a
parens a = "(" *> a <* ")" <|> a

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

readSpec :: (HasErr r, Read a) => String -> Sem r a
readSpec s = case readMaybe s of
  Nothing -> throw (T.pack s)
  Just x  -> pure x

decodeName :: HasErr r => ByteString -> Sem r CName
decodeName = fmap CName . decode

decode :: HasErr r => ByteString -> Sem r Text
decode bs = case decodeUtf8' bs of
  Left  e -> throw $ show e
  Right t -> pure t

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
