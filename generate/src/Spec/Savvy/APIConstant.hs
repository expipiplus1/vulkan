{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Spec.Savvy.APIConstant
  ( APIConstant(..)
  , ConstantValue(..)
  , specConstants
  ) where

import           Data.Bits
import           Data.Char
import           Data.Closure
import           Data.Either.Validation
import           Data.Maybe
import qualified Data.MultiMap          as MultiMap
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Word
import qualified Spec.Constant          as P
import           Spec.Savvy.Error
import qualified Spec.Spec              as P
import           Text.Read              hiding (parens)
import           Text.Regex.Applicative

data APIConstant = APIConstant
  { acName    :: Text
  , acValue   :: ConstantValue
  , acComment :: Maybe Text
  , acAliases :: [Text]
  }
  deriving (Show)

data ConstantValue = -- | An integral value with no specific size
                     IntegralValue Word
                   | -- | A value ending in 'f'
                     FloatValue Float
                   | -- | A value sized with 'U'
                     Word32Value Word32
                   | -- | A value sized with 'ULL'
                     Word64Value Word64
  deriving (Show)

specConstants :: P.Spec -> Validation [SpecError] [APIConstant]
specConstants P.Spec {..} =
  let --- | A map of (target, name) pairs
      constantAliases :: [(Text, Text)]
      constantAliases =
        [ (P.unConstantAlias name, alias)
          -- TODO: stop this being backwards...
        | P.Constant alias (Left name) _ <- sConstants
        ]
      aliasMap = MultiMap.fromList constantAliases
  in  sequenceA
        [ APIConstant name
          <$> parseValue value
          <*> pure comment
          <*> pure aliases
        | P.Constant name (Right value) comment <- sConstants
        , let aliases = closeNonReflexive (`MultiMap.lookup` aliasMap) [name]
        ]

parseValue :: Text -> Validation [SpecError] ConstantValue
parseValue s =
  fromMaybe (Failure [UnableToReadValue s]) (match parse (T.unpack s))

parse :: RE Char (Validation [SpecError] ConstantValue)
parse = parens parse'

parse' :: RE Char (Validation [SpecError] ConstantValue)
parse' =
  (fmap IntegralValue . readSpec <$> digits)
    <|> (fmap Word32Value <$> subtracted (inverted word32))
    <|> (fmap Word64Value <$> subtracted (inverted word64))
    <|> (fmap FloatValue <$> float)

-- | Oops, Applicative
subtracted
  :: Num a
  => RE Char (Validation [SpecError] a)
  -> RE Char (Validation [SpecError] a)
subtracted x =
  (fmap (subtract 1) <$> x <* "-1")
    <|> (fmap (subtract 2) <$> x <* "-2")
    <|> (  x
        *> "-"
        *> (Failure . (: []) . UnhandledSubtraction . T.pack <$> digits)
        )
    <|> x

inverted :: (Bits a, Functor f) => RE Char (f a) -> RE Char (f a)
inverted x = ("~" *> (fmap complement <$> x)) <|> x

word32 :: RE Char (Validation [SpecError] Word32)
word32 = readSpec <$> digits <* "U"

word64 :: RE Char (Validation [SpecError] Word64)
word64 = readSpec <$> digits <* "ULL"

float :: RE Char (Validation [SpecError] Float)
float = readSpec <$> (fmap concat . sequenceA $ [digits, ".", digits]) <* "f"

digits :: RE Char String
digits = many (psym isDigit)

parens :: RE Char a -> RE Char a
parens a = "(" *> a <* ")" <|> a

readSpec :: Read a => String -> Validation [SpecError] a
readSpec s = case readMaybe s of
  Nothing -> Failure [UnableToReadValue (T.pack s)]
  Just x  -> pure x
