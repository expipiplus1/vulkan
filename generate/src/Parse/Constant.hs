{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parse.Constant
  ( parseConstants
  ) where

import           Control.Applicative     ((<|>))
import           Data.Bits               (complement)
import           Parse.Utils
import           Spec.Constant
import           Text.Parser.Combinators hiding (optional)
import           Text.Parser.Token
import           Text.Trifecta           hiding (optional)
import           Text.XML.HXT.Core

parseConstants :: IOStateArrow s XmlTree [Constant]
parseConstants = extractFields
  "API Constants"
  (hasAttrValue "name" (== "API Constants"))
  (allChildren constantFailDiag [constantAlias, constant])

constantAlias :: IOStateArrow s XmlTree Constant
constantAlias = proc c -> do
  cName <- requiredAttrValueT "name" -< c
  cValue <- Left . ConstantAlias ^<< getAttrValue0T "alias" -< c
  let cComment = Nothing
  returnA -< Constant{..}

constant :: IOStateArrow s XmlTree Constant
constant = proc c -> do
  cName <- requiredAttrValueT "name" -< c
  cValue <- Right ^<< requiredAttrValueT "value" -< c
  cComment <- optionalAttrValueT "comment" -< c
  returnA -< Constant{..}

constantFailDiag :: IOStateArrow s XmlTree String
constantFailDiag = proc t -> do
  name <- optional (getAttrOrChildText "name") -< t
  returnA -< "Failed to parse constant"
          ++ maybe "" (" named " ++) name

----------------------------------------------------------------
-- Parsing values
----------------------------------------------------------------

-- parseConstantValue :: String -> Result ConstantValue
-- parseConstantValue = parseString (atom <* eof) mempty

-- atom :: Parser ConstantValue
-- atom = literal
--    <|> (string "~" *> (bitWiseNot <$> atom))
--    <|> parens atom

-- literal :: Parser ConstantValue
-- literal = try (FloatValue  . realToFrac   <$> double  <* string "f")
--       <|> try (Word64Value . fromIntegral <$> natural <* string "ULL")
--       <|> try (Word32Value . fromIntegral <$> natural <* string "U")
--       <|> try (IntegralValue              <$> natural)

-- bitWiseNot :: ConstantValue -> ConstantValue
-- bitWiseNot (IntegralValue _) = error "can only invert explicitly sized values"
-- bitWiseNot (FloatValue _)    = error "can only invert explicitly sized values"
-- bitWiseNot (Word32Value x)   = Word32Value (complement x)
-- bitWiseNot (Word64Value x)   = Word64Value (complement x)

