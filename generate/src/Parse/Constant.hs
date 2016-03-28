{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parse.Constant
  ( parseConstants
  ) where

import           Control.Applicative     ((<|>))
import           Data.Bits               (complement)
import           Parse.Utils
import           Prelude                 hiding (elem)
import           Spec.Constant
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Trifecta
import           Text.XML.HXT.Core

parseConstants :: IOStateArrow s XmlTree [Constant]
parseConstants = extractFields "API Constants"
                              (hasAttrValue "name" (=="API Constants"))
                              extract
  where extract = listA (parseConstant <<< getChildren)

parseConstant :: IOStateArrow s XmlTree Constant
parseConstant = extractFields "Constant"
                              (hasName "enum")
                              extract
  where extract = proc constant -> do
          cName    <- requiredAttrValue "name" -< constant
          cValueString <- requiredAttrValue "value" -< constant
          cValue   <- (arrF parseConstantValue) `orElse`
                      failA "Failed to read constant value" -< cValueString
          cComment <- constA Nothing -< constant -- TODO
          returnA -< Constant{..}

parseConstantValue :: String -> Result ConstantValue
parseConstantValue = parseString (atom <* eof) mempty

atom :: Parser ConstantValue
atom = literal
   <|> (string "~" *> (bitWiseNot <$> atom))
   <|> parens atom

literal :: Parser ConstantValue
literal = try (FloatValue  . realToFrac   <$> double  <* string "f")
      <|> try (Word64Value . fromIntegral <$> natural <* string "ULL")
      <|> try (Word32Value . fromIntegral <$> natural <* string "U")
      <|> try (IntegralValue              <$> natural)

bitWiseNot :: ConstantValue -> ConstantValue
bitWiseNot (IntegralValue _) = error "can only invert explicitly sized values"
bitWiseNot (FloatValue _)  = error "can only invert explicitly sized values"
bitWiseNot (Word32Value x) = Word32Value (complement x)
bitWiseNot (Word64Value x) = Word64Value (complement x)

