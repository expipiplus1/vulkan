{-# LANGUAGE Arrows #-}

module Parse.Enum where

import           Parse.Utils
import           Prelude           hiding (Enum, elem)
import           Spec.Enum
import           Text.XML.HXT.Core

parseEnum :: IOStateArrow s XmlTree Spec.Enum.Enum
parseEnum = hasName "enums" >>> hasAttrValue "type" (== "enum") >>>
            (extract `orElse` failA "Failed to extract enum fields")
  where extract = proc enum -> do
          name      <- requiredAttrValue "name" -< enum
          namespace <- optionalAttrValue "namespace" -< enum
          expand    <- optionalAttrValue "expand" -< enum
          comment   <- optionalAttrValue "comment" -< enum
          elements  <- listA (parseEnumElement <<< getChildren) -< enum
          returnA -< Enum{ eName = name
                         , eHsName = name
                         , eNamespace = namespace
                         , eExpand = expand
                         , eComment = comment
                         , eElements = elements
                         }

parseEnumElement :: IOStateArrow s XmlTree EnumElement
parseEnumElement = hasName "enum" >>>
                   (extract `orElse`
                    failA "Failed to extract enumerant fields")
  where extract = proc elem -> do
          name    <- requiredAttrValue "name" -< elem
          value   <- requiredRead <<< requiredAttrValue "value" -< elem
          comment <- optionalAttrValue "comment" -< elem
          returnA -< EnumElement{ eeName = name
                                , eeHsName = name
                                , eeValue = value
                                , eeComment = comment
                                }


