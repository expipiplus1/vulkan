{-# LANGUAGE Arrows #-}

module Parse.Bitmask where

import           Parse.Utils
import           Prelude           hiding (elem)
import           Spec.Bitmask
import           Text.XML.HXT.Core

parseBitmask :: IOStateArrow s XmlTree Spec.Bitmask.Bitmask
parseBitmask = hasName "enums" >>> hasAttrValue "type" (== "bitmask") >>>
               (extract `orElse` failA "Failed to extract bitmask fields")
  where extract = proc bitmask -> do
          name      <- requiredAttrValue "name" -< bitmask
          namespace <- optionalAttrValue "namespace" -< bitmask
          comment   <- optionalAttrValue "comment" -< bitmask
          values    <- listA (parseBitmaskValue <<< getChildren) -< bitmask
          bitPositions <-
                       listA (parseBitmaskBitPos <<< getChildren) -< bitmask
          returnA -< Bitmask{ bmName = name
                            , bmHsName = name
                            , bmNamespace = namespace
                            , bmComment = comment
                            , bmValues = values
                            , bmBitPositions = bitPositions
                            }

parseBitmaskValue :: IOStateArrow s XmlTree BitmaskValue
parseBitmaskValue = hasName "enum" >>> hasAttr "value" >>>
                      (extract `orElse`
                       failA "Failed to extract bitmask value fields")
  where extract = proc elem -> do
          name    <- requiredAttrValue "name" -< elem
          value   <- requiredRead <<< requiredAttrValue "value" -< elem
          comment <- optionalAttrValue "comment" -< elem
          returnA -< BitmaskValue{ bmvName = name
                                 , bmvHsName = name
                                 , bmvValue = value
                                 , bmvComment = comment
                                 }

parseBitmaskBitPos :: IOStateArrow s XmlTree BitmaskBitPosition
parseBitmaskBitPos = hasName "enum" >>> hasAttr "bitpos" >>>
                     (extract `orElse`
                      failA "Failed to extract bitmask bitposition fields")
  where extract = proc elem -> do
          name    <- requiredAttrValue "name" -< elem
          bitpos  <- requiredRead <<< requiredAttrValue "bitpos" -< elem
          comment <- optionalAttrValue "comment" -< elem
          returnA -< BitmaskBitPosition{ bmbpName = name
                                       , bmbpHsName = name
                                       , bmbpBitPos = bitpos
                                       , bmbpComment = comment
                                       }


