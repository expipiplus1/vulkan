{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parse.Enum
  ( parseEnum
  ) where

import           Data.Maybe        (listToMaybe)
import           Data.Text
import           Parse.Utils
import           Prelude           hiding (Enum)
import           Spec.Enum
import           Text.XML.HXT.Core

parseEnum :: IOStateArrow s XmlTree Spec.Enum.Enum
parseEnum = hasName "enums" >>> hasAttrValue "type" (== "enum") >>>
            (extract `orElse` failA "Failed to extract enum fields")
  where extract = proc e -> do
          eName      <- requiredAttrValueT "name" -< e
          eComment   <- optionalAttrValueT "comment" -< e
          es <- app -<
            (allChildren (enumElemFail eName) [
              AComment ^<< enumComment
            , AnUnusedStart ^<< enumUnusedStart
            , AnEnumElement ^<< enumElem
            ], e)
          let eElements = [e' | AnEnumElement e' <- es]
              eUnusedStart = listToMaybe [e' | AnUnusedStart e' <- es]
          returnA -< Enum{..}

data EnumListMember
  = AnEnumElement EnumElement
  | AnUnusedStart Text
  | AComment Text

enumElemFail
  :: Text
  --- ^ Enum name
  -> IOStateArrow s XmlTree String
enumElemFail n = proc t -> do
  name <- optional (getAttrOrChildText "name") -< t
  value <- optional (getAttrOrChildText "type") -< t
  returnA -< ("Failed to parse value of enumeration " ++ unpack n)
          ++ maybe "" (" named " ++) name
          ++ maybe "" (" with value " ++) value

enumElem :: IOStateArrow s XmlTree EnumElement
enumElem = proc e -> do
  eeName    <- requiredAttrValueT "name" -< e
  eeValue   <- requiredRead <<< requiredAttrValue "value" -< e
  eeComment <- optionalAttrValueT "comment" -< e
  returnA -< EnumElement{..}

--- | Comments which group the values (discarded at the moment)
enumComment :: IOStateArrow s XmlTree Text
enumComment = getAllTextT <<< hasName "comment"

enumUnusedStart :: IOStateArrow s XmlTree Text
enumUnusedStart = proc e -> do
  hasName "unused" -< e
  requiredAttrValueT "start" -< e
