{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}

module Parse.Enum where

import           Data.Maybe        (listToMaybe)
import           Parse.Utils
import           Prelude           hiding (Enum, elem)
import           Spec.Enum
import           Text.XML.HXT.Core

parseEnum :: IOStateArrow s XmlTree Spec.Enum.Enum
parseEnum = hasName "enums" >>> hasAttrValue "type" (== "enum") >>>
            (extract `orElse` failA "Failed to extract enum fields")
  where extract = proc e -> do
          eName      <- requiredAttrValue "name" -< e
          eComment   <- optionalAttrValue "comment" -< e
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
  | AnUnusedStart String
  | AComment String

enumElemFail
  :: String
  --- ^ Enum name
  -> IOStateArrow s XmlTree String
enumElemFail n = proc t -> do
  name <- optional (getAttrOrChildText "name") -< t
  value <- optional (getAttrOrChildText "type") -< t
  returnA -< ("Failed to parse value of enumeration " ++ n)
          ++ maybe "" (" named " ++) name
          ++ maybe "" (" with value " ++) value

enumElem :: IOStateArrow s XmlTree EnumElement
enumElem = proc e -> do
  eeName    <- requiredAttrValue "name" -< e
  eeValue   <- requiredRead <<< requiredAttrValue "value" -< e
  eeComment <- optionalAttrValue "comment" -< e
  returnA -< EnumElement{..}

--- | Comments which group the values (discarded at the moment)
enumComment :: IOStateArrow s XmlTree String
enumComment = getAllText <<< hasName "comment"

enumUnusedStart :: IOStateArrow s XmlTree String
enumUnusedStart = proc e -> do
  hasName "unused" -< e
  requiredAttrValue "start" -< e
