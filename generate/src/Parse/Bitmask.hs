{-# LANGUAGE Arrows           #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Parse.Bitmask where

import           Data.Word
import           Parse.Utils
import           Prelude           hiding (Bitmask)
import           Spec.Bitmask
import           Text.XML.HXT.Core

parseBitmask :: IOStateArrow s XmlTree Spec.Bitmask.Bitmask
parseBitmask = hasName "enums" >>> hasAttrValue "type" (== "bitmask") >>>
            (extract `orElse` failA "Failed to extract bitmask fields")
  where extract = proc bm -> do
          bmName      <- requiredAttrValue "name" -< bm
          bmComment   <- optionalAttrValue "comment" -< bm
          bms <- app -<
            (allChildren (bitmaskElemFail bmName) [
              AComment ^<< bitmaskComment
            , ABitmaskValue ^<< bitmaskValue
            , ABitmaskBitPosition ^<< bitmaskBitPos
            ], bm)
          let bmValues = [b | ABitmaskValue b <- bms]
              bmBitPositions = [b | ABitmaskBitPosition b <- bms]
          returnA -< Bitmask{..}

data BitmaskListMember
  = ABitmaskValue BitmaskValue
  | ABitmaskBitPosition BitmaskBitPosition
  | AComment String

bitmaskElemFail
  :: String
  --- ^ Bitmask name
  -> IOStateArrow s XmlTree String
bitmaskElemFail n = proc t -> do
  name <- optional (getAttrOrChildText "name") -< t
  bitpos <- optional (getAttrOrChildText "bitpos") -< t
  value <- optional (getAttrOrChildText "value") -< t
  returnA -< ("Failed to parse value of bitmask enumeration " ++ n)
          ++ maybe "" (" named " ++) name
          ++ maybe "" (" with value " ++) value
          ++ maybe "" (" with bitpos " ++) bitpos

bitmaskValue :: IOStateArrow s XmlTree BitmaskValue
bitmaskValue = proc e -> do
  hasName "enum" -< e
  bmvName    <- requiredAttrValue "name" -< e
  bmvValue   <- requiredRead @Word32 <<< getAttrValue0 "value" -< e
  bmvComment <- optionalAttrValue "comment" -< e
  returnA -< BitmaskValue{..}

bitmaskBitPos :: IOStateArrow s XmlTree BitmaskBitPosition
bitmaskBitPos = proc e -> do
  hasName "enum" -< e
  bmbpName    <- requiredAttrValue "name" -< e
  bmbpBitPos  <- requiredRead @Int <<< getAttrValue0 "bitpos" -< e
  bmbpComment <- optionalAttrValue "comment" -< e
  returnA -< BitmaskBitPosition{..}

--- | Comments which group the values (discarded at the moment)
bitmaskComment :: IOStateArrow s XmlTree String
bitmaskComment = getAllText <<< hasName "comment"

