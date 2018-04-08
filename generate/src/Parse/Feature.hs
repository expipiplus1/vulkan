{-# LANGUAGE Arrows          #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Parse.Feature
  ( parseFeature
  , interfaces
  ) where

import           Data.Maybe        (fromMaybe, listToMaybe)
import           Parse.Utils
import           Spec.Feature
import           Text.XML.HXT.Core

parseFeature :: IOStateArrow s XmlTree Feature
parseFeature = proc f -> do
  hasName "feature" -< f
  fAPI <- requiredAttrValue "api" -< f
  fName <- requiredAttrValue "name" -< f
  fNumber <- required "read feature number" readRational <<< requiredAttrValue "number" -< f
  fProtect <- optionalAttrValue "protect" -< f
  fComment <- optionalAttrValue "comment" -< f
  fElements <- app
            -< (allChildren (featureElemFail fName) [ARequirement ^<< requirement], f)
  returnA -< Feature{..}

featureElemFail
  :: String
  --- ^ feature name
  -> IOStateArrow s XmlTree String
featureElemFail n = proc t -> do
  comment <- optional (getAttrOrChildText "comment") -< t
  returnA -< ("Failed to parse value of feature " ++ n)
          ++ maybe "" (" with comment " ++) comment

requirement
  :: IOStateArrow s XmlTree FeatureRequirement
requirement = proc r -> do
  hasName "require" -< r
  frComment <- optionalAttrValue "comment" -< r
  frProfile <- optionalAttrValue "profile" -< r
  frExtension <- optionalAttrValue "extension" -< r
  frInterfaces <- app -< (interfaces (fromMaybe "no comment" frComment), r)
  returnA -< FeatureRequirement{..}

interfaces
  :: String
  -- ^ Parent Name
  -> IOStateArrow s XmlTree [InterfaceElement]
interfaces p = allChildren
  (interfaceElemFail p)
  [ -- The extensions must come first, as their attributes are a superset of
      -- the simple names.
    AnEnumExtension ^<< enumExtension
  , ABitmaskExtension ^<< bitmaskExtension
  , AnEnumAlias ^<< enumAlias
  , AnEnumExtensionAbsolute ^<< enumExtensionAbsolute
  , AnEnumName ^<< simpleName "enum" EnumName
  , ATypeName ^<< simpleName "type" TypeName
  , ACommandName ^<< simpleName "command" CommandName
  , AComment ^<< comment
  ]

interfaceElemFail
  :: String
  --- ^ requirement comment
  -> IOStateArrow s XmlTree String
interfaceElemFail n = proc t -> do
  name <- optional (getAttrOrChildText "name") -< t
  returnA -< ("Failed to parse value of requirement with comment  " ++ n)
          ++ maybe "" (" with name " ++) name

simpleName :: String -> (String -> a) -> IOStateArrow s XmlTree a
simpleName t c = c ^<< getAttrValue0 "name" <<< hasName t

enumExtension :: IOStateArrow s XmlTree EnumExtension
enumExtension = proc e -> do
  hasName "enum" -< e
  eexOffset <- requiredRead <<< getAttrValue0 "offset" -< e
  -- Because it has the offset attribute we know it's an enumeration
  -- extension and can start failing better.
  eexExtNumber <- optional (requiredRead <<< getAttrValue0 "extnumber") -< e
  eexExtends <- requiredAttrValue "extends" -< e
  eexDirection <- optional (required "direction parsing" parseDir <<< getAttrValue0 "dir") -< e
  eexName <- requiredAttrValue "name" -< e
  eexComment <- optionalAttrValue "comment" -< e
  returnA -< EnumExtension{..}

parseDir :: String -> Maybe Direction
parseDir = \case
  "-" -> Just Negative
  _ -> Nothing

bitmaskExtension :: IOStateArrow s XmlTree BitmaskExtension
bitmaskExtension = proc e -> do
  hasName "enum" -< e
  bmxBitPos <- requiredRead <<< getAttrValue0 "bitpos" -< e
  -- Because it has the bitpos attribute we know it's an bitmask
  -- extension and can start failing better.
  bmxExtends <- requiredAttrValue "extends" -< e
  bmxName <- requiredAttrValue "name" -< e
  bmxComment <- optionalAttrValue "comment" -< e
  returnA -< BitmaskExtension{..}

enumAlias :: IOStateArrow s XmlTree EnumAlias
enumAlias = proc e -> do
  hasName "enum" -< e
  eaAlias <- getAttrValue0 "alias" -< e
  eaName <- requiredAttrValue "name" -< e
  eaExtends <- requiredAttrValue "extends" -< e
  eaComment <- optionalAttrValue "comment" -< e
  returnA -< EnumAlias{..}

enumExtensionAbsolute :: IOStateArrow s XmlTree EnumExtensionAbsolute
enumExtensionAbsolute = proc e -> do
  hasName "enum" -< e
  eexaValue <- getAttrValue0 "value" -< e
  eexaName <- requiredAttrValue "name" -< e
  eexaExtends <- optionalAttrValue "extends" -< e
  eexaComment <- optionalAttrValue "comment" -< e
  returnA -< EnumExtensionAbsolute{..}

comment :: IOStateArrow s XmlTree String
comment = getAllText <<< hasName "comment"
