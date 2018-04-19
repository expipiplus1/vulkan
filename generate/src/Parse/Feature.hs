{-# LANGUAGE Arrows            #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parse.Feature
  ( parseFeature
  , interfaces
  ) where

import           Control.Applicative ((<|>))
import           Data.List
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Parse.Utils
import           Spec.Feature
import           Text.Read
import           Text.XML.HXT.Core

parseFeature :: IOStateArrow s XmlTree Feature
parseFeature = proc f -> do
  hasName "feature" -< f
  fAPI <- requiredAttrValueT "api" -< f
  fName <- requiredAttrValueT "name" -< f
  fNumber <- required "read feature number" readRational <<< requiredAttrValue "number" -< f
  fProtect <- optionalAttrValueT "protect" -< f
  fComment <- optionalAttrValueT "comment" -< f
  fElements <- app
            -< (allChildren (featureElemFail fName) [ARequirement ^<< requirement], f)
  returnA -< Feature{..}

featureElemFail
  :: Text
  --- ^ feature name
  -> IOStateArrow s XmlTree String
featureElemFail n = proc t -> do
  featureComment <- optional (getAttrOrChildText "comment") -< t
  returnA -< ("Failed to parse value of feature " ++ T.unpack n)
          ++ maybe "" (" with comment " ++) featureComment

requirement
  :: IOStateArrow s XmlTree FeatureRequirement
requirement = proc r -> do
  hasName "require" -< r
  frComment <- optionalAttrValueT "comment" -< r
  frProfile <- optionalAttrValueT "profile" -< r
  frExtension <- optionalAttrValueT "extension" -< r
  frInterfaces <- app -< (interfaces (fromMaybe "no comment" frComment), r)
  returnA -< FeatureRequirement{..}

interfaces
  :: Text
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
  , AnEnumValue ^<< enumValue
  , AnEnumName ^<< simpleName "enum" EnumName
  , ATypeName ^<< simpleName "type" TypeName
  , ACommandName ^<< simpleName "command" CommandName
  , AComment ^<< comment
  ]

interfaceElemFail
  :: Text
  --- ^ requirement comment
  -> IOStateArrow s XmlTree String
interfaceElemFail n = proc t -> do
  name <- optional (getAttrOrChildText "name") -< t
  returnA -< ("Failed to parse value of requirement with comment  " ++ T.unpack n)
          ++ maybe "" (" with name " ++) name

simpleName :: Text -> (Text -> a) -> IOStateArrow s XmlTree a
simpleName t c = c ^<< getAttrValue0T "name" <<< hasName (T.unpack t)

enumExtension :: IOStateArrow s XmlTree EnumExtension
enumExtension = proc e -> do
  hasName "enum" -< e
  eexOffset <- requiredRead <<< getAttrValue0 "offset" -< e
  -- Because it has the offset attribute we know it's an enumeration
  -- extension and can start failing better.
  eexExtNumber <- optional (requiredRead <<< getAttrValue0 "extnumber") -< e
  eexName <- requiredAttrValueT "name" -< e
  eexExtends <- requiredAttrValueT "extends" -< e
  eexDirection <- optional (required "direction parsing" parseDir <<< getAttrValue0 "dir") -< e
  eexComment <- optionalAttrValueT "comment" -< e
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
  bmxExtends <- requiredAttrValueT "extends" -< e
  bmxName <- requiredAttrValueT "name" -< e
  bmxComment <- optionalAttrValueT "comment" -< e
  returnA -< BitmaskExtension{..}

enumAlias :: IOStateArrow s XmlTree EnumAlias
enumAlias = proc e -> do
  hasName "enum" -< e
  eaAlias <- getAttrValue0T "alias" -< e
  eaName <- requiredAttrValueT "name" -< e
  eaExtends <- requiredAttrValueT "extends" -< e
  eaComment <- optionalAttrValueT "comment" -< e
  returnA -< EnumAlias{..}

enumExtensionAbsolute :: IOStateArrow s XmlTree EnumExtensionAbsolute
enumExtensionAbsolute = proc e -> do
  hasName "enum" -< e
  eexaExtends <- getAttrValue0T "extends" -< e
  eexaName <- requiredAttrValueT "name" -< e
  eexaValue <- (requiredRead <<< requiredAttrValue "value") `orElse` failA "hello" -< e
  eexaComment <- optionalAttrValueT "comment" -< e
  eexaExtNumber <- optional (requiredRead <<< getAttrValue0 "extnumber") -< e
  returnA -< EnumExtensionAbsolute{..}

enumValue :: IOStateArrow s XmlTree EnumValue
enumValue = proc e -> do
  hasName "enum" -< e
  evValue <- parseEnumValue ^<< getAttrValue0 "value" -< e
  evName <- requiredAttrValueT "name" -< e
  evComment <- optionalAttrValueT "comment" -< e
  returnA -< EnumValue{..}

parseEnumValue :: String -> EnumValueType
parseEnumValue t = fromMaybe
  (EnumValueAlias (T.pack t))
  (   (EnumValueString . T.pack <$> (dropPrefix "\"" =<< dropSuffix "\"" t))
  <|> (EnumValueInt <$> readMaybe t)
  )

dropPrefix :: String -> String -> Maybe String
dropPrefix prefix s = if prefix `isPrefixOf` s
                        then Just (drop (length prefix) s)
                        else Nothing

dropSuffix :: String -> String -> Maybe String
dropSuffix suffix s = if suffix `isSuffixOf` s
                        then Just (take (length s - length suffix) s)
                        else Nothing

comment :: IOStateArrow s XmlTree Text
comment = getAllTextT <<< hasName "comment"
