{-# LANGUAGE Arrows            #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parse.Extension
  ( parseExtensions
  ) where

import           Data.Maybe
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Parse.Feature
import           Parse.Utils
import           Spec.Extension
import           Text.XML.HXT.Core

parseExtensions :: IOStateArrow s XmlTree [Extension]
parseExtensions = extractFields "Extensions"
                                (hasName "extensions")
                                extract
  where extract = listA (parseExtension <<< getChildren)

parseExtension :: IOStateArrow s XmlTree Extension
parseExtension = proc f -> do
  hasName "extension" -< f
  extName      <- requiredAttrValueT "name" -< f
  extNumber    <- requiredRead <<< requiredAttrValue "number" -< f
  extProtect   <- optionalAttrValueT "protect" -< f
  extPlatform  <- optionalAttrValueT "platform" -< f
  extAuthor    <- optionalAttrValueT "author" -< f
  extContact   <- optionalAttrValueT "contact" -< f
  extType      <- optional (required "read extension type" readExtType <<< getAttrValue0 "type") -< f
  extRequires  <- optionalCommaSepListAttrT "requires" -< f
  extSupported <- required "read ext support" readExtensionSupport <<< requiredAttrValueT "supported" -< f
  extElements  <- app
              -< (allChildren (extensionElemFail extName) [AnExtensionRequirement ^<< requirement], f)
  returnA -< Extension{..}

extensionElemFail
  :: Text
  --- ^ extension name
  -> IOStateArrow s XmlTree String
extensionElemFail n = proc t -> do
  comment <- optional (getAttrOrChildText "comment") -< t
  returnA -< ("Failed to parse value of extension " ++ T.unpack n)
          ++ maybe "" (" with comment " ++) comment

requirement
  :: IOStateArrow s XmlTree ExtensionRequirement
requirement = proc r -> do
  hasName "require" -< r
  erAPI        <- optionalAttrValueT "api" -< r
  erProfile    <- optionalAttrValueT "profile" -< r
  erExtension  <- optionalAttrValueT "extension" -< r
  erFeature    <- optionalAttrValueT "feature" -< r
  erComment    <- getOptionalAttrOrChildTextT "comment" -< r
  erInterfaces <- app -< (interfaces (fromMaybe "no comment" erComment), r)
  returnA -< ExtensionRequirement{..}

readExtType :: String -> Maybe ExtensionType
readExtType = \case
  "device" -> Just Device
  "instance" -> Just Instance
  _ -> Nothing

readExtensionSupport :: Text -> Maybe ExtensionSupport
readExtensionSupport = \case
  "disabled" -> Just Disabled
  s -> Just (Profile s)
