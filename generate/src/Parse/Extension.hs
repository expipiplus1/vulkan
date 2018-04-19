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

-- parseExtension :: IOStateArrow s XmlTree Extension
-- parseExtension = extractFields "Extension"
--                                (hasName "extension")
--                                extract
--   where extract = proc extension -> do
--           eName <- requiredAttrValue "name" -< extension
--           eNumber <- requiredRead <<< requiredAttrValue "number" -< extension
--           eSupported <- requiredAttrValue "supported" -< extension
--           eProtect <- optionalAttrValue "protect" -< extension
--           eAuthor <- optionalAttrValue "author" -< extension
--           eContact <- optionalAttrValue "contact" -< extension
--           require <- oneRequired "require"
--                        (hasName "require" <<< getChildren) -< extension
--           eEnumExtensions <-
--             listA (parseEnumExtension <<< getChildren) -< require
--           eConstants <-
--             listA (parseExtensionConstant <<< getChildren) -< require
--           eCommandNames <-
--             listA (parseCommandName <<< getChildren) -< require
--           eTypeNames <-
--             listA (parseTypeName <<< getChildren) -< require
--           returnA -< Extension{..}

-- parseEnumExtension :: IOStateArrow s XmlTree EnumExtension
-- parseEnumExtension = extractFields "enum extension"
--                                    (hasName "enum" >>> hasAttr "extends")
--                                    extract
--   where extract = proc enumExtension -> do
--           eeName <- requiredAttrValue "name" -< enumExtension
--           eeExtends <- requiredAttrValue "extends" -< enumExtension
--           eeOffset <- requiredRead <<<
--                       requiredAttrValue "offset" -< enumExtension
--           eeDirection <- fromMaybe Positive ^<<
--                          traverseMaybeA parseDirection <<<
--                          optionalAttrValue "dir" -< enumExtension
--           returnA -< EnumExtension{..}

-- parseDirection :: IOStateArrow s String Direction
-- parseDirection = arrF p `orElse`
--                  (("Failed to parse direction: " ++) ^>> failString)
--     where p "-" = Just Negative
--           p _   = Nothing

-- parseExtensionConstant :: IOStateArrow s XmlTree ExtensionConstant
-- parseExtensionConstant = extractFields "extension constant"
--                                        (hasName "enum" >>> hasAttr "value")
--                                        extract
--   where extract = proc extensionConstant -> do
--           ecName <- requiredAttrValue "name" -< extensionConstant
--           ecValue <- ((Right ^<< arrF readMay) `orElse`
--                       (Left  ^<< arrF readMay) `orElse`
--                       (failString <<^
--                         ("Failed to read extension constant value: " ++))) <<<
--                       requiredAttrValue "value" -< extensionConstant
--           returnA -< ExtensionConstant{..}

-- parseCommandName :: IOStateArrow s XmlTree String
-- parseCommandName = extractFields "extension command name"
--                                  (hasName "command")
--                                  (requiredAttrValue "name")

-- parseTypeName :: IOStateArrow s XmlTree String
-- parseTypeName = extractFields "extension type name"
--                               (hasName "type")
--                               (requiredAttrValue "name")
