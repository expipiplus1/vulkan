{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}

module Parse.Extension
  ( parseExtensions
  ) where

import           Data.Maybe        (fromMaybe)
import           Parse.Utils
import           Safe              (readMay)
import           Spec.Extension
import           Text.XML.HXT.Core

parseExtensions :: IOStateArrow s XmlTree [Extension]
parseExtensions = extractFields "Extensions"
                                (hasName "extensions")
                                extract
  where extract = listA (parseExtension <<< getChildren)

parseExtension :: IOStateArrow s XmlTree Extension
parseExtension = extractFields "Extension"
                               (hasName "extension")
                               extract
  where extract = proc extension -> do
          eName <- requiredAttrValue "name" -< extension
          eNumber <- requiredRead <<< requiredAttrValue "number" -< extension
          eSupported <- requiredAttrValue "supported" -< extension
          eProtect <- optionalAttrValue "protect" -< extension
          eAuthor <- optionalAttrValue "author" -< extension
          eContact <- optionalAttrValue "contact" -< extension
          require <- oneRequired "require"
                       (hasName "require" <<< getChildren) -< extension
          eEnums <-
            listA (parseExtensionEnum <<< getChildren) -< require
          eConstants <-
            listA (parseExtensionConstant <<< getChildren) -< require
          eBitmasks <-
            listA (parseExtensionBitmask <<< getChildren) -< require
          eCommandNames <-
            listA (parseCommandName <<< getChildren) -< require
          eTypeNames <-
            listA (parseTypeName <<< getChildren) -< require
          returnA -< Extension{..}

parseExtensionEnum :: IOStateArrow s XmlTree ExtensionEnum
parseExtensionEnum = extractFields "enum extension"
                                   (hasName "enum" >>> hasAttr "offset")
                                   extract
  where extract = proc extensionEnum -> do
          eeName <- requiredAttrValue "name" -< extensionEnum
          eeExtends <- requiredAttrValue "extends" -< extensionEnum
          eeOffset <- requiredRead <<<
                      requiredAttrValue "offset" -< extensionEnum
          eeDirection <- fromMaybe Positive ^<<
                         traverseMaybeA parseDirection <<<
                         optionalAttrValue "dir" -< extensionEnum
          returnA -< ExtensionEnum{..}

parseDirection :: IOStateArrow s String Direction
parseDirection = arrF p `orElse`
                 (("Failed to parse direction: " ++) ^>> failString)
    where p "-" = Just Negative
          p _ = Nothing

parseExtensionConstant :: IOStateArrow s XmlTree ExtensionConstant
parseExtensionConstant = extractFields "extension constant"
                                       (hasName "enum" >>> hasAttr "value")
                                       extract
  where extract = proc extensionConstant -> do
          ecName <- requiredAttrValue "name" -< extensionConstant
          ecExtends <- optionalAttrValue "extends" -< extensionConstant
          ecValue <- ((Right ^<< arrF readMay) `orElse`
                      (Left  ^<< arrF Just) `orElse`
                      (failString <<^
                        ("Failed to read extension constant value: " ++))) <<<
                      requiredAttrValue "value" -< extensionConstant
          returnA -< ExtensionConstant{..}

parseExtensionBitmask :: IOStateArrow s XmlTree ExtensionBitmask
parseExtensionBitmask = extractFields "extension bitmask"
                                       (hasName "enum" >>> hasAttr "bitpos")
                                       extract
  where extract = proc extensionBitmask -> do
          ebmName <- requiredAttrValue "name" -< extensionBitmask
          ebmExtends <- optionalAttrValue "extends" -< extensionBitmask
          ebmBitpos <- requiredRead <<< requiredAttrValue "bitpos" -< extensionBitmask
          returnA -< ExtensionBitmask{..}

parseCommandName :: IOStateArrow s XmlTree String
parseCommandName = extractFields "extension command name"
                                 (hasName "command")
                                 (requiredAttrValue "name")

parseTypeName :: IOStateArrow s XmlTree String
parseTypeName = extractFields "extension type name"
                              (hasName "type")
                              (requiredAttrValue "name")
