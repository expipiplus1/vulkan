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
          eEnumExtensions <-
            listA (parseEnumExtension <<< getChildren) -< require
          eConstants <-
            listA (parseExtensionConstant <<< getChildren) -< require
          eCommandNames <-
            listA (parseCommandName <<< getChildren) -< require
          eTypeNames <-
            listA (parseTypeName <<< getChildren) -< require
          returnA -< Extension{..}

parseEnumExtension :: IOStateArrow s XmlTree EnumExtension
parseEnumExtension = extractFields "enum extension"
                                   (hasName "enum" >>> hasAttr "extends")
                                   extract
  where extract = proc enumExtension -> do
          eeName <- requiredAttrValue "name" -< enumExtension
          let eeHsName = eeName
          eeExtends <- requiredAttrValue "extends" -< enumExtension
          eeOffset <- requiredRead <<<
                      requiredAttrValue "offset" -< enumExtension
          eeDirection <- fromMaybe Positive ^<<
                         traverseMaybeA parseDirection <<<
                         optionalAttrValue "dir" -< enumExtension
          returnA -< EnumExtension{..}

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
          let ecHsName = ecName
          ecValue <- ((Right ^<< arrF readMay) `orElse`
                      (Left  ^<< arrF readMay) `orElse`
                      (failString <<^
                        ("Failed to read extension constant value: " ++))) <<<
                      requiredAttrValue "value" -< extensionConstant
          returnA -< ExtensionConstant{..}

parseCommandName :: IOStateArrow s XmlTree String
parseCommandName = extractFields "extension command name"
                                 (hasName "command")
                                 (requiredAttrValue "name")

parseTypeName :: IOStateArrow s XmlTree String
parseTypeName = extractFields "extension type name"
                              (hasName "type")
                              (requiredAttrValue "name")
