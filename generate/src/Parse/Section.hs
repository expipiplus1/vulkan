{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}

module Parse.Section
  ( parseSections
  ) where

import           Parse.Utils
import           Prelude           hiding (elem)
import           Spec.Section
import           Text.XML.HXT.Core

parseSections :: IOStateArrow s XmlTree [Section]
parseSections = extractFields "API Sections"
                              (hasName "feature" >>> hasAttrValue "api" (== "vulkan"))
                              extract
  where extract = listA (parseSection <<< getChildren)

parseSection :: IOStateArrow s XmlTree Section
parseSection = extractFields "Section"
                              (hasName "require")
                              extract
  where extract = proc section -> do
          sComment <- requiredAttrValue "comment" -< section
          sTypeNames <- listA (parseTypeName <<< getChildren) -< section
          sCommandNames <- listA (parseCommandName <<< getChildren) -< section
          sEnumNames <- listA (parseEnumName <<< getChildren) -< section
          returnA -< Section{..}

parseTypeName :: IOStateArrow s XmlTree String
parseTypeName = extractFields "type name"
                              (hasName "type")
                              extract
  where extract = getAttrValue0 "name"

parseCommandName :: IOStateArrow s XmlTree String
parseCommandName = extractFields "command name"
                              (hasName "command")
                              extract
  where extract = getAttrValue0 "name"

parseEnumName :: IOStateArrow s XmlTree String
parseEnumName = extractFields "enum name"
                              (hasName "enum")
                              extract
  where extract = getAttrValue0 "name"
