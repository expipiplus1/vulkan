{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}

module Parse.Tag
  ( parseTags
  ) where

import           Parse.Utils
import           Spec.ExtensionTag
import           Spec.Tag
import           Text.XML.HXT.Core

parseTags :: IOStateArrow s XmlTree [Tag]
parseTags = extractFields "Tags"
                          (hasName "tags")
                          extract
  where extract = listA (parseTag <<< getChildren)

parseTag :: IOStateArrow s XmlTree Tag
parseTag = extractFields "Tag"
                         (hasName "tag")
                         extract
  where extract = proc tag -> do
          tName <- arrF stringToExtensionTag <<< requiredAttrValue "name" -< tag
          tAuthor <- requiredAttrValue "author" -< tag
          tContact <- requiredAttrValue "contact" -< tag
          returnA -< Tag{..}
