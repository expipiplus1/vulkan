{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}

module Parse.Platform
  ( parsePlatforms
  ) where

import           Parse.Utils
import           Spec.ExtensionTag
import           Spec.Platform
import           Text.XML.HXT.Core

parsePlatforms :: IOStateArrow s XmlTree [Platform]
parsePlatforms = extractFields "Platforms"
                               (hasName "platforms")
                               extract
  where extract = listA (parsePlatform <<< getChildren)

parsePlatform :: IOStateArrow s XmlTree Platform
parsePlatform = extractFields "Platform"
                              (hasName "platform")
                              extract
  where extract = proc platform -> do
          pName <- requiredAttrValue "name" -< platform
          pProtect <- requiredAttrValue "protect" -< platform
          pComment <- requiredAttrValue "comment" -< platform
          returnA -< Platform{..}
