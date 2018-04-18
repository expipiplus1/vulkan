{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parse.Platform
  ( parsePlatforms
  ) where

import           Parse.Utils
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
          pName <- requiredAttrValueT "name" -< platform
          pProtect <- requiredAttrValueT "protect" -< platform
          pComment <- requiredAttrValueT "comment" -< platform
          returnA -< Platform{..}
