{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}

module Parse.VendorID
  ( parseVendorIDs
  ) where

import           Parse.Utils
import           Spec.ExtensionTag
import           Spec.VendorID
import           Text.XML.HXT.Core

-- TODO: refactor to use allChildren
parseVendorIDs :: IOStateArrow s XmlTree [VendorID]
parseVendorIDs = extractFields "VendorIDs"
                               (hasName "vendorids")
                               extract
  where extract = listA (parseVendorID <<< getChildren)

parseVendorID :: IOStateArrow s XmlTree VendorID
parseVendorID = extractFields "VendorID"
                              (hasName "vendorid")
                              extract
  where extract = proc vendorid -> do
          viName <- required "stringToExtensionTag" stringToExtensionTag <<<
                    requiredAttrValue "name" -< vendorid
          viID <- requiredRead <<< requiredAttrValue "id" -< vendorid
          viComment <- optionalAttrValue "comment" -< vendorid
          returnA -< VendorID{..}
