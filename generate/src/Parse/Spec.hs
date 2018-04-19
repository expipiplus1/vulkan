{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}

module Parse.Spec
  ( parseSpec
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Parse.Bitmask
import           Parse.Command
import           Parse.Constant
import           Parse.Copyright
import           Parse.Enum
import           Parse.Extension
import           Parse.Feature
import           Parse.Platform
import           Parse.Tag
import           Parse.Type
import           Parse.Utils
import           Parse.VendorID
import           Safe                   (headMay)
import           Spec.Spec
import           Text.XML.HXT.Core

parseSpec :: MonadIO m => String -> m (Maybe Spec)
parseSpec s = liftIO $ let doc = readString [withWarnings yes] s
                       in headMay <$>
                          runX (withOtherUserState ()
                                (doc >>> oneRequired "spec" parseSpecXML))

parseSpecXML :: IOStateArrow s XmlTree Spec
parseSpecXML = isRoot /> hasName "registry" >>> extract
  where extract = proc registry -> do
          setTraceLevel 9 -< ()
          sCopyright <- oneRequired "Copyright" (parseCopyright <<< getChildren) -< registry
          sVendorIDs <- oneRequired "vendorids" (parseVendorIDs <<< getChildren) -< registry
          sPlatforms <- oneRequired "platforms" (parsePlatforms <<< getChildren) -< registry
          sTags <- oneRequired "tags" (parseTags <<< getChildren) -< registry
          sTypes <- oneRequired "types" (parseTypes <<< getChildren) -< registry
          sConstants <- oneRequired "constants" (parseConstants <<< getChildren) -< registry
          -- Enums, bitmasks and commands are a little different, each of them
          -- is at the top level
          sEnums <- listA (parseEnum <<< getChildren) -< registry
          sBitmasks <- listA (deep parseBitmask <<< getChildren) -< registry
          (sCommandAliases, sCommands) <- oneRequired "Commands" (parseCommands <<< getChildren) -< registry
          sFeatures <- listA (parseFeature <<< getChildren) -< registry
          sExtensions <- oneRequired "extensions" (parseExtensions <<< getChildren) -< registry
          returnA -< Spec{..}
