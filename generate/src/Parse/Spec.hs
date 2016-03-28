{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}

module Parse.Spec
  ( parseSpec
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Parse.Bitmask
import           Parse.Command
import           Parse.Constant
import           Parse.CType
import           Parse.Enum
import           Parse.Extension
import           Parse.Section
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
                          runX (withOtherUserState initialSpecParseState
                                (doc >>> oneRequired "spec" parseSpecXML))

parseSpecXML :: ParseArrow XmlTree Spec
parseSpecXML = isRoot /> hasName "registry" >>> extract
  where extract = proc registry -> do
          sTypes <- parseTypes <<< getChildren -< registry
          sConstants <- oneRequired "constants" (deep parseConstants) -< registry
          sEnums <- listA (deep parseEnum) -< registry
          sBitmasks <- listA (deep parseBitmask) -< registry
          sCommands <- listA (deep parseCommand) <<<
                      onlyChildWithName "commands" -< registry
          sCopyright <- getAllText <<< onlyChildWithName "comment" -< registry
          sSections <- oneRequired "sections" (deep parseSections) -< registry
          sExtensions <- oneRequired "extensions" (deep parseExtensions) -< registry
          sTags <- oneRequired "tags" (deep parseTags) -< registry
          sVendorIDs <- oneRequired "vendorids" (deep parseVendorIDs) -< registry
          returnA -< Spec{..}


