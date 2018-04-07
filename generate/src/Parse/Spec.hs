{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}

module Parse.Spec
  ( parseSpec
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.List              (isPrefixOf)
import           Parse.Bitmask
import           Parse.Command
import           Parse.Constant
import           Parse.Copyright
import           Parse.CType
import           Parse.Enum
import           Parse.Extension
import           Parse.Platform
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
          let sConstants = []
              sEnums = []
              sBitmasks = []
              sCommands = []
              sSections = []
              sExtensions = []
          setTraceLevel 9 -< ()
          sCopyright <- oneRequired "Copyright" (parseCopyright <<< getChildren) -< registry
          sVendorIDs <- oneRequired "vendorids" (parseVendorIDs <<< getChildren) -< registry
          sPlatforms <- oneRequired "platforms" (parsePlatforms <<< getChildren) -< registry
          sTags <- oneRequired "tags" (parseTags <<< getChildren) -< registry
          sTypes <- parseTypes <<< getChildren -< registry
          -- sConstants <- oneRequired "constants" (deep parseConstants) -< registry
          -- sEnums <- listA (deep parseEnum) -< registry
          -- sBitmasks <- listA (deep parseBitmask) -< registry
          -- sCommands <- listA (deep parseCommand) <<<
          --             onlyChildWithName "commands" -< registry
          -- _ <- traceSource <<< hasName "comment" <<< getChildren -< registry
          -- sCopyright <- isA (`isPrefixOf`) <<< getAllText <<< onlyChildWithName "comment" -< registry
          -- sSections <- oneRequired "sections" (deep parseSections) -< registry
          -- sExtensions <- oneRequired "extensions" (deep parseExtensions) -< registry
          returnA -< Spec{..}


