{-# LANGUAGE Arrows #-}

module Parse
  ( parseSpec
  ) where

import Spec
import Parse.Enum
import Parse.Bitmask
import Parse.Utils
import Safe(headMay)
import Text.XML.HXT.Core
import Control.Monad.IO.Class(MonadIO, liftIO)

parseSpec :: MonadIO m => String -> m [Spec]
parseSpec s = liftIO $ let doc = readString [withWarnings yes] s 
                       in runX (doc >>> oneRequired "spec" parseSpecXML)

parseSpecXML :: IOStateArrow s XmlTree Spec
parseSpecXML = extract
  where extract = proc registry -> do
          enums <- listA (deep parseEnum) -< registry
          bitmasks <- listA (deep parseBitmask) -< registry
          returnA -< Spec{ sEnums = enums
                         , sBitmasks = bitmasks
                         }


