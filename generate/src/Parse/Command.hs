{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module Parse.Command where

import Spec.Command
import Parse.Utils
import Parse.CType
import Text.XML.HXT.Core
import Spec.CType

parseCommand :: IOStateArrow s XmlTree Spec.Command.Command
parseCommand = hasName "command" >>>
               (extract `orElse` failA "Failed to extract command fields")
  where extract = proc command -> do
          proto <- onlyChildWithName "proto" -< command

          cName <- getAllText <<< onlyChildWithName "name" -< proto

          cReturnType <- parseCType ^<<
            getAllText <<< processChildren (neg (hasName "name")) -< proto

          cParameters <- listA (parseParam <<< getChildren) -< command

          cImplicitExternSyncParams <- oneRequired 
            "implicit extern sync params"
            (optional (parseIESPBlock <<< getChildren)) -< command

          cQueues <- optionalCommaSepListAttr "queues" -< command

          cRenderPass <- optionalAttrValue "renderpass" -< command

          cCommandBufferLevels <- 
            optionalCommaSepListAttr "cmdbufferlevel" -< command

          cSuccessCodes <- optionalCommaSepListAttr "successcodes" -< command

          cErrorCodes <- optionalCommaSepListAttr "errorcodes" -< command

          cUsage <- oneRequired "usage"
            (optional (parseValidityBlock <<< getChildren)) -< command

          returnA -< Command{..}

parseValidityBlock :: ArrowXml a => a XmlTree [String]
parseValidityBlock = hasName "validity" >>> 
                     listA (getChildren >>> parseUsageString)
  where parseUsageString = hasName "usage" >>> getText

-- | Implicit External Sync Params
parseIESPBlock :: ArrowXml a => a XmlTree [String]
parseIESPBlock = 
  hasName "implicitexternsyncparams" >>> 
  listA (getChildren >>> hasName "param" >>> getText)

parseParam = hasName "param" >>> 
             (extract `orElse` failA "Failed to extract param fields")
  where extract = proc param -> do
          pName <- getAllText <<< onlyChildWithName "name" -< param
          pType <- parseCType ^<< 
                   getAllText <<< processChildren (neg (hasName "name")) 
                   -< param
          pIsOptional <- traverseMaybeA (mapA parseBool) <<< 
                         optionalCommaSepListAttr "optional" -< param
          pIsExternSync <- 
            fmap parseExternSync ^<< optionalAttrValue "externsync" -< param
          pLengths <- optionalCommaSepListAttr "len" -< param
          pNoAutoValidity <- (traverseMaybeA parseBool `orElse` 
                              failA "Failed to parse autovalidity attribute")
                             <<< optionalAttrValue "noautovalidity" -< param
          returnA -< Parameter{..}

traverseMaybeA :: ArrowIf a => a b c -> a (Maybe b) (Maybe c)
traverseMaybeA a = (arrF id >>> a >>^ Just) `orElse` constA Nothing

mapA :: ArrowList a => a b c -> a [b] [c]
mapA a = listA (a <<< unlistA)

parseBool :: IOStateArrow s String Bool
parseBool = arrF go `orElse` failA "Failed to parse bool"
  where go "true"  = Just True
        go "false" = Just False
        go _       = Nothing

parseExternSync :: String -> ExternSync
parseExternSync "true" = ExternSyncTrue
parseExternSync ss     = ExternSyncParams (commaSepList ss)
          
