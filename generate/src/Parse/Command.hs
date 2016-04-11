{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}

module Parse.Command where

import           Parse.CType
import           Parse.Utils
import           Spec.Command
import           Text.XML.HXT.Core

parseCommand :: ParseArrow XmlTree Command
parseCommand = hasName "command" >>>
               (extract `orElse` failA "Failed to extract command fields")
  where extract = proc command -> do
          proto <- onlyChildWithName "proto" -< command

          cName <- getAllText <<< onlyChildWithName "name" -< proto
          cSymbol <- parseIdentifier -< cName
          let cHsName = cName

          cReturnType <- parseCType <<<
            getAllText <<< processChildren (neg (hasName "name")) -< proto

          let cHsReturnType = undefined

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

-- | Implicit External Sync Params
parseIESPBlock :: ArrowXml a => a XmlTree [String]
parseIESPBlock =
  hasName "implicitexternsyncparams" >>>
  listA (getChildren >>> hasName "param" >>> getAllText)

parseParam :: ParseArrow XmlTree Parameter
parseParam = hasName "param" >>>
             (extract `orElse` failA "Failed to extract param fields")
  where extract = proc param -> do
          pName <- getAllText <<< onlyChildWithName "name" -< param
          let pHsName = pName
          pType <- parseCType <<<
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

parseExternSync :: String -> ExternSync
parseExternSync "true" = ExternSyncTrue
parseExternSync ss     = ExternSyncParams (commaSepList ss)
