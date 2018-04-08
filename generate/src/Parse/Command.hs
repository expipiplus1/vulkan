{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}

module Parse.Command where

import           Data.Either
import           Parse.CType
import           Parse.Utils
import           Spec.Command
import           Text.XML.HXT.Core

parseCommands :: IOStateArrow s XmlTree [Command]
parseCommands = extractFields
  "command decls"
  (hasName "commands")
  (   rights
  ^<< allChildren commandFailDiag [Left ^<< commandAlias, Right ^<< command]
  )

commandFailDiag :: IOStateArrow s XmlTree String
commandFailDiag = proc c -> do
  proto <- onlyChildWithName "proto" -< c
  name <- getAllText <<< onlyChildWithName "name" -< proto
  returnA -< "Failed to parse command named " ++ name

-- TODO: Handle
commandAlias :: IOStateArrow s XmlTree (String, String)
commandAlias = proc c -> do
  hasName "command" -< c
  name <- getAttrValue0 "name" -< c
  alias <- getAttrValue0 "alias" -< c
  returnA -< (name, alias)

command :: IOStateArrow s XmlTree Command
command = proc c -> do
  hasName "command"                                                   -< c
  -- The prototype is parsed here so that we can have a better error message
  -- (including the command name) should things fail later
  proto                  <- onlyChildWithName "proto"                 -< c
  cName                  <- getAllText <<< onlyChildWithName "name"   -< proto
  cReturnType            <- getAllText                                -< proto
  cQueues                <- optionalCommaSepListAttr "queues"         -< c
  cSuccessCodes          <- optionalCommaSepListAttr "successcodes"   -< c
  cErrorCodes            <- optionalCommaSepListAttr "errorcodes"     -< c
  cRenderPass            <- optionalAttrValue "renderpass"            -< c
  cCommandBufferLevels   <- optionalCommaSepListAttr "cmdbufferlevel" -< c
  cPipeline              <- optionalCommaSepListAttr "pipeline"       -< c
  cComment               <- optionalAttrValue "comment"               -< c
  cs <- app
     -< (allChildren (parameterFail cName) [
          const CommandProto ^<< hasName "proto"
        , CommandParam ^<< parameter
        , CommandImplicitExternSyncParams ^<< implicitExternSyncParams
        ], c)
  let cParameters = [p | CommandParam p <- cs]
      cImplicitExternSyncParams =
        [p | CommandImplicitExternSyncParams ps <- cs, p <- ps]
  returnA -< Command{..}

data CommandListMember
  = CommandProto -- handled separately, just here to have a return value for
                 -- 'allChildren'
  | CommandParam Parameter
  | CommandImplicitExternSyncParams [String]

-- | Implicit External Sync Params
implicitExternSyncParams :: IOStateArrow s XmlTree [String]
implicitExternSyncParams =
  hasName "implicitexternsyncparams" >>>
    allChildren (("Failed to parse IESP " ++) ^<< getAllText)
    [(hasName "param" >>> getAllText)]

parameter :: IOStateArrow s XmlTree Parameter
parameter = proc p -> do
  hasName "param" -< p
  pName <- getAllText <<< onlyChildWithName "name" -< p
  pType <- getAllText <<< processChildren (neg (hasName "name")) -< p
  pIsOptional <- traverseMaybeA (mapA parseBool) <<<
                 optionalCommaSepListAttr "optional" -< p
  pIsExternSync <-
    fmap externSync ^<< optionalAttrValue "externsync" -< p
  pLengths <- optionalCommaSepListAttr "len" -< p
  pAltLengths <- optionalCommaSepListAttr "altlen" -< p
  pNoAutoValidity <- (traverseMaybeA parseBool `orElse`
                      failA "Failed to parse autovalidity attribute")
                     <<< optionalAttrValue "noautovalidity" -< p
  returnA -< Parameter{..}

externSync :: String -> ExternSync
externSync "true" = ExternSyncTrue
externSync ss     = ExternSyncParams (commaSepList ss)

parameterFail
  :: String
  --- ^ Command name
  -> IOStateArrow s XmlTree String
parameterFail n = proc t -> do
  name <- optional (getAttrOrChildText "name") -< t
  returnA -< ("Failed to parse parameter of command " ++ n)
          ++ maybe "" (" named " ++) name
