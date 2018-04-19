{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parse.Command
  ( parseCommands
  ) where

import           Data.Either
import           Data.Text
import           Parse.Utils
import           Spec.Command
import           Text.XML.HXT.Core

parseCommands :: IOStateArrow s XmlTree ([CommandAlias], [Command])
parseCommands = extractFields
  "command decls"
  (hasName "commands")
  (   partitionEithers
  ^<< allChildren commandFailDiag [Left ^<< commandAlias, Right ^<< command]
  )

commandFailDiag :: IOStateArrow s XmlTree String
commandFailDiag = proc c -> do
  proto <- onlyChildWithName "proto" -< c
  name <- getAllText <<< onlyChildWithName "name" -< proto
  returnA -< "Failed to parse command named " ++ name

-- TODO: Handle
commandAlias :: IOStateArrow s XmlTree CommandAlias
commandAlias = proc c -> do
  hasName "command" -< c
  caName <- getAttrValue0T "name" -< c
  caAlias <- getAttrValue0T "alias" -< c
  caComment <- optionalAttrValueT "comment" -< c
  returnA -< CommandAlias{..}

command :: IOStateArrow s XmlTree Command
command = proc c -> do
  hasName "command"                                                   -< c
  -- The prototype is parsed here so that we can have a better error message
  -- (including the command name) should things fail later
  proto                  <- onlyChildWithName "proto"                 -< c
  cName                  <- getAllTextT <<< onlyChildWithName "name"   -< proto
  cReturnType            <- getAllTextT                                -< proto
  cQueues                <- optionalCommaSepListAttrT "queues"         -< c
  cSuccessCodes          <- optionalCommaSepListAttrT "successcodes"   -< c
  cErrorCodes            <- optionalCommaSepListAttrT "errorcodes"     -< c
  cRenderPass            <- optionalAttrValueT "renderpass"            -< c
  cCommandBufferLevels   <- optionalCommaSepListAttrT "cmdbufferlevel" -< c
  cPipeline              <- optionalCommaSepListAttrT "pipeline"       -< c
  cComment               <- optionalAttrValueT "comment"               -< c
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
  | CommandImplicitExternSyncParams [Text]

-- | Implicit External Sync Params
implicitExternSyncParams :: IOStateArrow s XmlTree [Text]
implicitExternSyncParams =
  hasName "implicitexternsyncparams" >>>
    allChildren (("Failed to parse IESP " ++) ^<< getAllText)
    [hasName "param" >>> getAllTextT]

parameter :: IOStateArrow s XmlTree Parameter
parameter = proc p -> do
  hasName "param" -< p
  pName <- getAllTextT <<< onlyChildWithName "name" -< p
  pType <- getAllTextT <<< processChildren (neg (hasName "name")) -< p
  pIsOptional <- traverseMaybeA (mapA parseBoolT) <<<
                 optionalCommaSepListAttrT "optional" -< p
  pIsExternSync <-
    fmap externSync ^<< optionalAttrValueT "externsync" -< p
  pLengths <- optionalCommaSepListAttrT "len" -< p
  pAltLengths <- optionalCommaSepListAttrT "altlen" -< p
  pNoAutoValidity <- (traverseMaybeA parseBool `orElse`
                      failA "Failed to parse autovalidity attribute")
                     <<< optionalAttrValue "noautovalidity" -< p
  returnA -< Parameter{..}

externSync :: Text -> ExternSync
externSync "true" = ExternSyncTrue
externSync ss     = ExternSyncParams (commaSepListT ss)

parameterFail
  :: Text
  --- ^ Command name
  -> IOStateArrow s XmlTree String
parameterFail n = proc t -> do
  name <- optional (getAttrOrChildText "name") -< t
  returnA -< ("Failed to parse parameter of command " ++ unpack n)
          ++ maybe "" (" named " ++) name
