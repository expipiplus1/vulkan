module Spec.Command where

import           Data.Text

data Command = Command { cName                     :: Text
                       , cReturnType               :: Text
                       , cParameters               :: [Parameter]
                       , cImplicitExternSyncParams :: [Text]
                       , cQueues                   :: Maybe [Text]
                       , cSuccessCodes             :: Maybe [Text]
                       , cErrorCodes               :: Maybe [Text]
                       , cRenderPass               :: Maybe Text
                       , cCommandBufferLevels      :: Maybe [Text]
                       , cPipeline                 :: Maybe [Text]
                       , cComment                  :: Maybe Text
                       }
  deriving (Show)

data Parameter = Parameter { pName           :: Text
                           , pType           :: Text
                           , pLengths        :: Maybe [Text]
                           , pAltLengths     :: Maybe [Text]
                           , pIsExternSync   :: Maybe ExternSync
                           , pIsOptional     :: Maybe [Bool]
                             -- ^ Values further into the list represent the
                             -- "optionality" of the types as it is
                             -- dereferenced further. For example, for the
                             -- type "int*" with pIsOptional = [False, Type].
                             -- The pointer must be valid, but the int it
                             -- points to can have a default value (usually
                             -- zero).
                           , pNoAutoValidity :: Maybe Bool
                           }
  deriving (Show)

data ExternSync = ExternSyncTrue
                | ExternSyncParams [Text]
  deriving (Show)

data CommandAlias = CommandAlias
  { caName    :: Text
  , caAlias   :: Text
  , caComment :: Maybe Text
  }
  deriving (Show)
