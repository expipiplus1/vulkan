module Spec.Command where

import           Spec.Type

data Command = Command { cName                     :: String
                       , cReturnType               :: String
                       , cParameters               :: [Parameter]
                       , cImplicitExternSyncParams :: [String]
                       , cQueues                   :: Maybe [String]
                       , cSuccessCodes             :: Maybe [String]
                       , cErrorCodes               :: Maybe [String]
                       , cRenderPass               :: Maybe String
                       , cCommandBufferLevels      :: Maybe [String]
                       , cPipeline                 :: Maybe [String]
                       , cComment                  :: Maybe String
                       }
  deriving (Show)

data Parameter = Parameter { pName           :: String
                           , pType           :: String
                           , pLengths        :: Maybe [String]
                           , pAltLengths     :: Maybe [String]
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
                | ExternSyncParams [String]
  deriving (Show)


