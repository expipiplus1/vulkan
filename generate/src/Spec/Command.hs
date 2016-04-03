module Spec.Command where

import           Language.C.Types (CIdentifier)
import           Spec.Type

data Command = Command { cName                     :: String
                       , cSymbol                   :: CIdentifier
                       , cReturnType               :: CType
                       , cParameters               :: [Parameter]
                       , cImplicitExternSyncParams :: Maybe [String]
                       , cQueues                   :: Maybe [String]
                       , cRenderPass               :: Maybe String
                       , cCommandBufferLevels      :: Maybe [String]
                       , cSuccessCodes             :: Maybe [String]
                       , cErrorCodes               :: Maybe [String]
                       , cUsage                    :: Maybe [String]
                       }
  deriving (Show)

data Parameter = Parameter { pName           :: String
                           , pType           :: CType
                           , pIsOptional     :: Maybe [Bool]
                             -- ^ Values further into the list represent the
                             -- "optionality" of the types as it is
                             -- dereferenced further. For example, for the
                             -- type "int*" with pIsOptional = [False, Type].
                             -- The pointer must be valid, but the int it
                             -- points to can have a default value (usually
                             -- zero).
                           , pIsExternSync   :: Maybe ExternSync
                           , pLengths        :: Maybe [String]
                           , pNoAutoValidity :: Maybe Bool
                           }
  deriving (Show)

data ExternSync = ExternSyncTrue
                | ExternSyncParams [String]
  deriving (Show)


