module Spec.TypeEnv where

import qualified Data.HashMap.Lazy as Map
import           Write.Utils

data TypeEnv = TypeEnv{ teTypeInfo          :: Map.HashMap String TypeInfo
                      , teIntegralConstants :: Map.HashMap String Integer
                      , teNameLocations     :: NameLocations
                      }

data TypeInfo = TypeInfo{ tiSize      :: !Int
                        , tiAlignment :: !Int
                        }

