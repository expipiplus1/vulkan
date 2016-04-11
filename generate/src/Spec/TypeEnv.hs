module Spec.TypeEnv where

import qualified Data.HashMap.Lazy as Map

data TypeEnv = TypeEnv{ teTypeInfo          :: Map.HashMap String TypeInfo
                      , teIntegralConstants :: Map.HashMap String Integer
                      , teTypeMap           :: Map.HashMap String String
                      }

data TypeInfo = TypeInfo{ tiSize      :: !Int
                        , tiAlignment :: !Int
                        }

