module Spec.Section where

data Section = Section{ sComment :: String
                      , sTypeNames :: [String]
                      , sCommandNames :: [String]
                      , sEnumNames :: [String]
                      }
  deriving (Eq, Show)
