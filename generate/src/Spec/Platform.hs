module Spec.Platform where

data Platform = Platform
  { pName    :: String
  , pProtect :: String
  , pComment :: String
  }
  deriving(Show)
