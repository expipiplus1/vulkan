module Spec.VendorID where

data VendorID = VendorID{ viName    :: String
                        , viID      :: Integer
                        , viComment :: String
                        }
  deriving(Show)

