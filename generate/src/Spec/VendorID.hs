module Spec.VendorID where

import           Spec.ExtensionTag

data VendorID = VendorID{ viName    :: ExtensionTag
                        , viID      :: Integer
                        , viComment :: String
                        }
  deriving(Show)

