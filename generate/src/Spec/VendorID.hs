module Spec.VendorID where

import           Spec.ExtensionTag

data VendorID = VendorID{ viName    :: ExtensionTag
                        , viID      :: Integer
                        , viComment :: Maybe String
                        }
  deriving(Show)

