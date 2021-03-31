{-# language CPP #-}
-- No documentation found for Chapter "InstanceCreateFlagBits"
module OpenXR.Core10.Enums.InstanceCreateFlagBits  ( InstanceCreateFlags
                                                   , InstanceCreateFlagBits(..)
                                                   ) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import OpenXR.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import OpenXR.Core10.FundamentalTypes (Flags64)
type InstanceCreateFlags = InstanceCreateFlagBits

-- No documentation found for TopLevel "XrInstanceCreateFlagBits"
newtype InstanceCreateFlagBits = InstanceCreateFlagBits Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameInstanceCreateFlagBits :: String
conNameInstanceCreateFlagBits = "InstanceCreateFlagBits"

enumPrefixInstanceCreateFlagBits :: String
enumPrefixInstanceCreateFlagBits = ""

showTableInstanceCreateFlagBits :: [(InstanceCreateFlagBits, String)]
showTableInstanceCreateFlagBits = []

instance Show InstanceCreateFlagBits where
  showsPrec = enumShowsPrec enumPrefixInstanceCreateFlagBits
                            showTableInstanceCreateFlagBits
                            conNameInstanceCreateFlagBits
                            (\(InstanceCreateFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read InstanceCreateFlagBits where
  readPrec = enumReadPrec enumPrefixInstanceCreateFlagBits
                          showTableInstanceCreateFlagBits
                          conNameInstanceCreateFlagBits
                          InstanceCreateFlagBits

