{-# language CPP #-}
-- No documentation found for Chapter "InstanceCreateFlags"
module OpenXR.Core10.Enums.InstanceCreateFlags  (InstanceCreateFlags(..)) where

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
-- | XrInstanceCreateFlags - Instance Creation Flags
--
-- = Description
--
-- There are currently no instance creation flags. This is reserved for
-- future use.
--
-- = See Also
--
-- 'OpenXR.Core10.Instance.InstanceCreateInfo',
-- 'OpenXR.Core10.Instance.createInstance'
newtype InstanceCreateFlags = InstanceCreateFlags Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameInstanceCreateFlags :: String
conNameInstanceCreateFlags = "InstanceCreateFlags"

enumPrefixInstanceCreateFlags :: String
enumPrefixInstanceCreateFlags = ""

showTableInstanceCreateFlags :: [(InstanceCreateFlags, String)]
showTableInstanceCreateFlags = []

instance Show InstanceCreateFlags where
  showsPrec = enumShowsPrec enumPrefixInstanceCreateFlags
                            showTableInstanceCreateFlags
                            conNameInstanceCreateFlags
                            (\(InstanceCreateFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read InstanceCreateFlags where
  readPrec = enumReadPrec enumPrefixInstanceCreateFlags
                          showTableInstanceCreateFlags
                          conNameInstanceCreateFlags
                          InstanceCreateFlags

