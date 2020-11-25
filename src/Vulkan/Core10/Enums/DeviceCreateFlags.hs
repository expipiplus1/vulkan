{-# language CPP #-}
-- No documentation found for Chapter "DeviceCreateFlags"
module Vulkan.Core10.Enums.DeviceCreateFlags  (DeviceCreateFlags(..)) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
-- No documentation found for TopLevel "VkDeviceCreateFlags"
newtype DeviceCreateFlags = DeviceCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameDeviceCreateFlags :: String
conNameDeviceCreateFlags = "DeviceCreateFlags"

enumPrefixDeviceCreateFlags :: String
enumPrefixDeviceCreateFlags = ""

showTableDeviceCreateFlags :: [(DeviceCreateFlags, String)]
showTableDeviceCreateFlags = []


instance Show DeviceCreateFlags where
showsPrec = enumShowsPrec enumPrefixDeviceCreateFlags
                          showTableDeviceCreateFlags
                          conNameDeviceCreateFlags
                          (\(DeviceCreateFlags x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read DeviceCreateFlags where
  readPrec =
    enumReadPrec enumPrefixDeviceCreateFlags showTableDeviceCreateFlags conNameDeviceCreateFlags DeviceCreateFlags

