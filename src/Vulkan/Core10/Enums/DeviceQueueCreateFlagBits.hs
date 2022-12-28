{-# language CPP #-}
-- No documentation found for Chapter "DeviceQueueCreateFlagBits"
module Vulkan.Core10.Enums.DeviceQueueCreateFlagBits  ( DeviceQueueCreateFlags
                                                      , DeviceQueueCreateFlagBits( DEVICE_QUEUE_CREATE_PROTECTED_BIT
                                                                                 , ..
                                                                                 )
                                                      ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type DeviceQueueCreateFlags = DeviceQueueCreateFlagBits

-- | VkDeviceQueueCreateFlagBits - Bitmask specifying behavior of the queue
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'DeviceQueueCreateFlags'
newtype DeviceQueueCreateFlagBits = DeviceQueueCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'DEVICE_QUEUE_CREATE_PROTECTED_BIT' specifies that the device queue is a
-- protected-capable queue.
pattern DEVICE_QUEUE_CREATE_PROTECTED_BIT = DeviceQueueCreateFlagBits 0x00000001

conNameDeviceQueueCreateFlagBits :: String
conNameDeviceQueueCreateFlagBits = "DeviceQueueCreateFlagBits"

enumPrefixDeviceQueueCreateFlagBits :: String
enumPrefixDeviceQueueCreateFlagBits = "DEVICE_QUEUE_CREATE_PROTECTED_BIT"

showTableDeviceQueueCreateFlagBits :: [(DeviceQueueCreateFlagBits, String)]
showTableDeviceQueueCreateFlagBits = [(DEVICE_QUEUE_CREATE_PROTECTED_BIT, "")]

instance Show DeviceQueueCreateFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixDeviceQueueCreateFlagBits
      showTableDeviceQueueCreateFlagBits
      conNameDeviceQueueCreateFlagBits
      (\(DeviceQueueCreateFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read DeviceQueueCreateFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixDeviceQueueCreateFlagBits
      showTableDeviceQueueCreateFlagBits
      conNameDeviceQueueCreateFlagBits
      DeviceQueueCreateFlagBits
