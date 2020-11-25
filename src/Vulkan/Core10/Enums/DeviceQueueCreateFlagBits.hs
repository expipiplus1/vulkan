{-# language CPP #-}
-- No documentation found for Chapter "DeviceQueueCreateFlagBits"
module Vulkan.Core10.Enums.DeviceQueueCreateFlagBits  ( DeviceQueueCreateFlags
                                                      , DeviceQueueCreateFlagBits( DEVICE_QUEUE_CREATE_PROTECTED_BIT
                                                                                 , ..
                                                                                 )
                                                      ) where

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
type DeviceQueueCreateFlags = DeviceQueueCreateFlagBits

-- | VkDeviceQueueCreateFlagBits - Bitmask specifying behavior of the queue
--
-- = See Also
--
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
  showsPrec = enumShowsPrec enumPrefixDeviceQueueCreateFlagBits
                            showTableDeviceQueueCreateFlagBits
                            conNameDeviceQueueCreateFlagBits
                            (\(DeviceQueueCreateFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read DeviceQueueCreateFlagBits where
  readPrec = enumReadPrec enumPrefixDeviceQueueCreateFlagBits
                          showTableDeviceQueueCreateFlagBits
                          conNameDeviceQueueCreateFlagBits
                          DeviceQueueCreateFlagBits

