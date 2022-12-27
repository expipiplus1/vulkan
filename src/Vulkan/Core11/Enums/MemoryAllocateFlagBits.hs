{-# language CPP #-}
-- No documentation found for Chapter "MemoryAllocateFlagBits"
module Vulkan.Core11.Enums.MemoryAllocateFlagBits  ( MemoryAllocateFlags
                                                   , MemoryAllocateFlagBits( MEMORY_ALLOCATE_DEVICE_MASK_BIT
                                                                           , MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT
                                                                           , MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT
                                                                           , ..
                                                                           )
                                                   ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Bits (Bits)
import GHC.Bits (FiniteBits)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type MemoryAllocateFlags = MemoryAllocateFlagBits

-- | VkMemoryAllocateFlagBits - Bitmask specifying flags for a device memory
-- allocation
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'MemoryAllocateFlags'
newtype MemoryAllocateFlagBits = MemoryAllocateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'MEMORY_ALLOCATE_DEVICE_MASK_BIT' specifies that memory will be
-- allocated for the devices in
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group.MemoryAllocateFlagsInfo'::@deviceMask@.
pattern MEMORY_ALLOCATE_DEVICE_MASK_BIT = MemoryAllocateFlagBits 0x00000001

-- | 'MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT' specifies that the
-- memory’s address /can/ be saved and reused on a subsequent run (e.g. for
-- trace capture and replay), see
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.BufferOpaqueCaptureAddressCreateInfo'
-- for more detail.
pattern MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT = MemoryAllocateFlagBits 0x00000004

-- | 'MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT' specifies that the memory /can/ be
-- attached to a buffer object created with the
-- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT'
-- bit set in @usage@, and that the memory handle /can/ be used to retrieve
-- an opaque address via
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getDeviceMemoryOpaqueCaptureAddress'.
pattern MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT = MemoryAllocateFlagBits 0x00000002

conNameMemoryAllocateFlagBits :: String
conNameMemoryAllocateFlagBits = "MemoryAllocateFlagBits"

enumPrefixMemoryAllocateFlagBits :: String
enumPrefixMemoryAllocateFlagBits = "MEMORY_ALLOCATE_DEVICE_"

showTableMemoryAllocateFlagBits :: [(MemoryAllocateFlagBits, String)]
showTableMemoryAllocateFlagBits =
  [
    ( MEMORY_ALLOCATE_DEVICE_MASK_BIT
    , "MASK_BIT"
    )
  ,
    ( MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT
    , "ADDRESS_CAPTURE_REPLAY_BIT"
    )
  ,
    ( MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT
    , "ADDRESS_BIT"
    )
  ]

instance Show MemoryAllocateFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixMemoryAllocateFlagBits
      showTableMemoryAllocateFlagBits
      conNameMemoryAllocateFlagBits
      (\(MemoryAllocateFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read MemoryAllocateFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixMemoryAllocateFlagBits
      showTableMemoryAllocateFlagBits
      conNameMemoryAllocateFlagBits
      MemoryAllocateFlagBits
