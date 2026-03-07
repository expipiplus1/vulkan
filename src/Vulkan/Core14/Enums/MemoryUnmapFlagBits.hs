{-# language CPP #-}
-- No documentation found for Chapter "MemoryUnmapFlagBits"
module Vulkan.Core14.Enums.MemoryUnmapFlagBits  ( MemoryUnmapFlags
                                                , MemoryUnmapFlagBits( MEMORY_UNMAP_RESERVE_BIT_EXT
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
type MemoryUnmapFlags = MemoryUnmapFlagBits

-- | VkMemoryUnmapFlagBits - Bitmask specifying additional parameters of a
-- memory unmap
--
-- = Description
--
-- -   'MEMORY_UNMAP_RESERVE_BIT_EXT' requests that virtual address range
--     currently occupied by the memory map remain reserved after the
--     'Vulkan.Core14.Promoted_From_VK_KHR_map_memory2Roadmap.unmapMemory2'
--     call completes. Future system memory map operations or calls to
--     'Vulkan.Core10.Memory.mapMemory' or
--     'Vulkan.Core14.Promoted_From_VK_KHR_map_memory2Roadmap.mapMemory2'
--     will not return addresses in that range unless the range has since
--     been unreserved by the client or the mapping is explicitly placed in
--     that range by calling
--     'Vulkan.Core14.Promoted_From_VK_KHR_map_memory2Roadmap.mapMemory2'
--     with
--     'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT',
--     or doing the system memory map equivalent. When
--     'MEMORY_UNMAP_RESERVE_BIT_EXT' is set, the memory unmap operation
--     /may/ fail, in which case the memory object will remain host mapped
--     and
--     'Vulkan.Core14.Promoted_From_VK_KHR_map_memory2Roadmap.unmapMemory2'
--     will return 'Vulkan.Core10.Enums.Result.ERROR_MEMORY_MAP_FAILED'.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_map_memory2 VK_KHR_map_memory2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'MemoryUnmapFlags'
newtype MemoryUnmapFlagBits = MemoryUnmapFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkMemoryUnmapFlagBits" "VK_MEMORY_UNMAP_RESERVE_BIT_EXT"
pattern MEMORY_UNMAP_RESERVE_BIT_EXT = MemoryUnmapFlagBits 0x00000001

conNameMemoryUnmapFlagBits :: String
conNameMemoryUnmapFlagBits = "MemoryUnmapFlagBits"

enumPrefixMemoryUnmapFlagBits :: String
enumPrefixMemoryUnmapFlagBits = "MEMORY_UNMAP_RESERVE_BIT_EXT"

showTableMemoryUnmapFlagBits :: [(MemoryUnmapFlagBits, String)]
showTableMemoryUnmapFlagBits = [(MEMORY_UNMAP_RESERVE_BIT_EXT, "")]

instance Show MemoryUnmapFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixMemoryUnmapFlagBits
      showTableMemoryUnmapFlagBits
      conNameMemoryUnmapFlagBits
      (\(MemoryUnmapFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read MemoryUnmapFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixMemoryUnmapFlagBits
      showTableMemoryUnmapFlagBits
      conNameMemoryUnmapFlagBits
      MemoryUnmapFlagBits
