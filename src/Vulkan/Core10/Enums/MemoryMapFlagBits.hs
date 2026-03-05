{-# language CPP #-}
-- No documentation found for Chapter "MemoryMapFlagBits"
module Vulkan.Core10.Enums.MemoryMapFlagBits  ( MemoryMapFlags
                                              , MemoryMapFlagBits( MEMORY_MAP_PLACED_BIT_EXT
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
type MemoryMapFlags = MemoryMapFlagBits

-- | VkMemoryMapFlagBits - Bitmask specifying additional parameters of a
-- memory map
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'MemoryMapFlags'
newtype MemoryMapFlagBits = MemoryMapFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'MEMORY_MAP_PLACED_BIT_EXT' requests that the implementation place the
-- memory map at the virtual address specified by the client via
-- 'Vulkan.Extensions.VK_EXT_map_memory_placed.MemoryMapPlacedInfoEXT'::@pPlacedAddress@,
-- replacing any existing mapping at that address. This flag /must/ not be
-- used with 'Vulkan.Core10.Memory.mapMemory' as there is no way to specify
-- the placement address.
pattern MEMORY_MAP_PLACED_BIT_EXT = MemoryMapFlagBits 0x00000001

conNameMemoryMapFlagBits :: String
conNameMemoryMapFlagBits = "MemoryMapFlagBits"

enumPrefixMemoryMapFlagBits :: String
enumPrefixMemoryMapFlagBits = "MEMORY_MAP_PLACED_BIT_EXT"

showTableMemoryMapFlagBits :: [(MemoryMapFlagBits, String)]
showTableMemoryMapFlagBits = [(MEMORY_MAP_PLACED_BIT_EXT, "")]

instance Show MemoryMapFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixMemoryMapFlagBits
      showTableMemoryMapFlagBits
      conNameMemoryMapFlagBits
      (\(MemoryMapFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read MemoryMapFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixMemoryMapFlagBits
      showTableMemoryMapFlagBits
      conNameMemoryMapFlagBits
      MemoryMapFlagBits
