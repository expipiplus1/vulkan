{-# language CPP #-}
-- No documentation found for Chapter "MemoryMapFlags"
module Vulkan.Core10.Enums.MemoryMapFlags  (MemoryMapFlags(..)) where

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
-- | VkMemoryMapFlags - Reserved for future use
--
-- = Description
--
-- 'MemoryMapFlags' is a bitmask type for setting a mask, but is currently
-- reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Extensions.VK_KHR_map_memory2.MemoryMapInfoKHR',
-- 'Vulkan.Core10.Memory.mapMemory'
newtype MemoryMapFlags = MemoryMapFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNameMemoryMapFlags :: String
conNameMemoryMapFlags = "MemoryMapFlags"

enumPrefixMemoryMapFlags :: String
enumPrefixMemoryMapFlags = ""

showTableMemoryMapFlags :: [(MemoryMapFlags, String)]
showTableMemoryMapFlags = []

instance Show MemoryMapFlags where
  showsPrec =
    enumShowsPrec
      enumPrefixMemoryMapFlags
      showTableMemoryMapFlags
      conNameMemoryMapFlags
      (\(MemoryMapFlags x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read MemoryMapFlags where
  readPrec =
    enumReadPrec
      enumPrefixMemoryMapFlags
      showTableMemoryMapFlags
      conNameMemoryMapFlags
      MemoryMapFlags
