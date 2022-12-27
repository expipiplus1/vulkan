{-# language CPP #-}
-- No documentation found for Chapter "BufferViewCreateFlags"
module Vulkan.Core10.Enums.BufferViewCreateFlags  (BufferViewCreateFlags(..)) where

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
-- | VkBufferViewCreateFlags - Reserved for future use
--
-- = Description
--
-- 'BufferViewCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.BufferView.BufferViewCreateInfo'
newtype BufferViewCreateFlags = BufferViewCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNameBufferViewCreateFlags :: String
conNameBufferViewCreateFlags = "BufferViewCreateFlags"

enumPrefixBufferViewCreateFlags :: String
enumPrefixBufferViewCreateFlags = ""

showTableBufferViewCreateFlags :: [(BufferViewCreateFlags, String)]
showTableBufferViewCreateFlags = []

instance Show BufferViewCreateFlags where
  showsPrec =
    enumShowsPrec
      enumPrefixBufferViewCreateFlags
      showTableBufferViewCreateFlags
      conNameBufferViewCreateFlags
      (\(BufferViewCreateFlags x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read BufferViewCreateFlags where
  readPrec =
    enumReadPrec
      enumPrefixBufferViewCreateFlags
      showTableBufferViewCreateFlags
      conNameBufferViewCreateFlags
      BufferViewCreateFlags
