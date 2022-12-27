{-# language CPP #-}
-- No documentation found for Chapter "DescriptorPoolResetFlags"
module Vulkan.Core10.Enums.DescriptorPoolResetFlags  (DescriptorPoolResetFlags(..)) where

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
-- | VkDescriptorPoolResetFlags - Reserved for future use
--
-- = Description
--
-- 'DescriptorPoolResetFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.DescriptorSet.resetDescriptorPool'
newtype DescriptorPoolResetFlags = DescriptorPoolResetFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNameDescriptorPoolResetFlags :: String
conNameDescriptorPoolResetFlags = "DescriptorPoolResetFlags"

enumPrefixDescriptorPoolResetFlags :: String
enumPrefixDescriptorPoolResetFlags = ""

showTableDescriptorPoolResetFlags :: [(DescriptorPoolResetFlags, String)]
showTableDescriptorPoolResetFlags = []

instance Show DescriptorPoolResetFlags where
  showsPrec =
    enumShowsPrec
      enumPrefixDescriptorPoolResetFlags
      showTableDescriptorPoolResetFlags
      conNameDescriptorPoolResetFlags
      (\(DescriptorPoolResetFlags x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read DescriptorPoolResetFlags where
  readPrec =
    enumReadPrec
      enumPrefixDescriptorPoolResetFlags
      showTableDescriptorPoolResetFlags
      conNameDescriptorPoolResetFlags
      DescriptorPoolResetFlags
