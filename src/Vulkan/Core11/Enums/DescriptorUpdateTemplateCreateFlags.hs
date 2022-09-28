{-# language CPP #-}
-- No documentation found for Chapter "DescriptorUpdateTemplateCreateFlags"
module Vulkan.Core11.Enums.DescriptorUpdateTemplateCreateFlags  (DescriptorUpdateTemplateCreateFlags(..)) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
-- | VkDescriptorUpdateTemplateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'DescriptorUpdateTemplateCreateFlags' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.DescriptorUpdateTemplateCreateInfo'
newtype DescriptorUpdateTemplateCreateFlags = DescriptorUpdateTemplateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNameDescriptorUpdateTemplateCreateFlags :: String
conNameDescriptorUpdateTemplateCreateFlags = "DescriptorUpdateTemplateCreateFlags"

enumPrefixDescriptorUpdateTemplateCreateFlags :: String
enumPrefixDescriptorUpdateTemplateCreateFlags = ""

showTableDescriptorUpdateTemplateCreateFlags :: [(DescriptorUpdateTemplateCreateFlags, String)]
showTableDescriptorUpdateTemplateCreateFlags = []

instance Show DescriptorUpdateTemplateCreateFlags where
  showsPrec =
    enumShowsPrec
      enumPrefixDescriptorUpdateTemplateCreateFlags
      showTableDescriptorUpdateTemplateCreateFlags
      conNameDescriptorUpdateTemplateCreateFlags
      (\(DescriptorUpdateTemplateCreateFlags x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read DescriptorUpdateTemplateCreateFlags where
  readPrec =
    enumReadPrec
      enumPrefixDescriptorUpdateTemplateCreateFlags
      showTableDescriptorUpdateTemplateCreateFlags
      conNameDescriptorUpdateTemplateCreateFlags
      DescriptorUpdateTemplateCreateFlags
