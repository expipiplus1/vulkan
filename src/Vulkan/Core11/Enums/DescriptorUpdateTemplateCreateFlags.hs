{-# language CPP #-}
-- No documentation found for Chapter "DescriptorUpdateTemplateCreateFlags"
module Vulkan.Core11.Enums.DescriptorUpdateTemplateCreateFlags  (DescriptorUpdateTemplateCreateFlags(..)) where

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
-- No documentation found for TopLevel "VkDescriptorUpdateTemplateCreateFlags"
newtype DescriptorUpdateTemplateCreateFlags = DescriptorUpdateTemplateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameDescriptorUpdateTemplateCreateFlags :: String
conNameDescriptorUpdateTemplateCreateFlags = "DescriptorUpdateTemplateCreateFlags"

enumPrefixDescriptorUpdateTemplateCreateFlags :: String
enumPrefixDescriptorUpdateTemplateCreateFlags = ""

showTableDescriptorUpdateTemplateCreateFlags :: [(DescriptorUpdateTemplateCreateFlags, String)]
showTableDescriptorUpdateTemplateCreateFlags = []


instance Show DescriptorUpdateTemplateCreateFlags where
showsPrec = enumShowsPrec enumPrefixDescriptorUpdateTemplateCreateFlags
                          showTableDescriptorUpdateTemplateCreateFlags
                          conNameDescriptorUpdateTemplateCreateFlags
                          (\(DescriptorUpdateTemplateCreateFlags x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read DescriptorUpdateTemplateCreateFlags where
  readPrec = enumReadPrec enumPrefixDescriptorUpdateTemplateCreateFlags
                          showTableDescriptorUpdateTemplateCreateFlags
                          conNameDescriptorUpdateTemplateCreateFlags
                          DescriptorUpdateTemplateCreateFlags

