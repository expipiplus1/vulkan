{-# language CPP #-}
-- No documentation found for Chapter "DescriptorPoolResetFlags"
module Vulkan.Core10.Enums.DescriptorPoolResetFlags  (DescriptorPoolResetFlags(..)) where

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
-- | VkDescriptorPoolResetFlags - Reserved for future use
--
-- = Description
--
-- 'DescriptorPoolResetFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
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
  showsPrec = enumShowsPrec enumPrefixDescriptorPoolResetFlags
                            showTableDescriptorPoolResetFlags
                            conNameDescriptorPoolResetFlags
                            (\(DescriptorPoolResetFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read DescriptorPoolResetFlags where
  readPrec = enumReadPrec enumPrefixDescriptorPoolResetFlags
                          showTableDescriptorPoolResetFlags
                          conNameDescriptorPoolResetFlags
                          DescriptorPoolResetFlags

