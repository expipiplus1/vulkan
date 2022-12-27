{-# language CPP #-}
-- No documentation found for Chapter "PrivateDataSlotCreateFlags"
module Vulkan.Core13.Enums.PrivateDataSlotCreateFlags  (PrivateDataSlotCreateFlags(..)) where

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
-- | VkPrivateDataSlotCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PrivateDataSlotCreateFlags' is a bitmask type for setting a mask, but
-- is currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_private_data VK_EXT_private_data>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core13.Promoted_From_VK_EXT_private_data.PrivateDataSlotCreateInfo'
newtype PrivateDataSlotCreateFlags = PrivateDataSlotCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNamePrivateDataSlotCreateFlags :: String
conNamePrivateDataSlotCreateFlags = "PrivateDataSlotCreateFlags"

enumPrefixPrivateDataSlotCreateFlags :: String
enumPrefixPrivateDataSlotCreateFlags = ""

showTablePrivateDataSlotCreateFlags :: [(PrivateDataSlotCreateFlags, String)]
showTablePrivateDataSlotCreateFlags = []

instance Show PrivateDataSlotCreateFlags where
  showsPrec =
    enumShowsPrec
      enumPrefixPrivateDataSlotCreateFlags
      showTablePrivateDataSlotCreateFlags
      conNamePrivateDataSlotCreateFlags
      (\(PrivateDataSlotCreateFlags x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PrivateDataSlotCreateFlags where
  readPrec =
    enumReadPrec
      enumPrefixPrivateDataSlotCreateFlags
      showTablePrivateDataSlotCreateFlags
      conNamePrivateDataSlotCreateFlags
      PrivateDataSlotCreateFlags
