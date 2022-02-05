{-# language CPP #-}
-- No documentation found for Chapter "PrivateDataSlotCreateFlagBits"
module Vulkan.Core13.Enums.PrivateDataSlotCreateFlagBits  ( PrivateDataSlotCreateFlags
                                                          , PrivateDataSlotCreateFlagBits(..)
                                                          ) where

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
type PrivateDataSlotCreateFlags = PrivateDataSlotCreateFlagBits

-- | VkPrivateDataSlotCreateFlagBits - Bitmask specifying additional
-- parameters for private data slot creation
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_private_data VK_EXT_private_data>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'PrivateDataSlotCreateFlags'
newtype PrivateDataSlotCreateFlagBits = PrivateDataSlotCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePrivateDataSlotCreateFlagBits :: String
conNamePrivateDataSlotCreateFlagBits = "PrivateDataSlotCreateFlagBits"

enumPrefixPrivateDataSlotCreateFlagBits :: String
enumPrefixPrivateDataSlotCreateFlagBits = ""

showTablePrivateDataSlotCreateFlagBits :: [(PrivateDataSlotCreateFlagBits, String)]
showTablePrivateDataSlotCreateFlagBits = []

instance Show PrivateDataSlotCreateFlagBits where
  showsPrec = enumShowsPrec enumPrefixPrivateDataSlotCreateFlagBits
                            showTablePrivateDataSlotCreateFlagBits
                            conNamePrivateDataSlotCreateFlagBits
                            (\(PrivateDataSlotCreateFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read PrivateDataSlotCreateFlagBits where
  readPrec = enumReadPrec enumPrefixPrivateDataSlotCreateFlagBits
                          showTablePrivateDataSlotCreateFlagBits
                          conNamePrivateDataSlotCreateFlagBits
                          PrivateDataSlotCreateFlagBits

