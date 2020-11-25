{-# language CPP #-}
-- No documentation found for Chapter "CommandPoolTrimFlags"
module Vulkan.Core11.Enums.CommandPoolTrimFlags  (CommandPoolTrimFlags(..)) where

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
-- | VkCommandPoolTrimFlags - Reserved for future use
--
-- = Description
--
-- 'CommandPoolTrimFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance1.trimCommandPool',
-- 'Vulkan.Extensions.VK_KHR_maintenance1.trimCommandPoolKHR'
newtype CommandPoolTrimFlags = CommandPoolTrimFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameCommandPoolTrimFlags :: String
conNameCommandPoolTrimFlags = "CommandPoolTrimFlags"

enumPrefixCommandPoolTrimFlags :: String
enumPrefixCommandPoolTrimFlags = ""

showTableCommandPoolTrimFlags :: [(CommandPoolTrimFlags, String)]
showTableCommandPoolTrimFlags = []

instance Show CommandPoolTrimFlags where
  showsPrec = enumShowsPrec enumPrefixCommandPoolTrimFlags
                            showTableCommandPoolTrimFlags
                            conNameCommandPoolTrimFlags
                            (\(CommandPoolTrimFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read CommandPoolTrimFlags where
  readPrec = enumReadPrec enumPrefixCommandPoolTrimFlags
                          showTableCommandPoolTrimFlags
                          conNameCommandPoolTrimFlags
                          CommandPoolTrimFlags

