{-# language CPP #-}
-- No documentation found for Chapter "QueryPoolCreateFlags"
module Vulkan.Core10.Enums.QueryPoolCreateFlags  (QueryPoolCreateFlags(..)) where

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
-- | VkQueryPoolCreateFlags - Reserved for future use
--
-- = Description
--
-- 'QueryPoolCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Query.QueryPoolCreateInfo'
newtype QueryPoolCreateFlags = QueryPoolCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNameQueryPoolCreateFlags :: String
conNameQueryPoolCreateFlags = "QueryPoolCreateFlags"

enumPrefixQueryPoolCreateFlags :: String
enumPrefixQueryPoolCreateFlags = ""

showTableQueryPoolCreateFlags :: [(QueryPoolCreateFlags, String)]
showTableQueryPoolCreateFlags = []

instance Show QueryPoolCreateFlags where
  showsPrec =
    enumShowsPrec
      enumPrefixQueryPoolCreateFlags
      showTableQueryPoolCreateFlags
      conNameQueryPoolCreateFlags
      (\(QueryPoolCreateFlags x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read QueryPoolCreateFlags where
  readPrec =
    enumReadPrec
      enumPrefixQueryPoolCreateFlags
      showTableQueryPoolCreateFlags
      conNameQueryPoolCreateFlags
      QueryPoolCreateFlags
