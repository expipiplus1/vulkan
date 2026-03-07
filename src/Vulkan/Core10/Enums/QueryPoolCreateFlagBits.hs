{-# language CPP #-}
-- No documentation found for Chapter "QueryPoolCreateFlagBits"
module Vulkan.Core10.Enums.QueryPoolCreateFlagBits  ( QueryPoolCreateFlags
                                                    , QueryPoolCreateFlagBits( QUERY_POOL_CREATE_RESET_BIT_KHR
                                                                             , ..
                                                                             )
                                                    ) where

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
type QueryPoolCreateFlags = QueryPoolCreateFlagBits

-- | VkQueryPoolCreateFlagBits - Bitmask specifying query pool properties
--
-- = Description
--
-- -   'QUERY_POOL_CREATE_RESET_BIT_KHR' specifies that queries in the
--     query pool are initialized on creation and do not need to be reset
--     before first use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'QueryPoolCreateFlags'
newtype QueryPoolCreateFlagBits = QueryPoolCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkQueryPoolCreateFlagBits" "VK_QUERY_POOL_CREATE_RESET_BIT_KHR"
pattern QUERY_POOL_CREATE_RESET_BIT_KHR = QueryPoolCreateFlagBits 0x00000001

conNameQueryPoolCreateFlagBits :: String
conNameQueryPoolCreateFlagBits = "QueryPoolCreateFlagBits"

enumPrefixQueryPoolCreateFlagBits :: String
enumPrefixQueryPoolCreateFlagBits = "QUERY_POOL_CREATE_RESET_BIT_KHR"

showTableQueryPoolCreateFlagBits :: [(QueryPoolCreateFlagBits, String)]
showTableQueryPoolCreateFlagBits = [(QUERY_POOL_CREATE_RESET_BIT_KHR, "")]

instance Show QueryPoolCreateFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixQueryPoolCreateFlagBits
      showTableQueryPoolCreateFlagBits
      conNameQueryPoolCreateFlagBits
      (\(QueryPoolCreateFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read QueryPoolCreateFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixQueryPoolCreateFlagBits
      showTableQueryPoolCreateFlagBits
      conNameQueryPoolCreateFlagBits
      QueryPoolCreateFlagBits
