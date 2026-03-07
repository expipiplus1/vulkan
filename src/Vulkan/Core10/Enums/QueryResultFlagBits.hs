{-# language CPP #-}
-- No documentation found for Chapter "QueryResultFlagBits"
module Vulkan.Core10.Enums.QueryResultFlagBits  ( QueryResultFlags
                                                , QueryResultFlagBits( QUERY_RESULT_64_BIT
                                                                     , QUERY_RESULT_WAIT_BIT
                                                                     , QUERY_RESULT_WITH_AVAILABILITY_BIT
                                                                     , QUERY_RESULT_PARTIAL_BIT
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
type QueryResultFlags = QueryResultFlagBits

-- | VkQueryResultFlagBits - Bitmask specifying how and when query results
-- are returned
--
-- = Description
--
-- -   'QUERY_RESULT_64_BIT' specifies the results will be written as an
--     array of 64-bit unsigned integer values. If this bit is not set, the
--     results will be written as an array of 32-bit unsigned integer
--     values.
--
-- -   'QUERY_RESULT_WAIT_BIT' specifies that Vulkan will wait for each
--     query’s status to become available before retrieving its results.
--
-- -   'QUERY_RESULT_WITH_AVAILABILITY_BIT' specifies that the availability
--     status accompanies the results.
--
-- -   'QUERY_RESULT_PARTIAL_BIT' specifies that returning partial results
--     is acceptable.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueryResultFlagBits VK_QUERY_RESULT_WITH_STATUS_BIT_KHR>
--     specifies that the last value returned in the query is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueryResultStatusKHR VkQueryResultStatusKHR>
--     value. See
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#queries-result-status-only result status query>
--     for information on how an application can determine whether the use
--     of this flag bit is supported.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'QueryResultFlags'
newtype QueryResultFlagBits = QueryResultFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkQueryResultFlagBits" "VK_QUERY_RESULT_64_BIT"
pattern QUERY_RESULT_64_BIT = QueryResultFlagBits 0x00000001

-- No documentation found for Nested "VkQueryResultFlagBits" "VK_QUERY_RESULT_WAIT_BIT"
pattern QUERY_RESULT_WAIT_BIT = QueryResultFlagBits 0x00000002

-- No documentation found for Nested "VkQueryResultFlagBits" "VK_QUERY_RESULT_WITH_AVAILABILITY_BIT"
pattern QUERY_RESULT_WITH_AVAILABILITY_BIT = QueryResultFlagBits 0x00000004

-- No documentation found for Nested "VkQueryResultFlagBits" "VK_QUERY_RESULT_PARTIAL_BIT"
pattern QUERY_RESULT_PARTIAL_BIT = QueryResultFlagBits 0x00000008

conNameQueryResultFlagBits :: String
conNameQueryResultFlagBits = "QueryResultFlagBits"

enumPrefixQueryResultFlagBits :: String
enumPrefixQueryResultFlagBits = "QUERY_RESULT_"

showTableQueryResultFlagBits :: [(QueryResultFlagBits, String)]
showTableQueryResultFlagBits =
  [ (QUERY_RESULT_64_BIT, "64_BIT")
  , (QUERY_RESULT_WAIT_BIT, "WAIT_BIT")
  ,
    ( QUERY_RESULT_WITH_AVAILABILITY_BIT
    , "WITH_AVAILABILITY_BIT"
    )
  , (QUERY_RESULT_PARTIAL_BIT, "PARTIAL_BIT")
  ]

instance Show QueryResultFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixQueryResultFlagBits
      showTableQueryResultFlagBits
      conNameQueryResultFlagBits
      (\(QueryResultFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read QueryResultFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixQueryResultFlagBits
      showTableQueryResultFlagBits
      conNameQueryResultFlagBits
      QueryResultFlagBits
