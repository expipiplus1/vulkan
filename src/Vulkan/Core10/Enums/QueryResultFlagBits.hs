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
type QueryResultFlags = QueryResultFlagBits

-- | VkQueryResultFlagBits - Bitmask specifying how and when query results
-- are returned
--
-- = See Also
--
-- 'QueryResultFlags'
newtype QueryResultFlagBits = QueryResultFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'QUERY_RESULT_64_BIT' specifies the results will be written as an array
-- of 64-bit unsigned integer values. If this bit is not set, the results
-- will be written as an array of 32-bit unsigned integer values.
pattern QUERY_RESULT_64_BIT                = QueryResultFlagBits 0x00000001
-- | 'QUERY_RESULT_WAIT_BIT' specifies that Vulkan will wait for each query’s
-- status to become available before retrieving its results.
pattern QUERY_RESULT_WAIT_BIT              = QueryResultFlagBits 0x00000002
-- | 'QUERY_RESULT_WITH_AVAILABILITY_BIT' specifies that the availability
-- status accompanies the results.
pattern QUERY_RESULT_WITH_AVAILABILITY_BIT = QueryResultFlagBits 0x00000004
-- | 'QUERY_RESULT_PARTIAL_BIT' specifies that returning partial results is
-- acceptable.
pattern QUERY_RESULT_PARTIAL_BIT           = QueryResultFlagBits 0x00000008

conNameQueryResultFlagBits :: String
conNameQueryResultFlagBits = "QueryResultFlagBits"

enumPrefixQueryResultFlagBits :: String
enumPrefixQueryResultFlagBits = "QUERY_RESULT_"

showTableQueryResultFlagBits :: [(QueryResultFlagBits, String)]
showTableQueryResultFlagBits =
  [ (QUERY_RESULT_64_BIT               , "64_BIT")
  , (QUERY_RESULT_WAIT_BIT             , "WAIT_BIT")
  , (QUERY_RESULT_WITH_AVAILABILITY_BIT, "WITH_AVAILABILITY_BIT")
  , (QUERY_RESULT_PARTIAL_BIT          , "PARTIAL_BIT")
  ]

instance Show QueryResultFlagBits where
  showsPrec = enumShowsPrec enumPrefixQueryResultFlagBits
                            showTableQueryResultFlagBits
                            conNameQueryResultFlagBits
                            (\(QueryResultFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read QueryResultFlagBits where
  readPrec = enumReadPrec enumPrefixQueryResultFlagBits
                          showTableQueryResultFlagBits
                          conNameQueryResultFlagBits
                          QueryResultFlagBits

