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
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type QueryResultFlags = QueryResultFlagBits

-- No documentation found for TopLevel "VkQueryResultFlagBits"
newtype QueryResultFlagBits = QueryResultFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkQueryResultFlagBits" "VK_QUERY_RESULT_64_BIT"
pattern QUERY_RESULT_64_BIT                = QueryResultFlagBits 0x00000001
-- No documentation found for Nested "VkQueryResultFlagBits" "VK_QUERY_RESULT_WAIT_BIT"
pattern QUERY_RESULT_WAIT_BIT              = QueryResultFlagBits 0x00000002
-- No documentation found for Nested "VkQueryResultFlagBits" "VK_QUERY_RESULT_WITH_AVAILABILITY_BIT"
pattern QUERY_RESULT_WITH_AVAILABILITY_BIT = QueryResultFlagBits 0x00000004
-- No documentation found for Nested "VkQueryResultFlagBits" "VK_QUERY_RESULT_PARTIAL_BIT"
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

