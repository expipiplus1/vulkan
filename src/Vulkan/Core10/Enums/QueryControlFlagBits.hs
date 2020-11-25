{-# language CPP #-}
-- No documentation found for Chapter "QueryControlFlagBits"
module Vulkan.Core10.Enums.QueryControlFlagBits  ( QueryControlFlags
                                                 , QueryControlFlagBits( QUERY_CONTROL_PRECISE_BIT
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
type QueryControlFlags = QueryControlFlagBits

-- No documentation found for TopLevel "VkQueryControlFlagBits"
newtype QueryControlFlagBits = QueryControlFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkQueryControlFlagBits" "VK_QUERY_CONTROL_PRECISE_BIT"
pattern QUERY_CONTROL_PRECISE_BIT = QueryControlFlagBits 0x00000001

conNameQueryControlFlagBits :: String
conNameQueryControlFlagBits = "QueryControlFlagBits"

enumPrefixQueryControlFlagBits :: String
enumPrefixQueryControlFlagBits = "QUERY_CONTROL_PRECISE_BIT"

showTableQueryControlFlagBits :: [(QueryControlFlagBits, String)]
showTableQueryControlFlagBits = [(QUERY_CONTROL_PRECISE_BIT, "")]


instance Show QueryControlFlagBits where
showsPrec = enumShowsPrec enumPrefixQueryControlFlagBits
                          showTableQueryControlFlagBits
                          conNameQueryControlFlagBits
                          (\(QueryControlFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read QueryControlFlagBits where
  readPrec = enumReadPrec enumPrefixQueryControlFlagBits
                          showTableQueryControlFlagBits
                          conNameQueryControlFlagBits
                          QueryControlFlagBits

