{-# language CPP #-}
-- No documentation found for Chapter "QueryPoolCreateFlags"
module Vulkan.Core10.Enums.QueryPoolCreateFlags  (QueryPoolCreateFlags(..)) where

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
-- No documentation found for TopLevel "VkQueryPoolCreateFlags"
newtype QueryPoolCreateFlags = QueryPoolCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameQueryPoolCreateFlags :: String
conNameQueryPoolCreateFlags = "QueryPoolCreateFlags"

enumPrefixQueryPoolCreateFlags :: String
enumPrefixQueryPoolCreateFlags = ""

showTableQueryPoolCreateFlags :: [(QueryPoolCreateFlags, String)]
showTableQueryPoolCreateFlags = []


instance Show QueryPoolCreateFlags where
showsPrec = enumShowsPrec enumPrefixQueryPoolCreateFlags
                          showTableQueryPoolCreateFlags
                          conNameQueryPoolCreateFlags
                          (\(QueryPoolCreateFlags x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read QueryPoolCreateFlags where
  readPrec = enumReadPrec enumPrefixQueryPoolCreateFlags
                          showTableQueryPoolCreateFlags
                          conNameQueryPoolCreateFlags
                          QueryPoolCreateFlags

