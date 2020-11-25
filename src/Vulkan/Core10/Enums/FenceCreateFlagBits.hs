{-# language CPP #-}
-- No documentation found for Chapter "FenceCreateFlagBits"
module Vulkan.Core10.Enums.FenceCreateFlagBits  ( FenceCreateFlags
                                                , FenceCreateFlagBits( FENCE_CREATE_SIGNALED_BIT
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
type FenceCreateFlags = FenceCreateFlagBits

-- No documentation found for TopLevel "VkFenceCreateFlagBits"
newtype FenceCreateFlagBits = FenceCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkFenceCreateFlagBits" "VK_FENCE_CREATE_SIGNALED_BIT"
pattern FENCE_CREATE_SIGNALED_BIT = FenceCreateFlagBits 0x00000001

conNameFenceCreateFlagBits :: String
conNameFenceCreateFlagBits = "FenceCreateFlagBits"

enumPrefixFenceCreateFlagBits :: String
enumPrefixFenceCreateFlagBits = "FENCE_CREATE_SIGNALED_BIT"

showTableFenceCreateFlagBits :: [(FenceCreateFlagBits, String)]
showTableFenceCreateFlagBits = [(FENCE_CREATE_SIGNALED_BIT, "")]


instance Show FenceCreateFlagBits where
showsPrec = enumShowsPrec enumPrefixFenceCreateFlagBits
                          showTableFenceCreateFlagBits
                          conNameFenceCreateFlagBits
                          (\(FenceCreateFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read FenceCreateFlagBits where
  readPrec = enumReadPrec enumPrefixFenceCreateFlagBits
                          showTableFenceCreateFlagBits
                          conNameFenceCreateFlagBits
                          FenceCreateFlagBits

