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
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type FenceCreateFlags = FenceCreateFlagBits

-- | VkFenceCreateFlagBits - Bitmask specifying initial state and behavior of
-- a fence
--
-- = See Also
--
-- 'FenceCreateFlags'
newtype FenceCreateFlagBits = FenceCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'FENCE_CREATE_SIGNALED_BIT' specifies that the fence object is created
-- in the signaled state. Otherwise, it is created in the unsignaled state.
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

