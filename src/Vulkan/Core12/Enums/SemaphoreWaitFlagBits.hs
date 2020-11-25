{-# language CPP #-}
-- No documentation found for Chapter "SemaphoreWaitFlagBits"
module Vulkan.Core12.Enums.SemaphoreWaitFlagBits  ( SemaphoreWaitFlags
                                                  , SemaphoreWaitFlagBits( SEMAPHORE_WAIT_ANY_BIT
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
type SemaphoreWaitFlags = SemaphoreWaitFlagBits

-- No documentation found for TopLevel "VkSemaphoreWaitFlagBits"
newtype SemaphoreWaitFlagBits = SemaphoreWaitFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkSemaphoreWaitFlagBits" "VK_SEMAPHORE_WAIT_ANY_BIT"
pattern SEMAPHORE_WAIT_ANY_BIT = SemaphoreWaitFlagBits 0x00000001

conNameSemaphoreWaitFlagBits :: String
conNameSemaphoreWaitFlagBits = "SemaphoreWaitFlagBits"

enumPrefixSemaphoreWaitFlagBits :: String
enumPrefixSemaphoreWaitFlagBits = "SEMAPHORE_WAIT_ANY_BIT"

showTableSemaphoreWaitFlagBits :: [(SemaphoreWaitFlagBits, String)]
showTableSemaphoreWaitFlagBits = [(SEMAPHORE_WAIT_ANY_BIT, "")]


instance Show SemaphoreWaitFlagBits where
showsPrec = enumShowsPrec enumPrefixSemaphoreWaitFlagBits
                          showTableSemaphoreWaitFlagBits
                          conNameSemaphoreWaitFlagBits
                          (\(SemaphoreWaitFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read SemaphoreWaitFlagBits where
  readPrec = enumReadPrec enumPrefixSemaphoreWaitFlagBits
                          showTableSemaphoreWaitFlagBits
                          conNameSemaphoreWaitFlagBits
                          SemaphoreWaitFlagBits

