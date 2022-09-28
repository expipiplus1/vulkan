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
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type SemaphoreWaitFlags = SemaphoreWaitFlagBits

-- | VkSemaphoreWaitFlagBits - Bitmask specifying additional parameters of a
-- semaphore wait operation
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_timeline_semaphore VK_KHR_timeline_semaphore>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'SemaphoreWaitFlags'
newtype SemaphoreWaitFlagBits = SemaphoreWaitFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'SEMAPHORE_WAIT_ANY_BIT' specifies that the semaphore wait condition is
-- that at least one of the semaphores in
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreWaitInfo'::@pSemaphores@
-- has reached the value specified by the corresponding element of
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreWaitInfo'::@pValues@.
-- If 'SEMAPHORE_WAIT_ANY_BIT' is not set, the semaphore wait condition is
-- that all of the semaphores in
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreWaitInfo'::@pSemaphores@
-- have reached the value specified by the corresponding element of
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreWaitInfo'::@pValues@.
pattern SEMAPHORE_WAIT_ANY_BIT = SemaphoreWaitFlagBits 0x00000001

conNameSemaphoreWaitFlagBits :: String
conNameSemaphoreWaitFlagBits = "SemaphoreWaitFlagBits"

enumPrefixSemaphoreWaitFlagBits :: String
enumPrefixSemaphoreWaitFlagBits = "SEMAPHORE_WAIT_ANY_BIT"

showTableSemaphoreWaitFlagBits :: [(SemaphoreWaitFlagBits, String)]
showTableSemaphoreWaitFlagBits = [(SEMAPHORE_WAIT_ANY_BIT, "")]

instance Show SemaphoreWaitFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixSemaphoreWaitFlagBits
      showTableSemaphoreWaitFlagBits
      conNameSemaphoreWaitFlagBits
      (\(SemaphoreWaitFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read SemaphoreWaitFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixSemaphoreWaitFlagBits
      showTableSemaphoreWaitFlagBits
      conNameSemaphoreWaitFlagBits
      SemaphoreWaitFlagBits
