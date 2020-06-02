{-# language CPP #-}
module Vulkan.Core12.Enums.SemaphoreWaitFlagBits  ( SemaphoreWaitFlagBits( SEMAPHORE_WAIT_ANY_BIT
                                                                         , ..
                                                                         )
                                                  , SemaphoreWaitFlags
                                                  ) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
-- | VkSemaphoreWaitFlagBits - Bitmask specifying additional parameters of a
-- semaphore wait operation
--
-- = See Also
--
-- 'SemaphoreWaitFlags'
newtype SemaphoreWaitFlagBits = SemaphoreWaitFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

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

type SemaphoreWaitFlags = SemaphoreWaitFlagBits

instance Show SemaphoreWaitFlagBits where
  showsPrec p = \case
    SEMAPHORE_WAIT_ANY_BIT -> showString "SEMAPHORE_WAIT_ANY_BIT"
    SemaphoreWaitFlagBits x -> showParen (p >= 11) (showString "SemaphoreWaitFlagBits 0x" . showHex x)

instance Read SemaphoreWaitFlagBits where
  readPrec = parens (choose [("SEMAPHORE_WAIT_ANY_BIT", pure SEMAPHORE_WAIT_ANY_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "SemaphoreWaitFlagBits")
                       v <- step readPrec
                       pure (SemaphoreWaitFlagBits v)))

