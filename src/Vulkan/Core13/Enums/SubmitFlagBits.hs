{-# language CPP #-}
-- No documentation found for Chapter "SubmitFlagBits"
module Vulkan.Core13.Enums.SubmitFlagBits  ( pattern SUBMIT_PROTECTED_BIT_KHR
                                           , SubmitFlags
                                           , SubmitFlagBits( SUBMIT_PROTECTED_BIT
                                                           , ..
                                                           )
                                           ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Bits (Bits)
import GHC.Bits (FiniteBits)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
-- No documentation found for TopLevel "VK_SUBMIT_PROTECTED_BIT_KHR"
pattern SUBMIT_PROTECTED_BIT_KHR = SUBMIT_PROTECTED_BIT


type SubmitFlags = SubmitFlagBits

-- | VkSubmitFlagBits - Bitmask specifying behavior of a submission
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'SubmitFlags'
newtype SubmitFlagBits = SubmitFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'SUBMIT_PROTECTED_BIT' specifies that this batch is a protected
-- submission.
pattern SUBMIT_PROTECTED_BIT = SubmitFlagBits 0x00000001

conNameSubmitFlagBits :: String
conNameSubmitFlagBits = "SubmitFlagBits"

enumPrefixSubmitFlagBits :: String
enumPrefixSubmitFlagBits = "SUBMIT_PROTECTED_BIT"

showTableSubmitFlagBits :: [(SubmitFlagBits, String)]
showTableSubmitFlagBits = [(SUBMIT_PROTECTED_BIT, "")]

instance Show SubmitFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixSubmitFlagBits
      showTableSubmitFlagBits
      conNameSubmitFlagBits
      (\(SubmitFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read SubmitFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixSubmitFlagBits
      showTableSubmitFlagBits
      conNameSubmitFlagBits
      SubmitFlagBits
