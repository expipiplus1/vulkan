{-# language CPP #-}
-- No documentation found for Chapter "SemaphoreImportFlagBits"
module Vulkan.Core11.Enums.SemaphoreImportFlagBits  ( SemaphoreImportFlags
                                                    , SemaphoreImportFlagBits( SEMAPHORE_IMPORT_TEMPORARY_BIT
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
type SemaphoreImportFlags = SemaphoreImportFlagBits

-- | VkSemaphoreImportFlagBits - Bitmask specifying additional parameters of
-- semaphore payload import
--
-- = Description
--
-- These bits have the following meanings:
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'SemaphoreImportFlags'
newtype SemaphoreImportFlagBits = SemaphoreImportFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'SEMAPHORE_IMPORT_TEMPORARY_BIT' specifies that the semaphore payload
-- will be imported only temporarily, as described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-semaphores-importing Importing Semaphore Payloads>,
-- regardless of the permanence of @handleType@.
pattern SEMAPHORE_IMPORT_TEMPORARY_BIT = SemaphoreImportFlagBits 0x00000001

conNameSemaphoreImportFlagBits :: String
conNameSemaphoreImportFlagBits = "SemaphoreImportFlagBits"

enumPrefixSemaphoreImportFlagBits :: String
enumPrefixSemaphoreImportFlagBits = "SEMAPHORE_IMPORT_TEMPORARY_BIT"

showTableSemaphoreImportFlagBits :: [(SemaphoreImportFlagBits, String)]
showTableSemaphoreImportFlagBits = [(SEMAPHORE_IMPORT_TEMPORARY_BIT, "")]

instance Show SemaphoreImportFlagBits where
  showsPrec = enumShowsPrec enumPrefixSemaphoreImportFlagBits
                            showTableSemaphoreImportFlagBits
                            conNameSemaphoreImportFlagBits
                            (\(SemaphoreImportFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read SemaphoreImportFlagBits where
  readPrec = enumReadPrec enumPrefixSemaphoreImportFlagBits
                          showTableSemaphoreImportFlagBits
                          conNameSemaphoreImportFlagBits
                          SemaphoreImportFlagBits

