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
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type SemaphoreImportFlags = SemaphoreImportFlagBits

-- No documentation found for TopLevel "VkSemaphoreImportFlagBits"
newtype SemaphoreImportFlagBits = SemaphoreImportFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkSemaphoreImportFlagBits" "VK_SEMAPHORE_IMPORT_TEMPORARY_BIT"
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

