{-# language CPP #-}
-- No documentation found for Chapter "FenceImportFlagBits"
module Vulkan.Core11.Enums.FenceImportFlagBits  ( FenceImportFlags
                                                , FenceImportFlagBits( FENCE_IMPORT_TEMPORARY_BIT
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
type FenceImportFlags = FenceImportFlagBits

-- No documentation found for TopLevel "VkFenceImportFlagBits"
newtype FenceImportFlagBits = FenceImportFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkFenceImportFlagBits" "VK_FENCE_IMPORT_TEMPORARY_BIT"
pattern FENCE_IMPORT_TEMPORARY_BIT = FenceImportFlagBits 0x00000001

conNameFenceImportFlagBits :: String
conNameFenceImportFlagBits = "FenceImportFlagBits"

enumPrefixFenceImportFlagBits :: String
enumPrefixFenceImportFlagBits = "FENCE_IMPORT_TEMPORARY_BIT"

showTableFenceImportFlagBits :: [(FenceImportFlagBits, String)]
showTableFenceImportFlagBits = [(FENCE_IMPORT_TEMPORARY_BIT, "")]


instance Show FenceImportFlagBits where
showsPrec = enumShowsPrec enumPrefixFenceImportFlagBits
                          showTableFenceImportFlagBits
                          conNameFenceImportFlagBits
                          (\(FenceImportFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read FenceImportFlagBits where
  readPrec = enumReadPrec enumPrefixFenceImportFlagBits
                          showTableFenceImportFlagBits
                          conNameFenceImportFlagBits
                          FenceImportFlagBits

