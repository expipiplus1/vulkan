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
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type FenceImportFlags = FenceImportFlagBits

-- | VkFenceImportFlagBits - Bitmask specifying additional parameters of
-- fence payload import
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'FenceImportFlags'
newtype FenceImportFlagBits = FenceImportFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'FENCE_IMPORT_TEMPORARY_BIT' specifies that the fence payload will be
-- imported only temporarily, as described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-fences-importing Importing Fence Payloads>,
-- regardless of the permanence of @handleType@.
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

