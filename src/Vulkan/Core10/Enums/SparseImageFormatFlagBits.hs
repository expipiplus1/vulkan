{-# language CPP #-}
-- No documentation found for Chapter "SparseImageFormatFlagBits"
module Vulkan.Core10.Enums.SparseImageFormatFlagBits  ( SparseImageFormatFlags
                                                      , SparseImageFormatFlagBits( SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT
                                                                                 , SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT
                                                                                 , SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT
                                                                                 , ..
                                                                                 )
                                                      ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type SparseImageFormatFlags = SparseImageFormatFlagBits

-- | VkSparseImageFormatFlagBits - Bitmask specifying additional information
-- about a sparse image resource
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'SparseImageFormatFlags'
newtype SparseImageFormatFlagBits = SparseImageFormatFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT' specifies that the image uses a
-- single mip tail region for all array layers.
pattern SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT = SparseImageFormatFlagBits 0x00000001

-- | 'SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT' specifies that the first mip
-- level whose dimensions are not integer multiples of the corresponding
-- dimensions of the sparse image block begins the mip tail region.
pattern SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT = SparseImageFormatFlagBits 0x00000002

-- | 'SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT' specifies that the
-- image uses non-standard sparse image block dimensions, and the
-- @imageGranularity@ values do not match the standard sparse image block
-- dimensions for the given format.
pattern SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT = SparseImageFormatFlagBits 0x00000004

conNameSparseImageFormatFlagBits :: String
conNameSparseImageFormatFlagBits = "SparseImageFormatFlagBits"

enumPrefixSparseImageFormatFlagBits :: String
enumPrefixSparseImageFormatFlagBits = "SPARSE_IMAGE_FORMAT_"

showTableSparseImageFormatFlagBits :: [(SparseImageFormatFlagBits, String)]
showTableSparseImageFormatFlagBits =
  [
    ( SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT
    , "SINGLE_MIPTAIL_BIT"
    )
  ,
    ( SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT
    , "ALIGNED_MIP_SIZE_BIT"
    )
  ,
    ( SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT
    , "NONSTANDARD_BLOCK_SIZE_BIT"
    )
  ]

instance Show SparseImageFormatFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixSparseImageFormatFlagBits
      showTableSparseImageFormatFlagBits
      conNameSparseImageFormatFlagBits
      (\(SparseImageFormatFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read SparseImageFormatFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixSparseImageFormatFlagBits
      showTableSparseImageFormatFlagBits
      conNameSparseImageFormatFlagBits
      SparseImageFormatFlagBits
