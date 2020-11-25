{-# language CPP #-}
-- No documentation found for Chapter "SparseImageFormatFlagBits"
module Vulkan.Core10.Enums.SparseImageFormatFlagBits  ( SparseImageFormatFlags
                                                      , SparseImageFormatFlagBits( SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT
                                                                                 , SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT
                                                                                 , SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT
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
type SparseImageFormatFlags = SparseImageFormatFlagBits

-- No documentation found for TopLevel "VkSparseImageFormatFlagBits"
newtype SparseImageFormatFlagBits = SparseImageFormatFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkSparseImageFormatFlagBits" "VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT"
pattern SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT         = SparseImageFormatFlagBits 0x00000001
-- No documentation found for Nested "VkSparseImageFormatFlagBits" "VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT"
pattern SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT       = SparseImageFormatFlagBits 0x00000002
-- No documentation found for Nested "VkSparseImageFormatFlagBits" "VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT"
pattern SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT = SparseImageFormatFlagBits 0x00000004

conNameSparseImageFormatFlagBits :: String
conNameSparseImageFormatFlagBits = "SparseImageFormatFlagBits"

enumPrefixSparseImageFormatFlagBits :: String
enumPrefixSparseImageFormatFlagBits = "SPARSE_IMAGE_FORMAT_"

showTableSparseImageFormatFlagBits :: [(SparseImageFormatFlagBits, String)]
showTableSparseImageFormatFlagBits =
  [ (SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT        , "SINGLE_MIPTAIL_BIT")
  , (SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT      , "ALIGNED_MIP_SIZE_BIT")
  , (SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT, "NONSTANDARD_BLOCK_SIZE_BIT")
  ]


instance Show SparseImageFormatFlagBits where
showsPrec = enumShowsPrec enumPrefixSparseImageFormatFlagBits
                          showTableSparseImageFormatFlagBits
                          conNameSparseImageFormatFlagBits
                          (\(SparseImageFormatFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read SparseImageFormatFlagBits where
  readPrec = enumReadPrec enumPrefixSparseImageFormatFlagBits
                          showTableSparseImageFormatFlagBits
                          conNameSparseImageFormatFlagBits
                          SparseImageFormatFlagBits

