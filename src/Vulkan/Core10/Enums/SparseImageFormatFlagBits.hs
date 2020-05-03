{-# language CPP #-}
module Vulkan.Core10.Enums.SparseImageFormatFlagBits  ( SparseImageFormatFlagBits( SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT
                                                                                 , SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT
                                                                                 , SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT
                                                                                 , ..
                                                                                 )
                                                      , SparseImageFormatFlags
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
import Vulkan.Core10.BaseType (Flags)
import Vulkan.Zero (Zero)
-- | VkSparseImageFormatFlagBits - Bitmask specifying additional information
-- about a sparse image resource
--
-- = See Also
--
-- 'SparseImageFormatFlags'
newtype SparseImageFormatFlagBits = SparseImageFormatFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

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

type SparseImageFormatFlags = SparseImageFormatFlagBits

instance Show SparseImageFormatFlagBits where
  showsPrec p = \case
    SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT -> showString "SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT"
    SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT -> showString "SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT"
    SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT -> showString "SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT"
    SparseImageFormatFlagBits x -> showParen (p >= 11) (showString "SparseImageFormatFlagBits 0x" . showHex x)

instance Read SparseImageFormatFlagBits where
  readPrec = parens (choose [("SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT", pure SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT)
                            , ("SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT", pure SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT)
                            , ("SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT", pure SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "SparseImageFormatFlagBits")
                       v <- step readPrec
                       pure (SparseImageFormatFlagBits v)))

