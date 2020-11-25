{-# language CPP #-}
-- No documentation found for Chapter "SparseImageFormatFlagBits"
module Vulkan.Core10.Enums.SparseImageFormatFlagBits  ( SparseImageFormatFlags
                                                      , SparseImageFormatFlagBits( SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT
                                                                                 , SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT
                                                                                 , SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT
                                                                                 , ..
                                                                                 )
                                                      ) where

import Data.Foldable (asum)
import GHC.Base ((<$))
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadP (string)
import Text.ParserCombinators.ReadPrec ((+++))
import qualified Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type SparseImageFormatFlags = SparseImageFormatFlagBits

-- | VkSparseImageFormatFlagBits - Bitmask specifying additional information
-- about a sparse image resource
--
-- = See Also
--
-- 'SparseImageFormatFlags'
newtype SparseImageFormatFlagBits = SparseImageFormatFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT' specifies that the image uses a
-- single mip tail region for all array layers.
pattern SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT         = SparseImageFormatFlagBits 0x00000001
-- | 'SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT' specifies that the first mip
-- level whose dimensions are not integer multiples of the corresponding
-- dimensions of the sparse image block begins the mip tail region.
pattern SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT       = SparseImageFormatFlagBits 0x00000002
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
  [ (SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT        , "SINGLE_MIPTAIL_BIT")
  , (SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT      , "ALIGNED_MIP_SIZE_BIT")
  , (SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT, "NONSTANDARD_BLOCK_SIZE_BIT")
  ]

instance Show SparseImageFormatFlagBits where
  showsPrec p e = case lookup e showTableSparseImageFormatFlagBits of
    Just s -> showString enumPrefixSparseImageFormatFlagBits . showString s
    Nothing ->
      let SparseImageFormatFlagBits x = e
      in  showParen (p >= 11) (showString conNameSparseImageFormatFlagBits . showString " 0x" . showHex x)

instance Read SparseImageFormatFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixSparseImageFormatFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableSparseImageFormatFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameSparseImageFormatFlagBits)
            v <- step readPrec
            pure (SparseImageFormatFlagBits v)
          )
    )

