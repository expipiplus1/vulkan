{-# language CPP #-}
-- No documentation found for Chapter "ImageViewCreateFlagBits"
module Vulkan.Core10.Enums.ImageViewCreateFlagBits  ( ImageViewCreateFlags
                                                    , ImageViewCreateFlagBits( IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT
                                                                             , IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT
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
type ImageViewCreateFlags = ImageViewCreateFlagBits

-- | VkImageViewCreateFlagBits - Bitmask specifying additional parameters of
-- an image view
--
-- = See Also
--
-- 'ImageViewCreateFlags'
newtype ImageViewCreateFlagBits = ImageViewCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT' specifies that
-- the fragment density map will be read by the host during
-- 'Vulkan.Core10.CommandBuffer.endCommandBuffer' for the primary command
-- buffer that the render pass is recorded into
pattern IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT = ImageViewCreateFlagBits 0x00000002
-- | 'IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT' specifies that
-- the fragment density map will be read by device during
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
pattern IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT  = ImageViewCreateFlagBits 0x00000001

conNameImageViewCreateFlagBits :: String
conNameImageViewCreateFlagBits = "ImageViewCreateFlagBits"

enumPrefixImageViewCreateFlagBits :: String
enumPrefixImageViewCreateFlagBits = "IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_D"

showTableImageViewCreateFlagBits :: [(ImageViewCreateFlagBits, String)]
showTableImageViewCreateFlagBits =
  [ (IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT, "EFERRED_BIT_EXT")
  , (IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT , "YNAMIC_BIT_EXT")
  ]

instance Show ImageViewCreateFlagBits where
  showsPrec p e = case lookup e showTableImageViewCreateFlagBits of
    Just s -> showString enumPrefixImageViewCreateFlagBits . showString s
    Nothing ->
      let ImageViewCreateFlagBits x = e
      in  showParen (p >= 11) (showString conNameImageViewCreateFlagBits . showString " 0x" . showHex x)

instance Read ImageViewCreateFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixImageViewCreateFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableImageViewCreateFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameImageViewCreateFlagBits)
            v <- step readPrec
            pure (ImageViewCreateFlagBits v)
          )
    )

