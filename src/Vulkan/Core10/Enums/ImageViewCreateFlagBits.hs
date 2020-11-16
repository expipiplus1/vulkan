{-# language CPP #-}
module Vulkan.Core10.Enums.ImageViewCreateFlagBits  ( ImageViewCreateFlagBits( IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT
                                                                             , IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT
                                                                             , ..
                                                                             )
                                                    , ImageViewCreateFlags
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
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
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
pattern IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT = ImageViewCreateFlagBits 0x00000001

type ImageViewCreateFlags = ImageViewCreateFlagBits

instance Show ImageViewCreateFlagBits where
  showsPrec p = \case
    IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT -> showString "IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT"
    IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT -> showString "IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT"
    ImageViewCreateFlagBits x -> showParen (p >= 11) (showString "ImageViewCreateFlagBits 0x" . showHex x)

instance Read ImageViewCreateFlagBits where
  readPrec = parens (choose [("IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT", pure IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT)
                            , ("IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT", pure IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "ImageViewCreateFlagBits")
                       v <- step readPrec
                       pure (ImageViewCreateFlagBits v)))

