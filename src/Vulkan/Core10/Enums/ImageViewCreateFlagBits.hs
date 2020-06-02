{-# language CPP #-}
module Vulkan.Core10.Enums.ImageViewCreateFlagBits  ( ImageViewCreateFlagBits( IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT
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
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT' prohibits the
-- implementation from accessing the fragment density map by the host
-- during 'Vulkan.Core10.CommandBufferBuilding.cmdBeginRenderPass' as the
-- contents are expected to change after recording
pattern IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT = ImageViewCreateFlagBits 0x00000001

type ImageViewCreateFlags = ImageViewCreateFlagBits

instance Show ImageViewCreateFlagBits where
  showsPrec p = \case
    IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT -> showString "IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT"
    ImageViewCreateFlagBits x -> showParen (p >= 11) (showString "ImageViewCreateFlagBits 0x" . showHex x)

instance Read ImageViewCreateFlagBits where
  readPrec = parens (choose [("IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT", pure IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "ImageViewCreateFlagBits")
                       v <- step readPrec
                       pure (ImageViewCreateFlagBits v)))

