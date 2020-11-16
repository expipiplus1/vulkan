{-# language CPP #-}
module Vulkan.Core10.Enums.FramebufferCreateFlagBits  ( FramebufferCreateFlagBits( FRAMEBUFFER_CREATE_IMAGELESS_BIT
                                                                                 , ..
                                                                                 )
                                                      , FramebufferCreateFlags
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
-- | VkFramebufferCreateFlagBits - Bitmask specifying framebuffer properties
--
-- = See Also
--
-- 'FramebufferCreateFlags'
newtype FramebufferCreateFlagBits = FramebufferCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'FRAMEBUFFER_CREATE_IMAGELESS_BIT' specifies that image views are not
-- specified, and only attachment compatibility information will be
-- provided via a
-- 'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentImageInfo'
-- structure.
pattern FRAMEBUFFER_CREATE_IMAGELESS_BIT = FramebufferCreateFlagBits 0x00000001

type FramebufferCreateFlags = FramebufferCreateFlagBits

instance Show FramebufferCreateFlagBits where
  showsPrec p = \case
    FRAMEBUFFER_CREATE_IMAGELESS_BIT -> showString "FRAMEBUFFER_CREATE_IMAGELESS_BIT"
    FramebufferCreateFlagBits x -> showParen (p >= 11) (showString "FramebufferCreateFlagBits 0x" . showHex x)

instance Read FramebufferCreateFlagBits where
  readPrec = parens (choose [("FRAMEBUFFER_CREATE_IMAGELESS_BIT", pure FRAMEBUFFER_CREATE_IMAGELESS_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "FramebufferCreateFlagBits")
                       v <- step readPrec
                       pure (FramebufferCreateFlagBits v)))

