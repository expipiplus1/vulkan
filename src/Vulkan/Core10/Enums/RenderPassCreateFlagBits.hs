{-# language CPP #-}
module Vulkan.Core10.Enums.RenderPassCreateFlagBits  ( RenderPassCreateFlagBits( RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM
                                                                               , ..
                                                                               )
                                                     , RenderPassCreateFlags
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
-- | VkRenderPassCreateFlagBits - Bitmask specifying additional properties of
-- a renderpass
--
-- = See Also
--
-- 'RenderPassCreateFlags'
newtype RenderPassCreateFlagBits = RenderPassCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM' specifies that the created
-- renderpass is compatible with
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vertexpostproc-renderpass-transform render pass transform>.
pattern RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM = RenderPassCreateFlagBits 0x00000002

type RenderPassCreateFlags = RenderPassCreateFlagBits

instance Show RenderPassCreateFlagBits where
  showsPrec p = \case
    RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM -> showString "RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM"
    RenderPassCreateFlagBits x -> showParen (p >= 11) (showString "RenderPassCreateFlagBits 0x" . showHex x)

instance Read RenderPassCreateFlagBits where
  readPrec = parens (choose [("RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM", pure RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM)]
                     +++
                     prec 10 (do
                       expectP (Ident "RenderPassCreateFlagBits")
                       v <- step readPrec
                       pure (RenderPassCreateFlagBits v)))

