{-# language CPP #-}
-- No documentation found for Chapter "RenderPassCreateFlagBits"
module Vulkan.Core10.Enums.RenderPassCreateFlagBits  ( RenderPassCreateFlags
                                                     , RenderPassCreateFlagBits( RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM
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
type RenderPassCreateFlags = RenderPassCreateFlagBits

-- | VkRenderPassCreateFlagBits - Bitmask specifying additional properties of
-- a renderpass
--
-- = See Also
--
-- 'RenderPassCreateFlags'
newtype RenderPassCreateFlagBits = RenderPassCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM' specifies that the created
-- renderpass is compatible with
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vertexpostproc-renderpass-transform render pass transform>.
pattern RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM = RenderPassCreateFlagBits 0x00000002

conNameRenderPassCreateFlagBits :: String
conNameRenderPassCreateFlagBits = "RenderPassCreateFlagBits"

enumPrefixRenderPassCreateFlagBits :: String
enumPrefixRenderPassCreateFlagBits = "RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM"

showTableRenderPassCreateFlagBits :: [(RenderPassCreateFlagBits, String)]
showTableRenderPassCreateFlagBits = [(RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM, "")]

instance Show RenderPassCreateFlagBits where
  showsPrec p e = case lookup e showTableRenderPassCreateFlagBits of
    Just s -> showString enumPrefixRenderPassCreateFlagBits . showString s
    Nothing ->
      let RenderPassCreateFlagBits x = e
      in  showParen (p >= 11) (showString conNameRenderPassCreateFlagBits . showString " 0x" . showHex x)

instance Read RenderPassCreateFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixRenderPassCreateFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableRenderPassCreateFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameRenderPassCreateFlagBits)
            v <- step readPrec
            pure (RenderPassCreateFlagBits v)
          )
    )

