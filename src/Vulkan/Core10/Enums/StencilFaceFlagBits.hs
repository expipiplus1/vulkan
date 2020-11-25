{-# language CPP #-}
-- No documentation found for Chapter "StencilFaceFlagBits"
module Vulkan.Core10.Enums.StencilFaceFlagBits  ( pattern STENCIL_FRONT_AND_BACK
                                                , StencilFaceFlags
                                                , StencilFaceFlagBits( STENCIL_FACE_FRONT_BIT
                                                                     , STENCIL_FACE_BACK_BIT
                                                                     , STENCIL_FACE_FRONT_AND_BACK
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
-- No documentation found for TopLevel "VK_STENCIL_FRONT_AND_BACK"
pattern STENCIL_FRONT_AND_BACK = STENCIL_FACE_FRONT_AND_BACK


type StencilFaceFlags = StencilFaceFlagBits

-- | VkStencilFaceFlagBits - Bitmask specifying sets of stencil state for
-- which to update the compare mask
--
-- = See Also
--
-- 'StencilFaceFlags'
newtype StencilFaceFlagBits = StencilFaceFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'STENCIL_FACE_FRONT_BIT' specifies that only the front set of stencil
-- state is updated.
pattern STENCIL_FACE_FRONT_BIT      = StencilFaceFlagBits 0x00000001
-- | 'STENCIL_FACE_BACK_BIT' specifies that only the back set of stencil
-- state is updated.
pattern STENCIL_FACE_BACK_BIT       = StencilFaceFlagBits 0x00000002
-- | 'STENCIL_FACE_FRONT_AND_BACK' is the combination of
-- 'STENCIL_FACE_FRONT_BIT' and 'STENCIL_FACE_BACK_BIT', and specifies that
-- both sets of stencil state are updated.
pattern STENCIL_FACE_FRONT_AND_BACK = StencilFaceFlagBits 0x00000003

conNameStencilFaceFlagBits :: String
conNameStencilFaceFlagBits = "StencilFaceFlagBits"

enumPrefixStencilFaceFlagBits :: String
enumPrefixStencilFaceFlagBits = "STENCIL_FACE_"

showTableStencilFaceFlagBits :: [(StencilFaceFlagBits, String)]
showTableStencilFaceFlagBits =
  [ (STENCIL_FACE_FRONT_BIT     , "FRONT_BIT")
  , (STENCIL_FACE_BACK_BIT      , "BACK_BIT")
  , (STENCIL_FACE_FRONT_AND_BACK, "FRONT_AND_BACK")
  ]

instance Show StencilFaceFlagBits where
  showsPrec p e = case lookup e showTableStencilFaceFlagBits of
    Just s -> showString enumPrefixStencilFaceFlagBits . showString s
    Nothing ->
      let StencilFaceFlagBits x = e
      in  showParen (p >= 11) (showString conNameStencilFaceFlagBits . showString " 0x" . showHex x)

instance Read StencilFaceFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixStencilFaceFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableStencilFaceFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameStencilFaceFlagBits)
            v <- step readPrec
            pure (StencilFaceFlagBits v)
          )
    )

