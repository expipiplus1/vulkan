{-# language CPP #-}
module Vulkan.Core10.Enums.StencilFaceFlagBits  ( StencilFaceFlagBits( STENCIL_FACE_FRONT_BIT
                                                                     , STENCIL_FACE_BACK_BIT
                                                                     , STENCIL_FACE_FRONT_AND_BACK
                                                                     , ..
                                                                     )
                                                , StencilFaceFlags
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
-- | VkStencilFaceFlagBits - Bitmask specifying sets of stencil state for
-- which to update the compare mask
--
-- = See Also
--
-- 'StencilFaceFlags'
newtype StencilFaceFlagBits = StencilFaceFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'STENCIL_FACE_FRONT_BIT' specifies that only the front set of stencil
-- state is updated.
pattern STENCIL_FACE_FRONT_BIT = StencilFaceFlagBits 0x00000001
-- | 'STENCIL_FACE_BACK_BIT' specifies that only the back set of stencil
-- state is updated.
pattern STENCIL_FACE_BACK_BIT = StencilFaceFlagBits 0x00000002
-- | 'STENCIL_FACE_FRONT_AND_BACK' is the combination of
-- 'STENCIL_FACE_FRONT_BIT' and 'STENCIL_FACE_BACK_BIT', and specifies that
-- both sets of stencil state are updated.
pattern STENCIL_FACE_FRONT_AND_BACK = StencilFaceFlagBits 0x00000003

type StencilFaceFlags = StencilFaceFlagBits

instance Show StencilFaceFlagBits where
  showsPrec p = \case
    STENCIL_FACE_FRONT_BIT -> showString "STENCIL_FACE_FRONT_BIT"
    STENCIL_FACE_BACK_BIT -> showString "STENCIL_FACE_BACK_BIT"
    STENCIL_FACE_FRONT_AND_BACK -> showString "STENCIL_FACE_FRONT_AND_BACK"
    StencilFaceFlagBits x -> showParen (p >= 11) (showString "StencilFaceFlagBits 0x" . showHex x)

instance Read StencilFaceFlagBits where
  readPrec = parens (choose [("STENCIL_FACE_FRONT_BIT", pure STENCIL_FACE_FRONT_BIT)
                            , ("STENCIL_FACE_BACK_BIT", pure STENCIL_FACE_BACK_BIT)
                            , ("STENCIL_FACE_FRONT_AND_BACK", pure STENCIL_FACE_FRONT_AND_BACK)]
                     +++
                     prec 10 (do
                       expectP (Ident "StencilFaceFlagBits")
                       v <- step readPrec
                       pure (StencilFaceFlagBits v)))

