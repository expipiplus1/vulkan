{-# language CPP #-}
module Vulkan.Core10.Enums.StencilOp  (StencilOp( STENCIL_OP_KEEP
                                                , STENCIL_OP_ZERO
                                                , STENCIL_OP_REPLACE
                                                , STENCIL_OP_INCREMENT_AND_CLAMP
                                                , STENCIL_OP_DECREMENT_AND_CLAMP
                                                , STENCIL_OP_INVERT
                                                , STENCIL_OP_INCREMENT_AND_WRAP
                                                , STENCIL_OP_DECREMENT_AND_WRAP
                                                , ..
                                                )) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Zero (Zero)
-- | VkStencilOp - Stencil comparison function
--
-- = Description
--
-- For purposes of increment and decrement, the stencil bits are considered
-- as an unsigned integer.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.StencilOpState'
newtype StencilOp = StencilOp Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'STENCIL_OP_KEEP' keeps the current value.
pattern STENCIL_OP_KEEP = StencilOp 0
-- | 'STENCIL_OP_ZERO' sets the value to 0.
pattern STENCIL_OP_ZERO = StencilOp 1
-- | 'STENCIL_OP_REPLACE' sets the value to @reference@.
pattern STENCIL_OP_REPLACE = StencilOp 2
-- | 'STENCIL_OP_INCREMENT_AND_CLAMP' increments the current value and clamps
-- to the maximum representable unsigned value.
pattern STENCIL_OP_INCREMENT_AND_CLAMP = StencilOp 3
-- | 'STENCIL_OP_DECREMENT_AND_CLAMP' decrements the current value and clamps
-- to 0.
pattern STENCIL_OP_DECREMENT_AND_CLAMP = StencilOp 4
-- | 'STENCIL_OP_INVERT' bitwise-inverts the current value.
pattern STENCIL_OP_INVERT = StencilOp 5
-- | 'STENCIL_OP_INCREMENT_AND_WRAP' increments the current value and wraps
-- to 0 when the maximum value would have been exceeded.
pattern STENCIL_OP_INCREMENT_AND_WRAP = StencilOp 6
-- | 'STENCIL_OP_DECREMENT_AND_WRAP' decrements the current value and wraps
-- to the maximum possible value when the value would go below 0.
pattern STENCIL_OP_DECREMENT_AND_WRAP = StencilOp 7
{-# complete STENCIL_OP_KEEP,
             STENCIL_OP_ZERO,
             STENCIL_OP_REPLACE,
             STENCIL_OP_INCREMENT_AND_CLAMP,
             STENCIL_OP_DECREMENT_AND_CLAMP,
             STENCIL_OP_INVERT,
             STENCIL_OP_INCREMENT_AND_WRAP,
             STENCIL_OP_DECREMENT_AND_WRAP :: StencilOp #-}

instance Show StencilOp where
  showsPrec p = \case
    STENCIL_OP_KEEP -> showString "STENCIL_OP_KEEP"
    STENCIL_OP_ZERO -> showString "STENCIL_OP_ZERO"
    STENCIL_OP_REPLACE -> showString "STENCIL_OP_REPLACE"
    STENCIL_OP_INCREMENT_AND_CLAMP -> showString "STENCIL_OP_INCREMENT_AND_CLAMP"
    STENCIL_OP_DECREMENT_AND_CLAMP -> showString "STENCIL_OP_DECREMENT_AND_CLAMP"
    STENCIL_OP_INVERT -> showString "STENCIL_OP_INVERT"
    STENCIL_OP_INCREMENT_AND_WRAP -> showString "STENCIL_OP_INCREMENT_AND_WRAP"
    STENCIL_OP_DECREMENT_AND_WRAP -> showString "STENCIL_OP_DECREMENT_AND_WRAP"
    StencilOp x -> showParen (p >= 11) (showString "StencilOp " . showsPrec 11 x)

instance Read StencilOp where
  readPrec = parens (choose [("STENCIL_OP_KEEP", pure STENCIL_OP_KEEP)
                            , ("STENCIL_OP_ZERO", pure STENCIL_OP_ZERO)
                            , ("STENCIL_OP_REPLACE", pure STENCIL_OP_REPLACE)
                            , ("STENCIL_OP_INCREMENT_AND_CLAMP", pure STENCIL_OP_INCREMENT_AND_CLAMP)
                            , ("STENCIL_OP_DECREMENT_AND_CLAMP", pure STENCIL_OP_DECREMENT_AND_CLAMP)
                            , ("STENCIL_OP_INVERT", pure STENCIL_OP_INVERT)
                            , ("STENCIL_OP_INCREMENT_AND_WRAP", pure STENCIL_OP_INCREMENT_AND_WRAP)
                            , ("STENCIL_OP_DECREMENT_AND_WRAP", pure STENCIL_OP_DECREMENT_AND_WRAP)]
                     +++
                     prec 10 (do
                       expectP (Ident "StencilOp")
                       v <- step readPrec
                       pure (StencilOp v)))

