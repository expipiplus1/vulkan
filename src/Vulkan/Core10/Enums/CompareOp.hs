{-# language CPP #-}
module Vulkan.Core10.Enums.CompareOp  (CompareOp( COMPARE_OP_NEVER
                                                , COMPARE_OP_LESS
                                                , COMPARE_OP_EQUAL
                                                , COMPARE_OP_LESS_OR_EQUAL
                                                , COMPARE_OP_GREATER
                                                , COMPARE_OP_NOT_EQUAL
                                                , COMPARE_OP_GREATER_OR_EQUAL
                                                , COMPARE_OP_ALWAYS
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
-- | VkCompareOp - Stencil comparison function
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo',
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo',
-- 'Vulkan.Core10.Pipeline.StencilOpState'
newtype CompareOp = CompareOp Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'COMPARE_OP_NEVER' specifies that the test never passes.
pattern COMPARE_OP_NEVER = CompareOp 0
-- | 'COMPARE_OP_LESS' specifies that the test passes when R \< S.
pattern COMPARE_OP_LESS = CompareOp 1
-- | 'COMPARE_OP_EQUAL' specifies that the test passes when R = S.
pattern COMPARE_OP_EQUAL = CompareOp 2
-- | 'COMPARE_OP_LESS_OR_EQUAL' specifies that the test passes when R ≤ S.
pattern COMPARE_OP_LESS_OR_EQUAL = CompareOp 3
-- | 'COMPARE_OP_GREATER' specifies that the test passes when R > S.
pattern COMPARE_OP_GREATER = CompareOp 4
-- | 'COMPARE_OP_NOT_EQUAL' specifies that the test passes when R ≠ S.
pattern COMPARE_OP_NOT_EQUAL = CompareOp 5
-- | 'COMPARE_OP_GREATER_OR_EQUAL' specifies that the test passes when R ≥ S.
pattern COMPARE_OP_GREATER_OR_EQUAL = CompareOp 6
-- | 'COMPARE_OP_ALWAYS' specifies that the test always passes.
pattern COMPARE_OP_ALWAYS = CompareOp 7
{-# complete COMPARE_OP_NEVER,
             COMPARE_OP_LESS,
             COMPARE_OP_EQUAL,
             COMPARE_OP_LESS_OR_EQUAL,
             COMPARE_OP_GREATER,
             COMPARE_OP_NOT_EQUAL,
             COMPARE_OP_GREATER_OR_EQUAL,
             COMPARE_OP_ALWAYS :: CompareOp #-}

instance Show CompareOp where
  showsPrec p = \case
    COMPARE_OP_NEVER -> showString "COMPARE_OP_NEVER"
    COMPARE_OP_LESS -> showString "COMPARE_OP_LESS"
    COMPARE_OP_EQUAL -> showString "COMPARE_OP_EQUAL"
    COMPARE_OP_LESS_OR_EQUAL -> showString "COMPARE_OP_LESS_OR_EQUAL"
    COMPARE_OP_GREATER -> showString "COMPARE_OP_GREATER"
    COMPARE_OP_NOT_EQUAL -> showString "COMPARE_OP_NOT_EQUAL"
    COMPARE_OP_GREATER_OR_EQUAL -> showString "COMPARE_OP_GREATER_OR_EQUAL"
    COMPARE_OP_ALWAYS -> showString "COMPARE_OP_ALWAYS"
    CompareOp x -> showParen (p >= 11) (showString "CompareOp " . showsPrec 11 x)

instance Read CompareOp where
  readPrec = parens (choose [("COMPARE_OP_NEVER", pure COMPARE_OP_NEVER)
                            , ("COMPARE_OP_LESS", pure COMPARE_OP_LESS)
                            , ("COMPARE_OP_EQUAL", pure COMPARE_OP_EQUAL)
                            , ("COMPARE_OP_LESS_OR_EQUAL", pure COMPARE_OP_LESS_OR_EQUAL)
                            , ("COMPARE_OP_GREATER", pure COMPARE_OP_GREATER)
                            , ("COMPARE_OP_NOT_EQUAL", pure COMPARE_OP_NOT_EQUAL)
                            , ("COMPARE_OP_GREATER_OR_EQUAL", pure COMPARE_OP_GREATER_OR_EQUAL)
                            , ("COMPARE_OP_ALWAYS", pure COMPARE_OP_ALWAYS)]
                     +++
                     prec 10 (do
                       expectP (Ident "CompareOp")
                       v <- step readPrec
                       pure (CompareOp v)))

