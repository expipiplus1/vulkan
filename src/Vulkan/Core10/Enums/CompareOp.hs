{-# language CPP #-}
-- No documentation found for Chapter "CompareOp"
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

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkCompareOp - Stencil comparison function
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo',
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo',
-- 'Vulkan.Core10.Pipeline.StencilOpState',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetDepthCompareOpEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetStencilOpEXT'
newtype CompareOp = CompareOp Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'COMPARE_OP_NEVER' specifies that the test evaluates to false.
pattern COMPARE_OP_NEVER            = CompareOp 0
-- | 'COMPARE_OP_LESS' specifies that the test evaluates A \< B.
pattern COMPARE_OP_LESS             = CompareOp 1
-- | 'COMPARE_OP_EQUAL' specifies that the test evaluates A = B.
pattern COMPARE_OP_EQUAL            = CompareOp 2
-- | 'COMPARE_OP_LESS_OR_EQUAL' specifies that the test evaluates A ≤ B.
pattern COMPARE_OP_LESS_OR_EQUAL    = CompareOp 3
-- | 'COMPARE_OP_GREATER' specifies that the test evaluates A > B.
pattern COMPARE_OP_GREATER          = CompareOp 4
-- | 'COMPARE_OP_NOT_EQUAL' specifies that the test evaluates A ≠ B.
pattern COMPARE_OP_NOT_EQUAL        = CompareOp 5
-- | 'COMPARE_OP_GREATER_OR_EQUAL' specifies that the test evaluates A ≥ B.
pattern COMPARE_OP_GREATER_OR_EQUAL = CompareOp 6
-- | 'COMPARE_OP_ALWAYS' specifies that the test evaluates to true.
pattern COMPARE_OP_ALWAYS           = CompareOp 7
{-# complete COMPARE_OP_NEVER,
             COMPARE_OP_LESS,
             COMPARE_OP_EQUAL,
             COMPARE_OP_LESS_OR_EQUAL,
             COMPARE_OP_GREATER,
             COMPARE_OP_NOT_EQUAL,
             COMPARE_OP_GREATER_OR_EQUAL,
             COMPARE_OP_ALWAYS :: CompareOp #-}

conNameCompareOp :: String
conNameCompareOp = "CompareOp"

enumPrefixCompareOp :: String
enumPrefixCompareOp = "COMPARE_OP_"

showTableCompareOp :: [(CompareOp, String)]
showTableCompareOp =
  [ (COMPARE_OP_NEVER           , "NEVER")
  , (COMPARE_OP_LESS            , "LESS")
  , (COMPARE_OP_EQUAL           , "EQUAL")
  , (COMPARE_OP_LESS_OR_EQUAL   , "LESS_OR_EQUAL")
  , (COMPARE_OP_GREATER         , "GREATER")
  , (COMPARE_OP_NOT_EQUAL       , "NOT_EQUAL")
  , (COMPARE_OP_GREATER_OR_EQUAL, "GREATER_OR_EQUAL")
  , (COMPARE_OP_ALWAYS          , "ALWAYS")
  ]

instance Show CompareOp where
  showsPrec =
    enumShowsPrec enumPrefixCompareOp showTableCompareOp conNameCompareOp (\(CompareOp x) -> x) (showsPrec 11)

instance Read CompareOp where
  readPrec = enumReadPrec enumPrefixCompareOp showTableCompareOp conNameCompareOp CompareOp

