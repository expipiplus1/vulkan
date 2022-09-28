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

-- | VkCompareOp - Comparison operator for depth, stencil, and sampler
-- operations
--
-- = Description
--
-- -   'COMPARE_OP_NEVER' specifies that the comparison always evaluates
--     false.
--
-- -   'COMPARE_OP_LESS' specifies that the comparison evaluates
--     /reference/ \< /test/.
--
-- -   'COMPARE_OP_EQUAL' specifies that the comparison evaluates
--     /reference/ = /test/.
--
-- -   'COMPARE_OP_LESS_OR_EQUAL' specifies that the comparison evaluates
--     /reference/ ≤ /test/.
--
-- -   'COMPARE_OP_GREATER' specifies that the comparison evaluates
--     /reference/ > /test/.
--
-- -   'COMPARE_OP_NOT_EQUAL' specifies that the comparison evaluates
--     /reference/ ≠ /test/.
--
-- -   'COMPARE_OP_GREATER_OR_EQUAL' specifies that the comparison
--     evaluates /reference/ ≥ /test/.
--
-- -   'COMPARE_OP_ALWAYS' specifies that the comparison always evaluates
--     true.
--
-- Comparison operators are used for:
--
-- -   The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-depth-compare-operation Depth Compare Operation>
--     operator for a sampler, specified by
--     'Vulkan.Core10.Sampler.SamplerCreateInfo'::@compareOp@.
--
-- -   The stencil comparison operator for the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragops-stencil stencil test>,
--     specified by
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetStencilOp'::@compareOp@
--     or 'Vulkan.Core10.Pipeline.StencilOpState'::@compareOp@.
--
-- -   The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragops-depth-comparison Depth Comparison>
--     operator for the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragops-depth depth test>,
--     specified by
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthCompareOp'::@depthCompareOp@
--     or
--     'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'::@depthCompareOp@.
--
-- Each such use describes how the /reference/ and /test/ values for that
-- comparison are determined.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo',
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo',
-- 'Vulkan.Core10.Pipeline.StencilOpState',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthCompareOp',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetDepthCompareOpEXT',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetStencilOp',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetStencilOpEXT'
newtype CompareOp = CompareOp Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_NEVER"
pattern COMPARE_OP_NEVER = CompareOp 0

-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_LESS"
pattern COMPARE_OP_LESS = CompareOp 1

-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_EQUAL"
pattern COMPARE_OP_EQUAL = CompareOp 2

-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_LESS_OR_EQUAL"
pattern COMPARE_OP_LESS_OR_EQUAL = CompareOp 3

-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_GREATER"
pattern COMPARE_OP_GREATER = CompareOp 4

-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_NOT_EQUAL"
pattern COMPARE_OP_NOT_EQUAL = CompareOp 5

-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_GREATER_OR_EQUAL"
pattern COMPARE_OP_GREATER_OR_EQUAL = CompareOp 6

-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_ALWAYS"
pattern COMPARE_OP_ALWAYS = CompareOp 7

{-# COMPLETE
  COMPARE_OP_NEVER
  , COMPARE_OP_LESS
  , COMPARE_OP_EQUAL
  , COMPARE_OP_LESS_OR_EQUAL
  , COMPARE_OP_GREATER
  , COMPARE_OP_NOT_EQUAL
  , COMPARE_OP_GREATER_OR_EQUAL
  , COMPARE_OP_ALWAYS ::
    CompareOp
  #-}

conNameCompareOp :: String
conNameCompareOp = "CompareOp"

enumPrefixCompareOp :: String
enumPrefixCompareOp = "COMPARE_OP_"

showTableCompareOp :: [(CompareOp, String)]
showTableCompareOp =
  [ (COMPARE_OP_NEVER, "NEVER")
  , (COMPARE_OP_LESS, "LESS")
  , (COMPARE_OP_EQUAL, "EQUAL")
  , (COMPARE_OP_LESS_OR_EQUAL, "LESS_OR_EQUAL")
  , (COMPARE_OP_GREATER, "GREATER")
  , (COMPARE_OP_NOT_EQUAL, "NOT_EQUAL")
  , (COMPARE_OP_GREATER_OR_EQUAL, "GREATER_OR_EQUAL")
  , (COMPARE_OP_ALWAYS, "ALWAYS")
  ]

instance Show CompareOp where
  showsPrec =
    enumShowsPrec
      enumPrefixCompareOp
      showTableCompareOp
      conNameCompareOp
      (\(CompareOp x) -> x)
      (showsPrec 11)

instance Read CompareOp where
  readPrec =
    enumReadPrec
      enumPrefixCompareOp
      showTableCompareOp
      conNameCompareOp
      CompareOp
