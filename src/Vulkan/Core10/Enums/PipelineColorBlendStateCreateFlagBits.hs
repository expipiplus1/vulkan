{-# language CPP #-}
-- No documentation found for Chapter "PipelineColorBlendStateCreateFlagBits"
module Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits  ( PipelineColorBlendStateCreateFlags
                                                                  , PipelineColorBlendStateCreateFlagBits( PIPELINE_COLOR_BLEND_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_BIT_EXT
                                                                                                         , ..
                                                                                                         )
                                                                  ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type PipelineColorBlendStateCreateFlags = PipelineColorBlendStateCreateFlagBits

-- | VkPipelineColorBlendStateCreateFlagBits - Bitmask specifying additional
-- parameters of an image
--
-- = Description
--
-- -   'PIPELINE_COLOR_BLEND_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_BIT_EXT'
--     indicates that access to color and input attachments will have
--     implicit framebuffer-local memory dependencies, allowing
--     applications to express custom blending operations in a fragment
--     shader.
--
-- When
-- 'PIPELINE_COLOR_BLEND_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_BIT_EXT'
-- is included in a pipeline, it forms a framebuffer-local memory
-- dependency for each fragment generated by draw commands for that
-- pipeline with the following scopes:
--
-- -   The first
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
--     includes the
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT'
--     pipeline stage executed by all previous fragments (as defined by
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#drawing-primitive-order primitive order>)
--     in the corresponding
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer regions>
--     including those generated by the same draw command.
--
-- -   The second
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
--     includes the
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_SHADER_BIT'
--     pipeline stage executed by the generated fragment.
--
-- -   The first
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
--     includes all writes to color attachments.
--
-- -   The second
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
--     includes all reads from input attachments.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_rasterization_order_attachment_access VK_EXT_rasterization_order_attachment_access>,
-- 'PipelineColorBlendStateCreateFlags'
newtype PipelineColorBlendStateCreateFlagBits = PipelineColorBlendStateCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPipelineColorBlendStateCreateFlagBits" "VK_PIPELINE_COLOR_BLEND_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_BIT_EXT"
pattern PIPELINE_COLOR_BLEND_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_BIT_EXT = PipelineColorBlendStateCreateFlagBits 0x00000001

conNamePipelineColorBlendStateCreateFlagBits :: String
conNamePipelineColorBlendStateCreateFlagBits = "PipelineColorBlendStateCreateFlagBits"

enumPrefixPipelineColorBlendStateCreateFlagBits :: String
enumPrefixPipelineColorBlendStateCreateFlagBits = "PIPELINE_COLOR_BLEND_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_BIT_EXT"

showTablePipelineColorBlendStateCreateFlagBits :: [(PipelineColorBlendStateCreateFlagBits, String)]
showTablePipelineColorBlendStateCreateFlagBits =
  [
    ( PIPELINE_COLOR_BLEND_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_BIT_EXT
    , ""
    )
  ]

instance Show PipelineColorBlendStateCreateFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineColorBlendStateCreateFlagBits
      showTablePipelineColorBlendStateCreateFlagBits
      conNamePipelineColorBlendStateCreateFlagBits
      (\(PipelineColorBlendStateCreateFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PipelineColorBlendStateCreateFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixPipelineColorBlendStateCreateFlagBits
      showTablePipelineColorBlendStateCreateFlagBits
      conNamePipelineColorBlendStateCreateFlagBits
      PipelineColorBlendStateCreateFlagBits
