{-# language CPP #-}
-- No documentation found for Chapter "PipelineDepthStencilStateCreateFlagBits"
module Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits  ( PipelineDepthStencilStateCreateFlags
                                                                    , PipelineDepthStencilStateCreateFlagBits( PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_ARM
                                                                                                             , PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_ARM
                                                                                                             , ..
                                                                                                             )
                                                                    ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type PipelineDepthStencilStateCreateFlags = PipelineDepthStencilStateCreateFlagBits

-- | VkPipelineDepthStencilStateCreateFlagBits - Bitmask specifying
-- additional depth\/stencil state information.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_rasterization_order_attachment_access VK_ARM_rasterization_order_attachment_access>,
-- 'PipelineDepthStencilStateCreateFlags'
newtype PipelineDepthStencilStateCreateFlagBits = PipelineDepthStencilStateCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_ARM'
-- indicates that access to the stencil aspects of depth\/stencil and input
-- attachments will have implicit framebuffer-local memory dependencies.
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-feedbackloop renderpass feedback loops>
-- for more information.
pattern PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_ARM =
  PipelineDepthStencilStateCreateFlagBits 0x00000002
-- | 'PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_ARM'
-- indicates that access to the depth aspects of depth\/stencil and input
-- attachments will have implicit framebuffer-local memory dependencies.
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-feedbackloop renderpass feedback loops>
-- for more information.
pattern PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_ARM =
  PipelineDepthStencilStateCreateFlagBits 0x00000001

conNamePipelineDepthStencilStateCreateFlagBits :: String
conNamePipelineDepthStencilStateCreateFlagBits = "PipelineDepthStencilStateCreateFlagBits"

enumPrefixPipelineDepthStencilStateCreateFlagBits :: String
enumPrefixPipelineDepthStencilStateCreateFlagBits =
  "PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_"

showTablePipelineDepthStencilStateCreateFlagBits :: [(PipelineDepthStencilStateCreateFlagBits, String)]
showTablePipelineDepthStencilStateCreateFlagBits =
  [ ( PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_ARM
    , "STENCIL_ACCESS_BIT_ARM"
    )
  , (PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_ARM, "DEPTH_ACCESS_BIT_ARM")
  ]

instance Show PipelineDepthStencilStateCreateFlagBits where
  showsPrec = enumShowsPrec enumPrefixPipelineDepthStencilStateCreateFlagBits
                            showTablePipelineDepthStencilStateCreateFlagBits
                            conNamePipelineDepthStencilStateCreateFlagBits
                            (\(PipelineDepthStencilStateCreateFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read PipelineDepthStencilStateCreateFlagBits where
  readPrec = enumReadPrec enumPrefixPipelineDepthStencilStateCreateFlagBits
                          showTablePipelineDepthStencilStateCreateFlagBits
                          conNamePipelineDepthStencilStateCreateFlagBits
                          PipelineDepthStencilStateCreateFlagBits

