{-# language CPP #-}
-- No documentation found for Chapter "SubpassDescriptionFlagBits"
module Vulkan.Core10.Enums.SubpassDescriptionFlagBits  ( SubpassDescriptionFlags
                                                       , SubpassDescriptionFlagBits( SUBPASS_DESCRIPTION_CUSTOM_RESOLVE_BIT_EXT
                                                                                   , SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_EXT
                                                                                   , SUBPASS_DESCRIPTION_ENABLE_LEGACY_DITHERING_BIT_EXT
                                                                                   , SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_EXT
                                                                                   , SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_EXT
                                                                                   , SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_COLOR_ACCESS_BIT_EXT
                                                                                   , SUBPASS_DESCRIPTION_TILE_SHADING_APRON_BIT_QCOM
                                                                                   , SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX
                                                                                   , SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX
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
type SubpassDescriptionFlags = SubpassDescriptionFlagBits

-- | VkSubpassDescriptionFlagBits - Bitmask specifying usage of a subpass
--
-- = Description
--
-- -   'SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX' specifies that
--     shaders compiled for this subpass write the attributes for all views
--     in a single invocation of each
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader stage>.
--     All pipelines compiled against a subpass that includes this bit
--     /must/ write per-view attributes to the @*PerViewNV[]@ shader
--     outputs, in addition to the non-per-view (e.g. @Position@) outputs.
--
-- -   'SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX' specifies
--     that shaders compiled for this subpass use per-view positions which
--     only differ in value in the x component. Per-view viewport mask
--     /can/ also be used.
--
-- -   'SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_EXT' specifies that the
--     framebuffer region is the fragment region, that is, the minimum
--     region dependencies are by pixel rather than by sample, such that
--     any fragment shader invocation /can/ access any sample associated
--     with that fragment shader invocation.
--
-- -   'SUBPASS_DESCRIPTION_CUSTOM_RESOLVE_BIT_EXT' specifies that the
--     subpass performs shader resolve operations.
--
-- -   'SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_COLOR_ACCESS_BIT_EXT'
--     specifies that this subpass supports pipelines created with
--     'Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits.PIPELINE_COLOR_BLEND_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_BIT_EXT'.
--
-- -   'SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_EXT'
--     specifies that this subpass supports pipelines created with
--     'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_EXT'.
--
-- -   'SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_EXT'
--     specifies that this subpass supports pipelines created with
--     'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_EXT'.
--
-- -   'SUBPASS_DESCRIPTION_ENABLE_LEGACY_DITHERING_BIT_EXT' specifies that
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-legacy-dithering Legacy Dithering>
--     is enabled for this subpass.
--
-- -   'SUBPASS_DESCRIPTION_TILE_SHADING_APRON_BIT_QCOM' specifies that
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-tile-shading-aprons apron regions>
--     /can/ be read within this subpass when
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-tile-shading tile shading is enabled>.
--
-- Shader resolve operations allow for custom resolve operations, but
-- overdrawing pixels /may/ have a performance and\/or power cost.
-- Furthermore, since the content of any depth stencil attachment or color
-- attachment is undefined at the beginning of a shader resolve subpass,
-- any depth testing, stencil testing, or blending operation which sources
-- these undefined values also has undefined result value.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'SubpassDescriptionFlags'
newtype SubpassDescriptionFlagBits = SubpassDescriptionFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkSubpassDescriptionFlagBits" "VK_SUBPASS_DESCRIPTION_CUSTOM_RESOLVE_BIT_EXT"
pattern SUBPASS_DESCRIPTION_CUSTOM_RESOLVE_BIT_EXT = SubpassDescriptionFlagBits 0x00000008

-- No documentation found for Nested "VkSubpassDescriptionFlagBits" "VK_SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_EXT"
pattern SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_EXT = SubpassDescriptionFlagBits 0x00000004

-- No documentation found for Nested "VkSubpassDescriptionFlagBits" "VK_SUBPASS_DESCRIPTION_ENABLE_LEGACY_DITHERING_BIT_EXT"
pattern SUBPASS_DESCRIPTION_ENABLE_LEGACY_DITHERING_BIT_EXT = SubpassDescriptionFlagBits 0x00000080

-- No documentation found for Nested "VkSubpassDescriptionFlagBits" "VK_SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_EXT"
pattern SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_EXT = SubpassDescriptionFlagBits 0x00000040

-- No documentation found for Nested "VkSubpassDescriptionFlagBits" "VK_SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_EXT"
pattern SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_EXT = SubpassDescriptionFlagBits 0x00000020

-- No documentation found for Nested "VkSubpassDescriptionFlagBits" "VK_SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_COLOR_ACCESS_BIT_EXT"
pattern SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_COLOR_ACCESS_BIT_EXT = SubpassDescriptionFlagBits 0x00000010

-- No documentation found for Nested "VkSubpassDescriptionFlagBits" "VK_SUBPASS_DESCRIPTION_TILE_SHADING_APRON_BIT_QCOM"
pattern SUBPASS_DESCRIPTION_TILE_SHADING_APRON_BIT_QCOM = SubpassDescriptionFlagBits 0x00000100

-- No documentation found for Nested "VkSubpassDescriptionFlagBits" "VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX"
pattern SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX = SubpassDescriptionFlagBits 0x00000002

-- No documentation found for Nested "VkSubpassDescriptionFlagBits" "VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX"
pattern SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX = SubpassDescriptionFlagBits 0x00000001

conNameSubpassDescriptionFlagBits :: String
conNameSubpassDescriptionFlagBits = "SubpassDescriptionFlagBits"

enumPrefixSubpassDescriptionFlagBits :: String
enumPrefixSubpassDescriptionFlagBits = "SUBPASS_DESCRIPTION_"

showTableSubpassDescriptionFlagBits :: [(SubpassDescriptionFlagBits, String)]
showTableSubpassDescriptionFlagBits =
  [
    ( SUBPASS_DESCRIPTION_CUSTOM_RESOLVE_BIT_EXT
    , "CUSTOM_RESOLVE_BIT_EXT"
    )
  ,
    ( SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_EXT
    , "FRAGMENT_REGION_BIT_EXT"
    )
  ,
    ( SUBPASS_DESCRIPTION_ENABLE_LEGACY_DITHERING_BIT_EXT
    , "ENABLE_LEGACY_DITHERING_BIT_EXT"
    )
  ,
    ( SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_EXT
    , "RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_EXT"
    )
  ,
    ( SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_EXT
    , "RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_EXT"
    )
  ,
    ( SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_COLOR_ACCESS_BIT_EXT
    , "RASTERIZATION_ORDER_ATTACHMENT_COLOR_ACCESS_BIT_EXT"
    )
  ,
    ( SUBPASS_DESCRIPTION_TILE_SHADING_APRON_BIT_QCOM
    , "TILE_SHADING_APRON_BIT_QCOM"
    )
  ,
    ( SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX
    , "PER_VIEW_POSITION_X_ONLY_BIT_NVX"
    )
  ,
    ( SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX
    , "PER_VIEW_ATTRIBUTES_BIT_NVX"
    )
  ]

instance Show SubpassDescriptionFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixSubpassDescriptionFlagBits
      showTableSubpassDescriptionFlagBits
      conNameSubpassDescriptionFlagBits
      (\(SubpassDescriptionFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read SubpassDescriptionFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixSubpassDescriptionFlagBits
      showTableSubpassDescriptionFlagBits
      conNameSubpassDescriptionFlagBits
      SubpassDescriptionFlagBits
