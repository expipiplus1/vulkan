{-# language CPP #-}
-- No documentation found for Chapter "ImageLayout"
module Vulkan.Core10.Enums.ImageLayout  (ImageLayout( IMAGE_LAYOUT_UNDEFINED
                                                    , IMAGE_LAYOUT_GENERAL
                                                    , IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                    , IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                    , IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
                                                    , IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                                                    , IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
                                                    , IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                                                    , IMAGE_LAYOUT_PREINITIALIZED
                                                    , IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT
                                                    , IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR
                                                    , IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT
                                                    , IMAGE_LAYOUT_SHARED_PRESENT_KHR
                                                    , IMAGE_LAYOUT_PRESENT_SRC_KHR
                                                    , IMAGE_LAYOUT_ATTACHMENT_OPTIMAL
                                                    , IMAGE_LAYOUT_READ_ONLY_OPTIMAL
                                                    , IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL
                                                    , IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL
                                                    , IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL
                                                    , IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL
                                                    , IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
                                                    , IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
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

-- | VkImageLayout - Layout of image and image subresources
--
-- = Description
--
-- The type(s) of device access supported by each layout are:
--
-- The layout of each image subresource is not a state of the image
-- subresource itself, but is rather a property of how the data in memory
-- is organized, and thus for each mechanism of accessing an image in the
-- API the application /must/ specify a parameter or structure member that
-- indicates which image layout the image subresource(s) are considered to
-- be in when the image will be accessed. For transfer commands, this is a
-- parameter to the command (see
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#clears>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#copies>).
-- For use as a framebuffer attachment, this is a member in the
-- substructures of the 'Vulkan.Core10.Pass.RenderPassCreateInfo' (see
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass Render Pass>).
-- For use in a descriptor set, this is a member in the
-- 'Vulkan.Core10.DescriptorSet.DescriptorImageInfo' structure (see
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-updates>).
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Pass.AttachmentDescription',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.AttachmentDescription2',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout',
-- 'Vulkan.Core10.Pass.AttachmentReference',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.AttachmentReference2',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentReferenceStencilLayout',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.BlitImageInfo2',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.CopyBufferToImageInfo2',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.CopyImageInfo2',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.CopyImageToBufferInfo2',
-- 'Vulkan.Core10.DescriptorSet.DescriptorImageInfo',
-- 'Vulkan.Core10.Image.ImageCreateInfo',
-- 'Vulkan.Core10.OtherTypes.ImageMemoryBarrier',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.ImageMemoryBarrier2',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingAttachmentInfo',
-- 'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentDensityMapAttachmentInfoEXT',
-- 'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentShadingRateAttachmentInfoKHR',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.ResolveImageInfo2',
-- 'Vulkan.Extensions.VK_NV_optical_flow.bindOpticalFlowSessionImageNV',
-- 'Vulkan.Extensions.VK_HUAWEI_invocation_mask.cmdBindInvocationMaskHUAWEI',
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.cmdBindShadingRateImageNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBlitImage',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdClearColorImage',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdClearDepthStencilImage',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyBufferToImage',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyImage',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyImageToBuffer',
-- 'Vulkan.Extensions.VK_NV_copy_memory_indirect.cmdCopyMemoryToImageIndirectNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdResolveImage'
newtype ImageLayout = ImageLayout Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'IMAGE_LAYOUT_UNDEFINED' specifies that the layout is unknown. Image
-- memory /cannot/ be transitioned into this layout. This layout /can/ be
-- used as the @initialLayout@ member of
-- 'Vulkan.Core10.Image.ImageCreateInfo'. This layout /can/ be used in
-- place of the current image layout in a layout transition, but doing so
-- will cause the contents of the image’s memory to be undefined.
pattern IMAGE_LAYOUT_UNDEFINED = ImageLayout 0

-- | 'IMAGE_LAYOUT_GENERAL' supports all types of device access.
pattern IMAGE_LAYOUT_GENERAL = ImageLayout 1

-- | 'IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL' /must/ only be used as a color
-- or resolve attachment in a 'Vulkan.Core10.Handles.Framebuffer'. This
-- layout is valid only for image subresources of images created with the
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
-- usage bit enabled.
pattern IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL = ImageLayout 2

-- | 'IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL' specifies a layout for
-- both the depth and stencil aspects of a depth\/stencil format image
-- allowing read and write access as a depth\/stencil attachment. It is
-- equivalent to 'IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL' and
-- 'IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'.
pattern IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL = ImageLayout 3

-- | 'IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL' specifies a layout for
-- both the depth and stencil aspects of a depth\/stencil format image
-- allowing read only access as a depth\/stencil attachment or in shaders
-- as a sampled image, combined image\/sampler, or input attachment. It is
-- equivalent to 'IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL' and
-- 'IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'.
pattern IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL = ImageLayout 4

-- | 'IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL' specifies a layout allowing
-- read-only access in a shader as a sampled image, combined
-- image\/sampler, or input attachment. This layout is valid only for image
-- subresources of images created with the
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT' or
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
-- usage bits enabled.
pattern IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = ImageLayout 5

-- | 'IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL' /must/ only be used as a source
-- image of a transfer command (see the definition of
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-transfer >).
-- This layout is valid only for image subresources of images created with
-- the
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
-- usage bit enabled.
pattern IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL = ImageLayout 6

-- | 'IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL' /must/ only be used as a destination
-- image of a transfer command. This layout is valid only for image
-- subresources of images created with the
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
-- usage bit enabled.
pattern IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL = ImageLayout 7

-- | 'IMAGE_LAYOUT_PREINITIALIZED' specifies that an image’s memory is in a
-- defined layout and /can/ be populated by data, but that it has not yet
-- been initialized by the driver. Image memory /cannot/ be transitioned
-- into this layout. This layout /can/ be used as the @initialLayout@
-- member of 'Vulkan.Core10.Image.ImageCreateInfo'. This layout is intended
-- to be used as the initial layout for an image whose contents are written
-- by the host, and hence the data /can/ be written to memory immediately,
-- without first executing a layout transition. Currently,
-- 'IMAGE_LAYOUT_PREINITIALIZED' is only useful with
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#glossary-linear-resource linear>
-- images because there is not a standard layout defined for
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL' images.
pattern IMAGE_LAYOUT_PREINITIALIZED = ImageLayout 8

-- | 'IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT' /must/ only be used
-- as either a color attachment or depth\/stencil attachment in a
-- 'Vulkan.Core10.Handles.Framebuffer' and\/or read-only access in a shader
-- as a sampled image, combined image\/sampler, or input attachment. This
-- layout is valid only for image subresources of images created with the
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
-- usage bit enabled and either the
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
-- or
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
-- and either the
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
-- or 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT'
-- usage bits enabled.
pattern IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT = ImageLayout 1000339000

-- | 'IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR' /must/ only
-- be used as a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment fragment shading rate attachment>
-- or
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-shading-rate-image shading rate image>.
-- This layout is valid only for image subresources of images created with
-- the
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
-- usage bit enabled.
pattern IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR = ImageLayout 1000164003

-- | 'IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT' /must/ only be used as a
-- fragment density map attachment in a 'Vulkan.Core10.Handles.RenderPass'.
-- This layout is valid only for image subresources of images created with
-- the
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT'
-- usage bit enabled.
pattern IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT = ImageLayout 1000218000

-- | 'IMAGE_LAYOUT_SHARED_PRESENT_KHR' is valid only for shared presentable
-- images, and /must/ be used for any usage the image supports.
pattern IMAGE_LAYOUT_SHARED_PRESENT_KHR = ImageLayout 1000111000

-- | 'IMAGE_LAYOUT_PRESENT_SRC_KHR' /must/ only be used for presenting a
-- presentable image for display.
pattern IMAGE_LAYOUT_PRESENT_SRC_KHR = ImageLayout 1000001002

-- | 'IMAGE_LAYOUT_ATTACHMENT_OPTIMAL' specifies a layout that /must/ only be
-- used with attachment accesses in the graphics pipeline.
pattern IMAGE_LAYOUT_ATTACHMENT_OPTIMAL = ImageLayout 1000314001

-- | 'IMAGE_LAYOUT_READ_ONLY_OPTIMAL' specifies a layout allowing read only
-- access as an attachment, or in shaders as a sampled image, combined
-- image\/sampler, or input attachment.
pattern IMAGE_LAYOUT_READ_ONLY_OPTIMAL = ImageLayout 1000314000

-- | 'IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL' specifies a layout for the
-- stencil aspect of a depth\/stencil format image allowing read-only
-- access as a stencil attachment or in shaders as a sampled image,
-- combined image\/sampler, or input attachment.
pattern IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL = ImageLayout 1000241003

-- | 'IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL' specifies a layout for the
-- stencil aspect of a depth\/stencil format image allowing read and write
-- access as a stencil attachment.
pattern IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL = ImageLayout 1000241002

-- | 'IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL' specifies a layout for the depth
-- aspect of a depth\/stencil format image allowing read-only access as a
-- depth attachment or in shaders as a sampled image, combined
-- image\/sampler, or input attachment.
pattern IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL = ImageLayout 1000241001

-- | 'IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL' specifies a layout for the depth
-- aspect of a depth\/stencil format image allowing read and write access
-- as a depth attachment.
pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL = ImageLayout 1000241000

-- | 'IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL' specifies a
-- layout for depth\/stencil format images allowing read and write access
-- to the depth aspect as a depth attachment, and read only access to the
-- stencil aspect as a stencil attachment or in shaders as a sampled image,
-- combined image\/sampler, or input attachment. It is equivalent to
-- 'IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL' and
-- 'IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'.
pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL = ImageLayout 1000117001

-- | 'IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL' specifies a
-- layout for depth\/stencil format images allowing read and write access
-- to the stencil aspect as a stencil attachment, and read only access to
-- the depth aspect as a depth attachment or in shaders as a sampled image,
-- combined image\/sampler, or input attachment. It is equivalent to
-- 'IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL' and
-- 'IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'.
pattern IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL = ImageLayout 1000117000

{-# COMPLETE
  IMAGE_LAYOUT_UNDEFINED
  , IMAGE_LAYOUT_GENERAL
  , IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
  , IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
  , IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
  , IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
  , IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
  , IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
  , IMAGE_LAYOUT_PREINITIALIZED
  , IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT
  , IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR
  , IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT
  , IMAGE_LAYOUT_SHARED_PRESENT_KHR
  , IMAGE_LAYOUT_PRESENT_SRC_KHR
  , IMAGE_LAYOUT_ATTACHMENT_OPTIMAL
  , IMAGE_LAYOUT_READ_ONLY_OPTIMAL
  , IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL
  , IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL
  , IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL
  , IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL
  , IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
  , IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL ::
    ImageLayout
  #-}

conNameImageLayout :: String
conNameImageLayout = "ImageLayout"

enumPrefixImageLayout :: String
enumPrefixImageLayout = "IMAGE_LAYOUT_"

showTableImageLayout :: [(ImageLayout, String)]
showTableImageLayout =
  [ (IMAGE_LAYOUT_UNDEFINED, "UNDEFINED")
  , (IMAGE_LAYOUT_GENERAL, "GENERAL")
  ,
    ( IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    , "COLOR_ATTACHMENT_OPTIMAL"
    )
  ,
    ( IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
    , "DEPTH_STENCIL_ATTACHMENT_OPTIMAL"
    )
  ,
    ( IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
    , "DEPTH_STENCIL_READ_ONLY_OPTIMAL"
    )
  ,
    ( IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    , "SHADER_READ_ONLY_OPTIMAL"
    )
  ,
    ( IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
    , "TRANSFER_SRC_OPTIMAL"
    )
  ,
    ( IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    , "TRANSFER_DST_OPTIMAL"
    )
  , (IMAGE_LAYOUT_PREINITIALIZED, "PREINITIALIZED")
  ,
    ( IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT
    , "ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT"
    )
  ,
    ( IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR
    , "FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR"
    )
  ,
    ( IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT
    , "FRAGMENT_DENSITY_MAP_OPTIMAL_EXT"
    )
  , (IMAGE_LAYOUT_SHARED_PRESENT_KHR, "SHARED_PRESENT_KHR")
  , (IMAGE_LAYOUT_PRESENT_SRC_KHR, "PRESENT_SRC_KHR")
  , (IMAGE_LAYOUT_ATTACHMENT_OPTIMAL, "ATTACHMENT_OPTIMAL")
  , (IMAGE_LAYOUT_READ_ONLY_OPTIMAL, "READ_ONLY_OPTIMAL")
  ,
    ( IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL
    , "STENCIL_READ_ONLY_OPTIMAL"
    )
  ,
    ( IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL
    , "STENCIL_ATTACHMENT_OPTIMAL"
    )
  ,
    ( IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL
    , "DEPTH_READ_ONLY_OPTIMAL"
    )
  ,
    ( IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL
    , "DEPTH_ATTACHMENT_OPTIMAL"
    )
  ,
    ( IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
    , "DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL"
    )
  ,
    ( IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
    , "DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL"
    )
  ]

instance Show ImageLayout where
  showsPrec =
    enumShowsPrec
      enumPrefixImageLayout
      showTableImageLayout
      conNameImageLayout
      (\(ImageLayout x) -> x)
      (showsPrec 11)

instance Read ImageLayout where
  readPrec =
    enumReadPrec
      enumPrefixImageLayout
      showTableImageLayout
      conNameImageLayout
      ImageLayout
