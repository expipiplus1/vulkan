{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.ImageLayout  (ImageLayout( IMAGE_LAYOUT_UNDEFINED
                                                             , IMAGE_LAYOUT_GENERAL
                                                             , IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                             , IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                             , IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
                                                             , IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                                                             , IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
                                                             , IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                                                             , IMAGE_LAYOUT_PREINITIALIZED
                                                             , IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT
                                                             , IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV
                                                             , IMAGE_LAYOUT_SHARED_PRESENT_KHR
                                                             , IMAGE_LAYOUT_PRESENT_SRC_KHR
                                                             , IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL
                                                             , IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL
                                                             , IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL
                                                             , IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL
                                                             , IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
                                                             , IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
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
import Graphics.Vulkan.Zero (Zero)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#clears>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies>).
-- For use as a framebuffer attachment, this is a member in the
-- substructures of the 'Graphics.Vulkan.Core10.Pass.RenderPassCreateInfo'
-- (see
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass Render Pass>).
-- For use in a descriptor set, this is a member in the
-- 'Graphics.Vulkan.Core10.DescriptorSet.DescriptorImageInfo' structure
-- (see
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-updates>).
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pass.AttachmentDescription',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.AttachmentDescription2',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout',
-- 'Graphics.Vulkan.Core10.Pass.AttachmentReference',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.AttachmentReference2',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentReferenceStencilLayout',
-- 'Graphics.Vulkan.Core10.DescriptorSet.DescriptorImageInfo',
-- 'Graphics.Vulkan.Core10.Image.ImageCreateInfo',
-- 'Graphics.Vulkan.Core10.OtherTypes.ImageMemoryBarrier',
-- 'Graphics.Vulkan.Extensions.VK_NV_shading_rate_image.cmdBindShadingRateImageNV',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBlitImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdClearColorImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdClearDepthStencilImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyBufferToImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyImageToBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdResolveImage'
newtype ImageLayout = ImageLayout Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'IMAGE_LAYOUT_UNDEFINED' does not support device access. This layout
-- /must/ only be used as the @initialLayout@ member of
-- 'Graphics.Vulkan.Core10.Image.ImageCreateInfo' or
-- 'Graphics.Vulkan.Core10.Pass.AttachmentDescription', or as the
-- @oldLayout@ in an image transition. When transitioning out of this
-- layout, the contents of the memory are not guaranteed to be preserved.
pattern IMAGE_LAYOUT_UNDEFINED = ImageLayout 0
-- | 'IMAGE_LAYOUT_GENERAL' supports all types of device access.
pattern IMAGE_LAYOUT_GENERAL = ImageLayout 1
-- | 'IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL' /must/ only be used as a color
-- or resolve attachment in a 'Graphics.Vulkan.Core10.Handles.Framebuffer'.
-- This layout is valid only for image subresources of images created with
-- the
-- 'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
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
-- allowing read only access as a depth\/stencil attachment or in shaders.
-- It is equivalent to 'IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL' and
-- 'IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'.
pattern IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL = ImageLayout 4
-- | 'IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL' /must/ only be used as a
-- read-only image in a shader (which /can/ be read as a sampled image,
-- combined image\/sampler and\/or input attachment). This layout is valid
-- only for image subresources of images created with the
-- 'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT'
-- or
-- 'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
-- usage bit enabled.
pattern IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = ImageLayout 5
-- | 'IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL' /must/ only be used as a source
-- image of a transfer command (see the definition of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-transfer >).
-- This layout is valid only for image subresources of images created with
-- the
-- 'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
-- usage bit enabled.
pattern IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL = ImageLayout 6
-- | 'IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL' /must/ only be used as a destination
-- image of a transfer command. This layout is valid only for image
-- subresources of images created with the
-- 'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
-- usage bit enabled.
pattern IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL = ImageLayout 7
-- | 'IMAGE_LAYOUT_PREINITIALIZED' does not support device access. This
-- layout /must/ only be used as the @initialLayout@ member of
-- 'Graphics.Vulkan.Core10.Image.ImageCreateInfo' or
-- 'Graphics.Vulkan.Core10.Pass.AttachmentDescription', or as the
-- @oldLayout@ in an image transition. When transitioning out of this
-- layout, the contents of the memory are preserved. This layout is
-- intended to be used as the initial layout for an image whose contents
-- are written by the host, and hence the data /can/ be written to memory
-- immediately, without first executing a layout transition. Currently,
-- 'IMAGE_LAYOUT_PREINITIALIZED' is only useful with
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#glossary-linear-resource linear>
-- images because there is not a standard layout defined for
-- 'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL' images.
pattern IMAGE_LAYOUT_PREINITIALIZED = ImageLayout 8
-- | 'IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT' /must/ only be used as a
-- fragment density map attachment in a
-- 'Graphics.Vulkan.Core10.Handles.RenderPass'. This layout is valid only
-- for image subresources of images created with the
-- 'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT'
-- usage bit enabled.
pattern IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT = ImageLayout 1000218000
-- | 'IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV' /must/ only be used as a
-- read-only
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-shading-rate-image shading-rate-image>.
-- This layout is valid only for image subresources of images created with
-- the
-- 'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV'
-- usage bit enabled.
pattern IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV = ImageLayout 1000164003
-- | 'IMAGE_LAYOUT_SHARED_PRESENT_KHR' is valid only for shared presentable
-- images, and /must/ be used for any usage the image supports.
pattern IMAGE_LAYOUT_SHARED_PRESENT_KHR = ImageLayout 1000111000
-- | 'IMAGE_LAYOUT_PRESENT_SRC_KHR' /must/ only be used for presenting a
-- presentable image for display. A swapchain’s image /must/ be
-- transitioned to this layout before calling
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR', and
-- /must/ be transitioned away from this layout after calling
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.acquireNextImageKHR'.
pattern IMAGE_LAYOUT_PRESENT_SRC_KHR = ImageLayout 1000001002
-- | 'IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL' specifies a layout for the
-- stencil aspect of a depth\/stencil format image allowing read-only
-- access as a stencil attachment or in shaders.
pattern IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL = ImageLayout 1000241003
-- | 'IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL' specifies a layout for the
-- stencil aspect of a depth\/stencil format image allowing read and write
-- access as a stencil attachment.
pattern IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL = ImageLayout 1000241002
-- | 'IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL' specifies a layout for the depth
-- aspect of a depth\/stencil format image allowing read-only access as a
-- depth attachment or in shaders.
pattern IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL = ImageLayout 1000241001
-- | 'IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL' specifies a layout for the depth
-- aspect of a depth\/stencil format image allowing read and write access
-- as a depth attachment.
pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL = ImageLayout 1000241000
-- | 'IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL' specifies a
-- layout for depth\/stencil format images allowing read and write access
-- to the depth aspect as a depth attachment, and read only access to the
-- stencil aspect as a stencil attachment or in shaders. It is equivalent
-- to 'IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL' and
-- 'IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'.
pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL = ImageLayout 1000117001
-- | 'IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL' specifies a
-- layout for depth\/stencil format images allowing read and write access
-- to the stencil aspect as a stencil attachment, and read only access to
-- the depth aspect as a depth attachment or in shaders. It is equivalent
-- to 'IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL' and
-- 'IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'.
pattern IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL = ImageLayout 1000117000
{-# complete IMAGE_LAYOUT_UNDEFINED,
             IMAGE_LAYOUT_GENERAL,
             IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
             IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
             IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,
             IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
             IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
             IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
             IMAGE_LAYOUT_PREINITIALIZED,
             IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT,
             IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV,
             IMAGE_LAYOUT_SHARED_PRESENT_KHR,
             IMAGE_LAYOUT_PRESENT_SRC_KHR,
             IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL,
             IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL,
             IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL,
             IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL,
             IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL,
             IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL :: ImageLayout #-}

instance Show ImageLayout where
  showsPrec p = \case
    IMAGE_LAYOUT_UNDEFINED -> showString "IMAGE_LAYOUT_UNDEFINED"
    IMAGE_LAYOUT_GENERAL -> showString "IMAGE_LAYOUT_GENERAL"
    IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL -> showString "IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL"
    IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL -> showString "IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL"
    IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL -> showString "IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL"
    IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL -> showString "IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL"
    IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL -> showString "IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL"
    IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL -> showString "IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL"
    IMAGE_LAYOUT_PREINITIALIZED -> showString "IMAGE_LAYOUT_PREINITIALIZED"
    IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT -> showString "IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT"
    IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV -> showString "IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV"
    IMAGE_LAYOUT_SHARED_PRESENT_KHR -> showString "IMAGE_LAYOUT_SHARED_PRESENT_KHR"
    IMAGE_LAYOUT_PRESENT_SRC_KHR -> showString "IMAGE_LAYOUT_PRESENT_SRC_KHR"
    IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL -> showString "IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL"
    IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL -> showString "IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL"
    IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL -> showString "IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL"
    IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL -> showString "IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL"
    IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL -> showString "IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL"
    IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL -> showString "IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL"
    ImageLayout x -> showParen (p >= 11) (showString "ImageLayout " . showsPrec 11 x)

instance Read ImageLayout where
  readPrec = parens (choose [("IMAGE_LAYOUT_UNDEFINED", pure IMAGE_LAYOUT_UNDEFINED)
                            , ("IMAGE_LAYOUT_GENERAL", pure IMAGE_LAYOUT_GENERAL)
                            , ("IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL", pure IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL)
                            , ("IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL", pure IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL)
                            , ("IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL", pure IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL)
                            , ("IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL", pure IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)
                            , ("IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL", pure IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL)
                            , ("IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL", pure IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL)
                            , ("IMAGE_LAYOUT_PREINITIALIZED", pure IMAGE_LAYOUT_PREINITIALIZED)
                            , ("IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT", pure IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT)
                            , ("IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV", pure IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV)
                            , ("IMAGE_LAYOUT_SHARED_PRESENT_KHR", pure IMAGE_LAYOUT_SHARED_PRESENT_KHR)
                            , ("IMAGE_LAYOUT_PRESENT_SRC_KHR", pure IMAGE_LAYOUT_PRESENT_SRC_KHR)
                            , ("IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL", pure IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL)
                            , ("IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL", pure IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL)
                            , ("IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL", pure IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL)
                            , ("IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL", pure IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL)
                            , ("IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL", pure IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL)
                            , ("IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL", pure IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL)]
                     +++
                     prec 10 (do
                       expectP (Ident "ImageLayout")
                       v <- step readPrec
                       pure (ImageLayout v)))

