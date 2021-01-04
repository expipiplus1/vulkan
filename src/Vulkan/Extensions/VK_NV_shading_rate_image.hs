{-# language CPP #-}
-- | = Name
--
-- VK_NV_shading_rate_image - device extension
--
-- == VK_NV_shading_rate_image
--
-- [__Name String__]
--     @VK_NV_shading_rate_image@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     165
--
-- [__Revision__]
--     3
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Pat Brown
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NV_shading_rate_image:%20&body=@nvpbrown%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-07-18
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_shading_rate.html SPV_NV_shading_rate>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nv/GLSL_NV_shading_rate_image.txt GL_NV_shading_rate_image>
--
-- [__Contributors__]
--
--     -   Pat Brown, NVIDIA
--
--     -   Carsten Rohde, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Mathias Schott, NVIDIA
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc.
--
-- == Description
--
-- This extension allows applications to use a variable shading rate when
-- processing fragments of rasterized primitives. By default, Vulkan will
-- spawn one fragment shader for each pixel covered by a primitive. In this
-- extension, applications can bind a /shading rate image/ that can be used
-- to vary the number of fragment shader invocations across the
-- framebuffer. Some portions of the screen may be configured to spawn up
-- to 16 fragment shaders for each pixel, while other portions may use a
-- single fragment shader invocation for a 4x4 block of pixels. This can be
-- useful for use cases like eye tracking, where the portion of the
-- framebuffer that the user is looking at directly can be processed at
-- high frequency, while distant corners of the image can be processed at
-- lower frequency. Each texel in the shading rate image represents a
-- fixed-size rectangle in the framebuffer, covering 16x16 pixels in the
-- initial implementation of this extension. When rasterizing a primitive
-- covering one of these rectangles, the Vulkan implementation reads a
-- texel in the bound shading rate image and looks up the fetched value in
-- a palette to determine a base shading rate.
--
-- In addition to the API support controlling rasterization, this extension
-- also adds Vulkan support for the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_shading_rate.html SPV_NV_shading_rate>
-- extension to SPIR-V. That extension provides two fragment shader
-- variable decorations that allow fragment shaders to determine the
-- shading rate used for processing the fragment:
--
-- -   @FragmentSizeNV@, which indicates the width and height of the set of
--     pixels processed by the fragment shader.
--
-- -   @InvocationsPerPixel@, which indicates the maximum number of
--     fragment shader invocations that could be spawned for the pixel(s)
--     covered by the fragment.
--
-- When using SPIR-V in conjunction with the OpenGL Shading Language
-- (GLSL), the fragment shader capabilities are provided by the
-- @GL_NV_shading_rate_image@ language extension and correspond to the
-- built-in variables @gl_FragmentSizeNV@ and @gl_InvocationsPerPixelNV@,
-- respectively.
--
-- == New Commands
--
-- -   'cmdBindShadingRateImageNV'
--
-- -   'cmdSetCoarseSampleOrderNV'
--
-- -   'cmdSetViewportShadingRatePaletteNV'
--
-- == New Structures
--
-- -   'CoarseSampleLocationNV'
--
-- -   'CoarseSampleOrderCustomNV'
--
-- -   'ShadingRatePaletteNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShadingRateImageFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShadingRateImagePropertiesNV'
--
-- -   Extending 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo':
--
--     -   'PipelineViewportCoarseSampleOrderStateCreateInfoNV'
--
--     -   'PipelineViewportShadingRateImageStateCreateInfoNV'
--
-- == New Enums
--
-- -   'CoarseSampleOrderTypeNV'
--
-- -   'ShadingRatePaletteEntryNV'
--
-- == New Enum Constants
--
-- -   'NV_SHADING_RATE_IMAGE_EXTENSION_NAME'
--
-- -   'NV_SHADING_RATE_IMAGE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits':
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.ImageLayout.ImageLayout':
--
--     -   'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV'
--
-- == Issues
--
-- (1) When using shading rates specifying “coarse” fragments covering
-- multiple pixels, we will generate a combined coverage mask that combines
-- the coverage masks of all pixels covered by the fragment. By default,
-- these masks are combined in an implementation-dependent order. Should we
-- provide a mechanism allowing applications to query or specify an exact
-- order?
--
-- __RESOLVED__: Yes, this feature is useful for cases where most of the
-- fragment shader can be evaluated once for an entire coarse fragment, but
-- where some per-pixel computations are also required. For example, a
-- per-pixel alpha test may want to kill all the samples for some pixels in
-- a coarse fragment. This sort of test can be implemented using an output
-- sample mask, but such a shader would need to know which bit in the mask
-- corresponds to each sample in the coarse fragment. We are including a
-- mechanism to allow aplications to specify the orders of coverage samples
-- for each shading rate and sample count, either as static pipeline state
-- or dynamically via a command buffer. This portion of the extension has
-- its own feature bit.
--
-- We will not be providing a query to determine the
-- implementation-dependent default ordering. The thinking here is that if
-- an application cares enough about the coarse fragment sample ordering to
-- perform such a query, it could instead just set its own order, also
-- using custom per-pixel sample locations if required.
--
-- (2) For the pipeline stage
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV',
-- should we specify a precise location in the pipeline the shading rate
-- image is accessed (after geometry shading, but before the early fragment
-- tests) or leave it under-specified in case there are other
-- implementations that access the image in a different pipeline location?
--
-- __RESOLVED__ We are specifying the pipeline stage to be between the
-- final stage used for vertex processing
-- ('Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT')
-- and before the first stage used for fragment processing
-- ('Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT'),
-- which seems to be the natural place to access the shading rate image.
--
-- (3) How do centroid-sampled variables work with fragments larger than
-- one pixel?
--
-- __RESOLVED__ For single-pixel fragments, fragment shader inputs
-- decorated with @Centroid@ are sampled at an implementation-dependent
-- location in the intersection of the area of the primitive being
-- rasterized and the area of the pixel that corresponds to the fragment.
-- With multi-pixel fragments, we follow a similar pattern, using the
-- intersection of the primitive and the __set__ of pixels corresponding to
-- the fragment.
--
-- One important thing to keep in mind when using such “coarse” shading
-- rates is that fragment attributes are sampled at the center of the
-- fragment by default, regardless of the set of pixels\/samples covered by
-- the fragment. For fragments with a size of 4x4 pixels, this center
-- location will be more than two pixels (1.5 * sqrt(2)) away from the
-- center of the pixels at the corners of the fragment. When rendering a
-- primitive that covers only a small part of a coarse fragment, sampling a
-- color outside the primitive can produce overly bright or dark color
-- values if the color values have a large gradient. To deal with this, an
-- application can use centroid sampling on attributes where
-- “extrapolation” artifacts can lead to overly bright or dark pixels. Note
-- that this same problem also exists for multisampling with single-pixel
-- fragments, but is less severe because it only affects certain samples of
-- a pixel and such bright\/dark samples may be averaged with other samples
-- that don’t have a similar problem.
--
-- == Version History
--
-- -   Revision 3, 2019-07-18 (Mathias Schott)
--
--     -   Fully list extension interfaces in this appendix.
--
-- -   Revision 2, 2018-09-13 (Pat Brown)
--
--     -   Miscellaneous edits preparing the specification for publication.
--
-- -   Revision 1, 2018-08-08 (Pat Brown)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'CoarseSampleLocationNV', 'CoarseSampleOrderCustomNV',
-- 'CoarseSampleOrderTypeNV', 'PhysicalDeviceShadingRateImageFeaturesNV',
-- 'PhysicalDeviceShadingRateImagePropertiesNV',
-- 'PipelineViewportCoarseSampleOrderStateCreateInfoNV',
-- 'PipelineViewportShadingRateImageStateCreateInfoNV',
-- 'ShadingRatePaletteEntryNV', 'ShadingRatePaletteNV',
-- 'cmdBindShadingRateImageNV', 'cmdSetCoarseSampleOrderNV',
-- 'cmdSetViewportShadingRatePaletteNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_shading_rate_image Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_shading_rate_image  ( cmdBindShadingRateImageNV
                                                   , cmdSetViewportShadingRatePaletteNV
                                                   , cmdSetCoarseSampleOrderNV
                                                   , ShadingRatePaletteNV(..)
                                                   , PipelineViewportShadingRateImageStateCreateInfoNV(..)
                                                   , PhysicalDeviceShadingRateImageFeaturesNV(..)
                                                   , PhysicalDeviceShadingRateImagePropertiesNV(..)
                                                   , CoarseSampleLocationNV(..)
                                                   , CoarseSampleOrderCustomNV(..)
                                                   , PipelineViewportCoarseSampleOrderStateCreateInfoNV(..)
                                                   , ShadingRatePaletteEntryNV( SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV
                                                                              , ..
                                                                              )
                                                   , CoarseSampleOrderTypeNV( COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV
                                                                            , COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV
                                                                            , COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV
                                                                            , COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV
                                                                            , ..
                                                                            )
                                                   , NV_SHADING_RATE_IMAGE_SPEC_VERSION
                                                   , pattern NV_SHADING_RATE_IMAGE_SPEC_VERSION
                                                   , NV_SHADING_RATE_IMAGE_EXTENSION_NAME
                                                   , pattern NV_SHADING_RATE_IMAGE_EXTENSION_NAME
                                                   ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindShadingRateImageNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetCoarseSampleOrderNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetViewportShadingRatePaletteNV))
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(..))
import Vulkan.Core10.Handles (ImageView)
import Vulkan.Core10.Handles (ImageView(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindShadingRateImageNV
  :: FunPtr (Ptr CommandBuffer_T -> ImageView -> ImageLayout -> IO ()) -> Ptr CommandBuffer_T -> ImageView -> ImageLayout -> IO ()

-- | vkCmdBindShadingRateImageNV - Bind a shading rate image on a command
-- buffer
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBindShadingRateImageNV-None-02058# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdBindShadingRateImageNV-imageView-02059# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', it /must/ be a valid
--     'Vulkan.Core10.Handles.ImageView' handle of type
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'
--
-- -   #VUID-vkCmdBindShadingRateImageNV-imageView-02060# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', it /must/ have a
--     format of 'Vulkan.Core10.Enums.Format.FORMAT_R8_UINT'
--
-- -   #VUID-vkCmdBindShadingRateImageNV-imageView-02061# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', it /must/ have been
--     created with a @usage@ value including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   #VUID-vkCmdBindShadingRateImageNV-imageView-02062# If @imageView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageLayout@ /must/
--     match the actual 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' of
--     each subresource accessible from @imageView@ at the time the
--     subresource is accessed
--
-- -   #VUID-vkCmdBindShadingRateImageNV-imageLayout-02063# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageLayout@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV'
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindShadingRateImageNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindShadingRateImageNV-imageView-parameter# If
--     @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @imageView@ /must/ be a valid 'Vulkan.Core10.Handles.ImageView'
--     handle
--
-- -   #VUID-vkCmdBindShadingRateImageNV-imageLayout-parameter#
--     @imageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-vkCmdBindShadingRateImageNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindShadingRateImageNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBindShadingRateImageNV-commonparent# Both of
--     @commandBuffer@, and @imageView@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Handles.ImageView'
cmdBindShadingRateImageNV :: forall io
                           . (MonadIO io)
                          => -- | @commandBuffer@ is the command buffer into which the command will be
                             -- recorded.
                             CommandBuffer
                          -> -- | @imageView@ is an image view handle specifying the shading rate image.
                             -- @imageView@ /may/ be set to 'Vulkan.Core10.APIConstants.NULL_HANDLE',
                             -- which is equivalent to specifying a view of an image filled with zero
                             -- values.
                             ImageView
                          -> -- | @imageLayout@ is the layout that the image subresources accessible from
                             -- @imageView@ will be in when the shading rate image is accessed.
                             ImageLayout
                          -> io ()
cmdBindShadingRateImageNV commandBuffer imageView imageLayout = liftIO $ do
  let vkCmdBindShadingRateImageNVPtr = pVkCmdBindShadingRateImageNV (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdBindShadingRateImageNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindShadingRateImageNV is null" Nothing Nothing
  let vkCmdBindShadingRateImageNV' = mkVkCmdBindShadingRateImageNV vkCmdBindShadingRateImageNVPtr
  traceAroundEvent "vkCmdBindShadingRateImageNV" (vkCmdBindShadingRateImageNV' (commandBufferHandle (commandBuffer)) (imageView) (imageLayout))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetViewportShadingRatePaletteNV
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr ShadingRatePaletteNV -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr ShadingRatePaletteNV -> IO ()

-- | vkCmdSetViewportShadingRatePaletteNV - Set shading rate image palettes
-- on a command buffer
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetViewportShadingRatePaletteNV-None-02064# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdSetViewportShadingRatePaletteNV-firstViewport-02067# The
--     sum of @firstViewport@ and @viewportCount@ /must/ be between @1@ and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   #VUID-vkCmdSetViewportShadingRatePaletteNV-firstViewport-02068# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @firstViewport@ /must/ be @0@
--
-- -   #VUID-vkCmdSetViewportShadingRatePaletteNV-viewportCount-02069# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @viewportCount@ /must/ be @1@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetViewportShadingRatePaletteNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetViewportShadingRatePaletteNV-pShadingRatePalettes-parameter#
--     @pShadingRatePalettes@ /must/ be a valid pointer to an array of
--     @viewportCount@ valid 'ShadingRatePaletteNV' structures
--
-- -   #VUID-vkCmdSetViewportShadingRatePaletteNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetViewportShadingRatePaletteNV-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetViewportShadingRatePaletteNV-viewportCount-arraylength#
--     @viewportCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'ShadingRatePaletteNV'
cmdSetViewportShadingRatePaletteNV :: forall io
                                    . (MonadIO io)
                                   => -- | @commandBuffer@ is the command buffer into which the command will be
                                      -- recorded.
                                      CommandBuffer
                                   -> -- | @firstViewport@ is the index of the first viewport whose shading rate
                                      -- palette is updated by the command.
                                      ("firstViewport" ::: Word32)
                                   -> -- | @pShadingRatePalettes@ is a pointer to an array of
                                      -- 'ShadingRatePaletteNV' structures defining the palette for each
                                      -- viewport.
                                      ("shadingRatePalettes" ::: Vector ShadingRatePaletteNV)
                                   -> io ()
cmdSetViewportShadingRatePaletteNV commandBuffer firstViewport shadingRatePalettes = liftIO . evalContT $ do
  let vkCmdSetViewportShadingRatePaletteNVPtr = pVkCmdSetViewportShadingRatePaletteNV (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetViewportShadingRatePaletteNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetViewportShadingRatePaletteNV is null" Nothing Nothing
  let vkCmdSetViewportShadingRatePaletteNV' = mkVkCmdSetViewportShadingRatePaletteNV vkCmdSetViewportShadingRatePaletteNVPtr
  pPShadingRatePalettes <- ContT $ allocaBytesAligned @ShadingRatePaletteNV ((Data.Vector.length (shadingRatePalettes)) * 16) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPShadingRatePalettes `plusPtr` (16 * (i)) :: Ptr ShadingRatePaletteNV) (e) . ($ ())) (shadingRatePalettes)
  lift $ traceAroundEvent "vkCmdSetViewportShadingRatePaletteNV" (vkCmdSetViewportShadingRatePaletteNV' (commandBufferHandle (commandBuffer)) (firstViewport) ((fromIntegral (Data.Vector.length $ (shadingRatePalettes)) :: Word32)) (pPShadingRatePalettes))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetCoarseSampleOrderNV
  :: FunPtr (Ptr CommandBuffer_T -> CoarseSampleOrderTypeNV -> Word32 -> Ptr CoarseSampleOrderCustomNV -> IO ()) -> Ptr CommandBuffer_T -> CoarseSampleOrderTypeNV -> Word32 -> Ptr CoarseSampleOrderCustomNV -> IO ()

-- | vkCmdSetCoarseSampleOrderNV - Set sample order for coarse fragments on a
-- command buffer
--
-- = Description
--
-- If @sampleOrderType@ is 'COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV', the
-- coverage sample order used for any combination of fragment area and
-- coverage sample count not enumerated in @pCustomSampleOrders@ will be
-- identical to that used for 'COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV'.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetCoarseSampleOrderNV-sampleOrderType-02081# If
--     @sampleOrderType@ is not 'COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV',
--     @customSamplerOrderCount@ /must/ be @0@
--
-- -   #VUID-vkCmdSetCoarseSampleOrderNV-pCustomSampleOrders-02235# The
--     array @pCustomSampleOrders@ /must/ not contain two structures with
--     matching values for both the @shadingRate@ and @sampleCount@ members
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetCoarseSampleOrderNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetCoarseSampleOrderNV-sampleOrderType-parameter#
--     @sampleOrderType@ /must/ be a valid 'CoarseSampleOrderTypeNV' value
--
-- -   #VUID-vkCmdSetCoarseSampleOrderNV-pCustomSampleOrders-parameter# If
--     @customSampleOrderCount@ is not @0@, @pCustomSampleOrders@ /must/ be
--     a valid pointer to an array of @customSampleOrderCount@ valid
--     'CoarseSampleOrderCustomNV' structures
--
-- -   #VUID-vkCmdSetCoarseSampleOrderNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetCoarseSampleOrderNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'CoarseSampleOrderCustomNV', 'CoarseSampleOrderTypeNV',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetCoarseSampleOrderNV :: forall io
                           . (MonadIO io)
                          => -- | @commandBuffer@ is the command buffer into which the command will be
                             -- recorded.
                             CommandBuffer
                          -> -- | @sampleOrderType@ specifies the mechanism used to order coverage samples
                             -- in fragments larger than one pixel.
                             CoarseSampleOrderTypeNV
                          -> -- | @pCustomSampleOrders@ is a pointer to an array of
                             -- 'CoarseSampleOrderCustomNV' structures, each of which specifies the
                             -- coverage sample order for a single combination of fragment area and
                             -- coverage sample count.
                             ("customSampleOrders" ::: Vector CoarseSampleOrderCustomNV)
                          -> io ()
cmdSetCoarseSampleOrderNV commandBuffer sampleOrderType customSampleOrders = liftIO . evalContT $ do
  let vkCmdSetCoarseSampleOrderNVPtr = pVkCmdSetCoarseSampleOrderNV (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetCoarseSampleOrderNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetCoarseSampleOrderNV is null" Nothing Nothing
  let vkCmdSetCoarseSampleOrderNV' = mkVkCmdSetCoarseSampleOrderNV vkCmdSetCoarseSampleOrderNVPtr
  pPCustomSampleOrders <- ContT $ allocaBytesAligned @CoarseSampleOrderCustomNV ((Data.Vector.length (customSampleOrders)) * 24) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPCustomSampleOrders `plusPtr` (24 * (i)) :: Ptr CoarseSampleOrderCustomNV) (e) . ($ ())) (customSampleOrders)
  lift $ traceAroundEvent "vkCmdSetCoarseSampleOrderNV" (vkCmdSetCoarseSampleOrderNV' (commandBufferHandle (commandBuffer)) (sampleOrderType) ((fromIntegral (Data.Vector.length $ (customSampleOrders)) :: Word32)) (pPCustomSampleOrders))
  pure $ ()


-- | VkShadingRatePaletteNV - Structure specifying a single shading rate
-- palette
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PipelineViewportShadingRateImageStateCreateInfoNV',
-- 'ShadingRatePaletteEntryNV', 'cmdSetViewportShadingRatePaletteNV'
data ShadingRatePaletteNV = ShadingRatePaletteNV
  { -- | @pShadingRatePaletteEntries@ is a pointer to an array of
    -- 'ShadingRatePaletteEntryNV' enums defining the shading rate for each
    -- palette entry.
    --
    -- #VUID-VkShadingRatePaletteNV-pShadingRatePaletteEntries-parameter#
    -- @pShadingRatePaletteEntries@ /must/ be a valid pointer to an array of
    -- @shadingRatePaletteEntryCount@ valid 'ShadingRatePaletteEntryNV' values
    shadingRatePaletteEntries :: Vector ShadingRatePaletteEntryNV }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ShadingRatePaletteNV)
#endif
deriving instance Show ShadingRatePaletteNV

instance ToCStruct ShadingRatePaletteNV where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ShadingRatePaletteNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (shadingRatePaletteEntries)) :: Word32))
    pPShadingRatePaletteEntries' <- ContT $ allocaBytesAligned @ShadingRatePaletteEntryNV ((Data.Vector.length (shadingRatePaletteEntries)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPShadingRatePaletteEntries' `plusPtr` (4 * (i)) :: Ptr ShadingRatePaletteEntryNV) (e)) (shadingRatePaletteEntries)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ShadingRatePaletteEntryNV))) (pPShadingRatePaletteEntries')
    lift $ f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    pPShadingRatePaletteEntries' <- ContT $ allocaBytesAligned @ShadingRatePaletteEntryNV ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPShadingRatePaletteEntries' `plusPtr` (4 * (i)) :: Ptr ShadingRatePaletteEntryNV) (e)) (mempty)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ShadingRatePaletteEntryNV))) (pPShadingRatePaletteEntries')
    lift $ f

instance FromCStruct ShadingRatePaletteNV where
  peekCStruct p = do
    shadingRatePaletteEntryCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pShadingRatePaletteEntries <- peek @(Ptr ShadingRatePaletteEntryNV) ((p `plusPtr` 8 :: Ptr (Ptr ShadingRatePaletteEntryNV)))
    pShadingRatePaletteEntries' <- generateM (fromIntegral shadingRatePaletteEntryCount) (\i -> peek @ShadingRatePaletteEntryNV ((pShadingRatePaletteEntries `advancePtrBytes` (4 * (i)) :: Ptr ShadingRatePaletteEntryNV)))
    pure $ ShadingRatePaletteNV
             pShadingRatePaletteEntries'

instance Zero ShadingRatePaletteNV where
  zero = ShadingRatePaletteNV
           mempty


-- | VkPipelineViewportShadingRateImageStateCreateInfoNV - Structure
-- specifying parameters controlling shading rate image usage
--
-- = Description
--
-- If this structure is not present, @shadingRateImageEnable@ is considered
-- to be 'Vulkan.Core10.FundamentalTypes.FALSE', and the shading rate image
-- and palettes are not used.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineViewportShadingRateImageStateCreateInfoNV-viewportCount-02054#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @viewportCount@ /must/ be @0@ or @1@
--
-- -   #VUID-VkPipelineViewportShadingRateImageStateCreateInfoNV-viewportCount-02055#
--     @viewportCount@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@
--
-- -   #VUID-VkPipelineViewportShadingRateImageStateCreateInfoNV-shadingRateImageEnable-02056#
--     If @shadingRateImageEnable@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', @viewportCount@ /must/ be
--     greater or equal to the @viewportCount@ member of
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineViewportShadingRateImageStateCreateInfoNV-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'ShadingRatePaletteNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineViewportShadingRateImageStateCreateInfoNV = PipelineViewportShadingRateImageStateCreateInfoNV
  { -- | @shadingRateImageEnable@ specifies whether shading rate image and
    -- palettes are used during rasterization.
    shadingRateImageEnable :: Bool
  , -- | @pShadingRatePalettes@ is a pointer to an array of
    -- 'ShadingRatePaletteNV' structures defining the palette for each
    -- viewport. If the shading rate palette state is dynamic, this member is
    -- ignored.
    shadingRatePalettes :: Vector ShadingRatePaletteNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineViewportShadingRateImageStateCreateInfoNV)
#endif
deriving instance Show PipelineViewportShadingRateImageStateCreateInfoNV

instance ToCStruct PipelineViewportShadingRateImageStateCreateInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineViewportShadingRateImageStateCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shadingRateImageEnable))
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (shadingRatePalettes)) :: Word32))
    pPShadingRatePalettes' <- ContT $ allocaBytesAligned @ShadingRatePaletteNV ((Data.Vector.length (shadingRatePalettes)) * 16) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPShadingRatePalettes' `plusPtr` (16 * (i)) :: Ptr ShadingRatePaletteNV) (e) . ($ ())) (shadingRatePalettes)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ShadingRatePaletteNV))) (pPShadingRatePalettes')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    pPShadingRatePalettes' <- ContT $ allocaBytesAligned @ShadingRatePaletteNV ((Data.Vector.length (mempty)) * 16) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPShadingRatePalettes' `plusPtr` (16 * (i)) :: Ptr ShadingRatePaletteNV) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ShadingRatePaletteNV))) (pPShadingRatePalettes')
    lift $ f

instance FromCStruct PipelineViewportShadingRateImageStateCreateInfoNV where
  peekCStruct p = do
    shadingRateImageEnable <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    viewportCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pShadingRatePalettes <- peek @(Ptr ShadingRatePaletteNV) ((p `plusPtr` 24 :: Ptr (Ptr ShadingRatePaletteNV)))
    pShadingRatePalettes' <- generateM (fromIntegral viewportCount) (\i -> peekCStruct @ShadingRatePaletteNV ((pShadingRatePalettes `advancePtrBytes` (16 * (i)) :: Ptr ShadingRatePaletteNV)))
    pure $ PipelineViewportShadingRateImageStateCreateInfoNV
             (bool32ToBool shadingRateImageEnable) pShadingRatePalettes'

instance Zero PipelineViewportShadingRateImageStateCreateInfoNV where
  zero = PipelineViewportShadingRateImageStateCreateInfoNV
           zero
           mempty


-- | VkPhysicalDeviceShadingRateImageFeaturesNV - Structure describing
-- shading rate image features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceShadingRateImageFeaturesNV' structure
-- describe the following features:
--
-- = Description
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-shading-rate-image Shading Rate Image>
-- for more information.
--
-- If the 'PhysicalDeviceShadingRateImageFeaturesNV' structure is included
-- in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceShadingRateImageFeaturesNV' /can/ also be included in the
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShadingRateImageFeaturesNV = PhysicalDeviceShadingRateImageFeaturesNV
  { -- | #features-shadingRateImage# @shadingRateImage@ indicates that the
    -- implementation supports the use of a shading rate image to derive an
    -- effective shading rate for fragment processing. It also indicates that
    -- the implementation supports the @ShadingRateNV@ SPIR-V execution mode.
    shadingRateImage :: Bool
  , -- | #features-shadingRateCoarseSampleOrder# @shadingRateCoarseSampleOrder@
    -- indicates that the implementation supports a user-configurable ordering
    -- of coverage samples in fragments larger than one pixel.
    shadingRateCoarseSampleOrder :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShadingRateImageFeaturesNV)
#endif
deriving instance Show PhysicalDeviceShadingRateImageFeaturesNV

instance ToCStruct PhysicalDeviceShadingRateImageFeaturesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShadingRateImageFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shadingRateImage))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (shadingRateCoarseSampleOrder))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShadingRateImageFeaturesNV where
  peekCStruct p = do
    shadingRateImage <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    shadingRateCoarseSampleOrder <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceShadingRateImageFeaturesNV
             (bool32ToBool shadingRateImage) (bool32ToBool shadingRateCoarseSampleOrder)

instance Storable PhysicalDeviceShadingRateImageFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShadingRateImageFeaturesNV where
  zero = PhysicalDeviceShadingRateImageFeaturesNV
           zero
           zero


-- | VkPhysicalDeviceShadingRateImagePropertiesNV - Structure describing
-- shading rate image limits that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceShadingRateImagePropertiesNV'
-- structure describe the following implementation-dependent properties
-- related to the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-shading-rate-image shading rate image>
-- feature:
--
-- = Description
--
-- If the 'PhysicalDeviceShadingRateImagePropertiesNV' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShadingRateImagePropertiesNV = PhysicalDeviceShadingRateImagePropertiesNV
  { -- | #limits-shading-rate-texel-size# @shadingRateTexelSize@ indicates the
    -- width and height of the portion of the framebuffer corresponding to each
    -- texel in the shading rate image.
    shadingRateTexelSize :: Extent2D
  , -- | #limits-shading-rate-palette-size# @shadingRatePaletteSize@ indicates
    -- the maximum number of palette entries supported for the shading rate
    -- image.
    shadingRatePaletteSize :: Word32
  , -- | #limits-shading-rate-max-coarse-samples# @shadingRateMaxCoarseSamples@
    -- specifies the maximum number of coverage samples supported in a single
    -- fragment. If the product of the fragment size derived from the base
    -- shading rate and the number of coverage samples per pixel exceeds this
    -- limit, the final shading rate will be adjusted so that its product does
    -- not exceed the limit.
    shadingRateMaxCoarseSamples :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShadingRateImagePropertiesNV)
#endif
deriving instance Show PhysicalDeviceShadingRateImagePropertiesNV

instance ToCStruct PhysicalDeviceShadingRateImagePropertiesNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShadingRateImagePropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (shadingRateTexelSize)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (shadingRatePaletteSize)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (shadingRateMaxCoarseSamples)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceShadingRateImagePropertiesNV where
  peekCStruct p = do
    shadingRateTexelSize <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    shadingRatePaletteSize <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    shadingRateMaxCoarseSamples <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pure $ PhysicalDeviceShadingRateImagePropertiesNV
             shadingRateTexelSize shadingRatePaletteSize shadingRateMaxCoarseSamples

instance Storable PhysicalDeviceShadingRateImagePropertiesNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShadingRateImagePropertiesNV where
  zero = PhysicalDeviceShadingRateImagePropertiesNV
           zero
           zero
           zero


-- | VkCoarseSampleLocationNV - Structure specifying parameters controlling
-- shading rate image usage
--
-- == Valid Usage
--
-- = See Also
--
-- 'CoarseSampleOrderCustomNV'
data CoarseSampleLocationNV = CoarseSampleLocationNV
  { -- | @pixelX@ is added to the x coordinate of the upper-leftmost pixel of
    -- each fragment to identify the pixel containing the coverage sample.
    --
    -- #VUID-VkCoarseSampleLocationNV-pixelX-02078# @pixelX@ /must/ be less
    -- than the width (in pixels) of the fragment
    pixelX :: Word32
  , -- | @pixelY@ is added to the y coordinate of the upper-leftmost pixel of
    -- each fragment to identify the pixel containing the coverage sample.
    --
    -- #VUID-VkCoarseSampleLocationNV-pixelY-02079# @pixelY@ /must/ be less
    -- than the height (in pixels) of the fragment
    pixelY :: Word32
  , -- | @sample@ is the number of the coverage sample in the pixel identified by
    -- @pixelX@ and @pixelY@.
    --
    -- #VUID-VkCoarseSampleLocationNV-sample-02080# @sample@ /must/ be less
    -- than the number of coverage samples in each pixel belonging to the
    -- fragment
    sample :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CoarseSampleLocationNV)
#endif
deriving instance Show CoarseSampleLocationNV

instance ToCStruct CoarseSampleLocationNV where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CoarseSampleLocationNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (pixelX)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (pixelY)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (sample)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct CoarseSampleLocationNV where
  peekCStruct p = do
    pixelX <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pixelY <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    sample <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ CoarseSampleLocationNV
             pixelX pixelY sample

instance Storable CoarseSampleLocationNV where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CoarseSampleLocationNV where
  zero = CoarseSampleLocationNV
           zero
           zero
           zero


-- | VkCoarseSampleOrderCustomNV - Structure specifying parameters
-- controlling shading rate image usage
--
-- = Description
--
-- When using a custom sample ordering, element /j/ in @pSampleLocations@
-- specifies a specific pixel location and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-multisampling-coverage-mask sample index>
-- that corresponds to
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-multisampling-coverage-mask coverage index>
-- /j/ in the multi-pixel fragment.
--
-- == Valid Usage
--
-- -   #VUID-VkCoarseSampleOrderCustomNV-shadingRate-02073# @shadingRate@
--     /must/ be a shading rate that generates fragments with more than one
--     pixel
--
-- -   #VUID-VkCoarseSampleOrderCustomNV-sampleCount-02074# @sampleCount@
--     /must/ correspond to a sample count enumerated in
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlags' whose
--     corresponding bit is set in
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@framebufferNoAttachmentsSampleCounts@
--
-- -   #VUID-VkCoarseSampleOrderCustomNV-sampleLocationCount-02075#
--     @sampleLocationCount@ /must/ be equal to the product of
--     @sampleCount@, the fragment width for @shadingRate@, and the
--     fragment height for @shadingRate@
--
-- -   #VUID-VkCoarseSampleOrderCustomNV-sampleLocationCount-02076#
--     @sampleLocationCount@ /must/ be less than or equal to the value of
--     'PhysicalDeviceShadingRateImagePropertiesNV'::@shadingRateMaxCoarseSamples@
--
-- -   #VUID-VkCoarseSampleOrderCustomNV-pSampleLocations-02077# The array
--     @pSampleLocations@ /must/ contain exactly one entry for every
--     combination of valid values for @pixelX@, @pixelY@, and @sample@ in
--     the structure 'CoarseSampleOrderCustomNV'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCoarseSampleOrderCustomNV-shadingRate-parameter#
--     @shadingRate@ /must/ be a valid 'ShadingRatePaletteEntryNV' value
--
-- -   #VUID-VkCoarseSampleOrderCustomNV-pSampleLocations-parameter#
--     @pSampleLocations@ /must/ be a valid pointer to an array of
--     @sampleLocationCount@ 'CoarseSampleLocationNV' structures
--
-- -   #VUID-VkCoarseSampleOrderCustomNV-sampleLocationCount-arraylength#
--     @sampleLocationCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'CoarseSampleLocationNV',
-- 'PipelineViewportCoarseSampleOrderStateCreateInfoNV',
-- 'ShadingRatePaletteEntryNV', 'cmdSetCoarseSampleOrderNV'
data CoarseSampleOrderCustomNV = CoarseSampleOrderCustomNV
  { -- | @shadingRate@ is a shading rate palette entry that identifies the
    -- fragment width and height for the combination of fragment area and
    -- per-pixel coverage sample count to control.
    shadingRate :: ShadingRatePaletteEntryNV
  , -- | @sampleCount@ identifies the per-pixel coverage sample count for the
    -- combination of fragment area and coverage sample count to control.
    sampleCount :: Word32
  , -- | @pSampleLocations@ is a pointer to an array of
    -- 'CoarseSampleOrderCustomNV' structures specifying the location of each
    -- sample in the custom ordering.
    sampleLocations :: Vector CoarseSampleLocationNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CoarseSampleOrderCustomNV)
#endif
deriving instance Show CoarseSampleOrderCustomNV

instance ToCStruct CoarseSampleOrderCustomNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CoarseSampleOrderCustomNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr ShadingRatePaletteEntryNV)) (shadingRate)
    lift $ poke ((p `plusPtr` 4 :: Ptr Word32)) (sampleCount)
    lift $ poke ((p `plusPtr` 8 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (sampleLocations)) :: Word32))
    pPSampleLocations' <- ContT $ allocaBytesAligned @CoarseSampleLocationNV ((Data.Vector.length (sampleLocations)) * 12) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSampleLocations' `plusPtr` (12 * (i)) :: Ptr CoarseSampleLocationNV) (e)) (sampleLocations)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CoarseSampleLocationNV))) (pPSampleLocations')
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr ShadingRatePaletteEntryNV)) (zero)
    lift $ poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    pPSampleLocations' <- ContT $ allocaBytesAligned @CoarseSampleLocationNV ((Data.Vector.length (mempty)) * 12) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSampleLocations' `plusPtr` (12 * (i)) :: Ptr CoarseSampleLocationNV) (e)) (mempty)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CoarseSampleLocationNV))) (pPSampleLocations')
    lift $ f

instance FromCStruct CoarseSampleOrderCustomNV where
  peekCStruct p = do
    shadingRate <- peek @ShadingRatePaletteEntryNV ((p `plusPtr` 0 :: Ptr ShadingRatePaletteEntryNV))
    sampleCount <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    sampleLocationCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pSampleLocations <- peek @(Ptr CoarseSampleLocationNV) ((p `plusPtr` 16 :: Ptr (Ptr CoarseSampleLocationNV)))
    pSampleLocations' <- generateM (fromIntegral sampleLocationCount) (\i -> peekCStruct @CoarseSampleLocationNV ((pSampleLocations `advancePtrBytes` (12 * (i)) :: Ptr CoarseSampleLocationNV)))
    pure $ CoarseSampleOrderCustomNV
             shadingRate sampleCount pSampleLocations'

instance Zero CoarseSampleOrderCustomNV where
  zero = CoarseSampleOrderCustomNV
           zero
           zero
           mempty


-- | VkPipelineViewportCoarseSampleOrderStateCreateInfoNV - Structure
-- specifying parameters controlling sample order in coarse fragments
--
-- = Description
--
-- If this structure is not present, @sampleOrderType@ is considered to be
-- 'COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV'.
--
-- If @sampleOrderType@ is 'COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV', the
-- coverage sample order used for any combination of fragment area and
-- coverage sample count not enumerated in @pCustomSampleOrders@ will be
-- identical to that used for 'COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV'.
--
-- If the pipeline was created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV',
-- the contents of this structure (if present) are ignored, and the
-- coverage sample order is instead specified by
-- 'cmdSetCoarseSampleOrderNV'.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineViewportCoarseSampleOrderStateCreateInfoNV-sampleOrderType-02072#
--     If @sampleOrderType@ is not 'COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV',
--     @customSamplerOrderCount@ /must/ be @0@
--
-- -   #VUID-VkPipelineViewportCoarseSampleOrderStateCreateInfoNV-pCustomSampleOrders-02234#
--     The array @pCustomSampleOrders@ /must/ not contain two structures
--     with matching values for both the @shadingRate@ and @sampleCount@
--     members
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineViewportCoarseSampleOrderStateCreateInfoNV-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV'
--
-- -   #VUID-VkPipelineViewportCoarseSampleOrderStateCreateInfoNV-sampleOrderType-parameter#
--     @sampleOrderType@ /must/ be a valid 'CoarseSampleOrderTypeNV' value
--
-- -   #VUID-VkPipelineViewportCoarseSampleOrderStateCreateInfoNV-pCustomSampleOrders-parameter#
--     If @customSampleOrderCount@ is not @0@, @pCustomSampleOrders@ /must/
--     be a valid pointer to an array of @customSampleOrderCount@ valid
--     'CoarseSampleOrderCustomNV' structures
--
-- = See Also
--
-- 'CoarseSampleOrderCustomNV', 'CoarseSampleOrderTypeNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineViewportCoarseSampleOrderStateCreateInfoNV = PipelineViewportCoarseSampleOrderStateCreateInfoNV
  { -- | @sampleOrderType@ specifies the mechanism used to order coverage samples
    -- in fragments larger than one pixel.
    sampleOrderType :: CoarseSampleOrderTypeNV
  , -- | @pCustomSampleOrders@ is a pointer to an array of
    -- @customSampleOrderCount@ 'CoarseSampleOrderCustomNV' structures, each of
    -- which specifies the coverage sample order for a single combination of
    -- fragment area and coverage sample count.
    customSampleOrders :: Vector CoarseSampleOrderCustomNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineViewportCoarseSampleOrderStateCreateInfoNV)
#endif
deriving instance Show PipelineViewportCoarseSampleOrderStateCreateInfoNV

instance ToCStruct PipelineViewportCoarseSampleOrderStateCreateInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineViewportCoarseSampleOrderStateCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr CoarseSampleOrderTypeNV)) (sampleOrderType)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (customSampleOrders)) :: Word32))
    pPCustomSampleOrders' <- ContT $ allocaBytesAligned @CoarseSampleOrderCustomNV ((Data.Vector.length (customSampleOrders)) * 24) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPCustomSampleOrders' `plusPtr` (24 * (i)) :: Ptr CoarseSampleOrderCustomNV) (e) . ($ ())) (customSampleOrders)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr CoarseSampleOrderCustomNV))) (pPCustomSampleOrders')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr CoarseSampleOrderTypeNV)) (zero)
    pPCustomSampleOrders' <- ContT $ allocaBytesAligned @CoarseSampleOrderCustomNV ((Data.Vector.length (mempty)) * 24) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPCustomSampleOrders' `plusPtr` (24 * (i)) :: Ptr CoarseSampleOrderCustomNV) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr CoarseSampleOrderCustomNV))) (pPCustomSampleOrders')
    lift $ f

instance FromCStruct PipelineViewportCoarseSampleOrderStateCreateInfoNV where
  peekCStruct p = do
    sampleOrderType <- peek @CoarseSampleOrderTypeNV ((p `plusPtr` 16 :: Ptr CoarseSampleOrderTypeNV))
    customSampleOrderCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pCustomSampleOrders <- peek @(Ptr CoarseSampleOrderCustomNV) ((p `plusPtr` 24 :: Ptr (Ptr CoarseSampleOrderCustomNV)))
    pCustomSampleOrders' <- generateM (fromIntegral customSampleOrderCount) (\i -> peekCStruct @CoarseSampleOrderCustomNV ((pCustomSampleOrders `advancePtrBytes` (24 * (i)) :: Ptr CoarseSampleOrderCustomNV)))
    pure $ PipelineViewportCoarseSampleOrderStateCreateInfoNV
             sampleOrderType pCustomSampleOrders'

instance Zero PipelineViewportCoarseSampleOrderStateCreateInfoNV where
  zero = PipelineViewportCoarseSampleOrderStateCreateInfoNV
           zero
           mempty


-- | VkShadingRatePaletteEntryNV - Shading rate image palette entry types
--
-- = Description
--
-- The following table indicates the width and height (in pixels) of each
-- fragment generated using the indicated shading rate, as well as the
-- maximum number of fragment shader invocations launched for each
-- fragment. When processing regions of a primitive that have a shading
-- rate of 'SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV', no fragments
-- will be generated in that region.
--
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | Shading Rate                                                | Width           | Height          | Invocations     |
-- +=============================================================+=================+=================+=================+
-- | 'SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV'              | 0               | 0               | 0               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV'    | 1               | 1               | 16              |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV'     | 1               | 1               | 8               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV'     | 1               | 1               | 4               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV'     | 1               | 1               | 2               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV'      | 1               | 1               | 1               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV' | 2               | 1               | 1               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV' | 1               | 2               | 1               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV' | 2               | 2               | 1               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV' | 4               | 2               | 1               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV' | 2               | 4               | 1               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV' | 4               | 4               | 1               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'CoarseSampleOrderCustomNV', 'ShadingRatePaletteNV'
newtype ShadingRatePaletteEntryNV = ShadingRatePaletteEntryNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV              = ShadingRatePaletteEntryNV 0
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV"
pattern SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV    = ShadingRatePaletteEntryNV 1
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV"
pattern SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV     = ShadingRatePaletteEntryNV 2
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV"
pattern SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV     = ShadingRatePaletteEntryNV 3
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV"
pattern SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV     = ShadingRatePaletteEntryNV 4
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV      = ShadingRatePaletteEntryNV 5
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV = ShadingRatePaletteEntryNV 6
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV = ShadingRatePaletteEntryNV 7
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV = ShadingRatePaletteEntryNV 8
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV = ShadingRatePaletteEntryNV 9
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV = ShadingRatePaletteEntryNV 10
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV = ShadingRatePaletteEntryNV 11
{-# complete SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV,
             SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV,
             SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV,
             SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV,
             SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV,
             SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV,
             SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV,
             SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV,
             SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV,
             SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV,
             SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV,
             SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV :: ShadingRatePaletteEntryNV #-}

conNameShadingRatePaletteEntryNV :: String
conNameShadingRatePaletteEntryNV = "ShadingRatePaletteEntryNV"

enumPrefixShadingRatePaletteEntryNV :: String
enumPrefixShadingRatePaletteEntryNV = "SHADING_RATE_PALETTE_ENTRY_"

showTableShadingRatePaletteEntryNV :: [(ShadingRatePaletteEntryNV, String)]
showTableShadingRatePaletteEntryNV =
  [ (SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV             , "NO_INVOCATIONS_NV")
  , (SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV   , "16_INVOCATIONS_PER_PIXEL_NV")
  , (SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV    , "8_INVOCATIONS_PER_PIXEL_NV")
  , (SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV    , "4_INVOCATIONS_PER_PIXEL_NV")
  , (SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV    , "2_INVOCATIONS_PER_PIXEL_NV")
  , (SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV     , "1_INVOCATION_PER_PIXEL_NV")
  , (SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV, "1_INVOCATION_PER_2X1_PIXELS_NV")
  , (SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV, "1_INVOCATION_PER_1X2_PIXELS_NV")
  , (SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV, "1_INVOCATION_PER_2X2_PIXELS_NV")
  , (SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV, "1_INVOCATION_PER_4X2_PIXELS_NV")
  , (SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV, "1_INVOCATION_PER_2X4_PIXELS_NV")
  , (SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV, "1_INVOCATION_PER_4X4_PIXELS_NV")
  ]

instance Show ShadingRatePaletteEntryNV where
  showsPrec = enumShowsPrec enumPrefixShadingRatePaletteEntryNV
                            showTableShadingRatePaletteEntryNV
                            conNameShadingRatePaletteEntryNV
                            (\(ShadingRatePaletteEntryNV x) -> x)
                            (showsPrec 11)

instance Read ShadingRatePaletteEntryNV where
  readPrec = enumReadPrec enumPrefixShadingRatePaletteEntryNV
                          showTableShadingRatePaletteEntryNV
                          conNameShadingRatePaletteEntryNV
                          ShadingRatePaletteEntryNV


-- | VkCoarseSampleOrderTypeNV - Shading rate image sample ordering types
--
-- = See Also
--
-- 'PipelineViewportCoarseSampleOrderStateCreateInfoNV',
-- 'cmdSetCoarseSampleOrderNV'
newtype CoarseSampleOrderTypeNV = CoarseSampleOrderTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV' specifies that coverage samples
-- will be ordered in an implementation-dependent manner.
pattern COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV      = CoarseSampleOrderTypeNV 0
-- | 'COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV' specifies that coverage samples
-- will be ordered according to the array of custom orderings provided in
-- either the @pCustomSampleOrders@ member of
-- 'PipelineViewportCoarseSampleOrderStateCreateInfoNV' or the
-- @pCustomSampleOrders@ member of 'cmdSetCoarseSampleOrderNV'.
pattern COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV       = CoarseSampleOrderTypeNV 1
-- | 'COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV' specifies that coverage
-- samples will be ordered sequentially, sorted first by pixel coordinate
-- (in row-major order) and then by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-multisampling-coverage-mask sample index>.
pattern COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV  = CoarseSampleOrderTypeNV 2
-- | 'COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV' specifies that coverage
-- samples will be ordered sequentially, sorted first by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-multisampling-coverage-mask sample index>
-- and then by pixel coordinate (in row-major order).
pattern COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV = CoarseSampleOrderTypeNV 3
{-# complete COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV,
             COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV,
             COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV,
             COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV :: CoarseSampleOrderTypeNV #-}

conNameCoarseSampleOrderTypeNV :: String
conNameCoarseSampleOrderTypeNV = "CoarseSampleOrderTypeNV"

enumPrefixCoarseSampleOrderTypeNV :: String
enumPrefixCoarseSampleOrderTypeNV = "COARSE_SAMPLE_ORDER_TYPE_"

showTableCoarseSampleOrderTypeNV :: [(CoarseSampleOrderTypeNV, String)]
showTableCoarseSampleOrderTypeNV =
  [ (COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV     , "DEFAULT_NV")
  , (COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV      , "CUSTOM_NV")
  , (COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV , "PIXEL_MAJOR_NV")
  , (COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV, "SAMPLE_MAJOR_NV")
  ]

instance Show CoarseSampleOrderTypeNV where
  showsPrec = enumShowsPrec enumPrefixCoarseSampleOrderTypeNV
                            showTableCoarseSampleOrderTypeNV
                            conNameCoarseSampleOrderTypeNV
                            (\(CoarseSampleOrderTypeNV x) -> x)
                            (showsPrec 11)

instance Read CoarseSampleOrderTypeNV where
  readPrec = enumReadPrec enumPrefixCoarseSampleOrderTypeNV
                          showTableCoarseSampleOrderTypeNV
                          conNameCoarseSampleOrderTypeNV
                          CoarseSampleOrderTypeNV


type NV_SHADING_RATE_IMAGE_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION"
pattern NV_SHADING_RATE_IMAGE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_SHADING_RATE_IMAGE_SPEC_VERSION = 3


type NV_SHADING_RATE_IMAGE_EXTENSION_NAME = "VK_NV_shading_rate_image"

-- No documentation found for TopLevel "VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME"
pattern NV_SHADING_RATE_IMAGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_SHADING_RATE_IMAGE_EXTENSION_NAME = "VK_NV_shading_rate_image"

