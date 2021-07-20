{-# language CPP #-}
-- No documentation found for Chapter "FundamentalTypes"
module Vulkan.Core10.FundamentalTypes  ( boolToBool32
                                       , bool32ToBool
                                       , Offset2D(..)
                                       , Offset3D(..)
                                       , Extent2D(..)
                                       , Extent3D(..)
                                       , Rect2D(..)
                                       , Bool32( FALSE
                                               , TRUE
                                               , ..
                                               )
                                       , SampleMask
                                       , Flags
                                       , Flags64
                                       , DeviceSize
                                       , DeviceAddress
                                       , StructureType(..)
                                       , Result(..)
                                       ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Data.Bool (bool)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
boolToBool32 :: Bool -> Bool32
boolToBool32 = bool FALSE TRUE

bool32ToBool :: Bool32 -> Bool
bool32ToBool = \case
  FALSE -> False
  TRUE  -> True


-- | VkOffset2D - Structure specifying a two-dimensional offset
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_display.DisplayPlaneCapabilitiesKHR',
-- 'Rect2D', 'Vulkan.Extensions.VK_KHR_incremental_present.RectLayerKHR',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoDecodeH264CapabilitiesEXT VkVideoDecodeH264CapabilitiesEXT>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoDecodeInfoKHR VkVideoDecodeInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoPictureResourceKHR VkVideoPictureResourceKHR>
data Offset2D = Offset2D
  { -- | @x@ is the x offset.
    x :: Int32
  , -- | @y@ is the y offset.
    y :: Int32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Offset2D)
#endif
deriving instance Show Offset2D

instance ToCStruct Offset2D where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Offset2D{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Int32)) (x)
    poke ((p `plusPtr` 4 :: Ptr Int32)) (y)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Int32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Int32)) (zero)
    f

instance FromCStruct Offset2D where
  peekCStruct p = do
    x <- peek @Int32 ((p `plusPtr` 0 :: Ptr Int32))
    y <- peek @Int32 ((p `plusPtr` 4 :: Ptr Int32))
    pure $ Offset2D
             x y

instance Storable Offset2D where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Offset2D where
  zero = Offset2D
           zero
           zero


-- | VkOffset3D - Structure specifying a three-dimensional offset
--
-- = See Also
--
-- 'Vulkan.Core10.CommandBufferBuilding.BufferImageCopy',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.BufferImageCopy2KHR',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageBlit',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.ImageBlit2KHR',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageCopy',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.ImageCopy2KHR',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageResolve',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.ImageResolve2KHR',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryBind'
data Offset3D = Offset3D
  { -- | @x@ is the x offset.
    x :: Int32
  , -- | @y@ is the y offset.
    y :: Int32
  , -- | @z@ is the z offset.
    z :: Int32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Offset3D)
#endif
deriving instance Show Offset3D

instance ToCStruct Offset3D where
  withCStruct x f = allocaBytes 12 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Offset3D{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Int32)) (x)
    poke ((p `plusPtr` 4 :: Ptr Int32)) (y)
    poke ((p `plusPtr` 8 :: Ptr Int32)) (z)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Int32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Int32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Int32)) (zero)
    f

instance FromCStruct Offset3D where
  peekCStruct p = do
    x <- peek @Int32 ((p `plusPtr` 0 :: Ptr Int32))
    y <- peek @Int32 ((p `plusPtr` 4 :: Ptr Int32))
    z <- peek @Int32 ((p `plusPtr` 8 :: Ptr Int32))
    pure $ Offset3D
             x y z

instance Storable Offset3D where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Offset3D where
  zero = Offset3D
           zero
           zero
           zero


-- | VkExtent2D - Structure specifying a two-dimensional extent
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_display.DisplayModeParametersKHR',
-- 'Vulkan.Extensions.VK_KHR_display.DisplayPlaneCapabilitiesKHR',
-- 'Vulkan.Extensions.VK_KHR_display.DisplayPropertiesKHR',
-- 'Vulkan.Extensions.VK_KHR_display.DisplaySurfaceCreateInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateAttachmentInfoKHR',
-- 'Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_fragment_density_map.PhysicalDeviceFragmentDensityMapPropertiesEXT',
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PhysicalDeviceFragmentShadingRateKHR',
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PhysicalDeviceFragmentShadingRatePropertiesKHR',
-- 'Vulkan.Extensions.VK_EXT_sample_locations.PhysicalDeviceSampleLocationsPropertiesEXT',
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.PhysicalDeviceShadingRateImagePropertiesNV',
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR',
-- 'Rect2D', 'Vulkan.Extensions.VK_KHR_incremental_present.RectLayerKHR',
-- 'Vulkan.Extensions.VK_EXT_sample_locations.SampleLocationsInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_display_surface_counter.SurfaceCapabilities2EXT',
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoCapabilitiesKHR VkVideoCapabilitiesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoDecodeInfoKHR VkVideoDecodeInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH264CapabilitiesEXT VkVideoEncodeH264CapabilitiesEXT>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH264SessionCreateInfoEXT VkVideoEncodeH264SessionCreateInfoEXT>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeInfoKHR VkVideoEncodeInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoPictureResourceKHR VkVideoPictureResourceKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoSessionCreateInfoKHR VkVideoSessionCreateInfoKHR>,
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.cmdSetFragmentShadingRateKHR',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEI vkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEI>,
-- 'Vulkan.Core10.Pass.getRenderAreaGranularity'
data Extent2D = Extent2D
  { -- | @width@ is the width of the extent.
    width :: Word32
  , -- | @height@ is the height of the extent.
    height :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Extent2D)
#endif
deriving instance Show Extent2D

instance ToCStruct Extent2D where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Extent2D{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (width)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (height)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    f

instance FromCStruct Extent2D where
  peekCStruct p = do
    width <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    height <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    pure $ Extent2D
             width height

instance Storable Extent2D where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Extent2D where
  zero = Extent2D
           zero
           zero


-- | VkExtent3D - Structure specifying a three-dimensional extent
--
-- = See Also
--
-- 'Vulkan.Core10.CommandBufferBuilding.BufferImageCopy',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.BufferImageCopy2KHR',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageCopy',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.ImageCopy2KHR',
-- 'Vulkan.Core10.Image.ImageCreateInfo',
-- 'Vulkan.Core10.DeviceInitialization.ImageFormatProperties',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageResolve',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.ImageResolve2KHR',
-- 'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageFormatProperties',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryBind'
data Extent3D = Extent3D
  { -- | @width@ is the width of the extent.
    width :: Word32
  , -- | @height@ is the height of the extent.
    height :: Word32
  , -- | @depth@ is the depth of the extent.
    depth :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Extent3D)
#endif
deriving instance Show Extent3D

instance ToCStruct Extent3D where
  withCStruct x f = allocaBytes 12 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Extent3D{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (width)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (height)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (depth)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct Extent3D where
  peekCStruct p = do
    width <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    height <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    depth <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ Extent3D
             width height depth

instance Storable Extent3D where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Extent3D where
  zero = Extent3D
           zero
           zero
           zero


-- | VkRect2D - Structure specifying a two-dimensional subregion
--
-- = See Also
--
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindImageMemoryDeviceGroupInfo',
-- 'Vulkan.Core10.CommandBufferBuilding.ClearRect',
-- 'Vulkan.Extensions.VK_QCOM_render_pass_transform.CommandBufferInheritanceRenderPassTransformInfoQCOM',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
-- 'Vulkan.Extensions.VK_KHR_display_swapchain.DisplayPresentInfoKHR',
-- 'Extent2D', 'Offset2D',
-- 'Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateInfoEXT',
-- 'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV',
-- 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo',
-- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo',
-- 'Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleEXT',
-- 'Vulkan.Extensions.VK_NV_scissor_exclusive.cmdSetExclusiveScissorNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetScissor',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT',
-- 'Vulkan.Extensions.VK_KHR_swapchain.getPhysicalDevicePresentRectanglesKHR'
data Rect2D = Rect2D
  { -- | @offset@ is a 'Offset2D' specifying the rectangle offset.
    offset :: Offset2D
  , -- | @extent@ is a 'Extent2D' specifying the rectangle extent.
    extent :: Extent2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Rect2D)
#endif
deriving instance Show Rect2D

instance ToCStruct Rect2D where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Rect2D{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Offset2D)) (offset)
    poke ((p `plusPtr` 8 :: Ptr Extent2D)) (extent)
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Offset2D)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Extent2D)) (zero)
    f

instance FromCStruct Rect2D where
  peekCStruct p = do
    offset <- peekCStruct @Offset2D ((p `plusPtr` 0 :: Ptr Offset2D))
    extent <- peekCStruct @Extent2D ((p `plusPtr` 8 :: Ptr Extent2D))
    pure $ Rect2D
             offset extent

instance Storable Rect2D where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Rect2D where
  zero = Rect2D
           zero
           zero


-- | VkBool32 - Vulkan boolean type
--
-- = Description
--
-- 'TRUE' represents a boolean __True__ (unsigned integer 1) value, and
-- 'FALSE' a boolean __False__ (unsigned integer 0) value.
--
-- All values returned from a Vulkan implementation in a 'Bool32' will be
-- either 'TRUE' or 'FALSE'.
--
-- Applications /must/ not pass any other values than 'TRUE' or 'FALSE'
-- into a Vulkan implementation where a 'Bool32' is expected.
--
-- = See Also
--
-- 'FALSE', 'TRUE',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryInstancesDataKHR',
-- 'Vulkan.Extensions.VK_EXT_conditional_rendering.CommandBufferInheritanceConditionalRenderingInfoEXT',
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo',
-- 'Vulkan.Extensions.VK_NV_inherited_viewport_scissor.CommandBufferInheritanceViewportScissorInfoNV',
-- 'Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationBufferCreateInfoNV',
-- 'Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationImageCreateInfoNV',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance3.DescriptorSetLayoutSupport',
-- 'Vulkan.Extensions.VK_AMD_display_native_hdr.DisplayNativeHdrSurfaceCapabilitiesAMD',
-- 'Vulkan.Extensions.VK_KHR_display_swapchain.DisplayPresentInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_display.DisplayPropertiesKHR',
-- 'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsLayoutTokenNV',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedRequirements',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.PerformanceOverrideInfoINTEL',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.PerformanceValueDataINTEL',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage.PhysicalDevice16BitStorageFeatures',
-- 'Vulkan.Extensions.VK_EXT_4444_formats.PhysicalDevice4444FormatsFeaturesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage.PhysicalDevice8BitStorageFeatures',
-- 'Vulkan.Extensions.VK_EXT_astc_decode_mode.PhysicalDeviceASTCDecodeFeaturesEXT',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.PhysicalDeviceAccelerationStructureFeaturesKHR',
-- 'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedPropertiesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeatures',
-- 'Vulkan.Extensions.VK_EXT_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeaturesEXT',
-- 'Vulkan.Extensions.VK_AMD_device_coherent_memory.PhysicalDeviceCoherentMemoryFeaturesAMD',
-- 'Vulkan.Extensions.VK_EXT_color_write_enable.PhysicalDeviceColorWriteEnableFeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_compute_shader_derivatives.PhysicalDeviceComputeShaderDerivativesFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_conditional_rendering.PhysicalDeviceConditionalRenderingFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_conservative_rasterization.PhysicalDeviceConservativeRasterizationPropertiesEXT',
-- 'Vulkan.Extensions.VK_NV_cooperative_matrix.PhysicalDeviceCooperativeMatrixFeaturesNV',
-- 'Vulkan.Extensions.VK_NV_corner_sampled_image.PhysicalDeviceCornerSampledImageFeaturesNV',
-- 'Vulkan.Extensions.VK_NV_coverage_reduction_mode.PhysicalDeviceCoverageReductionModeFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_custom_border_color.PhysicalDeviceCustomBorderColorFeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing.PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_depth_clip_enable.PhysicalDeviceDepthClipEnableFeaturesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.PhysicalDeviceDepthStencilResolveProperties',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingFeatures',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingProperties',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.PhysicalDeviceDeviceGeneratedCommandsFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_device_memory_report.PhysicalDeviceDeviceMemoryReportFeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_device_diagnostics_config.PhysicalDeviceDiagnosticsConfigFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_physical_device_drm.PhysicalDeviceDrmPropertiesEXT',
-- 'Vulkan.Extensions.VK_NV_scissor_exclusive.PhysicalDeviceExclusiveScissorFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.PhysicalDeviceExtendedDynamicState2FeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.PhysicalDeviceExtendedDynamicStateFeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_external_memory_rdma.PhysicalDeviceExternalMemoryRDMAFeaturesNV',
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceFeatures',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_shader_float_controls.PhysicalDeviceFloatControlsProperties',
-- 'Vulkan.Extensions.VK_EXT_fragment_density_map2.PhysicalDeviceFragmentDensityMap2FeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_fragment_density_map2.PhysicalDeviceFragmentDensityMap2PropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_fragment_density_map.PhysicalDeviceFragmentDensityMapFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_fragment_density_map.PhysicalDeviceFragmentDensityMapPropertiesEXT',
-- 'Vulkan.Extensions.VK_NV_fragment_shader_barycentric.PhysicalDeviceFragmentShaderBarycentricFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_fragment_shader_interlock.PhysicalDeviceFragmentShaderInterlockFeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PhysicalDeviceFragmentShadingRateEnumsFeaturesNV',
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PhysicalDeviceFragmentShadingRateFeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PhysicalDeviceFragmentShadingRatePropertiesKHR',
-- 'Vulkan.Extensions.VK_EXT_global_priority_query.PhysicalDeviceGlobalPriorityQueryFeaturesEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.PhysicalDeviceGroupProperties',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.PhysicalDeviceHostQueryResetFeatures',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceIDProperties',
-- 'Vulkan.Extensions.VK_EXT_image_robustness.PhysicalDeviceImageRobustnessFeaturesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.PhysicalDeviceImagelessFramebufferFeatures',
-- 'Vulkan.Extensions.VK_EXT_index_type_uint8.PhysicalDeviceIndexTypeUint8FeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_inherited_viewport_scissor.PhysicalDeviceInheritedViewportScissorFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_inline_uniform_block.PhysicalDeviceInlineUniformBlockFeaturesEXT',
-- 'Vulkan.Extensions.VK_HUAWEI_invocation_mask.PhysicalDeviceInvocationMaskFeaturesHUAWEI',
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits',
-- 'Vulkan.Extensions.VK_EXT_line_rasterization.PhysicalDeviceLineRasterizationFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_memory_priority.PhysicalDeviceMemoryPriorityFeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_mesh_shader.PhysicalDeviceMeshShaderFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_multi_draw.PhysicalDeviceMultiDrawFeaturesEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewFeatures',
-- 'Vulkan.Extensions.VK_NVX_multiview_per_view_attributes.PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX',
-- 'Vulkan.Extensions.VK_VALVE_mutable_descriptor_type.PhysicalDeviceMutableDescriptorTypeFeaturesVALVE',
-- 'Vulkan.Extensions.VK_KHR_performance_query.PhysicalDevicePerformanceQueryFeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_performance_query.PhysicalDevicePerformanceQueryPropertiesKHR',
-- 'Vulkan.Extensions.VK_EXT_pipeline_creation_cache_control.PhysicalDevicePipelineCreationCacheControlFeaturesEXT',
-- 'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PhysicalDevicePipelineExecutablePropertiesFeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_present_id.PhysicalDevicePresentIdFeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_present_wait.PhysicalDevicePresentWaitFeaturesKHR',
-- 'Vulkan.Extensions.VK_EXT_private_data.PhysicalDevicePrivateDataFeaturesEXT',
-- 'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.PhysicalDeviceProtectedMemoryFeatures',
-- 'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.PhysicalDeviceProtectedMemoryProperties',
-- 'Vulkan.Extensions.VK_EXT_provoking_vertex.PhysicalDeviceProvokingVertexFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_provoking_vertex.PhysicalDeviceProvokingVertexPropertiesEXT',
-- 'Vulkan.Extensions.VK_KHR_ray_query.PhysicalDeviceRayQueryFeaturesKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing_motion_blur.PhysicalDeviceRayTracingMotionBlurFeaturesNV',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.PhysicalDeviceRayTracingPipelineFeaturesKHR',
-- 'Vulkan.Extensions.VK_NV_representative_fragment_test.PhysicalDeviceRepresentativeFragmentTestFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_robustness2.PhysicalDeviceRobustness2FeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_sample_locations.PhysicalDeviceSampleLocationsPropertiesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.PhysicalDeviceSamplerFilterMinmaxProperties',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.PhysicalDeviceSamplerYcbcrConversionFeatures',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout.PhysicalDeviceScalarBlockLayoutFeatures',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.PhysicalDeviceSeparateDepthStencilLayoutsFeatures',
-- 'Vulkan.Extensions.VK_EXT_shader_atomic_float2.PhysicalDeviceShaderAtomicFloat2FeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_shader_atomic_float.PhysicalDeviceShaderAtomicFloatFeaturesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64.PhysicalDeviceShaderAtomicInt64Features',
-- 'Vulkan.Extensions.VK_KHR_shader_clock.PhysicalDeviceShaderClockFeaturesKHR',
-- 'Vulkan.Extensions.VK_EXT_shader_demote_to_helper_invocation.PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters.PhysicalDeviceShaderDrawParametersFeatures',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8.PhysicalDeviceShaderFloat16Int8Features',
-- 'Vulkan.Extensions.VK_EXT_shader_image_atomic_int64.PhysicalDeviceShaderImageAtomicInt64FeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_shader_image_footprint.PhysicalDeviceShaderImageFootprintFeaturesNV',
-- 'Vulkan.Extensions.VK_INTEL_shader_integer_functions2.PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL',
-- 'Vulkan.Extensions.VK_NV_shader_sm_builtins.PhysicalDeviceShaderSMBuiltinsFeaturesNV',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types.PhysicalDeviceShaderSubgroupExtendedTypesFeatures',
-- 'Vulkan.Extensions.VK_KHR_shader_subgroup_uniform_control_flow.PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_shader_terminate_invocation.PhysicalDeviceShaderTerminateInvocationFeaturesKHR',
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.PhysicalDeviceShadingRateImageFeaturesNV',
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceSparseProperties',
-- 'Vulkan.Core11.Originally_Based_On_VK_KHR_subgroup.PhysicalDeviceSubgroupProperties',
-- 'Vulkan.Extensions.VK_EXT_subgroup_size_control.PhysicalDeviceSubgroupSizeControlFeaturesEXT',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceSubpassShadingFeaturesHUAWEI VkPhysicalDeviceSubpassShadingFeaturesHUAWEI>,
-- 'Vulkan.Extensions.VK_KHR_synchronization2.PhysicalDeviceSynchronization2FeaturesKHR',
-- 'Vulkan.Extensions.VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentPropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_texture_compression_astc_hdr.PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.PhysicalDeviceTimelineSemaphoreFeatures',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackPropertiesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout.PhysicalDeviceUniformBufferStandardLayoutFeatures',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers.PhysicalDeviceVariablePointersFeatures',
-- 'Vulkan.Extensions.VK_EXT_vertex_attribute_divisor.PhysicalDeviceVertexAttributeDivisorFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.PhysicalDeviceVertexInputDynamicStateFeaturesEXT',
-- 'Vulkan.Core12.PhysicalDeviceVulkan11Features',
-- 'Vulkan.Core12.PhysicalDeviceVulkan11Properties',
-- 'Vulkan.Core12.PhysicalDeviceVulkan12Features',
-- 'Vulkan.Core12.PhysicalDeviceVulkan12Properties',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model.PhysicalDeviceVulkanMemoryModelFeatures',
-- 'Vulkan.Extensions.VK_KHR_workgroup_memory_explicit_layout.PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR',
-- 'Vulkan.Extensions.VK_EXT_ycbcr_2plane_444_formats.PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_ycbcr_image_arrays.PhysicalDeviceYcbcrImageArraysFeaturesEXT',
-- 'Vulkan.Extensions.VK_KHR_zero_initialize_workgroup_memory.PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR',
-- 'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PipelineColorBlendAdvancedStateCreateInfoEXT',
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendAttachmentState',
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_color_write_enable.PipelineColorWriteCreateInfoEXT',
-- 'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.PipelineCoverageModulationStateCreateInfoNV',
-- 'Vulkan.Extensions.VK_NV_fragment_coverage_to_color.PipelineCoverageToColorStateCreateInfoNV',
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo',
-- 'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PipelineExecutableInternalRepresentationKHR',
-- 'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PipelineExecutableStatisticValueKHR',
-- 'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo',
-- 'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_depth_clip_enable.PipelineRasterizationDepthClipStateCreateInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_line_rasterization.PipelineRasterizationLineStateCreateInfoEXT',
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo',
-- 'Vulkan.Extensions.VK_NV_representative_fragment_test.PipelineRepresentativeFragmentTestStateCreateInfoNV',
-- 'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT',
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV',
-- 'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV',
-- 'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.ProtectedSubmitInfo',
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceCapabilitiesFullScreenExclusiveEXT',
-- 'Vulkan.Extensions.VK_KHR_surface_protected_capabilities.SurfaceProtectedCapabilitiesKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR',
-- 'Vulkan.Extensions.VK_AMD_display_native_hdr.SwapchainDisplayNativeHdrCreateInfoAMD',
-- 'Vulkan.Extensions.VK_AMD_texture_gather_bias_lod.TextureLODGatherFormatPropertiesAMD',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH264EmitPictureParametersEXT VkVideoEncodeH264EmitPictureParametersEXT>,
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdBuildAccelerationStructureNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.cmdExecuteGeneratedCommandsNV',
-- 'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetDepthBiasEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetDepthBoundsTestEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetDepthTestEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetDepthWriteEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetPrimitiveRestartEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetStencilTestEnableEXT',
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR',
-- 'Vulkan.Extensions.VK_AMD_display_native_hdr.setLocalDimmingAMD',
-- 'Vulkan.Core10.Fence.waitForFences'
newtype Bool32 = Bool32 Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkBool32" "VK_FALSE"
pattern FALSE = Bool32 0
-- No documentation found for Nested "VkBool32" "VK_TRUE"
pattern TRUE  = Bool32 1
{-# complete FALSE,
             TRUE :: Bool32 #-}

conNameBool32 :: String
conNameBool32 = "Bool32"

enumPrefixBool32 :: String
enumPrefixBool32 = ""

showTableBool32 :: [(Bool32, String)]
showTableBool32 = [(FALSE, "FALSE"), (TRUE, "TRUE")]

instance Show Bool32 where
  showsPrec = enumShowsPrec enumPrefixBool32 showTableBool32 conNameBool32 (\(Bool32 x) -> x) (showsPrec 11)

instance Read Bool32 where
  readPrec = enumReadPrec enumPrefixBool32 showTableBool32 conNameBool32 Bool32


-- | VkSampleMask - Mask of sample coverage information
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'
type SampleMask = Word32


-- | VkFlags - Vulkan bitmasks
--
-- = Description
--
-- Bitmasks are passed to many commands and structures to compactly
-- represent options, but 'Flags' is not used directly in the API. Instead,
-- a 'Flags' type which is an alias of 'Flags', and whose name matches the
-- corresponding @Vk*FlagBits@ that are valid for that type, is used.
--
-- Any 'Flags' member or parameter used in the API as an input /must/ be a
-- valid combination of bit flags. A valid combination is either zero or
-- the bitwise OR of valid bit flags. A bit flag is valid if:
--
-- -   The bit flag is defined as part of the @Vk*FlagBits@ type, where the
--     bits type is obtained by taking the flag type and replacing the
--     trailing 'Flags' with @FlagBits@. For example, a flag value of type
--     'Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlags'
--     /must/ contain only bit flags defined by
--     'Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlagBits'.
--
-- -   The flag is allowed in the context in which it is being used. For
--     example, in some cases, certain bit flags or combinations of bit
--     flags are mutually exclusive.
--
-- Any 'Flags' member or parameter returned from a query command or
-- otherwise output from Vulkan to the application /may/ contain bit flags
-- undefined in its corresponding @Vk*FlagBits@ type. An application
-- /cannot/ rely on the state of these unspecified bits.
--
-- Only the low-order 31 bits (bit positions zero through 30) are available
-- for use as flag bits.
--
-- Note
--
-- This restriction is due to poorly defined behavior by C compilers given
-- a C enumerant value of @0x80000000@. In some cases adding this enumerant
-- value may increase the size of the underlying @Vk*FlagBits@ type,
-- breaking the ABI.
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlags'
type Flags = Word32


-- No documentation found for TopLevel "VkFlags64"
type Flags64 = Word64


-- | VkDeviceSize - Vulkan device memory size and offsets
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureBuildSizesInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureCreateInfoKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.AccelerationStructureCreateInfoNV',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryAabbsDataKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR',
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferPropertiesANDROID',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.BindAccelerationStructureMemoryInfoNV',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo',
-- 'Vulkan.Core10.CommandBufferBuilding.BufferCopy',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.BufferCopy2KHR',
-- 'Vulkan.Core10.Buffer.BufferCreateInfo',
-- 'Vulkan.Core10.CommandBufferBuilding.BufferImageCopy',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.BufferImageCopy2KHR',
-- 'Vulkan.Core10.OtherTypes.BufferMemoryBarrier',
-- 'Vulkan.Extensions.VK_KHR_synchronization2.BufferMemoryBarrier2KHR',
-- 'Vulkan.Core10.BufferView.BufferViewCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_conditional_rendering.ConditionalRenderingBeginInfoEXT',
-- 'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo',
-- 'Vulkan.Extensions.VK_EXT_device_memory_report.DeviceMemoryReportCallbackDataEXT',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GeneratedCommandsInfoNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.GeometryAABBNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.GeometryTrianglesNV',
-- 'Vulkan.Core10.DeviceInitialization.ImageFormatProperties',
-- 'Vulkan.Extensions.VK_NVX_image_view_handle.ImageViewAddressPropertiesNVX',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsStreamNV',
-- 'Vulkan.Core10.Memory.MappedMemoryRange',
-- 'Vulkan.Core10.Memory.MemoryAllocateInfo',
-- 'Vulkan.Core10.DeviceInitialization.MemoryHeap',
-- 'Vulkan.Core10.MemoryManagement.MemoryRequirements',
-- 'Vulkan.Extensions.VK_EXT_external_memory_host.PhysicalDeviceExternalMemoryHostPropertiesEXT',
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance3.PhysicalDeviceMaintenance3Properties',
-- 'Vulkan.Extensions.VK_EXT_memory_budget.PhysicalDeviceMemoryBudgetPropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_robustness2.PhysicalDeviceRobustness2PropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentPropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackPropertiesEXT',
-- 'Vulkan.Core12.PhysicalDeviceVulkan11Properties',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryBind',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryRequirements',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseMemoryBind',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.StridedDeviceAddressRegionKHR',
-- 'Vulkan.Core10.Image.SubresourceLayout',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoBindMemoryKHR VkVideoBindMemoryKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoCapabilitiesKHR VkVideoCapabilitiesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoDecodeInfoKHR VkVideoDecodeInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeInfoKHR VkVideoEncodeInfoKHR>,
-- 'Vulkan.Core10.MemoryManagement.bindBufferMemory',
-- 'Vulkan.Core10.MemoryManagement.bindImageMemory',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginTransformFeedbackEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBindTransformFeedbackBuffersEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2EXT',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdBuildAccelerationStructureNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDispatchIndirect',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCount',
-- 'Vulkan.Extensions.VK_AMD_draw_indirect_count.cmdDrawIndexedIndirectCountAMD',
-- 'Vulkan.Extensions.VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCountKHR',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdDrawIndirectByteCountEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndirectCount',
-- 'Vulkan.Extensions.VK_AMD_draw_indirect_count.cmdDrawIndirectCountAMD',
-- 'Vulkan.Extensions.VK_KHR_draw_indirect_count.cmdDrawIndirectCountKHR',
-- 'Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectCountNV',
-- 'Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectNV',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndTransformFeedbackEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdFillBuffer',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdTraceRaysNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdUpdateBuffer',
-- 'Vulkan.Extensions.VK_KHR_synchronization2.cmdWriteBufferMarker2AMD',
-- 'Vulkan.Extensions.VK_AMD_buffer_marker.cmdWriteBufferMarkerAMD',
-- 'Vulkan.Core10.Memory.getDeviceMemoryCommitment',
-- 'Vulkan.Core10.Query.getQueryPoolResults',
-- 'Vulkan.Core10.Memory.mapMemory'
type DeviceSize = Word64


-- | VkDeviceAddress - Vulkan device address type
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureCreateInfoKHR',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.BindIndexBufferIndirectCommandNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.BindVertexBufferIndirectCommandNV',
-- 'Vulkan.Extensions.VK_EXT_buffer_device_address.BufferDeviceAddressCreateInfoEXT',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressKHR',
-- 'Vulkan.Extensions.VK_NVX_image_view_handle.ImageViewAddressPropertiesNVX',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.StridedDeviceAddressRegionKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdBuildAccelerationStructuresIndirectKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.cmdTraceRaysIndirectKHR'
type DeviceAddress = Word64

