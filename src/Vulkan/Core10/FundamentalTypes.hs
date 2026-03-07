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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Extensions.VK_KHR_display.DisplayPlaneCapabilitiesKHR',
-- 'Vulkan.Extensions.VK_QCOM_image_processing.ImageViewSampleWeightCreateInfoQCOM',
-- 'Rect2D', 'Vulkan.Extensions.VK_KHR_incremental_present.RectLayerKHR',
-- 'Vulkan.Extensions.VK_EXT_fragment_density_map_offset.RenderPassFragmentDensityMapOffsetEndInfoEXT',
-- 'Vulkan.Extensions.VK_QCOM_tile_properties.TilePropertiesQCOM',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoDecodeH264CapabilitiesKHR VkVideoDecodeH264CapabilitiesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoPictureResourceInfoKHR VkVideoPictureResourceInfoKHR>
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.CommandBufferBuilding.BufferImageCopy',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.BufferImageCopy2',
-- 'Vulkan.Extensions.VK_KHR_copy_memory_indirect.CopyMemoryToImageIndirectCommandKHR',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageBlit',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.ImageBlit2',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageCopy',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.ImageCopy2',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageResolve',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.ImageResolve2',
-- 'Vulkan.Core14.PromotedStreamingTransfers'.ImageToMemoryCopy',
-- 'Vulkan.Core14.PromotedStreamingTransfers'.MemoryToImageCopy',
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Extensions.VK_KHR_display.DisplayModeParametersKHR',
-- 'Vulkan.Extensions.VK_KHR_display.DisplayPlaneCapabilitiesKHR',
-- 'Vulkan.Extensions.VK_KHR_display.DisplayPropertiesKHR',
-- 'Vulkan.Extensions.VK_KHR_display.DisplaySurfaceCreateInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateAttachmentInfoKHR',
-- 'Vulkan.Extensions.VK_QCOM_image_processing.ImageViewSampleWeightCreateInfoQCOM',
-- 'Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_fragment_density_map_offset.PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_fragment_density_map.PhysicalDeviceFragmentDensityMapPropertiesEXT',
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PhysicalDeviceFragmentShadingRateKHR',
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PhysicalDeviceFragmentShadingRatePropertiesKHR',
-- 'Vulkan.Extensions.VK_QCOM_image_processing2.PhysicalDeviceImageProcessing2PropertiesQCOM',
-- 'Vulkan.Extensions.VK_QCOM_image_processing.PhysicalDeviceImageProcessingPropertiesQCOM',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDevicePerformanceCountersByRegionPropertiesARM VkPhysicalDevicePerformanceCountersByRegionPropertiesARM>,
-- 'Vulkan.Extensions.VK_ARM_render_pass_striped.PhysicalDeviceRenderPassStripedPropertiesARM',
-- 'Vulkan.Extensions.VK_EXT_sample_locations.PhysicalDeviceSampleLocationsPropertiesEXT',
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.PhysicalDeviceShadingRateImagePropertiesNV',
-- 'Vulkan.Extensions.VK_QCOM_tile_shading.PhysicalDeviceTileShadingPropertiesQCOM',
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR',
-- 'Rect2D', 'Vulkan.Extensions.VK_KHR_incremental_present.RectLayerKHR',
-- 'Vulkan.Extensions.VK_QCOM_tile_shading.RenderPassTileShadingCreateInfoQCOM',
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.RenderingFragmentShadingRateAttachmentInfoKHR',
-- 'Vulkan.Extensions.VK_EXT_sample_locations.SampleLocationsInfoEXT',
-- 'Vulkan.Extensions.VK_QCOM_image_processing2.SamplerBlockMatchWindowCreateInfoQCOM',
-- 'Vulkan.Extensions.VK_EXT_display_surface_counter.SurfaceCapabilities2EXT',
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR',
-- 'Vulkan.Extensions.VK_KHR_surface_maintenance1.SurfacePresentScalingCapabilitiesKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR',
-- 'Vulkan.Extensions.VK_QCOM_tile_properties.TilePropertiesQCOM',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoCapabilitiesKHR VkVideoCapabilitiesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeAV1CapabilitiesKHR VkVideoEncodeAV1CapabilitiesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeCapabilitiesKHR VkVideoEncodeCapabilitiesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH265CapabilitiesKHR VkVideoEncodeH265CapabilitiesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeQuantizationMapCapabilitiesKHR VkVideoEncodeQuantizationMapCapabilitiesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeQuantizationMapInfoKHR VkVideoEncodeQuantizationMapInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeQuantizationMapSessionParametersCreateInfoKHR VkVideoEncodeQuantizationMapSessionParametersCreateInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoFormatQuantizationMapPropertiesKHR VkVideoFormatQuantizationMapPropertiesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoPictureResourceInfoKHR VkVideoPictureResourceInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoSessionCreateInfoKHR VkVideoSessionCreateInfoKHR>,
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.cmdSetFragmentShadingRateKHR',
-- 'Vulkan.Extensions.VK_HUAWEI_subpass_shading.getDeviceSubpassShadingMaxWorkgroupSizeHUAWEI',
-- 'Vulkan.Core10.Pass.getRenderAreaGranularity',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.getRenderingAreaGranularity',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.getRenderingAreaGranularity'
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.CommandBufferBuilding.BufferImageCopy',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.BufferImageCopy2',
-- 'Vulkan.Extensions.VK_KHR_copy_memory_indirect.CopyMemoryToImageIndirectCommandKHR',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageCopy',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.ImageCopy2',
-- 'Vulkan.Core10.Image.ImageCreateInfo',
-- 'Vulkan.Core10.DeviceInitialization.ImageFormatProperties',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageResolve',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.ImageResolve2',
-- 'Vulkan.Core14.PromotedStreamingTransfers'.ImageToMemoryCopy',
-- 'Vulkan.Core14.PromotedStreamingTransfers'.MemoryToImageCopy',
-- 'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageFormatProperties',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryBind',
-- 'Vulkan.Extensions.VK_QCOM_tile_properties.TilePropertiesQCOM'
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindImageMemoryDeviceGroupInfo',
-- 'Vulkan.Core10.CommandBufferBuilding.ClearRect',
-- 'Vulkan.Extensions.VK_QCOM_render_pass_transform.CommandBufferInheritanceRenderPassTransformInfoQCOM',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
-- 'Vulkan.Extensions.VK_KHR_display_swapchain.DisplayPresentInfoKHR',
-- 'Extent2D',
-- 'Vulkan.Extensions.VK_QCOM_multiview_per_view_render_areas.MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM',
-- 'Offset2D',
-- 'Vulkan.Extensions.VK_NV_optical_flow.OpticalFlowExecuteInfoNV',
-- 'Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateInfoEXT',
-- 'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV',
-- 'Vulkan.Core10.GraphicsPipeline.PipelineViewportStateCreateInfo',
-- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo',
-- 'Vulkan.Extensions.VK_ARM_render_pass_striped.RenderPassStripeInfoARM',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo',
-- 'Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleEXT',
-- 'Vulkan.Extensions.VK_NV_scissor_exclusive.cmdSetExclusiveScissorNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetScissor',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount',
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
-- 'Vulkan.Extensions.VK_EXT_debug_report.PFN_vkDebugReportCallbackEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.PFN_vkDebugUtilsMessengerCallbackEXT',
-- 'FALSE', 'TRUE',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryInstancesDataKHR',
-- 'Vulkan.Extensions.VK_KHR_unified_image_layouts.AttachmentFeedbackLoopInfoEXT',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkClusterAccelerationStructureMoveObjectsInputNV VkClusterAccelerationStructureMoveObjectsInputNV>,
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.ColorBlendAdvancedEXT',
-- 'Vulkan.Extensions.VK_EXT_conditional_rendering.CommandBufferInheritanceConditionalRenderingInfoEXT',
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo',
-- 'Vulkan.Extensions.VK_NV_inherited_viewport_scissor.CommandBufferInheritanceViewportScissorInfoNV',
-- 'Vulkan.Extensions.VK_NV_cooperative_matrix2.CooperativeMatrixFlexibleDimensionsPropertiesNV',
-- 'Vulkan.Extensions.VK_KHR_cooperative_matrix.CooperativeMatrixPropertiesKHR',
-- 'Vulkan.Extensions.VK_NV_cooperative_vector.CooperativeVectorPropertiesNV',
-- 'Vulkan.Extensions.VK_NVX_binary_import.CuModuleTexturingModeCreateInfoNVX',
-- 'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT',
-- 'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelinePropertyQueryResultARM',
-- 'Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationBufferCreateInfoNV',
-- 'Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationImageCreateInfoNV',
-- 'Vulkan.Extensions.VK_EXT_depth_bias_control.DepthBiasRepresentationInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.DescriptorMappingSourceIndirectIndexArrayEXT',
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.DescriptorMappingSourceIndirectIndexEXT',
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.DescriptorMappingSourcePushIndexEXT',
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.DescriptorMappingSourceShaderRecordIndexEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance3.DescriptorSetLayoutSupport',
-- 'Vulkan.Extensions.VK_KHR_pipeline_binary.DevicePipelineBinaryInternalCacheControlKHR',
-- 'Vulkan.Extensions.VK_NV_display_stereo.DisplayModeStereoPropertiesNV',
-- 'Vulkan.Extensions.VK_AMD_display_native_hdr.DisplayNativeHdrSurfaceCapabilitiesAMD',
-- 'Vulkan.Extensions.VK_KHR_display_swapchain.DisplayPresentInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_display.DisplayPropertiesKHR',
-- 'Vulkan.Extensions.VK_EXT_external_memory_acquire_unmodified.ExternalMemoryAcquireUnmodifiedEXT',
-- 'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT',
-- 'Vulkan.Core14.PromotedStreamingTransfers'.HostImageCopyDevicePerformanceQuery',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsLayoutTokenNV',
-- 'Vulkan.Extensions.VK_NV_low_latency2.LatencySleepModeInfoNV',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedRequirements',
-- 'Vulkan.Extensions.VK_EXT_opacity_micromap.MicromapBuildSizesInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.MultisampledRenderToSingleSampledInfoEXT',
-- 'Vulkan.Extensions.VK_NVX_multiview_per_view_attributes.MultiviewPerViewAttributesInfoNVX',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPartitionedAccelerationStructureFlagsNV VkPartitionedAccelerationStructureFlagsNV>,
-- 'Vulkan.Extensions.VK_EXT_present_timing.PastPresentationTimingEXT',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.PerformanceOverrideInfoINTEL',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.PerformanceValueDataINTEL',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage.PhysicalDevice16BitStorageFeatures',
-- 'Vulkan.Extensions.VK_EXT_4444_formats.PhysicalDevice4444FormatsFeaturesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage.PhysicalDevice8BitStorageFeatures',
-- 'Vulkan.Extensions.VK_EXT_astc_decode_mode.PhysicalDeviceASTCDecodeFeaturesEXT',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.PhysicalDeviceAccelerationStructureFeaturesKHR',
-- 'Vulkan.Extensions.VK_EXT_device_address_binding_report.PhysicalDeviceAddressBindingReportFeaturesEXT',
-- 'Vulkan.Extensions.VK_SEC_amigo_profiling.PhysicalDeviceAmigoProfilingFeaturesSEC',
-- 'Vulkan.Extensions.VK_AMD_anti_lag.PhysicalDeviceAntiLagFeaturesAMD',
-- 'Vulkan.Extensions.VK_EXT_attachment_feedback_loop_dynamic_state.PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_attachment_feedback_loop_layout.PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedPropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_border_color_swizzle.PhysicalDeviceBorderColorSwizzleFeaturesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeatures',
-- 'Vulkan.Extensions.VK_EXT_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeaturesEXT',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceClusterAccelerationStructureFeaturesNV VkPhysicalDeviceClusterAccelerationStructureFeaturesNV>,
-- 'Vulkan.Extensions.VK_HUAWEI_cluster_culling_shader.PhysicalDeviceClusterCullingShaderFeaturesHUAWEI',
-- 'Vulkan.Extensions.VK_HUAWEI_cluster_culling_shader.PhysicalDeviceClusterCullingShaderVrsFeaturesHUAWEI',
-- 'Vulkan.Extensions.VK_AMD_device_coherent_memory.PhysicalDeviceCoherentMemoryFeaturesAMD',
-- 'Vulkan.Extensions.VK_EXT_color_write_enable.PhysicalDeviceColorWriteEnableFeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_command_buffer_inheritance.PhysicalDeviceCommandBufferInheritanceFeaturesNV',
-- 'Vulkan.Extensions.VK_NV_compute_occupancy_priority.PhysicalDeviceComputeOccupancyPriorityFeaturesNV',
-- 'Vulkan.Extensions.VK_KHR_compute_shader_derivatives.PhysicalDeviceComputeShaderDerivativesFeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_compute_shader_derivatives.PhysicalDeviceComputeShaderDerivativesPropertiesKHR',
-- 'Vulkan.Extensions.VK_EXT_conditional_rendering.PhysicalDeviceConditionalRenderingFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_conservative_rasterization.PhysicalDeviceConservativeRasterizationPropertiesEXT',
-- 'Vulkan.Extensions.VK_NV_cooperative_matrix2.PhysicalDeviceCooperativeMatrix2FeaturesNV',
-- 'Vulkan.Extensions.VK_QCOM_cooperative_matrix_conversion.PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM',
-- 'Vulkan.Extensions.VK_KHR_cooperative_matrix.PhysicalDeviceCooperativeMatrixFeaturesKHR',
-- 'Vulkan.Extensions.VK_NV_cooperative_matrix.PhysicalDeviceCooperativeMatrixFeaturesNV',
-- 'Vulkan.Extensions.VK_NV_cooperative_vector.PhysicalDeviceCooperativeVectorFeaturesNV',
-- 'Vulkan.Extensions.VK_NV_cooperative_vector.PhysicalDeviceCooperativeVectorPropertiesNV',
-- 'Vulkan.Extensions.VK_KHR_copy_memory_indirect.PhysicalDeviceCopyMemoryIndirectFeaturesKHR',
-- 'Vulkan.Extensions.VK_NV_copy_memory_indirect.PhysicalDeviceCopyMemoryIndirectFeaturesNV',
-- 'Vulkan.Extensions.VK_NV_corner_sampled_image.PhysicalDeviceCornerSampledImageFeaturesNV',
-- 'Vulkan.Extensions.VK_NV_coverage_reduction_mode.PhysicalDeviceCoverageReductionModeFeaturesNV',
-- 'Vulkan.Extensions.VK_QCOM_filter_cubic_clamp.PhysicalDeviceCubicClampFeaturesQCOM',
-- 'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.PhysicalDeviceCubicWeightsFeaturesQCOM',
-- 'Vulkan.Extensions.VK_NV_cuda_kernel_launch.PhysicalDeviceCudaKernelLaunchFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_custom_border_color.PhysicalDeviceCustomBorderColorFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_custom_resolve.PhysicalDeviceCustomResolveFeaturesEXT',
-- 'Vulkan.Extensions.VK_ARM_data_graph.PhysicalDeviceDataGraphFeaturesARM',
-- 'Vulkan.Extensions.VK_QCOM_data_graph_model.PhysicalDeviceDataGraphModelFeaturesQCOM',
-- 'Vulkan.Extensions.VK_ARM_data_graph.PhysicalDeviceDataGraphProcessingEngineARM',
-- 'Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing.PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV',
-- 'Vulkan.Extensions.VK_AMDX_dense_geometry_format.PhysicalDeviceDenseGeometryFormatFeaturesAMDX',
-- 'Vulkan.Extensions.VK_EXT_depth_bias_control.PhysicalDeviceDepthBiasControlFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_depth_clamp_control.PhysicalDeviceDepthClampControlFeaturesEXT',
-- 'Vulkan.Extensions.VK_KHR_depth_clamp_zero_one.PhysicalDeviceDepthClampZeroOneFeaturesKHR',
-- 'Vulkan.Extensions.VK_EXT_depth_clip_control.PhysicalDeviceDepthClipControlFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_depth_clip_enable.PhysicalDeviceDepthClipEnableFeaturesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.PhysicalDeviceDepthStencilResolveProperties',
-- 'Vulkan.Extensions.VK_EXT_descriptor_buffer.PhysicalDeviceDescriptorBufferFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_descriptor_buffer.PhysicalDeviceDescriptorBufferPropertiesEXT',
-- 'Vulkan.Extensions.VK_ARM_tensors.PhysicalDeviceDescriptorBufferTensorFeaturesARM',
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.PhysicalDeviceDescriptorHeapFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.PhysicalDeviceDescriptorHeapPropertiesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingFeatures',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingProperties',
-- 'Vulkan.Extensions.VK_NV_descriptor_pool_overallocation.PhysicalDeviceDescriptorPoolOverallocationFeaturesNV',
-- 'Vulkan.Extensions.VK_VALVE_descriptor_set_host_mapping.PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands_compute.PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_device_generated_commands.PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.PhysicalDeviceDeviceGeneratedCommandsFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_device_generated_commands.PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_device_memory_report.PhysicalDeviceDeviceMemoryReportFeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_device_diagnostics_config.PhysicalDeviceDiagnosticsConfigFeaturesNV',
-- 'Vulkan.Extensions.VK_NV_displacement_micromap.PhysicalDeviceDisplacementMicromapFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_physical_device_drm.PhysicalDeviceDrmPropertiesEXT',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PhysicalDeviceDynamicRenderingFeatures',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap.PhysicalDeviceDynamicRenderingLocalReadFeatures',
-- 'Vulkan.Extensions.VK_EXT_dynamic_rendering_unused_attachments.PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_scissor_exclusive.PhysicalDeviceExclusiveScissorFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.PhysicalDeviceExtendedDynamicState2FeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.PhysicalDeviceExtendedDynamicState3FeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.PhysicalDeviceExtendedDynamicState3PropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.PhysicalDeviceExtendedDynamicStateFeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_extended_sparse_address_space.PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV',
-- 'Vulkan.Extensions.VK_ANDROID_external_format_resolve.PhysicalDeviceExternalFormatResolveFeaturesANDROID',
-- 'Vulkan.Extensions.VK_ANDROID_external_format_resolve.PhysicalDeviceExternalFormatResolvePropertiesANDROID',
-- 'Vulkan.Extensions.VK_NV_external_memory_rdma.PhysicalDeviceExternalMemoryRDMAFeaturesNV',
-- 'Vulkan.Extensions.VK_QNX_external_memory_screen_buffer.PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX',
-- 'Vulkan.Extensions.VK_EXT_device_fault.PhysicalDeviceFaultFeaturesEXT',
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceFeatures',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_shader_float_controls.PhysicalDeviceFloatControlsProperties',
-- 'Vulkan.Extensions.VK_ARM_format_pack.PhysicalDeviceFormatPackFeaturesARM',
-- 'Vulkan.Extensions.VK_EXT_fragment_density_map2.PhysicalDeviceFragmentDensityMap2FeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_fragment_density_map2.PhysicalDeviceFragmentDensityMap2PropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_fragment_density_map.PhysicalDeviceFragmentDensityMapFeaturesEXT',
-- 'Vulkan.Extensions.VK_VALVE_fragment_density_map_layered.PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE',
-- 'Vulkan.Extensions.VK_EXT_fragment_density_map_offset.PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_fragment_density_map.PhysicalDeviceFragmentDensityMapPropertiesEXT',
-- 'Vulkan.Extensions.VK_KHR_fragment_shader_barycentric.PhysicalDeviceFragmentShaderBarycentricFeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_fragment_shader_barycentric.PhysicalDeviceFragmentShaderBarycentricPropertiesKHR',
-- 'Vulkan.Extensions.VK_EXT_fragment_shader_interlock.PhysicalDeviceFragmentShaderInterlockFeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PhysicalDeviceFragmentShadingRateEnumsFeaturesNV',
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PhysicalDeviceFragmentShadingRateFeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PhysicalDeviceFragmentShadingRatePropertiesKHR',
-- 'Vulkan.Extensions.VK_EXT_frame_boundary.PhysicalDeviceFrameBoundaryFeaturesEXT',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_global_priority.PhysicalDeviceGlobalPriorityQueryFeatures',
-- 'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.PhysicalDeviceGroupProperties',
-- 'Vulkan.Extensions.VK_HUAWEI_hdr_vivid.PhysicalDeviceHdrVividFeaturesHUAWEI',
-- 'Vulkan.Core14.PromotedStreamingTransfers'.PhysicalDeviceHostImageCopyFeatures',
-- 'Vulkan.Core14.PromotedStreamingTransfers'.PhysicalDeviceHostImageCopyProperties',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.PhysicalDeviceHostQueryResetFeatures',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceIDProperties',
-- 'Vulkan.Extensions.VK_EXT_image_2d_view_of_3d.PhysicalDeviceImage2DViewOf3DFeaturesEXT',
-- 'Vulkan.Extensions.VK_MESA_image_alignment_control.PhysicalDeviceImageAlignmentControlFeaturesMESA',
-- 'Vulkan.Extensions.VK_EXT_image_compression_control.PhysicalDeviceImageCompressionControlFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_image_compression_control_swapchain.PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT',
-- 'Vulkan.Extensions.VK_QCOM_image_processing2.PhysicalDeviceImageProcessing2FeaturesQCOM',
-- 'Vulkan.Extensions.VK_QCOM_image_processing.PhysicalDeviceImageProcessingFeaturesQCOM',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_image_robustness.PhysicalDeviceImageRobustnessFeatures',
-- 'Vulkan.Extensions.VK_EXT_image_sliced_view_of_3d.PhysicalDeviceImageSlicedViewOf3DFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_image_view_min_lod.PhysicalDeviceImageViewMinLodFeaturesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.PhysicalDeviceImagelessFramebufferFeatures',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_index_type_uint8Roadmap.PhysicalDeviceIndexTypeUint8Features',
-- 'Vulkan.Extensions.VK_NV_inherited_viewport_scissor.PhysicalDeviceInheritedViewportScissorFeaturesNV',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_inline_uniform_block.PhysicalDeviceInlineUniformBlockFeatures',
-- 'Vulkan.Extensions.VK_KHR_internally_synchronized_queues.PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR',
-- 'Vulkan.Extensions.VK_HUAWEI_invocation_mask.PhysicalDeviceInvocationMaskFeaturesHUAWEI',
-- 'Vulkan.Extensions.VK_EXT_legacy_dithering.PhysicalDeviceLegacyDitheringFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_legacy_vertex_attributes.PhysicalDeviceLegacyVertexAttributesFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_legacy_vertex_attributes.PhysicalDeviceLegacyVertexAttributesPropertiesEXT',
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_line_rasterizationRoadmap.PhysicalDeviceLineRasterizationFeatures',
-- 'Vulkan.Extensions.VK_NV_linear_color_attachment.PhysicalDeviceLinearColorAttachmentFeaturesNV',
-- 'Vulkan.Extensions.VK_KHR_maintenance10.PhysicalDeviceMaintenance10FeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_maintenance10.PhysicalDeviceMaintenance10PropertiesKHR',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_maintenance4.PhysicalDeviceMaintenance4Features',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.PhysicalDeviceMaintenance5Features',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.PhysicalDeviceMaintenance5Properties',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'.PhysicalDeviceMaintenance6Features',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'.PhysicalDeviceMaintenance6Properties',
-- 'Vulkan.Extensions.VK_KHR_maintenance7.PhysicalDeviceMaintenance7FeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_maintenance7.PhysicalDeviceMaintenance7PropertiesKHR',
-- 'Vulkan.Extensions.VK_KHR_maintenance8.PhysicalDeviceMaintenance8FeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_maintenance9.PhysicalDeviceMaintenance9FeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_maintenance9.PhysicalDeviceMaintenance9PropertiesKHR',
-- 'Vulkan.Extensions.VK_EXT_map_memory_placed.PhysicalDeviceMapMemoryPlacedFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_memory_decompression.PhysicalDeviceMemoryDecompressionFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_memory_priority.PhysicalDeviceMemoryPriorityFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_mesh_shader.PhysicalDeviceMeshShaderFeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_mesh_shader.PhysicalDeviceMeshShaderFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_mesh_shader.PhysicalDeviceMeshShaderPropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_multi_draw.PhysicalDeviceMultiDrawFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewFeatures',
-- 'Vulkan.Extensions.VK_NVX_multiview_per_view_attributes.PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX',
-- 'Vulkan.Extensions.VK_QCOM_multiview_per_view_render_areas.PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM',
-- 'Vulkan.Extensions.VK_QCOM_multiview_per_view_viewports.PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM',
-- 'Vulkan.Extensions.VK_EXT_mutable_descriptor_type.PhysicalDeviceMutableDescriptorTypeFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_nested_command_buffer.PhysicalDeviceNestedCommandBufferFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_non_seamless_cube_map.PhysicalDeviceNonSeamlessCubeMapFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_opacity_micromap.PhysicalDeviceOpacityMicromapFeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_optical_flow.PhysicalDeviceOpticalFlowFeaturesNV',
-- 'Vulkan.Extensions.VK_NV_optical_flow.PhysicalDeviceOpticalFlowPropertiesNV',
-- 'Vulkan.Extensions.VK_EXT_pageable_device_local_memory.PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDevicePartitionedAccelerationStructureFeaturesNV VkPhysicalDevicePartitionedAccelerationStructureFeaturesNV>,
-- 'Vulkan.Extensions.VK_NV_per_stage_descriptor_set.PhysicalDevicePerStageDescriptorSetFeaturesNV',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDevicePerformanceCountersByRegionFeaturesARM VkPhysicalDevicePerformanceCountersByRegionFeaturesARM>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDevicePerformanceCountersByRegionPropertiesARM VkPhysicalDevicePerformanceCountersByRegionPropertiesARM>,
-- 'Vulkan.Extensions.VK_KHR_performance_query.PhysicalDevicePerformanceQueryFeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_performance_query.PhysicalDevicePerformanceQueryPropertiesKHR',
-- 'Vulkan.Extensions.VK_KHR_pipeline_binary.PhysicalDevicePipelineBinaryFeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_pipeline_binary.PhysicalDevicePipelineBinaryPropertiesKHR',
-- 'Vulkan.Extensions.VK_SEC_pipeline_cache_incremental_mode.PhysicalDevicePipelineCacheIncrementalModeFeaturesSEC',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_cache_control.PhysicalDevicePipelineCreationCacheControlFeatures',
-- 'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PhysicalDevicePipelineExecutablePropertiesFeaturesKHR',
-- 'Vulkan.Extensions.VK_EXT_pipeline_library_group_handles.PhysicalDevicePipelineLibraryGroupHandlesFeaturesEXT',
-- 'Vulkan.Extensions.VK_ARM_pipeline_opacity_micromap.PhysicalDevicePipelineOpacityMicromapFeaturesARM',
-- 'Vulkan.Extensions.VK_EXT_pipeline_properties.PhysicalDevicePipelinePropertiesFeaturesEXT',
-- 'Vulkan.Core14.Promoted_From_VK_EXT_pipeline_protected_accessAdditionalFunctionality'.PhysicalDevicePipelineProtectedAccessFeatures',
-- 'Vulkan.Core14.Promoted_From_VK_EXT_pipeline_robustnessAdditionalFunctionality'.PhysicalDevicePipelineRobustnessFeatures',
-- 'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR',
-- 'Vulkan.Extensions.VK_NV_present_barrier.PhysicalDevicePresentBarrierFeaturesNV',
-- 'Vulkan.Extensions.VK_KHR_present_id2.PhysicalDevicePresentId2FeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_present_id.PhysicalDevicePresentIdFeaturesKHR',
-- 'Vulkan.Extensions.VK_NV_present_metering.PhysicalDevicePresentMeteringFeaturesNV',
-- 'Vulkan.Extensions.VK_KHR_present_mode_fifo_latest_ready.PhysicalDevicePresentModeFifoLatestReadyFeaturesKHR',
-- 'Vulkan.Extensions.VK_EXT_present_timing.PhysicalDevicePresentTimingFeaturesEXT',
-- 'Vulkan.Extensions.VK_KHR_present_wait2.PhysicalDevicePresentWait2FeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_present_wait.PhysicalDevicePresentWaitFeaturesKHR',
-- 'Vulkan.Extensions.VK_EXT_primitive_topology_list_restart.PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_primitives_generated_query.PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_private_data.PhysicalDevicePrivateDataFeatures',
-- 'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.PhysicalDeviceProtectedMemoryFeatures',
-- 'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.PhysicalDeviceProtectedMemoryProperties',
-- 'Vulkan.Extensions.VK_EXT_provoking_vertex.PhysicalDeviceProvokingVertexFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_provoking_vertex.PhysicalDeviceProvokingVertexPropertiesEXT',
-- 'Vulkan.Extensions.VK_NV_push_constant_bank.PhysicalDevicePushConstantBankFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_rgba10x6_formats.PhysicalDeviceRGBA10X6FormatsFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_rasterization_order_attachment_access.PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_raw_access_chains.PhysicalDeviceRawAccessChainsFeaturesNV',
-- 'Vulkan.Extensions.VK_KHR_ray_query.PhysicalDeviceRayQueryFeaturesKHR',
-- 'Vulkan.Extensions.VK_EXT_ray_tracing_invocation_reorder.PhysicalDeviceRayTracingInvocationReorderFeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_ray_tracing_invocation_reorder.PhysicalDeviceRayTracingInvocationReorderFeaturesNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing_linear_swept_spheres.PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_maintenance1.PhysicalDeviceRayTracingMaintenance1FeaturesKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing_motion_blur.PhysicalDeviceRayTracingMotionBlurFeaturesNV',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.PhysicalDeviceRayTracingPipelineFeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_position_fetch.PhysicalDeviceRayTracingPositionFetchFeaturesKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing_validation.PhysicalDeviceRayTracingValidationFeaturesNV',
-- 'Vulkan.Extensions.VK_IMG_relaxed_line_rasterization.PhysicalDeviceRelaxedLineRasterizationFeaturesIMG',
-- 'Vulkan.Extensions.VK_ARM_render_pass_striped.PhysicalDeviceRenderPassStripedFeaturesARM',
-- 'Vulkan.Extensions.VK_NV_representative_fragment_test.PhysicalDeviceRepresentativeFragmentTestFeaturesNV',
-- 'Vulkan.Extensions.VK_KHR_robustness2.PhysicalDeviceRobustness2FeaturesKHR',
-- 'Vulkan.Extensions.VK_EXT_sample_locations.PhysicalDeviceSampleLocationsPropertiesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.PhysicalDeviceSamplerFilterMinmaxProperties',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.PhysicalDeviceSamplerYcbcrConversionFeatures',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout.PhysicalDeviceScalarBlockLayoutFeatures',
-- 'Vulkan.Extensions.VK_ARM_scheduling_controls.PhysicalDeviceSchedulingControlsFeaturesARM',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.PhysicalDeviceSeparateDepthStencilLayoutsFeatures',
-- 'Vulkan.Extensions.VK_EXT_shader_64bit_indexing.PhysicalDeviceShader64BitIndexingFeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_shader_atomic_float16_vector.PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_shader_atomic_float2.PhysicalDeviceShaderAtomicFloat2FeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_shader_atomic_float.PhysicalDeviceShaderAtomicFloatFeaturesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64.PhysicalDeviceShaderAtomicInt64Features',
-- 'Vulkan.Extensions.VK_KHR_shader_bfloat16.PhysicalDeviceShaderBfloat16FeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_shader_clock.PhysicalDeviceShaderClockFeaturesKHR',
-- 'Vulkan.Extensions.VK_ARM_shader_core_builtins.PhysicalDeviceShaderCoreBuiltinsFeaturesARM',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_shader_demote_to_helper_invocation.PhysicalDeviceShaderDemoteToHelperInvocationFeatures',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters.PhysicalDeviceShaderDrawParametersFeatures',
-- 'Vulkan.Extensions.VK_AMD_shader_early_and_late_fragment_tests.PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD',
-- 'Vulkan.Extensions.VK_AMDX_shader_enqueue.PhysicalDeviceShaderEnqueueFeaturesAMDX',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_shader_expect_assumeRoadmap.PhysicalDeviceShaderExpectAssumeFeatures',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8.PhysicalDeviceShaderFloat16Int8Features',
-- 'Vulkan.Extensions.VK_EXT_shader_float8.PhysicalDeviceShaderFloat8FeaturesEXT',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_shader_float_controls2Roadmap.PhysicalDeviceShaderFloatControls2Features',
-- 'Vulkan.Extensions.VK_KHR_shader_fma.PhysicalDeviceShaderFmaFeaturesKHR',
-- 'Vulkan.Extensions.VK_EXT_shader_image_atomic_int64.PhysicalDeviceShaderImageAtomicInt64FeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_shader_image_footprint.PhysicalDeviceShaderImageFootprintFeaturesNV',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_shader_integer_dot_product.PhysicalDeviceShaderIntegerDotProductFeatures',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_shader_integer_dot_product.PhysicalDeviceShaderIntegerDotProductProperties',
-- 'Vulkan.Extensions.VK_INTEL_shader_integer_functions2.PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL',
-- 'Vulkan.Extensions.VK_EXT_shader_long_vector.PhysicalDeviceShaderLongVectorFeaturesEXT',
-- 'Vulkan.Extensions.VK_KHR_shader_maximal_reconvergence.PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR',
-- 'Vulkan.Extensions.VK_EXT_shader_module_identifier.PhysicalDeviceShaderModuleIdentifierFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_shader_object.PhysicalDeviceShaderObjectFeaturesEXT',
-- 'Vulkan.Extensions.VK_KHR_shader_quad_control.PhysicalDeviceShaderQuadControlFeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_shader_relaxed_extended_instruction.PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR',
-- 'Vulkan.Extensions.VK_EXT_shader_replicated_composites.PhysicalDeviceShaderReplicatedCompositesFeaturesEXT',
-- 'Vulkan.Extensions.VK_NV_shader_sm_builtins.PhysicalDeviceShaderSMBuiltinsFeaturesNV',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types.PhysicalDeviceShaderSubgroupExtendedTypesFeatures',
-- 'Vulkan.Extensions.VK_EXT_shader_subgroup_partitioned.PhysicalDeviceShaderSubgroupPartitionedFeaturesEXT',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_shader_subgroup_rotateRoadmap.PhysicalDeviceShaderSubgroupRotateFeatures',
-- 'Vulkan.Extensions.VK_KHR_shader_subgroup_uniform_control_flow.PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_shader_terminate_invocation.PhysicalDeviceShaderTerminateInvocationFeatures',
-- 'Vulkan.Extensions.VK_EXT_shader_tile_image.PhysicalDeviceShaderTileImageFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_shader_tile_image.PhysicalDeviceShaderTileImagePropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_shader_uniform_buffer_unsized_array.PhysicalDeviceShaderUniformBufferUnsizedArrayFeaturesEXT',
-- 'Vulkan.Extensions.VK_KHR_shader_untyped_pointers.PhysicalDeviceShaderUntypedPointersFeaturesKHR',
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.PhysicalDeviceShadingRateImageFeaturesNV',
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceSparseProperties',
-- 'Vulkan.Core11.Originally_Based_On_VK_KHR_subgroup.PhysicalDeviceSubgroupProperties',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control.PhysicalDeviceSubgroupSizeControlFeatures',
-- 'Vulkan.Extensions.VK_EXT_subpass_merge_feedback.PhysicalDeviceSubpassMergeFeedbackFeaturesEXT',
-- 'Vulkan.Extensions.VK_HUAWEI_subpass_shading.PhysicalDeviceSubpassShadingFeaturesHUAWEI',
-- 'Vulkan.Extensions.VK_KHR_swapchain_maintenance1.PhysicalDeviceSwapchainMaintenance1FeaturesKHR',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.PhysicalDeviceSynchronization2Features',
-- 'Vulkan.Extensions.VK_ARM_tensors.PhysicalDeviceTensorFeaturesARM',
-- 'Vulkan.Extensions.VK_ARM_tensors.PhysicalDeviceTensorPropertiesARM',
-- 'Vulkan.Extensions.VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentFeaturesEXT',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentProperties',
-- 'Vulkan.Extensions.VK_EXT_texture_compression_astc_3d.PhysicalDeviceTextureCompressionASTC3DFeaturesEXT',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_texture_compression_astc_hdr.PhysicalDeviceTextureCompressionASTCHDRFeatures',
-- 'Vulkan.Extensions.VK_QCOM_tile_memory_heap.PhysicalDeviceTileMemoryHeapFeaturesQCOM',
-- 'Vulkan.Extensions.VK_QCOM_tile_memory_heap.PhysicalDeviceTileMemoryHeapPropertiesQCOM',
-- 'Vulkan.Extensions.VK_QCOM_tile_properties.PhysicalDeviceTilePropertiesFeaturesQCOM',
-- 'Vulkan.Extensions.VK_QCOM_tile_shading.PhysicalDeviceTileShadingFeaturesQCOM',
-- 'Vulkan.Extensions.VK_QCOM_tile_shading.PhysicalDeviceTileShadingPropertiesQCOM',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.PhysicalDeviceTimelineSemaphoreFeatures',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackPropertiesEXT',
-- 'Vulkan.Extensions.VK_KHR_unified_image_layouts.PhysicalDeviceUnifiedImageLayoutsFeaturesKHR',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout.PhysicalDeviceUniformBufferStandardLayoutFeatures',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers.PhysicalDeviceVariablePointersFeatures',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap.PhysicalDeviceVertexAttributeDivisorFeatures',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap.PhysicalDeviceVertexAttributeDivisorProperties',
-- 'Vulkan.Extensions.VK_EXT_vertex_attribute_robustness.PhysicalDeviceVertexAttributeRobustnessFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.PhysicalDeviceVertexInputDynamicStateFeaturesEXT',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceVideoDecodeVP9FeaturesKHR VkPhysicalDeviceVideoDecodeVP9FeaturesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceVideoEncodeAV1FeaturesKHR VkPhysicalDeviceVideoEncodeAV1FeaturesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceVideoEncodeIntraRefreshFeaturesKHR VkPhysicalDeviceVideoEncodeIntraRefreshFeaturesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceVideoEncodeQuantizationMapFeaturesKHR VkPhysicalDeviceVideoEncodeQuantizationMapFeaturesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceVideoEncodeRgbConversionFeaturesVALVE VkPhysicalDeviceVideoEncodeRgbConversionFeaturesVALVE>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceVideoMaintenance1FeaturesKHR VkPhysicalDeviceVideoMaintenance1FeaturesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceVideoMaintenance2FeaturesKHR VkPhysicalDeviceVideoMaintenance2FeaturesKHR>,
-- 'Vulkan.Core12.PhysicalDeviceVulkan11Features',
-- 'Vulkan.Core12.PhysicalDeviceVulkan11Properties',
-- 'Vulkan.Core12.PhysicalDeviceVulkan12Features',
-- 'Vulkan.Core12.PhysicalDeviceVulkan12Properties',
-- 'Vulkan.Core13.PhysicalDeviceVulkan13Features',
-- 'Vulkan.Core13.PhysicalDeviceVulkan13Properties',
-- 'Vulkan.Core14.PhysicalDeviceVulkan14Features',
-- 'Vulkan.Core14.PhysicalDeviceVulkan14Properties',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model.PhysicalDeviceVulkanMemoryModelFeatures',
-- 'Vulkan.Extensions.VK_KHR_workgroup_memory_explicit_layout.PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR',
-- 'Vulkan.Extensions.VK_EXT_ycbcr_2plane_444_formats.PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT',
-- 'Vulkan.Extensions.VK_QCOM_ycbcr_degamma.PhysicalDeviceYcbcrDegammaFeaturesQCOM',
-- 'Vulkan.Extensions.VK_EXT_ycbcr_image_arrays.PhysicalDeviceYcbcrImageArraysFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_zero_initialize_device_memory.PhysicalDeviceZeroInitializeDeviceMemoryFeaturesEXT',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_zero_initialize_workgroup_memory.PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures',
-- 'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PipelineColorBlendAdvancedStateCreateInfoEXT',
-- 'Vulkan.Core10.GraphicsPipeline.PipelineColorBlendAttachmentState',
-- 'Vulkan.Core10.GraphicsPipeline.PipelineColorBlendStateCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_color_write_enable.PipelineColorWriteCreateInfoEXT',
-- 'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.PipelineCoverageModulationStateCreateInfoNV',
-- 'Vulkan.Extensions.VK_NV_fragment_coverage_to_color.PipelineCoverageToColorStateCreateInfoNV',
-- 'Vulkan.Core10.GraphicsPipeline.PipelineDepthStencilStateCreateInfo',
-- 'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PipelineExecutableInternalRepresentationKHR',
-- 'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PipelineExecutableStatisticValueKHR',
-- 'Vulkan.Core10.GraphicsPipeline.PipelineInputAssemblyStateCreateInfo',
-- 'Vulkan.Core10.GraphicsPipeline.PipelineMultisampleStateCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_depth_clip_enable.PipelineRasterizationDepthClipStateCreateInfoEXT',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_line_rasterizationRoadmap.PipelineRasterizationLineStateCreateInfo',
-- 'Vulkan.Core10.GraphicsPipeline.PipelineRasterizationStateCreateInfo',
-- 'Vulkan.Extensions.VK_NV_representative_fragment_test.PipelineRepresentativeFragmentTestStateCreateInfoNV',
-- 'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_depth_clip_control.PipelineViewportDepthClipControlCreateInfoEXT',
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV',
-- 'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV',
-- 'Vulkan.Extensions.VK_EXT_present_timing.PresentTimingSurfaceCapabilitiesEXT',
-- 'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.ProtectedSubmitInfo',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFamilyQueryResultStatusPropertiesKHR VkQueueFamilyQueryResultStatusPropertiesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkRayTracingPipelineClusterAccelerationStructureCreateInfoNV VkRayTracingPipelineClusterAccelerationStructureCreateInfoNV>,
-- 'Vulkan.Extensions.VK_EXT_subpass_merge_feedback.RenderPassCreationControlEXT',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkRenderPassPerformanceCountersByRegionBeginInfoARM VkRenderPassPerformanceCountersByRegionBeginInfoARM>,
-- 'Vulkan.Extensions.VK_EXT_border_color_swizzle.SamplerBorderColorComponentMappingCreateInfoEXT',
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo',
-- 'Vulkan.Extensions.VK_QCOM_ycbcr_degamma.SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM',
-- 'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.SubpassResolvePerformanceQueryEXT',
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceCapabilitiesFullScreenExclusiveEXT',
-- 'Vulkan.Extensions.VK_NV_present_barrier.SurfaceCapabilitiesPresentBarrierNV',
-- 'Vulkan.Extensions.VK_KHR_present_id2.SurfaceCapabilitiesPresentId2KHR',
-- 'Vulkan.Extensions.VK_KHR_present_wait2.SurfaceCapabilitiesPresentWait2KHR',
-- 'Vulkan.Extensions.VK_KHR_surface_protected_capabilities.SurfaceProtectedCapabilitiesKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR',
-- 'Vulkan.Extensions.VK_AMD_display_native_hdr.SwapchainDisplayNativeHdrCreateInfoAMD',
-- 'Vulkan.Extensions.VK_NV_low_latency2.SwapchainLatencyCreateInfoNV',
-- 'Vulkan.Extensions.VK_NV_present_barrier.SwapchainPresentBarrierCreateInfoNV',
-- 'Vulkan.Extensions.VK_AMD_texture_gather_bias_lod.TextureLODGatherFormatPropertiesAMD',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoDecodeAV1ProfileInfoKHR VkVideoDecodeAV1ProfileInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeAV1CapabilitiesKHR VkVideoEncodeAV1CapabilitiesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeAV1GopRemainingFrameInfoKHR VkVideoEncodeAV1GopRemainingFrameInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeAV1PictureInfoKHR VkVideoEncodeAV1PictureInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeAV1RateControlLayerInfoKHR VkVideoEncodeAV1RateControlLayerInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeAV1SessionCreateInfoKHR VkVideoEncodeAV1SessionCreateInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH264CapabilitiesKHR VkVideoEncodeH264CapabilitiesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH264GopRemainingFrameInfoKHR VkVideoEncodeH264GopRemainingFrameInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH264PictureInfoKHR VkVideoEncodeH264PictureInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH264QualityLevelPropertiesKHR VkVideoEncodeH264QualityLevelPropertiesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH264RateControlLayerInfoKHR VkVideoEncodeH264RateControlLayerInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH264SessionCreateInfoKHR VkVideoEncodeH264SessionCreateInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH264SessionParametersFeedbackInfoKHR VkVideoEncodeH264SessionParametersFeedbackInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH264SessionParametersGetInfoKHR VkVideoEncodeH264SessionParametersGetInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH265CapabilitiesKHR VkVideoEncodeH265CapabilitiesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH265GopRemainingFrameInfoKHR VkVideoEncodeH265GopRemainingFrameInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH265RateControlLayerInfoKHR VkVideoEncodeH265RateControlLayerInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH265SessionCreateInfoKHR VkVideoEncodeH265SessionCreateInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH265SessionParametersFeedbackInfoKHR VkVideoEncodeH265SessionParametersFeedbackInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH265SessionParametersGetInfoKHR VkVideoEncodeH265SessionParametersGetInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeIntraRefreshCapabilitiesKHR VkVideoEncodeIntraRefreshCapabilitiesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeProfileRgbConversionInfoVALVE VkVideoEncodeProfileRgbConversionInfoVALVE>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeSessionParametersFeedbackInfoKHR VkVideoEncodeSessionParametersFeedbackInfoKHR>,
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdBuildAccelerationStructureNV',
-- 'Vulkan.Extensions.VK_EXT_device_generated_commands.cmdExecuteGeneratedCommandsEXT',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.cmdExecuteGeneratedCommandsNV',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetAlphaToCoverageEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetAlphaToOneEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageModulationTableEnableNV',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageToColorEnableNV',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetDepthBiasEnable',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetDepthBiasEnable',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthBoundsTestEnable',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthBoundsTestEnable',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetDepthClampEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetDepthClipEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetDepthClipNegativeOneToOneEXT',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthTestEnable',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthTestEnable',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthWriteEnable',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthWriteEnable',
-- 'Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleEnableEXT',
-- 'Vulkan.Extensions.VK_NV_scissor_exclusive.cmdSetExclusiveScissorEnableNV',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineStippleEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLogicOpEnableEXT',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetPrimitiveRestartEnable',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetPrimitiveRestartEnable',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRepresentativeFragmentTestEnableNV',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleLocationsEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetShadingRateImageEnableNV',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetStencilTestEnable',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetStencilTestEnable',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetViewportWScalingEnableNV',
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR',
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.registerCustomBorderColorEXT',
-- 'Vulkan.Extensions.VK_AMD_display_native_hdr.setLocalDimmingAMD',
-- 'Vulkan.Core10.Fence.waitForFences'
newtype Bool32 = Bool32 Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkBool32" "VK_FALSE"
pattern FALSE = Bool32 0

-- No documentation found for Nested "VkBool32" "VK_TRUE"
pattern TRUE = Bool32 1

{-# COMPLETE
  FALSE
  , TRUE ::
    Bool32
  #-}

conNameBool32 :: String
conNameBool32 = "Bool32"

enumPrefixBool32 :: String
enumPrefixBool32 = ""

showTableBool32 :: [(Bool32, String)]
showTableBool32 = [(FALSE, "FALSE"), (TRUE, "TRUE")]

instance Show Bool32 where
  showsPrec =
    enumShowsPrec
      enumPrefixBool32
      showTableBool32
      conNameBool32
      (\(Bool32 x) -> x)
      (showsPrec 11)

instance Read Bool32 where
  readPrec =
    enumReadPrec
      enumPrefixBool32
      showTableBool32
      conNameBool32
      Bool32

-- | VkSampleMask - Mask of sample coverage information
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.GraphicsPipeline.PipelineMultisampleStateCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleMaskEXT'
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
-- the bitwise OR of valid bit flags.
--
-- An individual bit flag is valid for a 'Flags' type if it would be a
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fundamentals-validusage-enums valid enumerant>
-- when used with the equivalent @Vk*FlagBits@ type, where the bits type is
-- obtained by taking the flag type and replacing the trailing 'Flags' with
-- @FlagBits@. For example, a flag value of type
-- 'Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlags' /must/
-- contain only bit flags defined by
-- 'Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlagBits'.
--
-- Any 'Flags' member or parameter returned from a query command or
-- otherwise output from Vulkan to the application /may/ contain bit flags
-- undefined in its corresponding @Vk*FlagBits@ type. An application
-- /cannot/ rely on the state of these unspecified bits.
--
-- Only the low-order 31 bits (bit positions zero through 30) are available
-- for use as flag bits.
--
-- This restriction is due to poorly defined behavior by C compilers given
-- a C enumerant value of @0x80000000@. In some cases adding this enumerant
-- value may increase the size of the underlying @Vk*FlagBits@ type,
-- breaking the ABI.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureCreateFlagsKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing_motion_blur.AccelerationStructureMotionInfoFlagsNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing_motion_blur.AccelerationStructureMotionInstanceFlagsNV',
-- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlags',
-- 'Vulkan.Extensions.VK_KHR_performance_query.AcquireProfilingLockFlagsKHR',
-- 'Vulkan.Extensions.VK_KHR_copy_memory_indirect.AddressCopyFlagsKHR',
-- 'Vulkan.Extensions.VK_KHR_android_surface.AndroidSurfaceCreateFlagsKHR',
-- 'Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.AttachmentDescriptionFlags',
-- 'Vulkan.Core10.Enums.BufferCreateFlagBits.BufferCreateFlags',
-- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlags',
-- 'Vulkan.Core10.Enums.BufferViewCreateFlags.BufferViewCreateFlags',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.BuildAccelerationStructureFlagsKHR',
-- 'Vulkan.Extensions.VK_EXT_opacity_micromap.BuildMicromapFlagsEXT',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkClusterAccelerationStructureAddressResolutionFlagsNV VkClusterAccelerationStructureAddressResolutionFlagsNV>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkClusterAccelerationStructureClusterFlagsNV VkClusterAccelerationStructureClusterFlagsNV>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkClusterAccelerationStructureGeometryFlagsNV VkClusterAccelerationStructureGeometryFlagsNV>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkClusterAccelerationStructureIndexFormatFlagsNV VkClusterAccelerationStructureIndexFormatFlagsNV>,
-- 'Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlags',
-- 'Vulkan.Core10.Enums.CommandBufferResetFlagBits.CommandBufferResetFlags',
-- 'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.CommandBufferUsageFlags',
-- 'Vulkan.Core10.Enums.CommandPoolCreateFlagBits.CommandPoolCreateFlags',
-- 'Vulkan.Core10.Enums.CommandPoolResetFlagBits.CommandPoolResetFlags',
-- 'Vulkan.Core11.Enums.CommandPoolTrimFlags.CommandPoolTrimFlags',
-- 'Vulkan.Extensions.VK_KHR_surface.CompositeAlphaFlagsKHR',
-- 'Vulkan.Extensions.VK_EXT_conditional_rendering.ConditionalRenderingFlagsEXT',
-- 'Vulkan.Core10.Enums.CullModeFlagBits.CullModeFlags',
-- 'Vulkan.Extensions.VK_EXT_debug_report.DebugReportFlagsEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsMessageSeverityFlagsEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsMessageTypeFlagsEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsMessengerCallbackDataFlagsEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsMessengerCreateFlagsEXT',
-- 'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlags',
-- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DescriptorBindingFlags',
-- 'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DescriptorPoolCreateFlags',
-- 'Vulkan.Core10.Enums.DescriptorPoolResetFlags.DescriptorPoolResetFlags',
-- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DescriptorSetLayoutCreateFlags',
-- 'Vulkan.Core11.Enums.DescriptorUpdateTemplateCreateFlags.DescriptorUpdateTemplateCreateFlags',
-- 'Vulkan.Extensions.VK_EXT_device_address_binding_report.DeviceAddressBindingFlagsEXT',
-- 'Vulkan.Core10.Enums.DeviceCreateFlags.DeviceCreateFlags',
-- 'Vulkan.Extensions.VK_NV_device_diagnostics_config.DeviceDiagnosticsConfigFlagsNV',
-- 'Vulkan.Extensions.VK_KHR_swapchain.DeviceGroupPresentModeFlagsKHR',
-- 'Vulkan.Extensions.VK_EXT_device_memory_report.DeviceMemoryReportFlagsEXT',
-- 'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DeviceQueueCreateFlags',
-- 'Vulkan.Extensions.VK_LUNARG_direct_driver_loading.DirectDriverLoadingFlagsLUNARG',
-- 'Vulkan.Extensions.VK_EXT_directfb_surface.DirectFBSurfaceCreateFlagsEXT',
-- 'Vulkan.Extensions.VK_KHR_display.DisplayModeCreateFlagsKHR',
-- 'Vulkan.Extensions.VK_KHR_display.DisplayPlaneAlphaFlagsKHR',
-- 'Vulkan.Extensions.VK_KHR_display.DisplaySurfaceCreateFlagsKHR',
-- 'Vulkan.Core10.Enums.EventCreateFlagBits.EventCreateFlags',
-- 'Vulkan.Extensions.VK_EXT_metal_objects.ExportMetalObjectTypeFlagsEXT',
-- 'Vulkan.Core11.Enums.ExternalFenceFeatureFlagBits.ExternalFenceFeatureFlags',
-- 'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.ExternalFenceHandleTypeFlags',
-- 'Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits.ExternalMemoryFeatureFlags',
-- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryFeatureFlagsNV',
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlags',
-- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagsNV',
-- 'Vulkan.Core11.Enums.ExternalSemaphoreFeatureFlagBits.ExternalSemaphoreFeatureFlags',
-- 'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlags',
-- 'Vulkan.Core10.Enums.FenceCreateFlagBits.FenceCreateFlags',
-- 'Vulkan.Core11.Enums.FenceImportFlagBits.FenceImportFlags', 'Flags64',
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlags',
-- 'Vulkan.Extensions.VK_EXT_frame_boundary.FrameBoundaryFlagsEXT',
-- 'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FramebufferCreateFlags',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryFlagsKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryInstanceFlagsKHR',
-- 'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryFlagsEXT',
-- 'Vulkan.Extensions.VK_EXT_headless_surface.HeadlessSurfaceCreateFlagsEXT',
-- 'Vulkan.Core14.Enums.HostImageCopyFlagBits.HostImageCopyFlags',
-- 'Vulkan.Extensions.VK_MVK_ios_surface.IOSSurfaceCreateFlagsMVK',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlags',
-- 'Vulkan.Extensions.VK_EXT_image_compression_control.ImageCompressionFixedRateFlagsEXT',
-- 'Vulkan.Extensions.VK_EXT_image_compression_control.ImageCompressionFlagsEXT',
-- 'Vulkan.Extensions.VK_FUCHSIA_buffer_collection.ImageConstraintsInfoFlagsFUCHSIA',
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlags',
-- 'Vulkan.Extensions.VK_FUCHSIA_buffer_collection.ImageFormatConstraintsFlagsFUCHSIA',
-- 'Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface.ImagePipeSurfaceCreateFlagsFUCHSIA',
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Vulkan.Core10.Enums.ImageViewCreateFlagBits.ImageViewCreateFlags',
-- 'Vulkan.Extensions.VK_EXT_device_generated_commands.IndirectCommandsInputModeFlagsEXT',
-- 'Vulkan.Extensions.VK_EXT_device_generated_commands.IndirectCommandsLayoutUsageFlagsEXT',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsLayoutUsageFlagsNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectStateFlagsNV',
-- 'Vulkan.Core10.Enums.InstanceCreateFlagBits.InstanceCreateFlags',
-- 'Vulkan.Extensions.VK_MVK_macos_surface.MacOSSurfaceCreateFlagsMVK',
-- 'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MemoryAllocateFlags',
-- 'Vulkan.Core10.Enums.MemoryHeapFlagBits.MemoryHeapFlags',
-- 'Vulkan.Core10.Enums.MemoryMapFlagBits.MemoryMapFlags',
-- 'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MemoryPropertyFlags',
-- 'Vulkan.Core14.Enums.MemoryUnmapFlagBits.MemoryUnmapFlags',
-- 'Vulkan.Extensions.VK_EXT_metal_surface.MetalSurfaceCreateFlagsEXT',
-- 'Vulkan.Extensions.VK_EXT_opacity_micromap.MicromapCreateFlagsEXT',
-- 'Vulkan.Extensions.VK_NV_optical_flow.OpticalFlowExecuteFlagsNV',
-- 'Vulkan.Extensions.VK_NV_optical_flow.OpticalFlowGridSizeFlagsNV',
-- 'Vulkan.Extensions.VK_NV_optical_flow.OpticalFlowSessionCreateFlagsNV',
-- 'Vulkan.Extensions.VK_NV_optical_flow.OpticalFlowUsageFlagsNV',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPartitionedAccelerationStructureInstanceFlagsNV VkPartitionedAccelerationStructureInstanceFlagsNV>,
-- 'Vulkan.Extensions.VK_EXT_present_timing.PastPresentationTimingFlagsEXT',
-- 'Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits.PeerMemoryFeatureFlags',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPerformanceCounterDescriptionFlagsARM VkPerformanceCounterDescriptionFlagsARM>,
-- 'Vulkan.Extensions.VK_KHR_performance_query.PerformanceCounterDescriptionFlagsKHR',
-- 'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PipelineCacheCreateFlags',
-- 'Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits.PipelineColorBlendStateCreateFlags',
-- 'Vulkan.Extensions.VK_AMD_pipeline_compiler_control.PipelineCompilerControlFlagsAMD',
-- 'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.PipelineCoverageModulationStateCreateFlagsNV',
-- 'Vulkan.Extensions.VK_NV_coverage_reduction_mode.PipelineCoverageReductionStateCreateFlagsNV',
-- 'Vulkan.Extensions.VK_NV_fragment_coverage_to_color.PipelineCoverageToColorStateCreateFlagsNV',
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlags',
-- 'Vulkan.Core13.Enums.PipelineCreationFeedbackFlagBits.PipelineCreationFeedbackFlags',
-- 'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PipelineDepthStencilStateCreateFlags',
-- 'Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateFlagsEXT',
-- 'Vulkan.Core10.Enums.PipelineDynamicStateCreateFlags.PipelineDynamicStateCreateFlags',
-- 'Vulkan.Core10.Enums.PipelineInputAssemblyStateCreateFlags.PipelineInputAssemblyStateCreateFlags',
-- 'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PipelineLayoutCreateFlags',
-- 'Vulkan.Core10.Enums.PipelineMultisampleStateCreateFlags.PipelineMultisampleStateCreateFlags',
-- 'Vulkan.Extensions.VK_EXT_conservative_rasterization.PipelineRasterizationConservativeStateCreateFlagsEXT',
-- 'Vulkan.Extensions.VK_EXT_depth_clip_enable.PipelineRasterizationDepthClipStateCreateFlagsEXT',
-- 'Vulkan.Core10.Enums.PipelineRasterizationStateCreateFlags.PipelineRasterizationStateCreateFlags',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.PipelineRasterizationStateStreamCreateFlagsEXT',
-- 'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PipelineShaderStageCreateFlags',
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlags',
-- 'Vulkan.Core10.Enums.PipelineTessellationStateCreateFlags.PipelineTessellationStateCreateFlags',
-- 'Vulkan.Core10.Enums.PipelineVertexInputStateCreateFlags.PipelineVertexInputStateCreateFlags',
-- 'Vulkan.Core10.Enums.PipelineViewportStateCreateFlags.PipelineViewportStateCreateFlags',
-- 'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateFlagsNV',
-- 'Vulkan.Extensions.VK_KHR_surface_maintenance1.PresentGravityFlagsKHR',
-- 'Vulkan.Extensions.VK_KHR_surface_maintenance1.PresentScalingFlagsKHR',
-- 'Vulkan.Extensions.VK_EXT_present_timing.PresentStageFlagsEXT',
-- 'Vulkan.Extensions.VK_EXT_present_timing.PresentTimingInfoFlagsEXT',
-- 'Vulkan.Core13.Enums.PrivateDataSlotCreateFlags.PrivateDataSlotCreateFlags',
-- 'Vulkan.Core10.Enums.QueryControlFlagBits.QueryControlFlags',
-- 'Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits.QueryPipelineStatisticFlags',
-- 'Vulkan.Core10.Enums.QueryPoolCreateFlagBits.QueryPoolCreateFlags',
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QueryResultFlags',
-- 'Vulkan.Core10.Enums.QueueFlagBits.QueueFlags',
-- 'Vulkan.Core10.Enums.RenderPassCreateFlagBits.RenderPassCreateFlags',
-- 'Vulkan.Extensions.VK_KHR_maintenance10.RenderingAttachmentFlagsKHR',
-- 'Vulkan.Core13.Enums.RenderingFlagBits.RenderingFlags',
-- 'Vulkan.Extensions.VK_KHR_maintenance10.ResolveImageFlagsKHR',
-- 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlags',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlags',
-- 'Vulkan.Core10.Enums.SamplerCreateFlagBits.SamplerCreateFlags',
-- 'Vulkan.Extensions.VK_QNX_screen_surface.ScreenSurfaceCreateFlagsQNX',
-- 'Vulkan.Core10.Enums.SemaphoreCreateFlags.SemaphoreCreateFlags',
-- 'Vulkan.Core11.Enums.SemaphoreImportFlagBits.SemaphoreImportFlags',
-- 'Vulkan.Core12.Enums.SemaphoreWaitFlagBits.SemaphoreWaitFlags',
-- 'Vulkan.Extensions.VK_AMD_shader_core_properties2.ShaderCorePropertiesFlagsAMD',
-- 'Vulkan.Extensions.VK_EXT_shader_object.ShaderCreateFlagsEXT',
-- 'Vulkan.Core10.Enums.ShaderModuleCreateFlags.ShaderModuleCreateFlags',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.SparseImageFormatFlagBits.SparseImageFormatFlags',
-- 'Vulkan.Core10.Enums.SparseMemoryBindFlagBits.SparseMemoryBindFlags',
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.SpirvResourceTypeFlagsEXT',
-- 'Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlags',
-- 'Vulkan.Extensions.VK_GGP_stream_descriptor_surface.StreamDescriptorSurfaceCreateFlagsGGP',
-- 'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SubgroupFeatureFlags',
-- 'Vulkan.Core13.Enums.SubmitFlagBits.SubmitFlags',
-- 'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SubpassDescriptionFlags',
-- 'Vulkan.Extensions.VK_EXT_display_surface_counter.SurfaceCounterFlagsEXT',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSurfaceCreateFlagsOHOS VkSurfaceCreateFlagsOHOS>,
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagsKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateFlagsKHR',
-- 'Vulkan.Extensions.VK_QCOM_tile_shading.TileShadingRenderPassFlagsQCOM',
-- 'Vulkan.Core13.Enums.ToolPurposeFlagBits.ToolPurposeFlags',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkUbmSurfaceCreateFlagsSEC VkUbmSurfaceCreateFlagsSEC>,
-- 'Vulkan.Extensions.VK_EXT_validation_cache.ValidationCacheCreateFlagsEXT',
-- 'Vulkan.Extensions.VK_NN_vi_surface.ViSurfaceCreateFlagsNN',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoBeginCodingFlagsKHR VkVideoBeginCodingFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoCapabilityFlagsKHR VkVideoCapabilityFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoChromaSubsamplingFlagsKHR VkVideoChromaSubsamplingFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoCodecOperationFlagsKHR VkVideoCodecOperationFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoCodingControlFlagsKHR VkVideoCodingControlFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoComponentBitDepthFlagsKHR VkVideoComponentBitDepthFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoDecodeCapabilityFlagsKHR VkVideoDecodeCapabilityFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoDecodeFlagsKHR VkVideoDecodeFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoDecodeH264PictureLayoutFlagsKHR VkVideoDecodeH264PictureLayoutFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoDecodeUsageFlagsKHR VkVideoDecodeUsageFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeAV1CapabilityFlagsKHR VkVideoEncodeAV1CapabilityFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeAV1RateControlFlagsKHR VkVideoEncodeAV1RateControlFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeAV1StdFlagsKHR VkVideoEncodeAV1StdFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeAV1SuperblockSizeFlagsKHR VkVideoEncodeAV1SuperblockSizeFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeCapabilityFlagsKHR VkVideoEncodeCapabilityFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeContentFlagsKHR VkVideoEncodeContentFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeFeedbackFlagsKHR VkVideoEncodeFeedbackFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeFlagsKHR VkVideoEncodeFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH264CapabilityFlagsKHR VkVideoEncodeH264CapabilityFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH264RateControlFlagsKHR VkVideoEncodeH264RateControlFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH264StdFlagsKHR VkVideoEncodeH264StdFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH265CapabilityFlagsKHR VkVideoEncodeH265CapabilityFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH265CtbSizeFlagsKHR VkVideoEncodeH265CtbSizeFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH265RateControlFlagsKHR VkVideoEncodeH265RateControlFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH265StdFlagsKHR VkVideoEncodeH265StdFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH265TransformBlockSizeFlagsKHR VkVideoEncodeH265TransformBlockSizeFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeIntraRefreshModeFlagsKHR VkVideoEncodeIntraRefreshModeFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeRateControlFlagsKHR VkVideoEncodeRateControlFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeRateControlModeFlagsKHR VkVideoEncodeRateControlModeFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeRgbChromaOffsetFlagsVALVE VkVideoEncodeRgbChromaOffsetFlagsVALVE>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeRgbModelConversionFlagsVALVE VkVideoEncodeRgbModelConversionFlagsVALVE>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeRgbRangeCompressionFlagsVALVE VkVideoEncodeRgbRangeCompressionFlagsVALVE>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeUsageFlagsKHR VkVideoEncodeUsageFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEndCodingFlagsKHR VkVideoEndCodingFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoSessionCreateFlagsKHR VkVideoSessionCreateFlagsKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoSessionParametersCreateFlagsKHR VkVideoSessionParametersCreateFlagsKHR>,
-- 'Vulkan.Extensions.VK_KHR_wayland_surface.WaylandSurfaceCreateFlagsKHR',
-- 'Vulkan.Extensions.VK_KHR_win32_surface.Win32SurfaceCreateFlagsKHR',
-- 'Vulkan.Extensions.VK_KHR_xcb_surface.XcbSurfaceCreateFlagsKHR',
-- 'Vulkan.Extensions.VK_KHR_xlib_surface.XlibSurfaceCreateFlagsKHR'
type Flags = Word32


-- | VkFlags64 - Vulkan 64-bit bitmasks
--
-- = Description
--
-- When the 31 bits available in 'Flags' are insufficient, the 'Flags64'
-- type can be passed to commands and structures to represent up to 64
-- options. 'Flags64' is not used directly in the API. Instead, a
-- @Vk*Flags2@ type which is an alias of 'Flags64', and whose name matches
-- the corresponding @Vk*FlagBits2@ that are valid for that type, is used.
--
-- Any @Vk*Flags2@ member or parameter used in the API as an input /must/
-- be a valid combination of bit flags. A valid combination is either zero
-- or the bitwise OR of valid bit flags.
--
-- An individual bit flag is valid for a @Vk*Flags2@ type if it would be a
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fundamentals-validusage-enums valid enumerant>
-- when used with the equivalent @Vk*FlagBits2@ type, where the bits type
-- is obtained by taking the flag type and replacing the trailing @Flags2@
-- with @FlagBits2@. For example, a flag value of type
-- 'Vulkan.Extensions.VK_KHR_synchronization2.AccessFlags2KHR' /must/
-- contain only bit flags defined by
-- 'Vulkan.Extensions.VK_KHR_synchronization2.AccessFlagBits2KHR'.
--
-- Any @Vk*Flags2@ member or parameter returned from a query command or
-- otherwise output from Vulkan to the application /may/ contain bit flags
-- undefined in its corresponding @Vk*FlagBits2@ type. An application
-- /cannot/ rely on the state of these unspecified bits.
--
-- Both the @Vk*FlagBits2@ type, and the individual bits defined for that
-- type, are defined as @uint64_t@ integers in the C API. This is in
-- contrast to the 32-bit types, where the @Vk*FlagBits@ type is defined as
-- a C @enum@ and the individual bits as enumerants belonging to that
-- @enum@. As a result, there is less compile time type checking possible
-- for the 64-bit types. This is unavoidable since there is no sufficiently
-- portable way to define a 64-bit @enum@ type in C99.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core13.Enums.AccessFlags2.AccessFlags2',
-- 'Vulkan.Extensions.VK_KHR_maintenance8.AccessFlags3KHR',
-- 'Vulkan.Core14.Enums.BufferUsageFlags2.BufferUsageFlags2',
-- 'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineDispatchFlagsARM',
-- 'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineSessionCreateFlagsARM',
-- 'Flags', 'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlags2',
-- 'Vulkan.Extensions.VK_EXT_memory_decompression.MemoryDecompressionMethodFlagsEXT',
-- 'Vulkan.Extensions.VK_ARM_scheduling_controls.PhysicalDeviceSchedulingControlsFlagsARM',
-- 'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlags2',
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2',
-- 'Vulkan.Extensions.VK_ARM_tensors.TensorCreateFlagsARM',
-- 'Vulkan.Extensions.VK_ARM_tensors.TensorUsageFlagsARM',
-- 'Vulkan.Extensions.VK_ARM_tensors.TensorViewCreateFlagsARM'
type Flags64 = Word64


-- | VkDeviceSize - Vulkan device memory size and offsets
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureBuildSizesInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureCreateInfoKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.AccelerationStructureCreateInfoNV',
-- 'Vulkan.Extensions.VK_AMDX_dense_geometry_format.AccelerationStructureDenseGeometryFormatTrianglesDataAMDX',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryAabbsDataKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing_linear_swept_spheres.AccelerationStructureGeometryLinearSweptSpheresDataNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing_linear_swept_spheres.AccelerationStructureGeometrySpheresDataNV',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR',
-- 'Vulkan.Extensions.VK_NV_displacement_micromap.AccelerationStructureTrianglesDisplacementMicromapNV',
-- 'Vulkan.Extensions.VK_EXT_opacity_micromap.AccelerationStructureTrianglesOpacityMicromapEXT',
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferPropertiesANDROID',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.BindAccelerationStructureMemoryInfoNV',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo',
-- 'Vulkan.Extensions.VK_ARM_data_graph.BindDataGraphPipelineSessionMemoryInfoARM',
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.BindHeapInfoEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo',
-- 'Vulkan.Extensions.VK_ARM_tensors.BindTensorMemoryInfoARM',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkBindVideoSessionMemoryInfoKHR VkBindVideoSessionMemoryInfoKHR>,
-- 'Vulkan.Core10.CommandBufferBuilding.BufferCopy',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.BufferCopy2',
-- 'Vulkan.Core10.Buffer.BufferCreateInfo',
-- 'Vulkan.Core10.CommandBufferBuilding.BufferImageCopy',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.BufferImageCopy2',
-- 'Vulkan.Core10.OtherTypes.BufferMemoryBarrier',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.BufferMemoryBarrier2',
-- 'Vulkan.Core10.BufferView.BufferViewCreateInfo',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkClusterAccelerationStructureMoveObjectsInputNV VkClusterAccelerationStructureMoveObjectsInputNV>,
-- 'Vulkan.Extensions.VK_NV_device_generated_commands_compute.ComputePipelineIndirectBufferInfoNV',
-- 'Vulkan.Extensions.VK_EXT_conditional_rendering.ConditionalRenderingBeginInfoEXT',
-- 'Vulkan.Extensions.VK_KHR_copy_memory_indirect.CopyMemoryIndirectCommandKHR',
-- 'Vulkan.Extensions.VK_EXT_memory_decompression.DecompressMemoryRegionEXT',
-- 'Vulkan.Extensions.VK_NV_memory_decompression.DecompressMemoryRegionNV',
-- 'Vulkan.Extensions.VK_EXT_descriptor_buffer.DescriptorAddressInfoEXT',
-- 'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo',
-- 'Vulkan.Extensions.VK_EXT_device_address_binding_report.DeviceAddressBindingCallbackDataEXT',
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.DeviceAddressRangeEXT',
-- 'Vulkan.Extensions.VK_EXT_device_fault.DeviceFaultAddressInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_device_fault.DeviceFaultCountsEXT',
-- 'Vulkan.Extensions.VK_EXT_device_memory_report.DeviceMemoryReportCallbackDataEXT',
-- 'Vulkan.Extensions.VK_AMDX_shader_enqueue.ExecutionGraphPipelineScratchSizeAMDX',
-- 'Vulkan.Extensions.VK_EXT_device_generated_commands.GeneratedCommandsInfoEXT',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GeneratedCommandsInfoNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.GeometryAABBNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.GeometryTrianglesNV',
-- 'Vulkan.Core10.DeviceInitialization.ImageFormatProperties',
-- 'Vulkan.Extensions.VK_NVX_image_view_handle.ImageViewAddressPropertiesNVX',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsStreamNV',
-- 'Vulkan.Core10.Memory.MappedMemoryRange',
-- 'Vulkan.Core10.Memory.MemoryAllocateInfo',
-- 'Vulkan.Core10.DeviceInitialization.MemoryHeap',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_map_memory2Roadmap.MemoryMapInfo',
-- 'Vulkan.Core10.MemoryManagement.MemoryRequirements',
-- 'Vulkan.Extensions.VK_EXT_opacity_micromap.MicromapBuildInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_opacity_micromap.MicromapBuildSizesInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_opacity_micromap.MicromapCreateInfoEXT',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkNativeBufferPropertiesOHOS VkNativeBufferPropertiesOHOS>,
-- 'Vulkan.Extensions.VK_HUAWEI_cluster_culling_shader.PhysicalDeviceClusterCullingShaderPropertiesHUAWEI',
-- 'Vulkan.Extensions.VK_EXT_descriptor_buffer.PhysicalDeviceDescriptorBufferPropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.PhysicalDeviceDescriptorHeapPropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.PhysicalDeviceDescriptorHeapTensorPropertiesARM',
-- 'Vulkan.Extensions.VK_NV_extended_sparse_address_space.PhysicalDeviceExtendedSparseAddressSpacePropertiesNV',
-- 'Vulkan.Extensions.VK_EXT_external_memory_host.PhysicalDeviceExternalMemoryHostPropertiesEXT',
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance3.PhysicalDeviceMaintenance3Properties',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_maintenance4.PhysicalDeviceMaintenance4Properties',
-- 'Vulkan.Extensions.VK_EXT_map_memory_placed.PhysicalDeviceMapMemoryPlacedPropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_memory_budget.PhysicalDeviceMemoryBudgetPropertiesEXT',
-- 'Vulkan.Extensions.VK_KHR_robustness2.PhysicalDeviceRobustness2PropertiesKHR',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentProperties',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackPropertiesEXT',
-- 'Vulkan.Core12.PhysicalDeviceVulkan11Properties',
-- 'Vulkan.Core13.PhysicalDeviceVulkan13Properties',
-- 'Vulkan.Extensions.VK_QNX_external_memory_screen_buffer.ScreenBufferPropertiesQNX',
-- 'Vulkan.Extensions.VK_KHR_maintenance6.SetDescriptorBufferOffsetsInfoEXT',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryBind',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryRequirements',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseMemoryBind',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkStridedDeviceAddressNV VkStridedDeviceAddressNV>,
-- 'Vulkan.Extensions.VK_KHR_copy_memory_indirect.StridedDeviceAddressRangeKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.StridedDeviceAddressRegionKHR',
-- 'Vulkan.Core14.PromotedStreamingTransfers'.SubresourceHostMemcpySize',
-- 'Vulkan.Core10.Image.SubresourceLayout',
-- 'Vulkan.Extensions.VK_QCOM_tile_memory_heap.TileMemoryRequirementsQCOM',
-- 'Vulkan.Extensions.VK_QCOM_tile_memory_heap.TileMemorySizeInfoQCOM',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_maintenance1.TraceRaysIndirectCommand2KHR',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoCapabilitiesKHR VkVideoCapabilitiesKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoDecodeInfoKHR VkVideoDecodeInfoKHR>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeInfoKHR VkVideoEncodeInfoKHR>,
-- 'Vulkan.Core10.MemoryManagement.bindBufferMemory',
-- 'Vulkan.Core10.MemoryManagement.bindImageMemory',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginTransformFeedbackEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.cmdBindIndexBuffer2',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.cmdBindIndexBuffer2',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBindTransformFeedbackBuffersEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdBuildAccelerationStructureNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults',
-- 'Vulkan.Extensions.VK_AMDX_shader_enqueue.cmdDispatchGraphAMDX',
-- 'Vulkan.Extensions.VK_AMDX_shader_enqueue.cmdDispatchGraphIndirectAMDX',
-- 'Vulkan.Extensions.VK_AMDX_shader_enqueue.cmdDispatchGraphIndirectCountAMDX',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDispatchIndirect',
-- 'Vulkan.Extensions.VK_HUAWEI_cluster_culling_shader.cmdDrawClusterIndirectHUAWEI',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCount',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCount',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCount',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdDrawIndirectByteCountEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndirectCount',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndirectCount',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndirectCount',
-- 'Vulkan.Extensions.VK_EXT_mesh_shader.cmdDrawMeshTasksIndirectCountEXT',
-- 'Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectCountNV',
-- 'Vulkan.Extensions.VK_EXT_mesh_shader.cmdDrawMeshTasksIndirectEXT',
-- 'Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectNV',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndTransformFeedbackEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdFillBuffer',
-- 'Vulkan.Extensions.VK_AMDX_shader_enqueue.cmdInitializeGraphScratchMemoryAMDX',
-- 'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdTraceRaysNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdUpdateBuffer',
-- 'Vulkan.Extensions.VK_AMD_buffer_marker.cmdWriteBufferMarker2AMD',
-- 'Vulkan.Extensions.VK_AMD_buffer_marker.cmdWriteBufferMarkerAMD',
-- 'Vulkan.Extensions.VK_EXT_descriptor_buffer.getDescriptorSetLayoutBindingOffsetEXT',
-- 'Vulkan.Extensions.VK_EXT_descriptor_buffer.getDescriptorSetLayoutSizeEXT',
-- 'Vulkan.Core10.Memory.getDeviceMemoryCommitment',
-- 'Vulkan.Core10.Query.getQueryPoolResults',
-- 'Vulkan.Core10.Memory.mapMemory'
type DeviceSize = Word64


-- | VkDeviceAddress - Vulkan device address type
--
-- == Valid Usage
--
-- -   #VUID-VkDeviceAddress-size-11364# A valid 'DeviceAddress' /must/ be
--     equal to the sum of an address retrieved from a
--     'Vulkan.Core10.Handles.Buffer' via
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress',
--     and any offset in the range [0, @size@), where @size@ is the value
--     of 'Vulkan.Core10.Buffer.BufferCreateInfo'::@size@ used to create
--     that 'Vulkan.Core10.Handles.Buffer'
--
-- -   #VUID-VkDeviceAddress-None-10894# If a 'DeviceAddress' was retrieved
--     from a non-sparse buffer, that buffer /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureCreateInfoKHR',
-- 'Vulkan.Extensions.VK_EXT_device_generated_commands.BindIndexBufferIndirectCommandEXT',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.BindIndexBufferIndirectCommandNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands_compute.BindPipelineIndirectCommandNV',
-- 'Vulkan.Extensions.VK_EXT_device_generated_commands.BindVertexBufferIndirectCommandEXT',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.BindVertexBufferIndirectCommandNV',
-- 'Vulkan.Extensions.VK_EXT_buffer_device_address.BufferDeviceAddressCreateInfoEXT',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkBuildPartitionedAccelerationStructureInfoNV VkBuildPartitionedAccelerationStructureInfoNV>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkClusterAccelerationStructureBuildClustersBottomLevelInfoNV VkClusterAccelerationStructureBuildClustersBottomLevelInfoNV>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkClusterAccelerationStructureBuildTriangleClusterInfoNV VkClusterAccelerationStructureBuildTriangleClusterInfoNV>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkClusterAccelerationStructureBuildTriangleClusterTemplateInfoNV VkClusterAccelerationStructureBuildTriangleClusterTemplateInfoNV>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkClusterAccelerationStructureCommandsInfoNV VkClusterAccelerationStructureCommandsInfoNV>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkClusterAccelerationStructureGetTemplateIndicesInfoNV VkClusterAccelerationStructureGetTemplateIndicesInfoNV>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkClusterAccelerationStructureInstantiateClusterInfoNV VkClusterAccelerationStructureInstantiateClusterInfoNV>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkClusterAccelerationStructureMoveObjectsInfoNV VkClusterAccelerationStructureMoveObjectsInfoNV>,
-- 'Vulkan.Extensions.VK_NV_device_generated_commands_compute.ComputePipelineIndirectBufferInfoNV',
-- 'Vulkan.Extensions.VK_KHR_copy_memory_indirect.CopyMemoryIndirectCommandKHR',
-- 'Vulkan.Extensions.VK_KHR_copy_memory_indirect.CopyMemoryToImageIndirectCommandKHR',
-- 'Vulkan.Extensions.VK_EXT_memory_decompression.DecompressMemoryRegionEXT',
-- 'Vulkan.Extensions.VK_NV_memory_decompression.DecompressMemoryRegionNV',
-- 'Vulkan.Extensions.VK_EXT_descriptor_buffer.DescriptorAddressInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_descriptor_buffer.DescriptorBufferBindingInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_descriptor_buffer.DescriptorDataEXT',
-- 'Vulkan.Extensions.VK_EXT_device_address_binding_report.DeviceAddressBindingCallbackDataEXT',
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.DeviceAddressRangeEXT',
-- 'Vulkan.Extensions.VK_EXT_device_fault.DeviceFaultAddressInfoEXT',
-- 'Vulkan.Extensions.VK_AMDX_shader_enqueue.DeviceOrHostAddressConstAMDX',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressKHR',
-- 'Vulkan.Extensions.VK_EXT_device_generated_commands.DrawIndirectCountIndirectCommandEXT',
-- 'Vulkan.Extensions.VK_EXT_device_generated_commands.GeneratedCommandsInfoEXT',
-- 'Vulkan.Extensions.VK_NVX_image_view_handle.ImageViewAddressPropertiesNVX',
-- 'Vulkan.Extensions.VK_EXT_opacity_micromap.MicromapCreateInfoEXT',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPartitionedAccelerationStructureUpdateInstanceDataNV VkPartitionedAccelerationStructureUpdateInstanceDataNV>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPartitionedAccelerationStructureWriteInstanceDataNV VkPartitionedAccelerationStructureWriteInstanceDataNV>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkRenderPassPerformanceCountersByRegionBeginInfoARM VkRenderPassPerformanceCountersByRegionBeginInfoARM>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkStridedDeviceAddressNV VkStridedDeviceAddressNV>,
-- 'Vulkan.Extensions.VK_KHR_copy_memory_indirect.StridedDeviceAddressRangeKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.StridedDeviceAddressRegionKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_maintenance1.TraceRaysIndirectCommand2KHR',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkWriteDescriptorSetPartitionedAccelerationStructureNV VkWriteDescriptorSetPartitionedAccelerationStructureNV>,
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdBuildAccelerationStructuresIndirectKHR',
-- 'Vulkan.Extensions.VK_NV_copy_memory_indirect.cmdCopyMemoryIndirectNV',
-- 'Vulkan.Extensions.VK_NV_copy_memory_indirect.cmdCopyMemoryToImageIndirectNV',
-- 'Vulkan.Extensions.VK_EXT_memory_decompression.cmdDecompressMemoryIndirectCountEXT',
-- 'Vulkan.Extensions.VK_NV_memory_decompression.cmdDecompressMemoryIndirectCountNV',
-- 'Vulkan.Extensions.VK_AMDX_shader_enqueue.cmdDispatchGraphAMDX',
-- 'Vulkan.Extensions.VK_AMDX_shader_enqueue.cmdDispatchGraphIndirectAMDX',
-- 'Vulkan.Extensions.VK_AMDX_shader_enqueue.cmdDispatchGraphIndirectCountAMDX',
-- 'Vulkan.Extensions.VK_AMDX_shader_enqueue.cmdInitializeGraphScratchMemoryAMDX',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_maintenance1.cmdTraceRaysIndirect2KHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.cmdTraceRaysIndirectKHR'
type DeviceAddress = Word64

