{-# language CPP #-}
-- No documentation found for Chapter "Core14"
module Vulkan.Core14  ( pattern API_VERSION_1_4
                      , PhysicalDeviceVulkan14Features(..)
                      , PhysicalDeviceVulkan14Properties(..)
                      , StructureType(..)
                      , module Vulkan.Core14.Enums
                      , module Vulkan.Core14.PromotedStreamingTransfers'
                      , module Vulkan.Core14.Promoted_From_VK_EXT_pipeline_protected_accessAdditionalFunctionality'
                      , module Vulkan.Core14.Promoted_From_VK_EXT_pipeline_robustnessAdditionalFunctionality'
                      , module Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap
                      , module Vulkan.Core14.Promoted_From_VK_KHR_global_priority
                      , module Vulkan.Core14.Promoted_From_VK_KHR_index_type_uint8Roadmap
                      , module Vulkan.Core14.Promoted_From_VK_KHR_line_rasterizationRoadmap
                      , module Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap
                      , module Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'
                      , module Vulkan.Core14.Promoted_From_VK_KHR_map_memory2Roadmap
                      , module Vulkan.Core14.Promoted_From_VK_KHR_push_descriptorRoadmap
                      , module Vulkan.Core14.Promoted_From_VK_KHR_shader_expect_assumeRoadmap
                      , module Vulkan.Core14.Promoted_From_VK_KHR_shader_float_controls2Roadmap
                      , module Vulkan.Core14.Promoted_From_VK_KHR_shader_subgroup_rotateRoadmap
                      , module Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap
                      ) where
import Vulkan.Core14.Enums
import Vulkan.Core14.PromotedStreamingTransfers'
import Vulkan.Core14.Promoted_From_VK_EXT_pipeline_protected_accessAdditionalFunctionality'
import Vulkan.Core14.Promoted_From_VK_EXT_pipeline_robustnessAdditionalFunctionality'
import Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap
import Vulkan.Core14.Promoted_From_VK_KHR_global_priority
import Vulkan.Core14.Promoted_From_VK_KHR_index_type_uint8Roadmap
import Vulkan.Core14.Promoted_From_VK_KHR_line_rasterizationRoadmap
import Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap
import Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'
import Vulkan.Core14.Promoted_From_VK_KHR_map_memory2Roadmap
import Vulkan.Core14.Promoted_From_VK_KHR_push_descriptorRoadmap
import Vulkan.Core14.Promoted_From_VK_KHR_shader_expect_assumeRoadmap
import Vulkan.Core14.Promoted_From_VK_KHR_shader_float_controls2Roadmap
import Vulkan.Core14.Promoted_From_VK_KHR_shader_subgroup_rotateRoadmap
import Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap
import Vulkan.CStruct.Utils (FixedArray)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthByteString)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior (PipelineRobustnessBufferBehavior)
import Vulkan.Core14.Enums.PipelineRobustnessImageBehavior (PipelineRobustnessImageBehavior)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.APIConstants (UUID_SIZE)
import Vulkan.Version (pattern MAKE_API_VERSION)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_4_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_4_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
pattern API_VERSION_1_4 :: Word32
pattern API_VERSION_1_4 = MAKE_API_VERSION 1 4 0


-- | VkPhysicalDeviceVulkan14Features - Structure describing the Vulkan 1.4
-- features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceVulkan14Features' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceVulkan14Features', it /must/ add an instance of the
-- structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVulkan14Features = PhysicalDeviceVulkan14Features
  { -- | #features-globalPriorityQuery# @globalPriorityQuery@ indicates whether
    -- the implementation supports the ability to query global queue
    -- priorities.
    globalPriorityQuery :: Bool
  , -- | #features-shaderSubgroupRotate# @shaderSubgroupRotate@ specifies whether
    -- shader modules /can/ declare the @GroupNonUniformRotateKHR@ capability.
    shaderSubgroupRotate :: Bool
  , -- | #features-shaderSubgroupRotateClustered# @shaderSubgroupRotateClustered@
    -- specifies whether shader modules /can/ use the @ClusterSize@ operand to
    -- @OpGroupNonUniformRotateKHR@.
    shaderSubgroupRotateClustered :: Bool
  , -- | #features-shaderFloatControls2# @shaderFloatControls2@ specifies whether
    -- shader modules /can/ declare the @FloatControls2@ capability.
    shaderFloatControls2 :: Bool
  , -- | #features-shaderExpectAssume# @shaderExpectAssume@ specifies whether
    -- shader modules /can/ declare the @ExpectAssumeKHR@ capability.
    shaderExpectAssume :: Bool
  , -- | #features-rectangularLines# @rectangularLines@ indicates whether the
    -- implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines rectangular line rasterization>.
    rectangularLines :: Bool
  , -- | #features-bresenhamLines# @bresenhamLines@ indicates whether the
    -- implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-bresenham Bresenham-style line rasterization>.
    bresenhamLines :: Bool
  , -- | #features-smoothLines# @smoothLines@ indicates whether the
    -- implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-smooth smooth line rasterization>.
    smoothLines :: Bool
  , -- | #features-stippledRectangularLines# @stippledRectangularLines@ indicates
    -- whether the implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-stipple stippled line rasterization>
    -- with
    -- 'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_RECTANGULAR'
    -- lines.
    stippledRectangularLines :: Bool
  , -- | #features-stippledBresenhamLines# @stippledBresenhamLines@ indicates
    -- whether the implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-stipple stippled line rasterization>
    -- with
    -- 'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_BRESENHAM'
    -- lines.
    stippledBresenhamLines :: Bool
  , -- | #features-stippledSmoothLines# @stippledSmoothLines@ indicates whether
    -- the implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-stipple stippled line rasterization>
    -- with
    -- 'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH'
    -- lines.
    stippledSmoothLines :: Bool
  , -- | #features-vertexAttributeInstanceRateDivisor#
    -- @vertexAttributeInstanceRateDivisor@ specifies whether vertex attribute
    -- fetching may be repeated in the case of instanced rendering.
    vertexAttributeInstanceRateDivisor :: Bool
  , -- | #features-vertexAttributeInstanceRateZeroDivisor#
    -- @vertexAttributeInstanceRateZeroDivisor@ specifies whether a zero value
    -- for
    -- 'Vulkan.Extensions.VK_EXT_vertex_attribute_divisor.VertexInputBindingDivisorDescriptionEXT'::@divisor@
    -- is supported.
    vertexAttributeInstanceRateZeroDivisor :: Bool
  , -- | #features-indexTypeUint8# @indexTypeUint8@ indicates that
    -- 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT8' can be used with
    -- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.cmdBindIndexBuffer2'
    -- and 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'.
    indexTypeUint8 :: Bool
  , -- | #features-dynamicRenderingLocalRead# @dynamicRenderingLocalRead@
    -- specifies that the implementation supports local reads inside dynamic
    -- render pass instances using the
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
    -- command.
    dynamicRenderingLocalRead :: Bool
  , -- | #features-maintenance5# @maintenance5@ indicates that the implementation
    -- supports the following:
    --
    -- -   The ability to expose support for the optional format
    --     'Vulkan.Core10.Enums.Format.FORMAT_A1B5G5R5_UNORM_PACK16'.
    --
    -- -   The ability to expose support for the optional format
    --     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM'.
    --
    -- -   A property to indicate that multisample coverage operations are
    --     performed after sample counting in EarlyFragmentTests mode.
    --
    -- -   Creating a 'Vulkan.Core10.Handles.BufferView' with a subset of the
    --     associated 'Vulkan.Core10.Handles.Buffer' usage using
    --     'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.BufferUsageFlags2CreateInfo'.
    --
    -- -   A new function
    --     'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.cmdBindIndexBuffer2',
    --     allowing a range of memory to be bound as an index buffer.
    --
    -- -   'Vulkan.Core10.DeviceInitialization.getDeviceProcAddr' will return
    --     @NULL@ for function pointers of core functions for versions higher
    --     than the version requested by the application.
    --
    -- -   'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2'
    --     supports using 'Vulkan.Core10.APIConstants.WHOLE_SIZE' in the
    --     @pSizes@ parameter.
    --
    -- -   If @PointSize@ is not written, a default value of @1.0@ is used for
    --     the size of points.
    --
    -- -   'Vulkan.Core10.Shader.ShaderModuleCreateInfo' /can/ be added as a
    --     chained structure to pipeline creation via
    --     'Vulkan.Core10.ComputePipeline.PipelineShaderStageCreateInfo',
    --     rather than having to create a shader module.
    --
    -- -   A function
    --     'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.getRenderingAreaGranularity'
    --     to query the optimal render area for a dynamic rendering instance.
    --
    -- -   A property to indicate that depth\/stencil texturing operations with
    --     'Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_ONE' have
    --     defined behavior.
    --
    -- -   'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.getDeviceImageSubresourceLayout'
    --     allows an application to perform a
    --     'Vulkan.Core10.Image.getImageSubresourceLayout' query without having
    --     to create an image.
    --
    -- -   'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS' as the
    --     @layerCount@ member of
    --     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'.
    --
    -- -   A property to indicate whether @PointSize@ controls the final
    --     rasterization of polygons if
    --     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-polygonmode polygon mode>
    --     is 'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_POINT'.
    --
    -- -   Two properties to indicate the non-strict line rasterization
    --     algorithm used.
    --
    -- -   Two new flags words
    --     'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlagBits2'
    --     and 'Vulkan.Core14.Enums.BufferUsageFlags2.BufferUsageFlagBits2'.
    --
    -- -   Physical-device-level functions /can/ now be called with any value
    --     in the valid range for a type beyond the defined enumerants, such
    --     that applications can avoid checking individual features,
    --     extensions, or versions before querying supported properties of a
    --     particular enumerant.
    --
    -- -   Copies between images of any type are allowed, with 1D images
    --     treated as 2D images with a height of @1@.
    maintenance5 :: Bool
  , -- | #features-maintenance6# @maintenance6@ indicates that the implementation
    -- supports the following:
    --
    -- -   'Vulkan.Core10.APIConstants.NULL_HANDLE' /can/ be used when binding
    --     an index buffer
    --
    -- -   'Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'.BindMemoryStatus'
    --     /can/ be included in the @pNext@ chain of the
    --     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo'
    --     and
    --     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo'
    --     structures, enabling applications to retrieve
    --     'Vulkan.Core10.Enums.Result.Result' values for individual memory
    --     binding operations.
    --
    -- -   'Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'.PhysicalDeviceMaintenance6Properties'::@blockTexelViewCompatibleMultipleLayers@
    --     property to indicate that the implementation supports creating image
    --     views with
    --     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
    --     where the @layerCount@ member of @subresourceRange@ is greater than
    --     @1@.
    --
    -- -   'Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'.PhysicalDeviceMaintenance6Properties'::@maxCombinedImageSamplerDescriptorCount@
    --     property which indicates the maximum descriptor size required for
    --     any
    --     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion format that requires a sampler Y′CBCR conversion>
    --     supported by the implementation.
    --
    -- -   A
    --     'Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'.PhysicalDeviceMaintenance6Properties'::@fragmentShadingRateClampCombinerInputs@
    --     property which indicates whether the implementation clamps the
    --     inputs to fragment shading rate combiner operations.
    maintenance6 :: Bool
  , -- | #features-pipelineProtectedAccess# @pipelineProtectedAccess@ indicates
    -- whether the implementation supports specifying protected access on
    -- individual pipelines.
    pipelineProtectedAccess :: Bool
  , -- | #features-pipelineRobustness# @pipelineRobustness@ indicates that
    -- robustness /can/ be requested on a per-pipeline-stage granularity.
    pipelineRobustness :: Bool
  , -- | #features-hostImageCopy# @hostImageCopy@ indicates that the
    -- implementation supports copying from host memory to images using the
    -- 'Vulkan.Core14.PromotedStreamingTransfers'.copyMemoryToImage' command,
    -- copying from images to host memory using the
    -- 'Vulkan.Core14.PromotedStreamingTransfers'.copyImageToMemory' command,
    -- and copying between images using the
    -- 'Vulkan.Core14.PromotedStreamingTransfers'.copyImageToImage' command.
    hostImageCopy :: Bool
  , -- | #features-pushDescriptor# @pushDescriptor@ indicates that the
    -- implementation supports push descriptors.
    pushDescriptor :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVulkan14Features)
#endif
deriving instance Show PhysicalDeviceVulkan14Features

instance ToCStruct PhysicalDeviceVulkan14Features where
  withCStruct x f = allocaBytes 104 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVulkan14Features{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_4_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (globalPriorityQuery))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (shaderSubgroupRotate))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (shaderSubgroupRotateClustered))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (shaderFloatControls2))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (shaderExpectAssume))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (rectangularLines))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (bresenhamLines))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (smoothLines))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (stippledRectangularLines))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (stippledBresenhamLines))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (stippledSmoothLines))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (vertexAttributeInstanceRateDivisor))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (vertexAttributeInstanceRateZeroDivisor))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (indexTypeUint8))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (dynamicRenderingLocalRead))
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (maintenance5))
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (maintenance6))
    poke ((p `plusPtr` 84 :: Ptr Bool32)) (boolToBool32 (pipelineProtectedAccess))
    poke ((p `plusPtr` 88 :: Ptr Bool32)) (boolToBool32 (pipelineRobustness))
    poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (hostImageCopy))
    poke ((p `plusPtr` 96 :: Ptr Bool32)) (boolToBool32 (pushDescriptor))
    f
  cStructSize = 104
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_4_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 84 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 88 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 96 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceVulkan14Features where
  peekCStruct p = do
    globalPriorityQuery <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    shaderSubgroupRotate <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    shaderSubgroupRotateClustered <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    shaderFloatControls2 <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    shaderExpectAssume <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    rectangularLines <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    bresenhamLines <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    smoothLines <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    stippledRectangularLines <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    stippledBresenhamLines <- peek @Bool32 ((p `plusPtr` 52 :: Ptr Bool32))
    stippledSmoothLines <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    vertexAttributeInstanceRateDivisor <- peek @Bool32 ((p `plusPtr` 60 :: Ptr Bool32))
    vertexAttributeInstanceRateZeroDivisor <- peek @Bool32 ((p `plusPtr` 64 :: Ptr Bool32))
    indexTypeUint8 <- peek @Bool32 ((p `plusPtr` 68 :: Ptr Bool32))
    dynamicRenderingLocalRead <- peek @Bool32 ((p `plusPtr` 72 :: Ptr Bool32))
    maintenance5 <- peek @Bool32 ((p `plusPtr` 76 :: Ptr Bool32))
    maintenance6 <- peek @Bool32 ((p `plusPtr` 80 :: Ptr Bool32))
    pipelineProtectedAccess <- peek @Bool32 ((p `plusPtr` 84 :: Ptr Bool32))
    pipelineRobustness <- peek @Bool32 ((p `plusPtr` 88 :: Ptr Bool32))
    hostImageCopy <- peek @Bool32 ((p `plusPtr` 92 :: Ptr Bool32))
    pushDescriptor <- peek @Bool32 ((p `plusPtr` 96 :: Ptr Bool32))
    pure $ PhysicalDeviceVulkan14Features
             (bool32ToBool globalPriorityQuery)
             (bool32ToBool shaderSubgroupRotate)
             (bool32ToBool shaderSubgroupRotateClustered)
             (bool32ToBool shaderFloatControls2)
             (bool32ToBool shaderExpectAssume)
             (bool32ToBool rectangularLines)
             (bool32ToBool bresenhamLines)
             (bool32ToBool smoothLines)
             (bool32ToBool stippledRectangularLines)
             (bool32ToBool stippledBresenhamLines)
             (bool32ToBool stippledSmoothLines)
             (bool32ToBool vertexAttributeInstanceRateDivisor)
             (bool32ToBool vertexAttributeInstanceRateZeroDivisor)
             (bool32ToBool indexTypeUint8)
             (bool32ToBool dynamicRenderingLocalRead)
             (bool32ToBool maintenance5)
             (bool32ToBool maintenance6)
             (bool32ToBool pipelineProtectedAccess)
             (bool32ToBool pipelineRobustness)
             (bool32ToBool hostImageCopy)
             (bool32ToBool pushDescriptor)

instance Storable PhysicalDeviceVulkan14Features where
  sizeOf ~_ = 104
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVulkan14Features where
  zero = PhysicalDeviceVulkan14Features
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceVulkan14Properties - Structure specifying physical
-- device properties for functionality promoted to Vulkan 1.4
--
-- = Description
--
-- If the 'PhysicalDeviceVulkan14Properties' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- These properties correspond to Vulkan 1.4 functionality.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PipelineRobustnessBufferBehavior',
-- 'Vulkan.Core14.Enums.PipelineRobustnessImageBehavior.PipelineRobustnessImageBehavior',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVulkan14Properties = PhysicalDeviceVulkan14Properties
  { -- | #limits-lineSubPixelPrecisionBits# @lineSubPixelPrecisionBits@ is the
    -- number of bits of subpixel precision in framebuffer coordinates xf and
    -- yf when rasterizing
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines line segments>.
    lineSubPixelPrecisionBits :: Word32
  , -- | #limits-maxVertexAttribDivisor# @maxVertexAttribDivisor@ is the maximum
    -- value of the number of instances that will repeat the value of vertex
    -- attribute data when instanced rendering is enabled.
    maxVertexAttribDivisor :: Word32
  , -- | #limits-supportsNonZeroFirstInstance# @supportsNonZeroFirstInstance@
    -- specifies whether a non-zero value for the @firstInstance@ parameter of
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing drawing commands>
    -- is supported when
    -- 'Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap.VertexInputBindingDivisorDescription'::@divisor@
    -- is not @1@.
    supportsNonZeroFirstInstance :: Bool
  , -- | #limits-maxPushDescriptors# @maxPushDescriptors@ is the maximum number
    -- of descriptors that /can/ be used in a descriptor set layout created
    -- with
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT'
    -- set.
    maxPushDescriptors :: Word32
  , -- | #limits-dynamicRenderingLocalReadDepthStencilAttachments#
    -- @dynamicRenderingLocalReadDepthStencilAttachments@ is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the implementation supports
    -- local reads of depth\/stencil attachments,
    -- 'Vulkan.Core10.FundamentalTypes.FALSE' otherwise.
    dynamicRenderingLocalReadDepthStencilAttachments :: Bool
  , -- | #limits-dynamicRenderingLocalReadMultisampledAttachments#
    -- @dynamicRenderingLocalReadMultisampledAttachments@ is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the implementation supports
    -- local reads of multisampled attachments,
    -- 'Vulkan.Core10.FundamentalTypes.FALSE' otherwise.
    dynamicRenderingLocalReadMultisampledAttachments :: Bool
  , -- | @earlyFragmentMultisampleCoverageAfterSampleCounting@ is a boolean value
    -- indicating whether the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-shader fragment shading>
    -- and
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-covg multisample coverage>
    -- operations are performed after
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-samplecount sample counting>
    -- for
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-shader fragment shaders>
    -- with @EarlyFragmentTests@ execution mode.
    earlyFragmentMultisampleCoverageAfterSampleCounting :: Bool
  , -- | @earlyFragmentSampleMaskTestBeforeSampleCounting@ is a boolean value
    -- indicating whether the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-samplemask sample mask test>
    -- operation is performed before
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-samplecount sample counting>
    -- for
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-shader fragment shaders>
    -- using the @EarlyFragmentTests@ execution mode.
    earlyFragmentSampleMaskTestBeforeSampleCounting :: Bool
  , -- | @depthStencilSwizzleOneSupport@ is a boolean indicating that
    -- depth\/stencil texturing operations with
    -- 'Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_ONE' have
    -- defined behavior.
    depthStencilSwizzleOneSupport :: Bool
  , -- | @polygonModePointSize@ is a boolean value indicating whether the point
    -- size of the final rasterization of polygons with
    -- 'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_POINT' is controlled by
    -- @PointSize@.
    polygonModePointSize :: Bool
  , -- | @nonStrictSinglePixelWideLinesUseParallelogram@ is a boolean value
    -- indicating whether non-strict lines with a width of 1.0 are rasterized
    -- as parallelograms or using Bresenham’s algorithm.
    nonStrictSinglePixelWideLinesUseParallelogram :: Bool
  , -- | @nonStrictWideLinesUseParallelogram@ is a boolean value indicating
    -- whether non-strict lines with a width greater than 1.0 are rasterized as
    -- parallelograms or using Bresenham’s algorithm.
    nonStrictWideLinesUseParallelogram :: Bool
  , -- | @blockTexelViewCompatibleMultipleLayers@ is a boolean value indicating
    -- that an implementation supports creating image views with
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
    -- where the @layerCount@ member of @subresourceRange@ is greater than @1@.
    blockTexelViewCompatibleMultipleLayers :: Bool
  , -- | @maxCombinedImageSamplerDescriptorCount@ is the maximum number of
    -- combined image sampler descriptors that the implementation uses to
    -- access any of the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion formats that require a sampler Y′CBCR conversion>
    -- supported by the implementation.
    maxCombinedImageSamplerDescriptorCount :: Word32
  , -- | @fragmentShadingRateClampCombinerInputs@ is a boolean value indicating
    -- that an implementation clamps the inputs to
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-combining combiner operations>.
    fragmentShadingRateClampCombinerInputs :: Bool
  , -- | @defaultRobustnessStorageBuffers@ describes the behavior of out of
    -- bounds accesses made to storage buffers when no robustness features are
    -- enabled
    defaultRobustnessStorageBuffers :: PipelineRobustnessBufferBehavior
  , -- | @defaultRobustnessUniformBuffers@ describes the behavior of out of
    -- bounds accesses made to uniform buffers when no robustness features are
    -- enabled
    defaultRobustnessUniformBuffers :: PipelineRobustnessBufferBehavior
  , -- | @defaultRobustnessVertexInputs@ describes the behavior of out of bounds
    -- accesses made to vertex input attributes when no robustness features are
    -- enabled
    defaultRobustnessVertexInputs :: PipelineRobustnessBufferBehavior
  , -- | @defaultRobustnessImages@ describes the behavior of out of bounds
    -- accesses made to images when no robustness features are enabled
    defaultRobustnessImages :: PipelineRobustnessImageBehavior
  , -- | @copySrcLayoutCount@ is an integer related to the number of image
    -- layouts for host copies from images available or queried, as described
    -- below.
    copySrcLayoutCount :: Word32
  , -- | @pCopySrcLayouts@ is a pointer to an array of
    -- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' in which supported image
    -- layouts for use with host copy operations from images are returned.
    copySrcLayouts :: Ptr ImageLayout
  , -- | @copyDstLayoutCount@ is an integer related to the number of image
    -- layouts for host copies to images available or queried, as described
    -- below.
    copyDstLayoutCount :: Word32
  , -- | @pCopyDstLayouts@ is a pointer to an array of
    -- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' in which supported image
    -- layouts for use with host copy operations to images are returned.
    copyDstLayouts :: Ptr ImageLayout
  , -- | @optimalTilingLayoutUUID@ is an array of
    -- 'Vulkan.Core10.APIConstants.UUID_SIZE' @uint8_t@ values representing a
    -- universally unique identifier for the implementation’s swizzling layout
    -- of images created with
    -- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL'.
    optimalTilingLayoutUUID :: ByteString
  , -- | @identicalMemoryTypeRequirements@ indicates that specifying the
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT'
    -- flag in 'Vulkan.Core10.Image.ImageCreateInfo'::@usage@ does not affect
    -- the memory type requirements of the image.
    identicalMemoryTypeRequirements :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVulkan14Properties)
#endif
deriving instance Show PhysicalDeviceVulkan14Properties

instance ToCStruct PhysicalDeviceVulkan14Properties where
  withCStruct x f = allocaBytes 144 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVulkan14Properties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_4_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (lineSubPixelPrecisionBits)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxVertexAttribDivisor)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (supportsNonZeroFirstInstance))
    poke ((p `plusPtr` 28 :: Ptr Word32)) (maxPushDescriptors)
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (dynamicRenderingLocalReadDepthStencilAttachments))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (dynamicRenderingLocalReadMultisampledAttachments))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (earlyFragmentMultisampleCoverageAfterSampleCounting))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (earlyFragmentSampleMaskTestBeforeSampleCounting))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (depthStencilSwizzleOneSupport))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (polygonModePointSize))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (nonStrictSinglePixelWideLinesUseParallelogram))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (nonStrictWideLinesUseParallelogram))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (blockTexelViewCompatibleMultipleLayers))
    poke ((p `plusPtr` 68 :: Ptr Word32)) (maxCombinedImageSamplerDescriptorCount)
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateClampCombinerInputs))
    poke ((p `plusPtr` 76 :: Ptr PipelineRobustnessBufferBehavior)) (defaultRobustnessStorageBuffers)
    poke ((p `plusPtr` 80 :: Ptr PipelineRobustnessBufferBehavior)) (defaultRobustnessUniformBuffers)
    poke ((p `plusPtr` 84 :: Ptr PipelineRobustnessBufferBehavior)) (defaultRobustnessVertexInputs)
    poke ((p `plusPtr` 88 :: Ptr PipelineRobustnessImageBehavior)) (defaultRobustnessImages)
    poke ((p `plusPtr` 92 :: Ptr Word32)) (copySrcLayoutCount)
    poke ((p `plusPtr` 96 :: Ptr (Ptr ImageLayout))) (copySrcLayouts)
    poke ((p `plusPtr` 104 :: Ptr Word32)) (copyDstLayoutCount)
    poke ((p `plusPtr` 112 :: Ptr (Ptr ImageLayout))) (copyDstLayouts)
    pokeFixedLengthByteString ((p `plusPtr` 120 :: Ptr (FixedArray UUID_SIZE Word8))) (optimalTilingLayoutUUID)
    poke ((p `plusPtr` 136 :: Ptr Bool32)) (boolToBool32 (identicalMemoryTypeRequirements))
    f
  cStructSize = 144
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_4_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 68 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 76 :: Ptr PipelineRobustnessBufferBehavior)) (zero)
    poke ((p `plusPtr` 80 :: Ptr PipelineRobustnessBufferBehavior)) (zero)
    poke ((p `plusPtr` 84 :: Ptr PipelineRobustnessBufferBehavior)) (zero)
    poke ((p `plusPtr` 88 :: Ptr PipelineRobustnessImageBehavior)) (zero)
    poke ((p `plusPtr` 136 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceVulkan14Properties where
  peekCStruct p = do
    lineSubPixelPrecisionBits <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxVertexAttribDivisor <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    supportsNonZeroFirstInstance <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    maxPushDescriptors <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    dynamicRenderingLocalReadDepthStencilAttachments <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    dynamicRenderingLocalReadMultisampledAttachments <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    earlyFragmentMultisampleCoverageAfterSampleCounting <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    earlyFragmentSampleMaskTestBeforeSampleCounting <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    depthStencilSwizzleOneSupport <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    polygonModePointSize <- peek @Bool32 ((p `plusPtr` 52 :: Ptr Bool32))
    nonStrictSinglePixelWideLinesUseParallelogram <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    nonStrictWideLinesUseParallelogram <- peek @Bool32 ((p `plusPtr` 60 :: Ptr Bool32))
    blockTexelViewCompatibleMultipleLayers <- peek @Bool32 ((p `plusPtr` 64 :: Ptr Bool32))
    maxCombinedImageSamplerDescriptorCount <- peek @Word32 ((p `plusPtr` 68 :: Ptr Word32))
    fragmentShadingRateClampCombinerInputs <- peek @Bool32 ((p `plusPtr` 72 :: Ptr Bool32))
    defaultRobustnessStorageBuffers <- peek @PipelineRobustnessBufferBehavior ((p `plusPtr` 76 :: Ptr PipelineRobustnessBufferBehavior))
    defaultRobustnessUniformBuffers <- peek @PipelineRobustnessBufferBehavior ((p `plusPtr` 80 :: Ptr PipelineRobustnessBufferBehavior))
    defaultRobustnessVertexInputs <- peek @PipelineRobustnessBufferBehavior ((p `plusPtr` 84 :: Ptr PipelineRobustnessBufferBehavior))
    defaultRobustnessImages <- peek @PipelineRobustnessImageBehavior ((p `plusPtr` 88 :: Ptr PipelineRobustnessImageBehavior))
    copySrcLayoutCount <- peek @Word32 ((p `plusPtr` 92 :: Ptr Word32))
    pCopySrcLayouts <- peek @(Ptr ImageLayout) ((p `plusPtr` 96 :: Ptr (Ptr ImageLayout)))
    copyDstLayoutCount <- peek @Word32 ((p `plusPtr` 104 :: Ptr Word32))
    pCopyDstLayouts <- peek @(Ptr ImageLayout) ((p `plusPtr` 112 :: Ptr (Ptr ImageLayout)))
    optimalTilingLayoutUUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 120 :: Ptr (FixedArray UUID_SIZE Word8)))
    identicalMemoryTypeRequirements <- peek @Bool32 ((p `plusPtr` 136 :: Ptr Bool32))
    pure $ PhysicalDeviceVulkan14Properties
             lineSubPixelPrecisionBits
             maxVertexAttribDivisor
             (bool32ToBool supportsNonZeroFirstInstance)
             maxPushDescriptors
             (bool32ToBool dynamicRenderingLocalReadDepthStencilAttachments)
             (bool32ToBool dynamicRenderingLocalReadMultisampledAttachments)
             (bool32ToBool earlyFragmentMultisampleCoverageAfterSampleCounting)
             (bool32ToBool earlyFragmentSampleMaskTestBeforeSampleCounting)
             (bool32ToBool depthStencilSwizzleOneSupport)
             (bool32ToBool polygonModePointSize)
             (bool32ToBool nonStrictSinglePixelWideLinesUseParallelogram)
             (bool32ToBool nonStrictWideLinesUseParallelogram)
             (bool32ToBool blockTexelViewCompatibleMultipleLayers)
             maxCombinedImageSamplerDescriptorCount
             (bool32ToBool fragmentShadingRateClampCombinerInputs)
             defaultRobustnessStorageBuffers
             defaultRobustnessUniformBuffers
             defaultRobustnessVertexInputs
             defaultRobustnessImages
             copySrcLayoutCount
             pCopySrcLayouts
             copyDstLayoutCount
             pCopyDstLayouts
             optimalTilingLayoutUUID
             (bool32ToBool identicalMemoryTypeRequirements)

instance Storable PhysicalDeviceVulkan14Properties where
  sizeOf ~_ = 144
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVulkan14Properties where
  zero = PhysicalDeviceVulkan14Properties
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           mempty
           zero

