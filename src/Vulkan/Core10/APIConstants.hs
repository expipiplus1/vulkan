{-# language CPP #-}
-- No documentation found for Chapter "APIConstants"
module Vulkan.Core10.APIConstants  ( pattern LOD_CLAMP_NONE
                                   , pattern COMPUTE_OCCUPANCY_PRIORITY_LOW_NV
                                   , pattern COMPUTE_OCCUPANCY_PRIORITY_NORMAL_NV
                                   , pattern COMPUTE_OCCUPANCY_PRIORITY_HIGH_NV
                                   , MAX_PHYSICAL_DEVICE_NAME_SIZE
                                   , pattern MAX_PHYSICAL_DEVICE_NAME_SIZE
                                   , UUID_SIZE
                                   , pattern UUID_SIZE
                                   , LUID_SIZE
                                   , pattern LUID_SIZE
                                   , MAX_EXTENSION_NAME_SIZE
                                   , pattern MAX_EXTENSION_NAME_SIZE
                                   , MAX_DESCRIPTION_SIZE
                                   , pattern MAX_DESCRIPTION_SIZE
                                   , MAX_MEMORY_TYPES
                                   , pattern MAX_MEMORY_TYPES
                                   , MAX_MEMORY_HEAPS
                                   , pattern MAX_MEMORY_HEAPS
                                   , REMAINING_MIP_LEVELS
                                   , pattern REMAINING_MIP_LEVELS
                                   , REMAINING_ARRAY_LAYERS
                                   , pattern REMAINING_ARRAY_LAYERS
                                   , REMAINING_3D_SLICES_EXT
                                   , pattern REMAINING_3D_SLICES_EXT
                                   , WHOLE_SIZE
                                   , pattern WHOLE_SIZE
                                   , ATTACHMENT_UNUSED
                                   , pattern ATTACHMENT_UNUSED
                                   , QUEUE_FAMILY_IGNORED
                                   , pattern QUEUE_FAMILY_IGNORED
                                   , QUEUE_FAMILY_EXTERNAL
                                   , pattern QUEUE_FAMILY_EXTERNAL
                                   , QUEUE_FAMILY_FOREIGN_EXT
                                   , pattern QUEUE_FAMILY_FOREIGN_EXT
                                   , SUBPASS_EXTERNAL
                                   , pattern SUBPASS_EXTERNAL
                                   , MAX_DEVICE_GROUP_SIZE
                                   , pattern MAX_DEVICE_GROUP_SIZE
                                   , MAX_DRIVER_NAME_SIZE
                                   , pattern MAX_DRIVER_NAME_SIZE
                                   , MAX_DRIVER_INFO_SIZE
                                   , pattern MAX_DRIVER_INFO_SIZE
                                   , SHADER_UNUSED_KHR
                                   , pattern SHADER_UNUSED_KHR
                                   , MAX_GLOBAL_PRIORITY_SIZE
                                   , pattern MAX_GLOBAL_PRIORITY_SIZE
                                   , MAX_SHADER_MODULE_IDENTIFIER_SIZE_EXT
                                   , pattern MAX_SHADER_MODULE_IDENTIFIER_SIZE_EXT
                                   , MAX_PIPELINE_BINARY_KEY_SIZE_KHR
                                   , pattern MAX_PIPELINE_BINARY_KEY_SIZE_KHR
                                   , MAX_VIDEO_AV1_REFERENCES_PER_FRAME_KHR
                                   , pattern MAX_VIDEO_AV1_REFERENCES_PER_FRAME_KHR
                                   , MAX_VIDEO_VP9_REFERENCES_PER_FRAME_KHR
                                   , pattern MAX_VIDEO_VP9_REFERENCES_PER_FRAME_KHR
                                   , SHADER_INDEX_UNUSED_AMDX
                                   , pattern SHADER_INDEX_UNUSED_AMDX
                                   , PARTITIONED_ACCELERATION_STRUCTURE_PARTITION_INDEX_GLOBAL_NV
                                   , pattern PARTITIONED_ACCELERATION_STRUCTURE_PARTITION_INDEX_GLOBAL_NV
                                   , COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_ALIGNMENT_AMDX
                                   , pattern COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_ALIGNMENT_AMDX
                                   , COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_STRIDE_AMDX
                                   , pattern COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_STRIDE_AMDX
                                   , MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM
                                   , pattern MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM
                                   , DATA_GRAPH_MODEL_TOOLCHAIN_VERSION_LENGTH_QCOM
                                   , pattern DATA_GRAPH_MODEL_TOOLCHAIN_VERSION_LENGTH_QCOM
                                   , pattern NULL_HANDLE
                                   , IsHandle
                                   , HasObjectType(..)
                                   , Bool32(..)
                                   ) where

import Vulkan.Zero (Zero(..))
import Data.Word (Word32)
import Data.Word (Word64)
import Vulkan.Core10.Enums.ObjectType (ObjectType)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
-- | VK_LOD_CLAMP_NONE - Maximum LOD unclamped access sentinel
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>
pattern LOD_CLAMP_NONE :: Float
pattern LOD_CLAMP_NONE = 1000.0


-- | VK_COMPUTE_OCCUPANCY_PRIORITY_LOW_NV - Low occupancy priority constant
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_compute_occupancy_priority VK_NV_compute_occupancy_priority>
pattern COMPUTE_OCCUPANCY_PRIORITY_LOW_NV :: Float
pattern COMPUTE_OCCUPANCY_PRIORITY_LOW_NV = 0.25


-- | VK_COMPUTE_OCCUPANCY_PRIORITY_NORMAL_NV - Normal occupancy priority
-- constant
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_compute_occupancy_priority VK_NV_compute_occupancy_priority>
pattern COMPUTE_OCCUPANCY_PRIORITY_NORMAL_NV :: Float
pattern COMPUTE_OCCUPANCY_PRIORITY_NORMAL_NV = 0.5


-- | VK_COMPUTE_OCCUPANCY_PRIORITY_HIGH_NV - High occupancy priority constant
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_compute_occupancy_priority VK_NV_compute_occupancy_priority>
pattern COMPUTE_OCCUPANCY_PRIORITY_HIGH_NV :: Float
pattern COMPUTE_OCCUPANCY_PRIORITY_HIGH_NV = 0.75


type MAX_PHYSICAL_DEVICE_NAME_SIZE = 256

-- | VK_MAX_PHYSICAL_DEVICE_NAME_SIZE - Length of a physical device name
-- string
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>
pattern MAX_PHYSICAL_DEVICE_NAME_SIZE :: forall a . Integral a => a
pattern MAX_PHYSICAL_DEVICE_NAME_SIZE = 256


type UUID_SIZE = 16

-- | VK_UUID_SIZE - Length of a universally unique device or driver build
-- identifier
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>
pattern UUID_SIZE :: forall a . Integral a => a
pattern UUID_SIZE = 16


type LUID_SIZE = 8

-- | VK_LUID_SIZE - Length of a locally unique device identifier
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_fence_capabilities VK_KHR_external_fence_capabilities>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory_capabilities VK_KHR_external_memory_capabilities>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_semaphore_capabilities VK_KHR_external_semaphore_capabilities>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>
pattern LUID_SIZE :: forall a . Integral a => a
pattern LUID_SIZE = 8


type MAX_EXTENSION_NAME_SIZE = 256

-- | VK_MAX_EXTENSION_NAME_SIZE - Maximum length of a layer of extension name
-- string
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>
pattern MAX_EXTENSION_NAME_SIZE :: forall a . Integral a => a
pattern MAX_EXTENSION_NAME_SIZE = 256


type MAX_DESCRIPTION_SIZE = 256

-- | VK_MAX_DESCRIPTION_SIZE - Length of a driver name string
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>
pattern MAX_DESCRIPTION_SIZE :: forall a . Integral a => a
pattern MAX_DESCRIPTION_SIZE = 256


type MAX_MEMORY_TYPES = 32

-- | VK_MAX_MEMORY_TYPES - Length of an array of memory types
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>
pattern MAX_MEMORY_TYPES :: forall a . Integral a => a
pattern MAX_MEMORY_TYPES = 32


type MAX_MEMORY_HEAPS = 16

-- | VK_MAX_MEMORY_HEAPS - Length of an array of memory heaps
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>
pattern MAX_MEMORY_HEAPS :: forall a . Integral a => a
pattern MAX_MEMORY_HEAPS = 16


type REMAINING_MIP_LEVELS = 0xffffffff

-- | VK_REMAINING_MIP_LEVELS - Sentinel for all remaining mipmap levels
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>
pattern REMAINING_MIP_LEVELS :: Word32
pattern REMAINING_MIP_LEVELS = 0xffffffff


type REMAINING_ARRAY_LAYERS = 0xffffffff

-- | VK_REMAINING_ARRAY_LAYERS - Sentinel for all remaining array layers
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>
pattern REMAINING_ARRAY_LAYERS :: Word32
pattern REMAINING_ARRAY_LAYERS = 0xffffffff


type REMAINING_3D_SLICES_EXT = 0xffffffff

-- | VK_REMAINING_3D_SLICES_EXT - Sentinel for all remaining 3D slices
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_sliced_view_of_3d VK_EXT_image_sliced_view_of_3d>
pattern REMAINING_3D_SLICES_EXT :: Word32
pattern REMAINING_3D_SLICES_EXT = 0xffffffff


type WHOLE_SIZE = 0xffffffffffffffff

-- | VK_WHOLE_SIZE - Sentinel value to use entire remaining array length
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>
pattern WHOLE_SIZE :: Word64
pattern WHOLE_SIZE = 0xffffffffffffffff


type ATTACHMENT_UNUSED = 0xffffffff

-- | VK_ATTACHMENT_UNUSED - Unused attachment sentinel
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>
pattern ATTACHMENT_UNUSED :: Word32
pattern ATTACHMENT_UNUSED = 0xffffffff


type QUEUE_FAMILY_IGNORED = 0xffffffff

-- | VK_QUEUE_FAMILY_IGNORED - Ignored queue family index sentinel
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>
pattern QUEUE_FAMILY_IGNORED :: Word32
pattern QUEUE_FAMILY_IGNORED = 0xffffffff


type QUEUE_FAMILY_EXTERNAL = 0xfffffffe

-- | VK_QUEUE_FAMILY_EXTERNAL - External queue family index sentinel
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory VK_KHR_external_memory>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>
pattern QUEUE_FAMILY_EXTERNAL :: Word32
pattern QUEUE_FAMILY_EXTERNAL = 0xfffffffe


type QUEUE_FAMILY_FOREIGN_EXT = 0xfffffffd

-- | VK_QUEUE_FAMILY_FOREIGN_EXT - Foreign queue family index sentinel
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_queue_family_foreign VK_EXT_queue_family_foreign>
pattern QUEUE_FAMILY_FOREIGN_EXT :: Word32
pattern QUEUE_FAMILY_FOREIGN_EXT = 0xfffffffd


type SUBPASS_EXTERNAL = 0xffffffff

-- | VK_SUBPASS_EXTERNAL - Subpass index sentinel expanding synchronization
-- scope outside a subpass
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>
pattern SUBPASS_EXTERNAL :: Word32
pattern SUBPASS_EXTERNAL = 0xffffffff


type MAX_DEVICE_GROUP_SIZE = 32

-- | VK_MAX_DEVICE_GROUP_SIZE - Length of a physical device handle array
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group_creation VK_KHR_device_group_creation>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>
pattern MAX_DEVICE_GROUP_SIZE :: forall a . Integral a => a
pattern MAX_DEVICE_GROUP_SIZE = 32


type MAX_DRIVER_NAME_SIZE = 256

-- | VK_MAX_DRIVER_NAME_SIZE - Maximum length of a physical device driver
-- name string
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_driver_properties VK_KHR_driver_properties>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>
pattern MAX_DRIVER_NAME_SIZE :: forall a . Integral a => a
pattern MAX_DRIVER_NAME_SIZE = 256


type MAX_DRIVER_INFO_SIZE = 256

-- | VK_MAX_DRIVER_INFO_SIZE - Length of a physical device driver information
-- string
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_driver_properties VK_KHR_driver_properties>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>
pattern MAX_DRIVER_INFO_SIZE :: forall a . Integral a => a
pattern MAX_DRIVER_INFO_SIZE = 256


type SHADER_UNUSED_KHR = 0xffffffff

-- | VK_SHADER_UNUSED_KHR - Sentinel for an unused shader index
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
pattern SHADER_UNUSED_KHR :: Word32
pattern SHADER_UNUSED_KHR = 0xffffffff


type MAX_GLOBAL_PRIORITY_SIZE = 16

-- | VK_MAX_GLOBAL_PRIORITY_SIZE - Length of an array of global queue
-- priorities
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_global_priority_query VK_EXT_global_priority_query>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_global_priority VK_KHR_global_priority>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>
pattern MAX_GLOBAL_PRIORITY_SIZE :: forall a . Integral a => a
pattern MAX_GLOBAL_PRIORITY_SIZE = 16


type MAX_SHADER_MODULE_IDENTIFIER_SIZE_EXT = 32

-- | VK_MAX_SHADER_MODULE_IDENTIFIER_SIZE_EXT - Maximum length of a shader
-- module identifier
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_module_identifier VK_EXT_shader_module_identifier>
pattern MAX_SHADER_MODULE_IDENTIFIER_SIZE_EXT :: forall a . Integral a => a
pattern MAX_SHADER_MODULE_IDENTIFIER_SIZE_EXT = 32


type MAX_PIPELINE_BINARY_KEY_SIZE_KHR = 32

-- | VK_MAX_PIPELINE_BINARY_KEY_SIZE_KHR - Maximum length of binary key
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_binary VK_KHR_pipeline_binary>
pattern MAX_PIPELINE_BINARY_KEY_SIZE_KHR :: forall a . Integral a => a
pattern MAX_PIPELINE_BINARY_KEY_SIZE_KHR = 32


type MAX_VIDEO_AV1_REFERENCES_PER_FRAME_KHR = 7

-- | VK_MAX_VIDEO_AV1_REFERENCES_PER_FRAME_KHR - Length of an array of
-- supported queue priorities
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_video_decode_av1 VK_KHR_video_decode_av1>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_video_encode_av1 VK_KHR_video_encode_av1>
pattern MAX_VIDEO_AV1_REFERENCES_PER_FRAME_KHR :: forall a . Integral a => a
pattern MAX_VIDEO_AV1_REFERENCES_PER_FRAME_KHR = 7


type MAX_VIDEO_VP9_REFERENCES_PER_FRAME_KHR = 3

-- | VK_MAX_VIDEO_VP9_REFERENCES_PER_FRAME_KHR - Length of an array of
-- supported queue priorities
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_video_decode_vp9 VK_KHR_video_decode_vp9>
pattern MAX_VIDEO_VP9_REFERENCES_PER_FRAME_KHR :: forall a . Integral a => a
pattern MAX_VIDEO_VP9_REFERENCES_PER_FRAME_KHR = 3


type SHADER_INDEX_UNUSED_AMDX = 0xffffffff

-- | VK_SHADER_INDEX_UNUSED_AMDX - Sentinel for an unused shader index
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMDX_shader_enqueue VK_AMDX_shader_enqueue>
pattern SHADER_INDEX_UNUSED_AMDX :: Word32
pattern SHADER_INDEX_UNUSED_AMDX = 0xffffffff


type PARTITIONED_ACCELERATION_STRUCTURE_PARTITION_INDEX_GLOBAL_NV = 0xffffffff

-- | VK_PARTITIONED_ACCELERATION_STRUCTURE_PARTITION_INDEX_GLOBAL_NV -
-- Sentinel for global acceleration structure partitions
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_partitioned_acceleration_structure VK_NV_partitioned_acceleration_structure>
pattern PARTITIONED_ACCELERATION_STRUCTURE_PARTITION_INDEX_GLOBAL_NV :: Word32
pattern PARTITIONED_ACCELERATION_STRUCTURE_PARTITION_INDEX_GLOBAL_NV = 0xffffffff


type COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_ALIGNMENT_AMDX = 128

-- | VK_COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_ALIGNMENT_AMDX - Alignment
-- requirement for DGF1 compressed data
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMDX_dense_geometry_format VK_AMDX_dense_geometry_format>
pattern COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_ALIGNMENT_AMDX :: forall a . Integral a => a
pattern COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_ALIGNMENT_AMDX = 128


type COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_STRIDE_AMDX = 128

-- | VK_COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_STRIDE_AMDX - Alignment
-- requirement for DGF1 compressed data
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMDX_dense_geometry_format VK_AMDX_dense_geometry_format>
pattern COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_STRIDE_AMDX :: forall a . Integral a => a
pattern COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_STRIDE_AMDX = 128


type MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM = 128

-- | VK_MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM - Length
-- of a data graph operation name string
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>
pattern MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM :: forall a . Integral a => a
pattern MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM = 128


type DATA_GRAPH_MODEL_TOOLCHAIN_VERSION_LENGTH_QCOM = 3

-- | VK_DATA_GRAPH_MODEL_TOOLCHAIN_VERSION_LENGTH_QCOM - Length of a data
-- graph toolchain version string
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_data_graph_model VK_QCOM_data_graph_model>
pattern DATA_GRAPH_MODEL_TOOLCHAIN_VERSION_LENGTH_QCOM :: forall a . Integral a => a
pattern DATA_GRAPH_MODEL_TOOLCHAIN_VERSION_LENGTH_QCOM = 3


-- | VK_NULL_HANDLE - Reserved non-valid object handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_DEFINE_NON_DISPATCHABLE_HANDLE VK_DEFINE_NON_DISPATCHABLE_HANDLE>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_USE_64_BIT_PTR_DEFINES VK_USE_64_BIT_PTR_DEFINES>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>
pattern NULL_HANDLE :: IsHandle a => a
pattern NULL_HANDLE <- ((== zero) -> True)
  where NULL_HANDLE = zero

-- | A class for things which can be created with 'NULL_HANDLE'.
class (Eq a, Zero a) => IsHandle a where


class HasObjectType a where
  objectTypeAndHandle :: a -> (ObjectType, Word64)

