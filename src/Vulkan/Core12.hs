{-# language CPP #-}
-- No documentation found for Chapter "Core12"
module Vulkan.Core12  ( pattern API_VERSION_1_2
                      , PhysicalDeviceVulkan11Features(..)
                      , PhysicalDeviceVulkan11Properties(..)
                      , PhysicalDeviceVulkan12Features(..)
                      , PhysicalDeviceVulkan12Properties(..)
                      , StructureType(..)
                      , module Vulkan.Core12.Enums
                      , module Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing
                      , module Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset
                      , module Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax
                      , module Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout
                      , module Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage
                      , module Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage
                      , module Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address
                      , module Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2
                      , module Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve
                      , module Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count
                      , module Vulkan.Core12.Promoted_From_VK_KHR_driver_properties
                      , module Vulkan.Core12.Promoted_From_VK_KHR_image_format_list
                      , module Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer
                      , module Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts
                      , module Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64
                      , module Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8
                      , module Vulkan.Core12.Promoted_From_VK_KHR_shader_float_controls
                      , module Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types
                      , module Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
                      , module Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout
                      , module Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model
                      ) where
import Vulkan.Core12.Enums
import Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing
import Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset
import Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax
import Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout
import Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage
import Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage
import Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address
import Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2
import Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve
import Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count
import Vulkan.Core12.Promoted_From_VK_KHR_driver_properties
import Vulkan.Core12.Promoted_From_VK_KHR_image_format_list
import Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer
import Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts
import Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64
import Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8
import Vulkan.Core12.Promoted_From_VK_KHR_shader_float_controls
import Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
import Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout
import Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model
import Vulkan.CStruct.Utils (FixedArray)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCString)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthByteString)
import Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core12.Promoted_From_VK_KHR_driver_properties (ConformanceVersion)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core12.Enums.DriverId (DriverId)
import Vulkan.Core10.APIConstants (LUID_SIZE)
import Vulkan.Core10.APIConstants (MAX_DRIVER_INFO_SIZE)
import Vulkan.Core10.APIConstants (MAX_DRIVER_NAME_SIZE)
import Vulkan.Core11.Enums.PointClippingBehavior (PointClippingBehavior)
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlags)
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlags)
import Vulkan.Core12.Enums.ShaderFloatControlsIndependence (ShaderFloatControlsIndependence)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
import Vulkan.Core10.APIConstants (UUID_SIZE)
import Vulkan.Version (pattern MAKE_API_VERSION)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
pattern API_VERSION_1_2 :: Word32
pattern API_VERSION_1_2 = MAKE_API_VERSION 1 2 0


-- | VkPhysicalDeviceVulkan11Features - Structure describing the Vulkan 1.1
-- features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceVulkan11Features' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceVulkan11Features' /can/ also be used in the
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively
-- enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVulkan11Features = PhysicalDeviceVulkan11Features
  { -- | #features-storageBuffer16BitAccess# @storageBuffer16BitAccess@ specifies
    -- whether objects in the @StorageBuffer@, @ShaderRecordBufferKHR@, or
    -- @PhysicalStorageBuffer@ storage class with the @Block@ decoration /can/
    -- have 16-bit integer and 16-bit floating-point members. If this feature
    -- is not enabled, 16-bit integer or 16-bit floating-point members /must/
    -- not be used in such objects. This also specifies whether shader modules
    -- /can/ declare the @StorageBuffer16BitAccess@ capability.
    storageBuffer16BitAccess :: Bool
  , -- | #features-uniformAndStorageBuffer16BitAccess#
    -- @uniformAndStorageBuffer16BitAccess@ specifies whether objects in the
    -- @Uniform@ storage class with the @Block@ decoration /can/ have 16-bit
    -- integer and 16-bit floating-point members. If this feature is not
    -- enabled, 16-bit integer or 16-bit floating-point members /must/ not be
    -- used in such objects. This also specifies whether shader modules /can/
    -- declare the @UniformAndStorageBuffer16BitAccess@ capability.
    uniformAndStorageBuffer16BitAccess :: Bool
  , -- | #features-storagePushConstant16# @storagePushConstant16@ specifies
    -- whether objects in the @PushConstant@ storage class /can/ have 16-bit
    -- integer and 16-bit floating-point members. If this feature is not
    -- enabled, 16-bit integer or floating-point members /must/ not be used in
    -- such objects. This also specifies whether shader modules /can/ declare
    -- the @StoragePushConstant16@ capability.
    storagePushConstant16 :: Bool
  , -- | #features-storageInputOutput16# @storageInputOutput16@ specifies whether
    -- objects in the @Input@ and @Output@ storage classes /can/ have 16-bit
    -- integer and 16-bit floating-point members. If this feature is not
    -- enabled, 16-bit integer or 16-bit floating-point members /must/ not be
    -- used in such objects. This also specifies whether shader modules /can/
    -- declare the @StorageInputOutput16@ capability.
    storageInputOutput16 :: Bool
  , -- | #features-multiview# @multiview@ specifies whether the implementation
    -- supports multiview rendering within a render pass. If this feature is
    -- not enabled, the view mask of each subpass /must/ always be zero.
    multiview :: Bool
  , -- | #features-multiview-gs# @multiviewGeometryShader@ specifies whether the
    -- implementation supports multiview rendering within a render pass, with
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#geometry geometry shaders>.
    -- If this feature is not enabled, then a pipeline compiled against a
    -- subpass with a non-zero view mask /must/ not include a geometry shader.
    multiviewGeometryShader :: Bool
  , -- | #features-multiview-tess# @multiviewTessellationShader@ specifies
    -- whether the implementation supports multiview rendering within a render
    -- pass, with
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#tessellation tessellation shaders>.
    -- If this feature is not enabled, then a pipeline compiled against a
    -- subpass with a non-zero view mask /must/ not include any tessellation
    -- shaders.
    multiviewTessellationShader :: Bool
  , -- | #features-variablePointersStorageBuffer# @variablePointersStorageBuffer@
    -- specifies whether the implementation supports the SPIR-V
    -- @VariablePointersStorageBuffer@ capability. When this feature is not
    -- enabled, shader modules /must/ not declare the
    -- @SPV_KHR_variable_pointers@ extension or the
    -- @VariablePointersStorageBuffer@ capability.
    variablePointersStorageBuffer :: Bool
  , -- | #features-variablePointers# @variablePointers@ specifies whether the
    -- implementation supports the SPIR-V @VariablePointers@ capability. When
    -- this feature is not enabled, shader modules /must/ not declare the
    -- @VariablePointers@ capability.
    variablePointers :: Bool
  , -- | #features-protectedMemory# @protectedMemory@ specifies whether
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-protected-memory protected memory>
    -- is supported.
    protectedMemory :: Bool
  , -- | #features-samplerYcbcrConversion# @samplerYcbcrConversion@ specifies
    -- whether the implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>.
    -- If @samplerYcbcrConversion@ is 'Vulkan.Core10.FundamentalTypes.FALSE',
    -- sampler Y′CBCR conversion is not supported, and samplers using sampler
    -- Y′CBCR conversion /must/ not be used.
    samplerYcbcrConversion :: Bool
  , -- | #features-shaderDrawParameters# @shaderDrawParameters@ specifies whether
    -- the implementation supports the SPIR-V @DrawParameters@ capability. When
    -- this feature is not enabled, shader modules /must/ not declare the
    -- @SPV_KHR_shader_draw_parameters@ extension or the @DrawParameters@
    -- capability.
    shaderDrawParameters :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVulkan11Features)
#endif
deriving instance Show PhysicalDeviceVulkan11Features

instance ToCStruct PhysicalDeviceVulkan11Features where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVulkan11Features{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (storageBuffer16BitAccess))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (uniformAndStorageBuffer16BitAccess))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (storagePushConstant16))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (storageInputOutput16))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (multiview))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (multiviewGeometryShader))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (multiviewTessellationShader))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (variablePointersStorageBuffer))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (variablePointers))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (protectedMemory))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (samplerYcbcrConversion))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (shaderDrawParameters))
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES)
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
    f

instance FromCStruct PhysicalDeviceVulkan11Features where
  peekCStruct p = do
    storageBuffer16BitAccess <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    uniformAndStorageBuffer16BitAccess <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    storagePushConstant16 <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    storageInputOutput16 <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    multiview <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    multiviewGeometryShader <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    multiviewTessellationShader <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    variablePointersStorageBuffer <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    variablePointers <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    protectedMemory <- peek @Bool32 ((p `plusPtr` 52 :: Ptr Bool32))
    samplerYcbcrConversion <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    shaderDrawParameters <- peek @Bool32 ((p `plusPtr` 60 :: Ptr Bool32))
    pure $ PhysicalDeviceVulkan11Features
             (bool32ToBool storageBuffer16BitAccess)
             (bool32ToBool uniformAndStorageBuffer16BitAccess)
             (bool32ToBool storagePushConstant16)
             (bool32ToBool storageInputOutput16)
             (bool32ToBool multiview)
             (bool32ToBool multiviewGeometryShader)
             (bool32ToBool multiviewTessellationShader)
             (bool32ToBool variablePointersStorageBuffer)
             (bool32ToBool variablePointers)
             (bool32ToBool protectedMemory)
             (bool32ToBool samplerYcbcrConversion)
             (bool32ToBool shaderDrawParameters)

instance Storable PhysicalDeviceVulkan11Features where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVulkan11Features where
  zero = PhysicalDeviceVulkan11Features
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


-- | VkPhysicalDeviceVulkan11Properties - Structure specifying physical
-- device properties for functionality promoted to Vulkan 1.1
--
-- = Description
--
-- If the 'PhysicalDeviceVulkan11Properties' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- These properties correspond to Vulkan 1.1 functionality.
--
-- The members of 'PhysicalDeviceVulkan11Properties' have the same values
-- as the corresponding members of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceIDProperties',
-- 'Vulkan.Core11.Originally_Based_On_VK_KHR_subgroup.PhysicalDeviceSubgroupProperties',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.PhysicalDevicePointClippingProperties',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties',
-- 'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.PhysicalDeviceProtectedMemoryProperties',
-- and
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance3.PhysicalDeviceMaintenance3Properties'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core11.Enums.PointClippingBehavior.PointClippingBehavior',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SubgroupFeatureFlags'
data PhysicalDeviceVulkan11Properties = PhysicalDeviceVulkan11Properties
  { -- | @deviceUUID@ is an array of 'Vulkan.Core10.APIConstants.UUID_SIZE'
    -- @uint8_t@ values representing a universally unique identifier for the
    -- device.
    deviceUUID :: ByteString
  , -- | @driverUUID@ is an array of 'Vulkan.Core10.APIConstants.UUID_SIZE'
    -- @uint8_t@ values representing a universally unique identifier for the
    -- driver build in use by the device.
    driverUUID :: ByteString
  , -- | @deviceLUID@ is an array of 'Vulkan.Core10.APIConstants.LUID_SIZE'
    -- @uint8_t@ values representing a locally unique identifier for the
    -- device.
    deviceLUID :: ByteString
  , -- | @deviceNodeMask@ is a @uint32_t@ bitfield identifying the node within a
    -- linked device adapter corresponding to the device.
    deviceNodeMask :: Word32
  , -- | @deviceLUIDValid@ is a boolean value that will be
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if @deviceLUID@ contains a valid
    -- LUID and @deviceNodeMask@ contains a valid node mask, and
    -- 'Vulkan.Core10.FundamentalTypes.FALSE' if they do not.
    deviceLUIDValid :: Bool
  , -- | #limits-subgroup-size# @subgroupSize@ is the default number of
    -- invocations in each subgroup. @subgroupSize@ is at least 1 if any of the
    -- physical device’s queues support
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT'. @subgroupSize@ is
    -- a power-of-two.
    subgroupSize :: Word32
  , -- | #limits-subgroupSupportedStages# @subgroupSupportedStages@ is a bitfield
    -- of 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits'
    -- describing the shader stages that
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-group-operations group operations>
    -- with
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-scope-subgroup subgroup scope>
    -- are supported in. @subgroupSupportedStages@ will have the
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT' bit
    -- set if any of the physical device’s queues support
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT'.
    subgroupSupportedStages :: ShaderStageFlags
  , -- | @subgroupSupportedOperations@ is a bitmask of
    -- 'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SubgroupFeatureFlagBits'
    -- specifying the sets of
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-group-operations group operations>
    -- with
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-scope-subgroup subgroup scope>
    -- supported on this device. @subgroupSupportedOperations@ will have the
    -- 'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SUBGROUP_FEATURE_BASIC_BIT'
    -- bit set if any of the physical device’s queues support
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT'.
    subgroupSupportedOperations :: SubgroupFeatureFlags
  , -- | #limits-subgroupQuadOperationsInAllStages#
    -- @subgroupQuadOperationsInAllStages@ is a boolean specifying whether
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-quad-operations quad group operations>
    -- are available in all stages, or are restricted to fragment and compute
    -- stages.
    subgroupQuadOperationsInAllStages :: Bool
  , -- | #limits-pointClipping# @pointClippingBehavior@ is a
    -- 'Vulkan.Core11.Enums.PointClippingBehavior.PointClippingBehavior' value
    -- specifying the point clipping behavior supported by the implementation.
    pointClippingBehavior :: PointClippingBehavior
  , -- | #limits-maxMultiviewViewCount# @maxMultiviewViewCount@ is one greater
    -- than the maximum view index that /can/ be used in a subpass.
    maxMultiviewViewCount :: Word32
  , -- | #limits-maxMultiviewInstanceIndex# @maxMultiviewInstanceIndex@ is the
    -- maximum valid value of instance index allowed to be generated by a
    -- drawing command recorded within a subpass of a multiview render pass
    -- instance.
    maxMultiviewInstanceIndex :: Word32
  , -- | #limits-protectedNoFault# @protectedNoFault@ specifies how an
    -- implementation behaves when an application attempts to write to
    -- unprotected memory in a protected queue operation, read from protected
    -- memory in an unprotected queue operation, or perform a query in a
    -- protected queue operation. If this limit is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE', such writes will be discarded or
    -- have undefined values written, reads and queries will return undefined
    -- values. If this limit is 'Vulkan.Core10.FundamentalTypes.FALSE',
    -- applications /must/ not perform these operations. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-protected-access-rules ???>
    -- for more information.
    protectedNoFault :: Bool
  , -- | #limits-maxPerSetDescriptors# @maxPerSetDescriptors@ is a maximum number
    -- of descriptors (summed over all descriptor types) in a single descriptor
    -- set that is guaranteed to satisfy any implementation-dependent
    -- constraints on the size of a descriptor set itself. Applications /can/
    -- query whether a descriptor set that goes beyond this limit is supported
    -- using
    -- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance3.getDescriptorSetLayoutSupport'.
    maxPerSetDescriptors :: Word32
  , -- | #limits-maxMemoryAllocationSize# @maxMemoryAllocationSize@ is the
    -- maximum size of a memory allocation that /can/ be created, even if there
    -- is more space available in the heap.
    maxMemoryAllocationSize :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVulkan11Properties)
#endif
deriving instance Show PhysicalDeviceVulkan11Properties

instance ToCStruct PhysicalDeviceVulkan11Properties where
  withCStruct x f = allocaBytes 112 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVulkan11Properties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8))) (deviceUUID)
    pokeFixedLengthByteString ((p `plusPtr` 32 :: Ptr (FixedArray UUID_SIZE Word8))) (driverUUID)
    pokeFixedLengthByteString ((p `plusPtr` 48 :: Ptr (FixedArray LUID_SIZE Word8))) (deviceLUID)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (deviceNodeMask)
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (deviceLUIDValid))
    poke ((p `plusPtr` 64 :: Ptr Word32)) (subgroupSize)
    poke ((p `plusPtr` 68 :: Ptr ShaderStageFlags)) (subgroupSupportedStages)
    poke ((p `plusPtr` 72 :: Ptr SubgroupFeatureFlags)) (subgroupSupportedOperations)
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (subgroupQuadOperationsInAllStages))
    poke ((p `plusPtr` 80 :: Ptr PointClippingBehavior)) (pointClippingBehavior)
    poke ((p `plusPtr` 84 :: Ptr Word32)) (maxMultiviewViewCount)
    poke ((p `plusPtr` 88 :: Ptr Word32)) (maxMultiviewInstanceIndex)
    poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (protectedNoFault))
    poke ((p `plusPtr` 96 :: Ptr Word32)) (maxPerSetDescriptors)
    poke ((p `plusPtr` 104 :: Ptr DeviceSize)) (maxMemoryAllocationSize)
    f
  cStructSize = 112
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8))) (mempty)
    pokeFixedLengthByteString ((p `plusPtr` 32 :: Ptr (FixedArray UUID_SIZE Word8))) (mempty)
    pokeFixedLengthByteString ((p `plusPtr` 48 :: Ptr (FixedArray LUID_SIZE Word8))) (mempty)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 64 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 68 :: Ptr ShaderStageFlags)) (zero)
    poke ((p `plusPtr` 72 :: Ptr SubgroupFeatureFlags)) (zero)
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 80 :: Ptr PointClippingBehavior)) (zero)
    poke ((p `plusPtr` 84 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 88 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 96 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 104 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct PhysicalDeviceVulkan11Properties where
  peekCStruct p = do
    deviceUUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8)))
    driverUUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 32 :: Ptr (FixedArray UUID_SIZE Word8)))
    deviceLUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 48 :: Ptr (FixedArray LUID_SIZE Word8)))
    deviceNodeMask <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    deviceLUIDValid <- peek @Bool32 ((p `plusPtr` 60 :: Ptr Bool32))
    subgroupSize <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    subgroupSupportedStages <- peek @ShaderStageFlags ((p `plusPtr` 68 :: Ptr ShaderStageFlags))
    subgroupSupportedOperations <- peek @SubgroupFeatureFlags ((p `plusPtr` 72 :: Ptr SubgroupFeatureFlags))
    subgroupQuadOperationsInAllStages <- peek @Bool32 ((p `plusPtr` 76 :: Ptr Bool32))
    pointClippingBehavior <- peek @PointClippingBehavior ((p `plusPtr` 80 :: Ptr PointClippingBehavior))
    maxMultiviewViewCount <- peek @Word32 ((p `plusPtr` 84 :: Ptr Word32))
    maxMultiviewInstanceIndex <- peek @Word32 ((p `plusPtr` 88 :: Ptr Word32))
    protectedNoFault <- peek @Bool32 ((p `plusPtr` 92 :: Ptr Bool32))
    maxPerSetDescriptors <- peek @Word32 ((p `plusPtr` 96 :: Ptr Word32))
    maxMemoryAllocationSize <- peek @DeviceSize ((p `plusPtr` 104 :: Ptr DeviceSize))
    pure $ PhysicalDeviceVulkan11Properties
             deviceUUID
             driverUUID
             deviceLUID
             deviceNodeMask
             (bool32ToBool deviceLUIDValid)
             subgroupSize
             subgroupSupportedStages
             subgroupSupportedOperations
             (bool32ToBool subgroupQuadOperationsInAllStages)
             pointClippingBehavior
             maxMultiviewViewCount
             maxMultiviewInstanceIndex
             (bool32ToBool protectedNoFault)
             maxPerSetDescriptors
             maxMemoryAllocationSize

instance Storable PhysicalDeviceVulkan11Properties where
  sizeOf ~_ = 112
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVulkan11Properties where
  zero = PhysicalDeviceVulkan11Properties
           mempty
           mempty
           mempty
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


-- | VkPhysicalDeviceVulkan12Features - Structure describing the Vulkan 1.2
-- features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- -   @sType@ is the type of this structure.
--
-- -   @pNext@ is @NULL@ or a pointer to a structure extending this
--     structure.
--
-- -   #features-samplerMirrorClampToEdge# @samplerMirrorClampToEdge@
--     indicates whether the implementation supports the
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE'
--     sampler address mode. If this feature is not enabled, the
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE'
--     sampler address mode /must/ not be used.
--
-- -   #features-drawIndirectCount# @drawIndirectCount@ indicates whether
--     the implementation supports the
--     'Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndirectCount'
--     and
--     'Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCount'
--     functions. If this feature is not enabled, these functions /must/
--     not be used.
--
-- -   #features-storageBuffer8BitAccess# @storageBuffer8BitAccess@
--     indicates whether objects in the @StorageBuffer@,
--     @ShaderRecordBufferKHR@, or @PhysicalStorageBuffer@ storage class
--     with the @Block@ decoration /can/ have 8-bit integer members. If
--     this feature is not enabled, 8-bit integer members /must/ not be
--     used in such objects. This also indicates whether shader modules
--     /can/ declare the @StorageBuffer8BitAccess@ capability.
--
-- -   #features-uniformAndStorageBuffer8BitAccess#
--     @uniformAndStorageBuffer8BitAccess@ indicates whether objects in the
--     @Uniform@ storage class with the @Block@ decoration /can/ have 8-bit
--     integer members. If this feature is not enabled, 8-bit integer
--     members /must/ not be used in such objects. This also indicates
--     whether shader modules /can/ declare the
--     @UniformAndStorageBuffer8BitAccess@ capability.
--
-- -   #features-storagePushConstant8# @storagePushConstant8@ indicates
--     whether objects in the @PushConstant@ storage class /can/ have 8-bit
--     integer members. If this feature is not enabled, 8-bit integer
--     members /must/ not be used in such objects. This also indicates
--     whether shader modules /can/ declare the @StoragePushConstant8@
--     capability.
--
-- -   #features-shaderBufferInt64Atomics# @shaderBufferInt64Atomics@
--     indicates whether shaders /can/ perform 64-bit unsigned and signed
--     integer atomic operations on buffers.
--
-- -   #features-shaderSharedInt64Atomics# @shaderSharedInt64Atomics@
--     indicates whether shaders /can/ perform 64-bit unsigned and signed
--     integer atomic operations on shared and payload memory.
--
-- -   #features-shaderFloat16# @shaderFloat16@ indicates whether 16-bit
--     floats (halfs) are supported in shader code. This also indicates
--     whether shader modules /can/ declare the @Float16@ capability.
--     However, this only enables a subset of the storage classes that
--     SPIR-V allows for the @Float16@ SPIR-V capability: Declaring and
--     using 16-bit floats in the @Private@, @Workgroup@ (for non-Block
--     variables), and @Function@ storage classes is enabled, while
--     declaring them in the interface storage classes (e.g.,
--     @UniformConstant@, @Uniform@, @StorageBuffer@, @Input@, @Output@,
--     and @PushConstant@) is not enabled.
--
-- -   #features-shaderInt8# @shaderInt8@ indicates whether 8-bit integers
--     (signed and unsigned) are supported in shader code. This also
--     indicates whether shader modules /can/ declare the @Int8@
--     capability. However, this only enables a subset of the storage
--     classes that SPIR-V allows for the @Int8@ SPIR-V capability:
--     Declaring and using 8-bit integers in the @Private@, @Workgroup@
--     (for non-Block variables), and @Function@ storage classes is
--     enabled, while declaring them in the interface storage classes
--     (e.g., @UniformConstant@, @Uniform@, @StorageBuffer@, @Input@,
--     @Output@, and @PushConstant@) is not enabled.
--
-- -   #features-descriptorIndexing# @descriptorIndexing@ indicates whether
--     the implementation supports the minimum set of descriptor indexing
--     features as described in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-requirements Feature Requirements>
--     section. Enabling the @descriptorIndexing@ member when
--     'Vulkan.Core10.Device.createDevice' is called does not imply the
--     other minimum descriptor indexing features are also enabled. Those
--     other descriptor indexing features /must/ be enabled individually as
--     needed by the application.
--
-- -   #features-shaderInputAttachmentArrayDynamicIndexing#
--     @shaderInputAttachmentArrayDynamicIndexing@ indicates whether arrays
--     of input attachments /can/ be indexed by dynamically uniform integer
--     expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     /must/ be indexed only by constant integral expressions when
--     aggregated into arrays in shader code. This also indicates whether
--     shader modules /can/ declare the
--     @InputAttachmentArrayDynamicIndexing@ capability.
--
-- -   #features-shaderUniformTexelBufferArrayDynamicIndexing#
--     @shaderUniformTexelBufferArrayDynamicIndexing@ indicates whether
--     arrays of uniform texel buffers /can/ be indexed by dynamically
--     uniform integer expressions in shader code. If this feature is not
--     enabled, resources with a descriptor type of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     /must/ be indexed only by constant integral expressions when
--     aggregated into arrays in shader code. This also indicates whether
--     shader modules /can/ declare the
--     @UniformTexelBufferArrayDynamicIndexing@ capability.
--
-- -   #features-shaderStorageTexelBufferArrayDynamicIndexing#
--     @shaderStorageTexelBufferArrayDynamicIndexing@ indicates whether
--     arrays of storage texel buffers /can/ be indexed by dynamically
--     uniform integer expressions in shader code. If this feature is not
--     enabled, resources with a descriptor type of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     /must/ be indexed only by constant integral expressions when
--     aggregated into arrays in shader code. This also indicates whether
--     shader modules /can/ declare the
--     @StorageTexelBufferArrayDynamicIndexing@ capability.
--
-- -   #features-shaderUniformBufferArrayNonUniformIndexing#
--     @shaderUniformBufferArrayNonUniformIndexing@ indicates whether
--     arrays of uniform buffers /can/ be indexed by non-uniform integer
--     expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     /must/ not be indexed by non-uniform integer expressions when
--     aggregated into arrays in shader code. This also indicates whether
--     shader modules /can/ declare the
--     @UniformBufferArrayNonUniformIndexing@ capability.
--
-- -   #features-shaderSampledImageArrayNonUniformIndexing#
--     @shaderSampledImageArrayNonUniformIndexing@ indicates whether arrays
--     of samplers or sampled images /can/ be indexed by non-uniform
--     integer expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE'
--     /must/ not be indexed by non-uniform integer expressions when
--     aggregated into arrays in shader code. This also indicates whether
--     shader modules /can/ declare the
--     @SampledImageArrayNonUniformIndexing@ capability.
--
-- -   #features-shaderStorageBufferArrayNonUniformIndexing#
--     @shaderStorageBufferArrayNonUniformIndexing@ indicates whether
--     arrays of storage buffers /can/ be indexed by non-uniform integer
--     expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--     /must/ not be indexed by non-uniform integer expressions when
--     aggregated into arrays in shader code. This also indicates whether
--     shader modules /can/ declare the
--     @StorageBufferArrayNonUniformIndexing@ capability.
--
-- -   #features-shaderStorageImageArrayNonUniformIndexing#
--     @shaderStorageImageArrayNonUniformIndexing@ indicates whether arrays
--     of storage images /can/ be indexed by non-uniform integer
--     expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'
--     /must/ not be indexed by non-uniform integer expressions when
--     aggregated into arrays in shader code. This also indicates whether
--     shader modules /can/ declare the
--     @StorageImageArrayNonUniformIndexing@ capability.
--
-- -   #features-shaderInputAttachmentArrayNonUniformIndexing#
--     @shaderInputAttachmentArrayNonUniformIndexing@ indicates whether
--     arrays of input attachments /can/ be indexed by non-uniform integer
--     expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     /must/ not be indexed by non-uniform integer expressions when
--     aggregated into arrays in shader code. This also indicates whether
--     shader modules /can/ declare the
--     @InputAttachmentArrayNonUniformIndexing@ capability.
--
-- -   #features-shaderUniformTexelBufferArrayNonUniformIndexing#
--     @shaderUniformTexelBufferArrayNonUniformIndexing@ indicates whether
--     arrays of uniform texel buffers /can/ be indexed by non-uniform
--     integer expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     /must/ not be indexed by non-uniform integer expressions when
--     aggregated into arrays in shader code. This also indicates whether
--     shader modules /can/ declare the
--     @UniformTexelBufferArrayNonUniformIndexing@ capability.
--
-- -   #features-shaderStorageTexelBufferArrayNonUniformIndexing#
--     @shaderStorageTexelBufferArrayNonUniformIndexing@ indicates whether
--     arrays of storage texel buffers /can/ be indexed by non-uniform
--     integer expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     /must/ not be indexed by non-uniform integer expressions when
--     aggregated into arrays in shader code. This also indicates whether
--     shader modules /can/ declare the
--     @StorageTexelBufferArrayNonUniformIndexing@ capability.
--
-- -   #features-descriptorBindingUniformBufferUpdateAfterBind#
--     @descriptorBindingUniformBufferUpdateAfterBind@ indicates whether
--     the implementation supports updating uniform buffer descriptors
--     after a set is bound. If this feature is not enabled,
--     'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--     /must/ not be used with
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'.
--
-- -   #features-descriptorBindingSampledImageUpdateAfterBind#
--     @descriptorBindingSampledImageUpdateAfterBind@ indicates whether the
--     implementation supports updating sampled image descriptors after a
--     set is bound. If this feature is not enabled,
--     'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--     /must/ not be used with
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE'.
--
-- -   #features-descriptorBindingStorageImageUpdateAfterBind#
--     @descriptorBindingStorageImageUpdateAfterBind@ indicates whether the
--     implementation supports updating storage image descriptors after a
--     set is bound. If this feature is not enabled,
--     'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--     /must/ not be used with
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'.
--
-- -   #features-descriptorBindingStorageBufferUpdateAfterBind#
--     @descriptorBindingStorageBufferUpdateAfterBind@ indicates whether
--     the implementation supports updating storage buffer descriptors
--     after a set is bound. If this feature is not enabled,
--     'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--     /must/ not be used with
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'.
--
-- -   #features-descriptorBindingUniformTexelBufferUpdateAfterBind#
--     @descriptorBindingUniformTexelBufferUpdateAfterBind@ indicates
--     whether the implementation supports updating uniform texel buffer
--     descriptors after a set is bound. If this feature is not enabled,
--     'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--     /must/ not be used with
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'.
--
-- -   #features-descriptorBindingStorageTexelBufferUpdateAfterBind#
--     @descriptorBindingStorageTexelBufferUpdateAfterBind@ indicates
--     whether the implementation supports updating storage texel buffer
--     descriptors after a set is bound. If this feature is not enabled,
--     'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--     /must/ not be used with
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'.
--
-- -   #features-descriptorBindingUpdateUnusedWhilePending#
--     @descriptorBindingUpdateUnusedWhilePending@ indicates whether the
--     implementation supports updating descriptors while the set is in
--     use. If this feature is not enabled,
--     'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT'
--     /must/ not be used.
--
-- -   #features-descriptorBindingPartiallyBound#
--     @descriptorBindingPartiallyBound@ indicates whether the
--     implementation supports statically using a descriptor set binding in
--     which some descriptors are not valid. If this feature is not
--     enabled,
--     'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT'
--     /must/ not be used.
--
-- -   #features-descriptorBindingVariableDescriptorCount#
--     @descriptorBindingVariableDescriptorCount@ indicates whether the
--     implementation supports descriptor sets with a variable-sized last
--     binding. If this feature is not enabled,
--     'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT'
--     /must/ not be used.
--
-- -   #features-runtimeDescriptorArray# @runtimeDescriptorArray@ indicates
--     whether the implementation supports the SPIR-V
--     @RuntimeDescriptorArray@ capability. If this feature is not enabled,
--     descriptors /must/ not be declared in runtime arrays.
--
-- -   #features-samplerFilterMinmax# @samplerFilterMinmax@ indicates
--     whether the implementation supports a minimum set of required
--     formats supporting min\/max filtering as defined by the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-filterMinmaxSingleComponentFormats-minimum-requirements filterMinmaxSingleComponentFormats>
--     property minimum requirements. If this feature is not enabled, then
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'
--     /must/ only use
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE'.
--
-- -   #features-scalarBlockLayout# @scalarBlockLayout@ indicates that the
--     implementation supports the layout of resource blocks in shaders
--     using
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-alignment-requirements scalar alignment>.
--
-- -   #features-imagelessFramebuffer# @imagelessFramebuffer@ indicates
--     that the implementation supports specifying the image view for
--     attachments at render pass begin time via
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'.
--
-- -   #features-uniformBufferStandardLayout# @uniformBufferStandardLayout@
--     indicates that the implementation supports the same layouts for
--     uniform buffers as for storage and other kinds of buffers. See
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-resources-standard-layout Standard Buffer Layout>.
--
-- -   #features-subgroup-extended-types# @shaderSubgroupExtendedTypes@ is
--     a boolean specifying whether subgroup operations can use 8-bit
--     integer, 16-bit integer, 64-bit integer, 16-bit floating-point, and
--     vectors of these types in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-group-operations group operations>
--     with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-scope-subgroup subgroup scope>,
--     if the implementation supports the types.
--
-- -   #features-separateDepthStencilLayouts# @separateDepthStencilLayouts@
--     indicates whether the implementation supports a
--     'Vulkan.Core10.OtherTypes.ImageMemoryBarrier' for a depth\/stencil
--     image with only one of
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--     set, and whether
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--     can be used.
--
-- -   #features-hostQueryReset# @hostQueryReset@ indicates that the
--     implementation supports resetting queries from the host with
--     'Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.resetQueryPool'.
--
-- -   #features-timelineSemaphore# @timelineSemaphore@ indicates whether
--     semaphores created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE' are
--     supported.
--
-- -   #features-bufferDeviceAddress# @bufferDeviceAddress@ indicates that
--     the implementation supports accessing buffer memory in shaders as
--     storage buffers via an address queried from
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'.
--
-- -   #features-bufferDeviceAddressCaptureReplay#
--     @bufferDeviceAddressCaptureReplay@ indicates that the implementation
--     supports saving and reusing buffer and device addresses, e.g. for
--     trace capture and replay.
--
-- -   #features-bufferDeviceAddressMultiDevice#
--     @bufferDeviceAddressMultiDevice@ indicates that the implementation
--     supports the @bufferDeviceAddress@ , @rayTracingPipeline@ and
--     @rayQuery@ features for logical devices created with multiple
--     physical devices. If this feature is not supported, buffer and
--     acceleration structure addresses /must/ not be queried on a logical
--     device created with more than one physical device.
--
-- -   #features-vulkanMemoryModel# @vulkanMemoryModel@ indicates whether
--     the Vulkan Memory Model is supported, as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-model Vulkan Memory Model>.
--     This also indicates whether shader modules /can/ declare the
--     @VulkanMemoryModel@ capability.
--
-- -   #features-vulkanMemoryModelDeviceScope#
--     @vulkanMemoryModelDeviceScope@ indicates whether the Vulkan Memory
--     Model can use 'Vulkan.Core10.Handles.Device' scope synchronization.
--     This also indicates whether shader modules /can/ declare the
--     @VulkanMemoryModelDeviceScope@ capability.
--
-- -   #features-vulkanMemoryModelAvailabilityVisibilityChains#
--     @vulkanMemoryModelAvailabilityVisibilityChains@ indicates whether
--     the Vulkan Memory Model can use
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-model-availability-visibility availability and visibility chains>
--     with more than one element.
--
-- -   #features-shaderOutputViewportIndex# @shaderOutputViewportIndex@
--     indicates whether the implementation supports the
--     @ShaderViewportIndex@ SPIR-V capability enabling variables decorated
--     with the @ViewportIndex@ built-in to be exported from mesh, vertex
--     or tessellation evaluation shaders. If this feature is not enabled,
--     the @ViewportIndex@ built-in decoration /must/ not be used on
--     outputs in mesh, vertex or tessellation evaluation shaders.
--
-- -   #features-shaderOutputLayer# @shaderOutputLayer@ indicates whether
--     the implementation supports the @ShaderLayer@ SPIR-V capability
--     enabling variables decorated with the @Layer@ built-in to be
--     exported from mesh, vertex or tessellation evaluation shaders. If
--     this feature is not enabled, the @Layer@ built-in decoration /must/
--     not be used on outputs in mesh, vertex or tessellation evaluation
--     shaders.
--
-- -   #features-subgroupBroadcastDynamicId# If
--     @subgroupBroadcastDynamicId@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', the “Id” operand of
--     @OpGroupNonUniformBroadcast@ /can/ be dynamically uniform within a
--     subgroup, and the “Index” operand of
--     @OpGroupNonUniformQuadBroadcast@ /can/ be dynamically uniform within
--     the derivative group. If it is
--     'Vulkan.Core10.FundamentalTypes.FALSE', these operands /must/ be
--     constants.
--
-- If the 'PhysicalDeviceVulkan12Features' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceVulkan12Features' /can/ also be used in the
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively
-- enable these features.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceVulkan12Features-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVulkan12Features = PhysicalDeviceVulkan12Features
  { -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "samplerMirrorClampToEdge"
    samplerMirrorClampToEdge :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "drawIndirectCount"
    drawIndirectCount :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "storageBuffer8BitAccess"
    storageBuffer8BitAccess :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "uniformAndStorageBuffer8BitAccess"
    uniformAndStorageBuffer8BitAccess :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "storagePushConstant8"
    storagePushConstant8 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "shaderBufferInt64Atomics"
    shaderBufferInt64Atomics :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "shaderSharedInt64Atomics"
    shaderSharedInt64Atomics :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "shaderFloat16"
    shaderFloat16 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "shaderInt8"
    shaderInt8 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "descriptorIndexing"
    descriptorIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "shaderInputAttachmentArrayDynamicIndexing"
    shaderInputAttachmentArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "shaderUniformTexelBufferArrayDynamicIndexing"
    shaderUniformTexelBufferArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "shaderStorageTexelBufferArrayDynamicIndexing"
    shaderStorageTexelBufferArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "shaderUniformBufferArrayNonUniformIndexing"
    shaderUniformBufferArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "shaderSampledImageArrayNonUniformIndexing"
    shaderSampledImageArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "shaderStorageBufferArrayNonUniformIndexing"
    shaderStorageBufferArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "shaderStorageImageArrayNonUniformIndexing"
    shaderStorageImageArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "shaderInputAttachmentArrayNonUniformIndexing"
    shaderInputAttachmentArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "shaderUniformTexelBufferArrayNonUniformIndexing"
    shaderUniformTexelBufferArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "shaderStorageTexelBufferArrayNonUniformIndexing"
    shaderStorageTexelBufferArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "descriptorBindingUniformBufferUpdateAfterBind"
    descriptorBindingUniformBufferUpdateAfterBind :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "descriptorBindingSampledImageUpdateAfterBind"
    descriptorBindingSampledImageUpdateAfterBind :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "descriptorBindingStorageImageUpdateAfterBind"
    descriptorBindingStorageImageUpdateAfterBind :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "descriptorBindingStorageBufferUpdateAfterBind"
    descriptorBindingStorageBufferUpdateAfterBind :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "descriptorBindingUniformTexelBufferUpdateAfterBind"
    descriptorBindingUniformTexelBufferUpdateAfterBind :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "descriptorBindingStorageTexelBufferUpdateAfterBind"
    descriptorBindingStorageTexelBufferUpdateAfterBind :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "descriptorBindingUpdateUnusedWhilePending"
    descriptorBindingUpdateUnusedWhilePending :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "descriptorBindingPartiallyBound"
    descriptorBindingPartiallyBound :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "descriptorBindingVariableDescriptorCount"
    descriptorBindingVariableDescriptorCount :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "runtimeDescriptorArray"
    runtimeDescriptorArray :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "samplerFilterMinmax"
    samplerFilterMinmax :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "scalarBlockLayout"
    scalarBlockLayout :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "imagelessFramebuffer"
    imagelessFramebuffer :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "uniformBufferStandardLayout"
    uniformBufferStandardLayout :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "shaderSubgroupExtendedTypes"
    shaderSubgroupExtendedTypes :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "separateDepthStencilLayouts"
    separateDepthStencilLayouts :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "hostQueryReset"
    hostQueryReset :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "timelineSemaphore"
    timelineSemaphore :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "bufferDeviceAddress"
    bufferDeviceAddress :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "bufferDeviceAddressCaptureReplay"
    bufferDeviceAddressCaptureReplay :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "bufferDeviceAddressMultiDevice"
    bufferDeviceAddressMultiDevice :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "vulkanMemoryModel"
    vulkanMemoryModel :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "vulkanMemoryModelDeviceScope"
    vulkanMemoryModelDeviceScope :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "vulkanMemoryModelAvailabilityVisibilityChains"
    vulkanMemoryModelAvailabilityVisibilityChains :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "shaderOutputViewportIndex"
    shaderOutputViewportIndex :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "shaderOutputLayer"
    shaderOutputLayer :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Features" "subgroupBroadcastDynamicId"
    subgroupBroadcastDynamicId :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVulkan12Features)
#endif
deriving instance Show PhysicalDeviceVulkan12Features

instance ToCStruct PhysicalDeviceVulkan12Features where
  withCStruct x f = allocaBytes 208 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVulkan12Features{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (samplerMirrorClampToEdge))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (drawIndirectCount))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (storageBuffer8BitAccess))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (uniformAndStorageBuffer8BitAccess))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (storagePushConstant8))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (shaderBufferInt64Atomics))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (shaderSharedInt64Atomics))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (shaderFloat16))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (shaderInt8))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (descriptorIndexing))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (shaderInputAttachmentArrayDynamicIndexing))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (shaderUniformTexelBufferArrayDynamicIndexing))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (shaderStorageTexelBufferArrayDynamicIndexing))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (shaderUniformBufferArrayNonUniformIndexing))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (shaderSampledImageArrayNonUniformIndexing))
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (shaderStorageBufferArrayNonUniformIndexing))
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (shaderStorageImageArrayNonUniformIndexing))
    poke ((p `plusPtr` 84 :: Ptr Bool32)) (boolToBool32 (shaderInputAttachmentArrayNonUniformIndexing))
    poke ((p `plusPtr` 88 :: Ptr Bool32)) (boolToBool32 (shaderUniformTexelBufferArrayNonUniformIndexing))
    poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (shaderStorageTexelBufferArrayNonUniformIndexing))
    poke ((p `plusPtr` 96 :: Ptr Bool32)) (boolToBool32 (descriptorBindingUniformBufferUpdateAfterBind))
    poke ((p `plusPtr` 100 :: Ptr Bool32)) (boolToBool32 (descriptorBindingSampledImageUpdateAfterBind))
    poke ((p `plusPtr` 104 :: Ptr Bool32)) (boolToBool32 (descriptorBindingStorageImageUpdateAfterBind))
    poke ((p `plusPtr` 108 :: Ptr Bool32)) (boolToBool32 (descriptorBindingStorageBufferUpdateAfterBind))
    poke ((p `plusPtr` 112 :: Ptr Bool32)) (boolToBool32 (descriptorBindingUniformTexelBufferUpdateAfterBind))
    poke ((p `plusPtr` 116 :: Ptr Bool32)) (boolToBool32 (descriptorBindingStorageTexelBufferUpdateAfterBind))
    poke ((p `plusPtr` 120 :: Ptr Bool32)) (boolToBool32 (descriptorBindingUpdateUnusedWhilePending))
    poke ((p `plusPtr` 124 :: Ptr Bool32)) (boolToBool32 (descriptorBindingPartiallyBound))
    poke ((p `plusPtr` 128 :: Ptr Bool32)) (boolToBool32 (descriptorBindingVariableDescriptorCount))
    poke ((p `plusPtr` 132 :: Ptr Bool32)) (boolToBool32 (runtimeDescriptorArray))
    poke ((p `plusPtr` 136 :: Ptr Bool32)) (boolToBool32 (samplerFilterMinmax))
    poke ((p `plusPtr` 140 :: Ptr Bool32)) (boolToBool32 (scalarBlockLayout))
    poke ((p `plusPtr` 144 :: Ptr Bool32)) (boolToBool32 (imagelessFramebuffer))
    poke ((p `plusPtr` 148 :: Ptr Bool32)) (boolToBool32 (uniformBufferStandardLayout))
    poke ((p `plusPtr` 152 :: Ptr Bool32)) (boolToBool32 (shaderSubgroupExtendedTypes))
    poke ((p `plusPtr` 156 :: Ptr Bool32)) (boolToBool32 (separateDepthStencilLayouts))
    poke ((p `plusPtr` 160 :: Ptr Bool32)) (boolToBool32 (hostQueryReset))
    poke ((p `plusPtr` 164 :: Ptr Bool32)) (boolToBool32 (timelineSemaphore))
    poke ((p `plusPtr` 168 :: Ptr Bool32)) (boolToBool32 (bufferDeviceAddress))
    poke ((p `plusPtr` 172 :: Ptr Bool32)) (boolToBool32 (bufferDeviceAddressCaptureReplay))
    poke ((p `plusPtr` 176 :: Ptr Bool32)) (boolToBool32 (bufferDeviceAddressMultiDevice))
    poke ((p `plusPtr` 180 :: Ptr Bool32)) (boolToBool32 (vulkanMemoryModel))
    poke ((p `plusPtr` 184 :: Ptr Bool32)) (boolToBool32 (vulkanMemoryModelDeviceScope))
    poke ((p `plusPtr` 188 :: Ptr Bool32)) (boolToBool32 (vulkanMemoryModelAvailabilityVisibilityChains))
    poke ((p `plusPtr` 192 :: Ptr Bool32)) (boolToBool32 (shaderOutputViewportIndex))
    poke ((p `plusPtr` 196 :: Ptr Bool32)) (boolToBool32 (shaderOutputLayer))
    poke ((p `plusPtr` 200 :: Ptr Bool32)) (boolToBool32 (subgroupBroadcastDynamicId))
    f
  cStructSize = 208
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES)
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
    poke ((p `plusPtr` 100 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 104 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 108 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 112 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 116 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 120 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 124 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 128 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 132 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 136 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 140 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 144 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 148 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 152 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 156 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 160 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 164 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 168 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 172 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 176 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 180 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 184 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 188 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 192 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 196 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 200 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceVulkan12Features where
  peekCStruct p = do
    samplerMirrorClampToEdge <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    drawIndirectCount <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    storageBuffer8BitAccess <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    uniformAndStorageBuffer8BitAccess <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    storagePushConstant8 <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    shaderBufferInt64Atomics <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    shaderSharedInt64Atomics <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    shaderFloat16 <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    shaderInt8 <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    descriptorIndexing <- peek @Bool32 ((p `plusPtr` 52 :: Ptr Bool32))
    shaderInputAttachmentArrayDynamicIndexing <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    shaderUniformTexelBufferArrayDynamicIndexing <- peek @Bool32 ((p `plusPtr` 60 :: Ptr Bool32))
    shaderStorageTexelBufferArrayDynamicIndexing <- peek @Bool32 ((p `plusPtr` 64 :: Ptr Bool32))
    shaderUniformBufferArrayNonUniformIndexing <- peek @Bool32 ((p `plusPtr` 68 :: Ptr Bool32))
    shaderSampledImageArrayNonUniformIndexing <- peek @Bool32 ((p `plusPtr` 72 :: Ptr Bool32))
    shaderStorageBufferArrayNonUniformIndexing <- peek @Bool32 ((p `plusPtr` 76 :: Ptr Bool32))
    shaderStorageImageArrayNonUniformIndexing <- peek @Bool32 ((p `plusPtr` 80 :: Ptr Bool32))
    shaderInputAttachmentArrayNonUniformIndexing <- peek @Bool32 ((p `plusPtr` 84 :: Ptr Bool32))
    shaderUniformTexelBufferArrayNonUniformIndexing <- peek @Bool32 ((p `plusPtr` 88 :: Ptr Bool32))
    shaderStorageTexelBufferArrayNonUniformIndexing <- peek @Bool32 ((p `plusPtr` 92 :: Ptr Bool32))
    descriptorBindingUniformBufferUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 96 :: Ptr Bool32))
    descriptorBindingSampledImageUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 100 :: Ptr Bool32))
    descriptorBindingStorageImageUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 104 :: Ptr Bool32))
    descriptorBindingStorageBufferUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 108 :: Ptr Bool32))
    descriptorBindingUniformTexelBufferUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 112 :: Ptr Bool32))
    descriptorBindingStorageTexelBufferUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 116 :: Ptr Bool32))
    descriptorBindingUpdateUnusedWhilePending <- peek @Bool32 ((p `plusPtr` 120 :: Ptr Bool32))
    descriptorBindingPartiallyBound <- peek @Bool32 ((p `plusPtr` 124 :: Ptr Bool32))
    descriptorBindingVariableDescriptorCount <- peek @Bool32 ((p `plusPtr` 128 :: Ptr Bool32))
    runtimeDescriptorArray <- peek @Bool32 ((p `plusPtr` 132 :: Ptr Bool32))
    samplerFilterMinmax <- peek @Bool32 ((p `plusPtr` 136 :: Ptr Bool32))
    scalarBlockLayout <- peek @Bool32 ((p `plusPtr` 140 :: Ptr Bool32))
    imagelessFramebuffer <- peek @Bool32 ((p `plusPtr` 144 :: Ptr Bool32))
    uniformBufferStandardLayout <- peek @Bool32 ((p `plusPtr` 148 :: Ptr Bool32))
    shaderSubgroupExtendedTypes <- peek @Bool32 ((p `plusPtr` 152 :: Ptr Bool32))
    separateDepthStencilLayouts <- peek @Bool32 ((p `plusPtr` 156 :: Ptr Bool32))
    hostQueryReset <- peek @Bool32 ((p `plusPtr` 160 :: Ptr Bool32))
    timelineSemaphore <- peek @Bool32 ((p `plusPtr` 164 :: Ptr Bool32))
    bufferDeviceAddress <- peek @Bool32 ((p `plusPtr` 168 :: Ptr Bool32))
    bufferDeviceAddressCaptureReplay <- peek @Bool32 ((p `plusPtr` 172 :: Ptr Bool32))
    bufferDeviceAddressMultiDevice <- peek @Bool32 ((p `plusPtr` 176 :: Ptr Bool32))
    vulkanMemoryModel <- peek @Bool32 ((p `plusPtr` 180 :: Ptr Bool32))
    vulkanMemoryModelDeviceScope <- peek @Bool32 ((p `plusPtr` 184 :: Ptr Bool32))
    vulkanMemoryModelAvailabilityVisibilityChains <- peek @Bool32 ((p `plusPtr` 188 :: Ptr Bool32))
    shaderOutputViewportIndex <- peek @Bool32 ((p `plusPtr` 192 :: Ptr Bool32))
    shaderOutputLayer <- peek @Bool32 ((p `plusPtr` 196 :: Ptr Bool32))
    subgroupBroadcastDynamicId <- peek @Bool32 ((p `plusPtr` 200 :: Ptr Bool32))
    pure $ PhysicalDeviceVulkan12Features
             (bool32ToBool samplerMirrorClampToEdge)
             (bool32ToBool drawIndirectCount)
             (bool32ToBool storageBuffer8BitAccess)
             (bool32ToBool uniformAndStorageBuffer8BitAccess)
             (bool32ToBool storagePushConstant8)
             (bool32ToBool shaderBufferInt64Atomics)
             (bool32ToBool shaderSharedInt64Atomics)
             (bool32ToBool shaderFloat16)
             (bool32ToBool shaderInt8)
             (bool32ToBool descriptorIndexing)
             (bool32ToBool shaderInputAttachmentArrayDynamicIndexing)
             (bool32ToBool shaderUniformTexelBufferArrayDynamicIndexing)
             (bool32ToBool shaderStorageTexelBufferArrayDynamicIndexing)
             (bool32ToBool shaderUniformBufferArrayNonUniformIndexing)
             (bool32ToBool shaderSampledImageArrayNonUniformIndexing)
             (bool32ToBool shaderStorageBufferArrayNonUniformIndexing)
             (bool32ToBool shaderStorageImageArrayNonUniformIndexing)
             (bool32ToBool shaderInputAttachmentArrayNonUniformIndexing)
             (bool32ToBool shaderUniformTexelBufferArrayNonUniformIndexing)
             (bool32ToBool shaderStorageTexelBufferArrayNonUniformIndexing)
             (bool32ToBool descriptorBindingUniformBufferUpdateAfterBind)
             (bool32ToBool descriptorBindingSampledImageUpdateAfterBind)
             (bool32ToBool descriptorBindingStorageImageUpdateAfterBind)
             (bool32ToBool descriptorBindingStorageBufferUpdateAfterBind)
             (bool32ToBool descriptorBindingUniformTexelBufferUpdateAfterBind)
             (bool32ToBool descriptorBindingStorageTexelBufferUpdateAfterBind)
             (bool32ToBool descriptorBindingUpdateUnusedWhilePending)
             (bool32ToBool descriptorBindingPartiallyBound)
             (bool32ToBool descriptorBindingVariableDescriptorCount)
             (bool32ToBool runtimeDescriptorArray)
             (bool32ToBool samplerFilterMinmax)
             (bool32ToBool scalarBlockLayout)
             (bool32ToBool imagelessFramebuffer)
             (bool32ToBool uniformBufferStandardLayout)
             (bool32ToBool shaderSubgroupExtendedTypes)
             (bool32ToBool separateDepthStencilLayouts)
             (bool32ToBool hostQueryReset)
             (bool32ToBool timelineSemaphore)
             (bool32ToBool bufferDeviceAddress)
             (bool32ToBool bufferDeviceAddressCaptureReplay)
             (bool32ToBool bufferDeviceAddressMultiDevice)
             (bool32ToBool vulkanMemoryModel)
             (bool32ToBool vulkanMemoryModelDeviceScope)
             (bool32ToBool vulkanMemoryModelAvailabilityVisibilityChains)
             (bool32ToBool shaderOutputViewportIndex)
             (bool32ToBool shaderOutputLayer)
             (bool32ToBool subgroupBroadcastDynamicId)

instance Storable PhysicalDeviceVulkan12Features where
  sizeOf ~_ = 208
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVulkan12Features where
  zero = PhysicalDeviceVulkan12Features
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
           zero


-- | VkPhysicalDeviceVulkan12Properties - Structure specifying physical
-- device properties for functionality promoted to Vulkan 1.2
--
-- = Description
--
-- If the 'PhysicalDeviceVulkan12Properties' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- These properties correspond to Vulkan 1.2 functionality.
--
-- The members of 'PhysicalDeviceVulkan12Properties' /must/ have the same
-- values as the corresponding members of
-- 'Vulkan.Core12.Promoted_From_VK_KHR_driver_properties.PhysicalDeviceDriverProperties',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_shader_float_controls.PhysicalDeviceFloatControlsProperties',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingProperties',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.PhysicalDeviceDepthStencilResolveProperties',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.PhysicalDeviceSamplerFilterMinmaxProperties',
-- and
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.PhysicalDeviceTimelineSemaphoreProperties'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_driver_properties.ConformanceVersion',
-- 'Vulkan.Core12.Enums.DriverId.DriverId',
-- 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlags',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlags',
-- 'Vulkan.Core12.Enums.ShaderFloatControlsIndependence.ShaderFloatControlsIndependence',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVulkan12Properties = PhysicalDeviceVulkan12Properties
  { -- | @driverID@ is a unique identifier for the driver of the physical device.
    driverID :: DriverId
  , -- | @driverName@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_DRIVER_NAME_SIZE' @char@ containing a
    -- null-terminated UTF-8 string which is the name of the driver.
    driverName :: ByteString
  , -- | @driverInfo@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_DRIVER_INFO_SIZE' @char@ containing a
    -- null-terminated UTF-8 string with additional information about the
    -- driver.
    driverInfo :: ByteString
  , -- | @conformanceVersion@ is the version of the Vulkan conformance test this
    -- driver is conformant against (see
    -- 'Vulkan.Core12.Promoted_From_VK_KHR_driver_properties.ConformanceVersion').
    conformanceVersion :: ConformanceVersion
  , -- | #features-denormBehaviorIndependence# @denormBehaviorIndependence@ is a
    -- 'Vulkan.Core12.Enums.ShaderFloatControlsIndependence.ShaderFloatControlsIndependence'
    -- value indicating whether, and how, denorm behavior can be set
    -- independently for different bit widths.
    denormBehaviorIndependence :: ShaderFloatControlsIndependence
  , -- | #features-roundingModeIndependence# @roundingModeIndependence@ is a
    -- 'Vulkan.Core12.Enums.ShaderFloatControlsIndependence.ShaderFloatControlsIndependence'
    -- value indicating whether, and how, rounding modes can be set
    -- independently for different bit widths.
    roundingModeIndependence :: ShaderFloatControlsIndependence
  , -- | #limits-shaderSignedZeroInfNanPreserveFloat16#
    -- @shaderSignedZeroInfNanPreserveFloat16@ is a boolean value indicating
    -- whether sign of a zero, Nans and \(\pm\infty\) /can/ be preserved in
    -- 16-bit floating-point computations. It also indicates whether the
    -- @SignedZeroInfNanPreserve@ execution mode /can/ be used for 16-bit
    -- floating-point types.
    shaderSignedZeroInfNanPreserveFloat16 :: Bool
  , -- | #limits-shaderSignedZeroInfNanPreserveFloat32#
    -- @shaderSignedZeroInfNanPreserveFloat32@ is a boolean value indicating
    -- whether sign of a zero, Nans and \(\pm\infty\) /can/ be preserved in
    -- 32-bit floating-point computations. It also indicates whether the
    -- @SignedZeroInfNanPreserve@ execution mode /can/ be used for 32-bit
    -- floating-point types.
    shaderSignedZeroInfNanPreserveFloat32 :: Bool
  , -- | #limits-shaderSignedZeroInfNanPreserveFloat64#
    -- @shaderSignedZeroInfNanPreserveFloat64@ is a boolean value indicating
    -- whether sign of a zero, Nans and \(\pm\infty\) /can/ be preserved in
    -- 64-bit floating-point computations. It also indicates whether the
    -- @SignedZeroInfNanPreserve@ execution mode /can/ be used for 64-bit
    -- floating-point types.
    shaderSignedZeroInfNanPreserveFloat64 :: Bool
  , -- | #limits-shaderDenormPreserveFloat16# @shaderDenormPreserveFloat16@ is a
    -- boolean value indicating whether denormals /can/ be preserved in 16-bit
    -- floating-point computations. It also indicates whether the
    -- @DenormPreserve@ execution mode /can/ be used for 16-bit floating-point
    -- types.
    shaderDenormPreserveFloat16 :: Bool
  , -- | #limits-shaderDenormPreserveFloat32# @shaderDenormPreserveFloat32@ is a
    -- boolean value indicating whether denormals /can/ be preserved in 32-bit
    -- floating-point computations. It also indicates whether the
    -- @DenormPreserve@ execution mode /can/ be used for 32-bit floating-point
    -- types.
    shaderDenormPreserveFloat32 :: Bool
  , -- | #limits-shaderDenormPreserveFloat64# @shaderDenormPreserveFloat64@ is a
    -- boolean value indicating whether denormals /can/ be preserved in 64-bit
    -- floating-point computations. It also indicates whether the
    -- @DenormPreserve@ execution mode /can/ be used for 64-bit floating-point
    -- types.
    shaderDenormPreserveFloat64 :: Bool
  , -- | #limits-shaderDenormFlushToZeroFloat16# @shaderDenormFlushToZeroFloat16@
    -- is a boolean value indicating whether denormals /can/ be flushed to zero
    -- in 16-bit floating-point computations. It also indicates whether the
    -- @DenormFlushToZero@ execution mode /can/ be used for 16-bit
    -- floating-point types.
    shaderDenormFlushToZeroFloat16 :: Bool
  , -- | #limits-shaderDenormFlushToZeroFloat32# @shaderDenormFlushToZeroFloat32@
    -- is a boolean value indicating whether denormals /can/ be flushed to zero
    -- in 32-bit floating-point computations. It also indicates whether the
    -- @DenormFlushToZero@ execution mode /can/ be used for 32-bit
    -- floating-point types.
    shaderDenormFlushToZeroFloat32 :: Bool
  , -- | #limits-shaderDenormFlushToZeroFloat64# @shaderDenormFlushToZeroFloat64@
    -- is a boolean value indicating whether denormals /can/ be flushed to zero
    -- in 64-bit floating-point computations. It also indicates whether the
    -- @DenormFlushToZero@ execution mode /can/ be used for 64-bit
    -- floating-point types.
    shaderDenormFlushToZeroFloat64 :: Bool
  , -- | #limits-shaderRoundingModeRTEFloat16# @shaderRoundingModeRTEFloat16@ is
    -- a boolean value indicating whether an implementation supports the
    -- round-to-nearest-even rounding mode for 16-bit floating-point arithmetic
    -- and conversion instructions. It also indicates whether the
    -- @RoundingModeRTE@ execution mode /can/ be used for 16-bit floating-point
    -- types.
    shaderRoundingModeRTEFloat16 :: Bool
  , -- | #limits-shaderRoundingModeRTEFloat32# @shaderRoundingModeRTEFloat32@ is
    -- a boolean value indicating whether an implementation supports the
    -- round-to-nearest-even rounding mode for 32-bit floating-point arithmetic
    -- and conversion instructions. It also indicates whether the
    -- @RoundingModeRTE@ execution mode /can/ be used for 32-bit floating-point
    -- types.
    shaderRoundingModeRTEFloat32 :: Bool
  , -- | #limits-shaderRoundingModeRTEFloat64# @shaderRoundingModeRTEFloat64@ is
    -- a boolean value indicating whether an implementation supports the
    -- round-to-nearest-even rounding mode for 64-bit floating-point arithmetic
    -- and conversion instructions. It also indicates whether the
    -- @RoundingModeRTE@ execution mode /can/ be used for 64-bit floating-point
    -- types.
    shaderRoundingModeRTEFloat64 :: Bool
  , -- | #limits-shaderRoundingModeRTZFloat16# @shaderRoundingModeRTZFloat16@ is
    -- a boolean value indicating whether an implementation supports the
    -- round-towards-zero rounding mode for 16-bit floating-point arithmetic
    -- and conversion instructions. It also indicates whether the
    -- @RoundingModeRTZ@ execution mode /can/ be used for 16-bit floating-point
    -- types.
    shaderRoundingModeRTZFloat16 :: Bool
  , -- | #limits-shaderRoundingModeRTZFloat32# @shaderRoundingModeRTZFloat32@ is
    -- a boolean value indicating whether an implementation supports the
    -- round-towards-zero rounding mode for 32-bit floating-point arithmetic
    -- and conversion instructions. It also indicates whether the
    -- @RoundingModeRTZ@ execution mode /can/ be used for 32-bit floating-point
    -- types.
    shaderRoundingModeRTZFloat32 :: Bool
  , -- | #limits-shaderRoundingModeRTZFloat64# @shaderRoundingModeRTZFloat64@ is
    -- a boolean value indicating whether an implementation supports the
    -- round-towards-zero rounding mode for 64-bit floating-point arithmetic
    -- and conversion instructions. It also indicates whether the
    -- @RoundingModeRTZ@ execution mode /can/ be used for 64-bit floating-point
    -- types.
    shaderRoundingModeRTZFloat64 :: Bool
  , -- | #limits-maxUpdateAfterBindDescriptorsInAllPools#
    -- @maxUpdateAfterBindDescriptorsInAllPools@ is the maximum number of
    -- descriptors (summed over all descriptor types) that /can/ be created
    -- across all pools that are created with the
    -- 'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT'
    -- bit set. Pool creation /may/ fail when this limit is exceeded, or when
    -- the space this limit represents is unable to satisfy a pool creation due
    -- to fragmentation.
    maxUpdateAfterBindDescriptorsInAllPools :: Word32
  , -- | #limits-shaderUniformBufferArrayNonUniformIndexingNative#
    -- @shaderUniformBufferArrayNonUniformIndexingNative@ is a boolean value
    -- indicating whether uniform buffer descriptors natively support
    -- nonuniform indexing. If this is 'Vulkan.Core10.FundamentalTypes.FALSE',
    -- then a single dynamic instance of an instruction that nonuniformly
    -- indexes an array of uniform buffers /may/ execute multiple times in
    -- order to access all the descriptors.
    shaderUniformBufferArrayNonUniformIndexingNative :: Bool
  , -- | #limits-shaderSampledImageArrayNonUniformIndexingNative#
    -- @shaderSampledImageArrayNonUniformIndexingNative@ is a boolean value
    -- indicating whether sampler and image descriptors natively support
    -- nonuniform indexing. If this is 'Vulkan.Core10.FundamentalTypes.FALSE',
    -- then a single dynamic instance of an instruction that nonuniformly
    -- indexes an array of samplers or images /may/ execute multiple times in
    -- order to access all the descriptors.
    shaderSampledImageArrayNonUniformIndexingNative :: Bool
  , -- | #limits-shaderStorageBufferArrayNonUniformIndexingNative#
    -- @shaderStorageBufferArrayNonUniformIndexingNative@ is a boolean value
    -- indicating whether storage buffer descriptors natively support
    -- nonuniform indexing. If this is 'Vulkan.Core10.FundamentalTypes.FALSE',
    -- then a single dynamic instance of an instruction that nonuniformly
    -- indexes an array of storage buffers /may/ execute multiple times in
    -- order to access all the descriptors.
    shaderStorageBufferArrayNonUniformIndexingNative :: Bool
  , -- | #limits-shaderStorageImageArrayNonUniformIndexingNative#
    -- @shaderStorageImageArrayNonUniformIndexingNative@ is a boolean value
    -- indicating whether storage image descriptors natively support nonuniform
    -- indexing. If this is 'Vulkan.Core10.FundamentalTypes.FALSE', then a
    -- single dynamic instance of an instruction that nonuniformly indexes an
    -- array of storage images /may/ execute multiple times in order to access
    -- all the descriptors.
    shaderStorageImageArrayNonUniformIndexingNative :: Bool
  , -- | #limits-shaderInputAttachmentArrayNonUniformIndexingNative#
    -- @shaderInputAttachmentArrayNonUniformIndexingNative@ is a boolean value
    -- indicating whether input attachment descriptors natively support
    -- nonuniform indexing. If this is 'Vulkan.Core10.FundamentalTypes.FALSE',
    -- then a single dynamic instance of an instruction that nonuniformly
    -- indexes an array of input attachments /may/ execute multiple times in
    -- order to access all the descriptors.
    shaderInputAttachmentArrayNonUniformIndexingNative :: Bool
  , -- | #limits-robustBufferAccessUpdateAfterBind#
    -- @robustBufferAccessUpdateAfterBind@ is a boolean value indicating
    -- whether
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
    -- /can/ be enabled on a device simultaneously with
    -- @descriptorBindingUniformBufferUpdateAfterBind@,
    -- @descriptorBindingStorageBufferUpdateAfterBind@,
    -- @descriptorBindingUniformTexelBufferUpdateAfterBind@, and\/or
    -- @descriptorBindingStorageTexelBufferUpdateAfterBind@. If this is
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', then either @robustBufferAccess@
    -- /must/ be disabled or all of these update-after-bind features /must/ be
    -- disabled.
    robustBufferAccessUpdateAfterBind :: Bool
  , -- | #limits-quadDivergentImplicitLod# @quadDivergentImplicitLod@ is a
    -- boolean value indicating whether implicit level of detail calculations
    -- for image operations have well-defined results when the image and\/or
    -- sampler objects used for the instruction are not uniform within a quad.
    -- See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-derivative-image-operations Derivative Image Operations>.
    quadDivergentImplicitLod :: Bool
  , -- | #limits-maxPerStageDescriptorUpdateAfterBindSamplers#
    -- @maxPerStageDescriptorUpdateAfterBindSamplers@ is similar to
    -- @maxPerStageDescriptorSamplers@ but counts descriptors from descriptor
    -- sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindSamplers :: Word32
  , -- | #limits-maxPerStageDescriptorUpdateAfterBindUniformBuffers#
    -- @maxPerStageDescriptorUpdateAfterBindUniformBuffers@ is similar to
    -- @maxPerStageDescriptorUniformBuffers@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindUniformBuffers :: Word32
  , -- | #limits-maxPerStageDescriptorUpdateAfterBindStorageBuffers#
    -- @maxPerStageDescriptorUpdateAfterBindStorageBuffers@ is similar to
    -- @maxPerStageDescriptorStorageBuffers@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindStorageBuffers :: Word32
  , -- | #limits-maxPerStageDescriptorUpdateAfterBindSampledImages#
    -- @maxPerStageDescriptorUpdateAfterBindSampledImages@ is similar to
    -- @maxPerStageDescriptorSampledImages@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindSampledImages :: Word32
  , -- | #limits-maxPerStageDescriptorUpdateAfterBindStorageImages#
    -- @maxPerStageDescriptorUpdateAfterBindStorageImages@ is similar to
    -- @maxPerStageDescriptorStorageImages@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindStorageImages :: Word32
  , -- | #limits-maxPerStageDescriptorUpdateAfterBindInputAttachments#
    -- @maxPerStageDescriptorUpdateAfterBindInputAttachments@ is similar to
    -- @maxPerStageDescriptorInputAttachments@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindInputAttachments :: Word32
  , -- | #limits-maxPerStageUpdateAfterBindResources#
    -- @maxPerStageUpdateAfterBindResources@ is similar to
    -- @maxPerStageResources@ but counts descriptors from descriptor sets
    -- created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageUpdateAfterBindResources :: Word32
  , -- | #limits-maxDescriptorSetUpdateAfterBindSamplers#
    -- @maxDescriptorSetUpdateAfterBindSamplers@ is similar to
    -- @maxDescriptorSetSamplers@ but counts descriptors from descriptor sets
    -- created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindSamplers :: Word32
  , -- | #limits-maxDescriptorSetUpdateAfterBindUniformBuffers#
    -- @maxDescriptorSetUpdateAfterBindUniformBuffers@ is similar to
    -- @maxDescriptorSetUniformBuffers@ but counts descriptors from descriptor
    -- sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindUniformBuffers :: Word32
  , -- | #limits-maxDescriptorSetUpdateAfterBindUniformBuffersDynamic#
    -- @maxDescriptorSetUpdateAfterBindUniformBuffersDynamic@ is similar to
    -- @maxDescriptorSetUniformBuffersDynamic@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set. While an application /can/ allocate dynamic uniform buffer
    -- descriptors from a pool created with the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT',
    -- bindings for these descriptors /must/ not be present in any descriptor
    -- set layout that includes bindings created with
    -- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'.
    maxDescriptorSetUpdateAfterBindUniformBuffersDynamic :: Word32
  , -- | #limits-maxDescriptorSetUpdateAfterBindStorageBuffers#
    -- @maxDescriptorSetUpdateAfterBindStorageBuffers@ is similar to
    -- @maxDescriptorSetStorageBuffers@ but counts descriptors from descriptor
    -- sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindStorageBuffers :: Word32
  , -- | #limits-maxDescriptorSetUpdateAfterBindStorageBuffersDynamic#
    -- @maxDescriptorSetUpdateAfterBindStorageBuffersDynamic@ is similar to
    -- @maxDescriptorSetStorageBuffersDynamic@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set. While an application /can/ allocate dynamic storage buffer
    -- descriptors from a pool created with the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT',
    -- bindings for these descriptors /must/ not be present in any descriptor
    -- set layout that includes bindings created with
    -- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'.
    maxDescriptorSetUpdateAfterBindStorageBuffersDynamic :: Word32
  , -- | #limits-maxDescriptorSetUpdateAfterBindSampledImages#
    -- @maxDescriptorSetUpdateAfterBindSampledImages@ is similar to
    -- @maxDescriptorSetSampledImages@ but counts descriptors from descriptor
    -- sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindSampledImages :: Word32
  , -- | #limits-maxDescriptorSetUpdateAfterBindStorageImages#
    -- @maxDescriptorSetUpdateAfterBindStorageImages@ is similar to
    -- @maxDescriptorSetStorageImages@ but counts descriptors from descriptor
    -- sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindStorageImages :: Word32
  , -- | #limits-maxDescriptorSetUpdateAfterBindInputAttachments#
    -- @maxDescriptorSetUpdateAfterBindInputAttachments@ is similar to
    -- @maxDescriptorSetInputAttachments@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindInputAttachments :: Word32
  , -- | #features-depthResolveModes# @supportedDepthResolveModes@ is a bitmask
    -- of 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits'
    -- indicating the set of supported depth resolve modes.
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_SAMPLE_ZERO_BIT'
    -- /must/ be included in the set but implementations /may/ support
    -- additional modes.
    supportedDepthResolveModes :: ResolveModeFlags
  , -- | #features-stencilResolveModes# @supportedStencilResolveModes@ is a
    -- bitmask of 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits'
    -- indicating the set of supported stencil resolve modes.
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_SAMPLE_ZERO_BIT'
    -- /must/ be included in the set but implementations /may/ support
    -- additional modes.
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_AVERAGE_BIT'
    -- /must/ not be included in the set.
    supportedStencilResolveModes :: ResolveModeFlags
  , -- | #features-independentResolveNone# @independentResolveNone@ is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the implementation supports
    -- setting the depth and stencil resolve modes to different values when one
    -- of those modes is
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE'. Otherwise
    -- the implementation only supports setting both modes to the same value.
    independentResolveNone :: Bool
  , -- | #features-independentResolve# @independentResolve@ is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the implementation supports all
    -- combinations of the supported depth and stencil resolve modes, including
    -- setting either depth or stencil resolve mode to
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE'. An
    -- implementation that supports @independentResolve@ /must/ also support
    -- @independentResolveNone@.
    independentResolve :: Bool
  , -- | #limits-filterMinmaxSingleComponentFormats#
    -- @filterMinmaxSingleComponentFormats@ is a boolean value indicating
    -- whether a minimum set of required formats support min\/max filtering.
    filterMinmaxSingleComponentFormats :: Bool
  , -- | #limits-filterMinmaxImageComponentMapping#
    -- @filterMinmaxImageComponentMapping@ is a boolean value indicating
    -- whether the implementation supports non-identity component mapping of
    -- the image when doing min\/max filtering.
    filterMinmaxImageComponentMapping :: Bool
  , -- | #limits-maxTimelineSemaphoreValueDifference#
    -- @maxTimelineSemaphoreValueDifference@ indicates the maximum difference
    -- allowed by the implementation between the current value of a timeline
    -- semaphore and any pending signal or wait operations.
    maxTimelineSemaphoreValueDifference :: Word64
  , -- | #limits-framebufferIntegerColorSampleCounts#
    -- @framebufferIntegerColorSampleCounts@ is a bitmask of
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' indicating
    -- the color sample counts that are supported for all framebuffer color
    -- attachments with integer formats.
    framebufferIntegerColorSampleCounts :: SampleCountFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVulkan12Properties)
#endif
deriving instance Show PhysicalDeviceVulkan12Properties

instance ToCStruct PhysicalDeviceVulkan12Properties where
  withCStruct x f = allocaBytes 736 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVulkan12Properties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DriverId)) (driverID)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 20 :: Ptr (FixedArray MAX_DRIVER_NAME_SIZE CChar))) (driverName)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 276 :: Ptr (FixedArray MAX_DRIVER_INFO_SIZE CChar))) (driverInfo)
    poke ((p `plusPtr` 532 :: Ptr ConformanceVersion)) (conformanceVersion)
    poke ((p `plusPtr` 536 :: Ptr ShaderFloatControlsIndependence)) (denormBehaviorIndependence)
    poke ((p `plusPtr` 540 :: Ptr ShaderFloatControlsIndependence)) (roundingModeIndependence)
    poke ((p `plusPtr` 544 :: Ptr Bool32)) (boolToBool32 (shaderSignedZeroInfNanPreserveFloat16))
    poke ((p `plusPtr` 548 :: Ptr Bool32)) (boolToBool32 (shaderSignedZeroInfNanPreserveFloat32))
    poke ((p `plusPtr` 552 :: Ptr Bool32)) (boolToBool32 (shaderSignedZeroInfNanPreserveFloat64))
    poke ((p `plusPtr` 556 :: Ptr Bool32)) (boolToBool32 (shaderDenormPreserveFloat16))
    poke ((p `plusPtr` 560 :: Ptr Bool32)) (boolToBool32 (shaderDenormPreserveFloat32))
    poke ((p `plusPtr` 564 :: Ptr Bool32)) (boolToBool32 (shaderDenormPreserveFloat64))
    poke ((p `plusPtr` 568 :: Ptr Bool32)) (boolToBool32 (shaderDenormFlushToZeroFloat16))
    poke ((p `plusPtr` 572 :: Ptr Bool32)) (boolToBool32 (shaderDenormFlushToZeroFloat32))
    poke ((p `plusPtr` 576 :: Ptr Bool32)) (boolToBool32 (shaderDenormFlushToZeroFloat64))
    poke ((p `plusPtr` 580 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTEFloat16))
    poke ((p `plusPtr` 584 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTEFloat32))
    poke ((p `plusPtr` 588 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTEFloat64))
    poke ((p `plusPtr` 592 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTZFloat16))
    poke ((p `plusPtr` 596 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTZFloat32))
    poke ((p `plusPtr` 600 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTZFloat64))
    poke ((p `plusPtr` 604 :: Ptr Word32)) (maxUpdateAfterBindDescriptorsInAllPools)
    poke ((p `plusPtr` 608 :: Ptr Bool32)) (boolToBool32 (shaderUniformBufferArrayNonUniformIndexingNative))
    poke ((p `plusPtr` 612 :: Ptr Bool32)) (boolToBool32 (shaderSampledImageArrayNonUniformIndexingNative))
    poke ((p `plusPtr` 616 :: Ptr Bool32)) (boolToBool32 (shaderStorageBufferArrayNonUniformIndexingNative))
    poke ((p `plusPtr` 620 :: Ptr Bool32)) (boolToBool32 (shaderStorageImageArrayNonUniformIndexingNative))
    poke ((p `plusPtr` 624 :: Ptr Bool32)) (boolToBool32 (shaderInputAttachmentArrayNonUniformIndexingNative))
    poke ((p `plusPtr` 628 :: Ptr Bool32)) (boolToBool32 (robustBufferAccessUpdateAfterBind))
    poke ((p `plusPtr` 632 :: Ptr Bool32)) (boolToBool32 (quadDivergentImplicitLod))
    poke ((p `plusPtr` 636 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindSamplers)
    poke ((p `plusPtr` 640 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindUniformBuffers)
    poke ((p `plusPtr` 644 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindStorageBuffers)
    poke ((p `plusPtr` 648 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindSampledImages)
    poke ((p `plusPtr` 652 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindStorageImages)
    poke ((p `plusPtr` 656 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindInputAttachments)
    poke ((p `plusPtr` 660 :: Ptr Word32)) (maxPerStageUpdateAfterBindResources)
    poke ((p `plusPtr` 664 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindSamplers)
    poke ((p `plusPtr` 668 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindUniformBuffers)
    poke ((p `plusPtr` 672 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindUniformBuffersDynamic)
    poke ((p `plusPtr` 676 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindStorageBuffers)
    poke ((p `plusPtr` 680 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindStorageBuffersDynamic)
    poke ((p `plusPtr` 684 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindSampledImages)
    poke ((p `plusPtr` 688 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindStorageImages)
    poke ((p `plusPtr` 692 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindInputAttachments)
    poke ((p `plusPtr` 696 :: Ptr ResolveModeFlags)) (supportedDepthResolveModes)
    poke ((p `plusPtr` 700 :: Ptr ResolveModeFlags)) (supportedStencilResolveModes)
    poke ((p `plusPtr` 704 :: Ptr Bool32)) (boolToBool32 (independentResolveNone))
    poke ((p `plusPtr` 708 :: Ptr Bool32)) (boolToBool32 (independentResolve))
    poke ((p `plusPtr` 712 :: Ptr Bool32)) (boolToBool32 (filterMinmaxSingleComponentFormats))
    poke ((p `plusPtr` 716 :: Ptr Bool32)) (boolToBool32 (filterMinmaxImageComponentMapping))
    poke ((p `plusPtr` 720 :: Ptr Word64)) (maxTimelineSemaphoreValueDifference)
    poke ((p `plusPtr` 728 :: Ptr SampleCountFlags)) (framebufferIntegerColorSampleCounts)
    f
  cStructSize = 736
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DriverId)) (zero)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 20 :: Ptr (FixedArray MAX_DRIVER_NAME_SIZE CChar))) (mempty)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 276 :: Ptr (FixedArray MAX_DRIVER_INFO_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 532 :: Ptr ConformanceVersion)) (zero)
    poke ((p `plusPtr` 536 :: Ptr ShaderFloatControlsIndependence)) (zero)
    poke ((p `plusPtr` 540 :: Ptr ShaderFloatControlsIndependence)) (zero)
    poke ((p `plusPtr` 544 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 548 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 552 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 556 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 560 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 564 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 568 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 572 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 576 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 580 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 584 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 588 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 592 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 596 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 600 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 604 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 608 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 612 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 616 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 620 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 624 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 628 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 632 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 636 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 640 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 644 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 648 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 652 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 656 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 660 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 664 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 668 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 672 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 676 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 680 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 684 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 688 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 692 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 696 :: Ptr ResolveModeFlags)) (zero)
    poke ((p `plusPtr` 700 :: Ptr ResolveModeFlags)) (zero)
    poke ((p `plusPtr` 704 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 708 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 712 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 716 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 720 :: Ptr Word64)) (zero)
    f

instance FromCStruct PhysicalDeviceVulkan12Properties where
  peekCStruct p = do
    driverID <- peek @DriverId ((p `plusPtr` 16 :: Ptr DriverId))
    driverName <- packCString (lowerArrayPtr ((p `plusPtr` 20 :: Ptr (FixedArray MAX_DRIVER_NAME_SIZE CChar))))
    driverInfo <- packCString (lowerArrayPtr ((p `plusPtr` 276 :: Ptr (FixedArray MAX_DRIVER_INFO_SIZE CChar))))
    conformanceVersion <- peekCStruct @ConformanceVersion ((p `plusPtr` 532 :: Ptr ConformanceVersion))
    denormBehaviorIndependence <- peek @ShaderFloatControlsIndependence ((p `plusPtr` 536 :: Ptr ShaderFloatControlsIndependence))
    roundingModeIndependence <- peek @ShaderFloatControlsIndependence ((p `plusPtr` 540 :: Ptr ShaderFloatControlsIndependence))
    shaderSignedZeroInfNanPreserveFloat16 <- peek @Bool32 ((p `plusPtr` 544 :: Ptr Bool32))
    shaderSignedZeroInfNanPreserveFloat32 <- peek @Bool32 ((p `plusPtr` 548 :: Ptr Bool32))
    shaderSignedZeroInfNanPreserveFloat64 <- peek @Bool32 ((p `plusPtr` 552 :: Ptr Bool32))
    shaderDenormPreserveFloat16 <- peek @Bool32 ((p `plusPtr` 556 :: Ptr Bool32))
    shaderDenormPreserveFloat32 <- peek @Bool32 ((p `plusPtr` 560 :: Ptr Bool32))
    shaderDenormPreserveFloat64 <- peek @Bool32 ((p `plusPtr` 564 :: Ptr Bool32))
    shaderDenormFlushToZeroFloat16 <- peek @Bool32 ((p `plusPtr` 568 :: Ptr Bool32))
    shaderDenormFlushToZeroFloat32 <- peek @Bool32 ((p `plusPtr` 572 :: Ptr Bool32))
    shaderDenormFlushToZeroFloat64 <- peek @Bool32 ((p `plusPtr` 576 :: Ptr Bool32))
    shaderRoundingModeRTEFloat16 <- peek @Bool32 ((p `plusPtr` 580 :: Ptr Bool32))
    shaderRoundingModeRTEFloat32 <- peek @Bool32 ((p `plusPtr` 584 :: Ptr Bool32))
    shaderRoundingModeRTEFloat64 <- peek @Bool32 ((p `plusPtr` 588 :: Ptr Bool32))
    shaderRoundingModeRTZFloat16 <- peek @Bool32 ((p `plusPtr` 592 :: Ptr Bool32))
    shaderRoundingModeRTZFloat32 <- peek @Bool32 ((p `plusPtr` 596 :: Ptr Bool32))
    shaderRoundingModeRTZFloat64 <- peek @Bool32 ((p `plusPtr` 600 :: Ptr Bool32))
    maxUpdateAfterBindDescriptorsInAllPools <- peek @Word32 ((p `plusPtr` 604 :: Ptr Word32))
    shaderUniformBufferArrayNonUniformIndexingNative <- peek @Bool32 ((p `plusPtr` 608 :: Ptr Bool32))
    shaderSampledImageArrayNonUniformIndexingNative <- peek @Bool32 ((p `plusPtr` 612 :: Ptr Bool32))
    shaderStorageBufferArrayNonUniformIndexingNative <- peek @Bool32 ((p `plusPtr` 616 :: Ptr Bool32))
    shaderStorageImageArrayNonUniformIndexingNative <- peek @Bool32 ((p `plusPtr` 620 :: Ptr Bool32))
    shaderInputAttachmentArrayNonUniformIndexingNative <- peek @Bool32 ((p `plusPtr` 624 :: Ptr Bool32))
    robustBufferAccessUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 628 :: Ptr Bool32))
    quadDivergentImplicitLod <- peek @Bool32 ((p `plusPtr` 632 :: Ptr Bool32))
    maxPerStageDescriptorUpdateAfterBindSamplers <- peek @Word32 ((p `plusPtr` 636 :: Ptr Word32))
    maxPerStageDescriptorUpdateAfterBindUniformBuffers <- peek @Word32 ((p `plusPtr` 640 :: Ptr Word32))
    maxPerStageDescriptorUpdateAfterBindStorageBuffers <- peek @Word32 ((p `plusPtr` 644 :: Ptr Word32))
    maxPerStageDescriptorUpdateAfterBindSampledImages <- peek @Word32 ((p `plusPtr` 648 :: Ptr Word32))
    maxPerStageDescriptorUpdateAfterBindStorageImages <- peek @Word32 ((p `plusPtr` 652 :: Ptr Word32))
    maxPerStageDescriptorUpdateAfterBindInputAttachments <- peek @Word32 ((p `plusPtr` 656 :: Ptr Word32))
    maxPerStageUpdateAfterBindResources <- peek @Word32 ((p `plusPtr` 660 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindSamplers <- peek @Word32 ((p `plusPtr` 664 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindUniformBuffers <- peek @Word32 ((p `plusPtr` 668 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindUniformBuffersDynamic <- peek @Word32 ((p `plusPtr` 672 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindStorageBuffers <- peek @Word32 ((p `plusPtr` 676 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindStorageBuffersDynamic <- peek @Word32 ((p `plusPtr` 680 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindSampledImages <- peek @Word32 ((p `plusPtr` 684 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindStorageImages <- peek @Word32 ((p `plusPtr` 688 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindInputAttachments <- peek @Word32 ((p `plusPtr` 692 :: Ptr Word32))
    supportedDepthResolveModes <- peek @ResolveModeFlags ((p `plusPtr` 696 :: Ptr ResolveModeFlags))
    supportedStencilResolveModes <- peek @ResolveModeFlags ((p `plusPtr` 700 :: Ptr ResolveModeFlags))
    independentResolveNone <- peek @Bool32 ((p `plusPtr` 704 :: Ptr Bool32))
    independentResolve <- peek @Bool32 ((p `plusPtr` 708 :: Ptr Bool32))
    filterMinmaxSingleComponentFormats <- peek @Bool32 ((p `plusPtr` 712 :: Ptr Bool32))
    filterMinmaxImageComponentMapping <- peek @Bool32 ((p `plusPtr` 716 :: Ptr Bool32))
    maxTimelineSemaphoreValueDifference <- peek @Word64 ((p `plusPtr` 720 :: Ptr Word64))
    framebufferIntegerColorSampleCounts <- peek @SampleCountFlags ((p `plusPtr` 728 :: Ptr SampleCountFlags))
    pure $ PhysicalDeviceVulkan12Properties
             driverID
             driverName
             driverInfo
             conformanceVersion
             denormBehaviorIndependence
             roundingModeIndependence
             (bool32ToBool shaderSignedZeroInfNanPreserveFloat16)
             (bool32ToBool shaderSignedZeroInfNanPreserveFloat32)
             (bool32ToBool shaderSignedZeroInfNanPreserveFloat64)
             (bool32ToBool shaderDenormPreserveFloat16)
             (bool32ToBool shaderDenormPreserveFloat32)
             (bool32ToBool shaderDenormPreserveFloat64)
             (bool32ToBool shaderDenormFlushToZeroFloat16)
             (bool32ToBool shaderDenormFlushToZeroFloat32)
             (bool32ToBool shaderDenormFlushToZeroFloat64)
             (bool32ToBool shaderRoundingModeRTEFloat16)
             (bool32ToBool shaderRoundingModeRTEFloat32)
             (bool32ToBool shaderRoundingModeRTEFloat64)
             (bool32ToBool shaderRoundingModeRTZFloat16)
             (bool32ToBool shaderRoundingModeRTZFloat32)
             (bool32ToBool shaderRoundingModeRTZFloat64)
             maxUpdateAfterBindDescriptorsInAllPools
             (bool32ToBool shaderUniformBufferArrayNonUniformIndexingNative)
             (bool32ToBool shaderSampledImageArrayNonUniformIndexingNative)
             (bool32ToBool shaderStorageBufferArrayNonUniformIndexingNative)
             (bool32ToBool shaderStorageImageArrayNonUniformIndexingNative)
             (bool32ToBool shaderInputAttachmentArrayNonUniformIndexingNative)
             (bool32ToBool robustBufferAccessUpdateAfterBind)
             (bool32ToBool quadDivergentImplicitLod)
             maxPerStageDescriptorUpdateAfterBindSamplers
             maxPerStageDescriptorUpdateAfterBindUniformBuffers
             maxPerStageDescriptorUpdateAfterBindStorageBuffers
             maxPerStageDescriptorUpdateAfterBindSampledImages
             maxPerStageDescriptorUpdateAfterBindStorageImages
             maxPerStageDescriptorUpdateAfterBindInputAttachments
             maxPerStageUpdateAfterBindResources
             maxDescriptorSetUpdateAfterBindSamplers
             maxDescriptorSetUpdateAfterBindUniformBuffers
             maxDescriptorSetUpdateAfterBindUniformBuffersDynamic
             maxDescriptorSetUpdateAfterBindStorageBuffers
             maxDescriptorSetUpdateAfterBindStorageBuffersDynamic
             maxDescriptorSetUpdateAfterBindSampledImages
             maxDescriptorSetUpdateAfterBindStorageImages
             maxDescriptorSetUpdateAfterBindInputAttachments
             supportedDepthResolveModes
             supportedStencilResolveModes
             (bool32ToBool independentResolveNone)
             (bool32ToBool independentResolve)
             (bool32ToBool filterMinmaxSingleComponentFormats)
             (bool32ToBool filterMinmaxImageComponentMapping)
             maxTimelineSemaphoreValueDifference
             framebufferIntegerColorSampleCounts

instance Storable PhysicalDeviceVulkan12Properties where
  sizeOf ~_ = 736
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVulkan12Properties where
  zero = PhysicalDeviceVulkan12Properties
           zero
           mempty
           mempty
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
           zero
           zero
           zero

