{-# language CPP #-}
module Graphics.Vulkan.Core12  ( pattern API_VERSION_1_2
                               , PhysicalDeviceVulkan11Features(..)
                               , PhysicalDeviceVulkan11Properties(..)
                               , PhysicalDeviceVulkan12Features(..)
                               , PhysicalDeviceVulkan12Properties(..)
                               , StructureType(..)
                               , module Graphics.Vulkan.Core12.Enums
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_driver_properties
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_image_format_list
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_float_controls
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout
                               , module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model
                               ) where
import Graphics.Vulkan.Core12.Enums
import Graphics.Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing
import Graphics.Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset
import Graphics.Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax
import Graphics.Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout
import Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_driver_properties
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_image_format_list
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_float_controls
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import qualified Data.Vector.Storable.Sized (Vector)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.CStruct.Utils (lowerArrayPtr)
import Graphics.Vulkan.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import Graphics.Vulkan.CStruct.Utils (pokeFixedLengthByteString)
import Graphics.Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_driver_properties (ConformanceVersion)
import Graphics.Vulkan.Core10.BaseType (DeviceSize)
import Graphics.Vulkan.Core12.Enums.DriverId (DriverId)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.APIConstants (LUID_SIZE)
import Graphics.Vulkan.Core10.APIConstants (MAX_DRIVER_INFO_SIZE)
import Graphics.Vulkan.Core10.APIConstants (MAX_DRIVER_NAME_SIZE)
import Graphics.Vulkan.Core11.Enums.PointClippingBehavior (PointClippingBehavior)
import Graphics.Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlags)
import Graphics.Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlags)
import Graphics.Vulkan.Core12.Enums.ShaderFloatControlsIndependence (ShaderFloatControlsIndependence)
import Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Core10.APIConstants (UUID_SIZE)
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Version (pattern MAKE_VERSION)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(..))
pattern API_VERSION_1_2 :: Word32
pattern API_VERSION_1_2 = MAKE_VERSION 1 2 0


-- | VkPhysicalDeviceVulkan11Features - Structure describing the Vulkan 1.1
-- features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceVulkan11Features' structure describe
-- the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceVulkan11Features' structure is included in the
-- @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceVulkan11Features' /can/ also be used in the @pNext@ chain
-- of 'Graphics.Vulkan.Core10.Device.DeviceCreateInfo' to enable the
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVulkan11Features = PhysicalDeviceVulkan11Features
  { -- | @storageBuffer16BitAccess@ specifies whether objects in the
    -- @StorageBuffer@ or @PhysicalStorageBuffer@ storage class with the
    -- @Block@ decoration /can/ have 16-bit integer and 16-bit floating-point
    -- members. If this feature is not enabled, 16-bit integer or 16-bit
    -- floating-point members /must/ not be used in such objects. This also
    -- specifies whether shader modules /can/ declare the
    -- @StorageBuffer16BitAccess@ capability.
    storageBuffer16BitAccess :: Bool
  , -- | @uniformAndStorageBuffer16BitAccess@ specifies whether objects in the
    -- @Uniform@ storage class with the @Block@ decoration and in the
    -- @StorageBuffer@ or @PhysicalStorageBuffer@ storage class with the same
    -- decoration /can/ have 16-bit integer and 16-bit floating-point members.
    -- If this feature is not enabled, 16-bit integer or 16-bit floating-point
    -- members /must/ not be used in such objects. This also specifies whether
    -- shader modules /can/ declare the @UniformAndStorageBuffer16BitAccess@
    -- capability.
    uniformAndStorageBuffer16BitAccess :: Bool
  , -- | @storagePushConstant16@ specifies whether objects in the @PushConstant@
    -- storage class /can/ have 16-bit integer and 16-bit floating-point
    -- members. If this feature is not enabled, 16-bit integer or
    -- floating-point members /must/ not be used in such objects. This also
    -- specifies whether shader modules /can/ declare the
    -- @StoragePushConstant16@ capability.
    storagePushConstant16 :: Bool
  , -- | @storageInputOutput16@ specifies whether objects in the @Input@ and
    -- @Output@ storage classes /can/ have 16-bit integer and 16-bit
    -- floating-point members. If this feature is not enabled, 16-bit integer
    -- or 16-bit floating-point members /must/ not be used in such objects.
    -- This also specifies whether shader modules /can/ declare the
    -- @StorageInputOutput16@ capability.
    storageInputOutput16 :: Bool
  , -- | @multiview@ specifies whether the implementation supports multiview
    -- rendering within a render pass. If this feature is not enabled, the view
    -- mask of each subpass /must/ always be zero.
    multiview :: Bool
  , -- | @multiviewGeometryShader@ specifies whether the implementation supports
    -- multiview rendering within a render pass, with
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#geometry geometry shaders>.
    -- If this feature is not enabled, then a pipeline compiled against a
    -- subpass with a non-zero view mask /must/ not include a geometry shader.
    multiviewGeometryShader :: Bool
  , -- | @multiviewTessellationShader@ specifies whether the implementation
    -- supports multiview rendering within a render pass, with
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#tessellation tessellation shaders>.
    -- If this feature is not enabled, then a pipeline compiled against a
    -- subpass with a non-zero view mask /must/ not include any tessellation
    -- shaders.
    multiviewTessellationShader :: Bool
  , -- | @variablePointersStorageBuffer@ specifies whether the implementation
    -- supports the SPIR-V @VariablePointersStorageBuffer@ capability. When
    -- this feature is not enabled, shader modules /must/ not declare the
    -- @SPV_KHR_variable_pointers@ extension or the
    -- @VariablePointersStorageBuffer@ capability.
    variablePointersStorageBuffer :: Bool
  , -- | @variablePointers@ specifies whether the implementation supports the
    -- SPIR-V @VariablePointers@ capability. When this feature is not enabled,
    -- shader modules /must/ not declare the @VariablePointers@ capability.
    variablePointers :: Bool
  , -- | @protectedMemory@ specifies whether protected memory is supported.
    protectedMemory :: Bool
  , -- | @samplerYcbcrConversion@ specifies whether the implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>.
    -- If @samplerYcbcrConversion@ is 'Graphics.Vulkan.Core10.BaseType.FALSE',
    -- sampler Y′CBCR conversion is not supported, and samplers using sampler
    -- Y′CBCR conversion /must/ not be used.
    samplerYcbcrConversion :: Bool
  , -- | @shaderDrawParameters@ specifies whether shader draw parameters are
    -- supported.
    shaderDrawParameters :: Bool
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceVulkan11Features

instance ToCStruct PhysicalDeviceVulkan11Features where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
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
             (bool32ToBool storageBuffer16BitAccess) (bool32ToBool uniformAndStorageBuffer16BitAccess) (bool32ToBool storagePushConstant16) (bool32ToBool storageInputOutput16) (bool32ToBool multiview) (bool32ToBool multiviewGeometryShader) (bool32ToBool multiviewTessellationShader) (bool32ToBool variablePointersStorageBuffer) (bool32ToBool variablePointers) (bool32ToBool protectedMemory) (bool32ToBool samplerYcbcrConversion) (bool32ToBool shaderDrawParameters)

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
-- The members of 'PhysicalDeviceVulkan11Properties' /must/ have the same
-- values as the corresponding members of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceIDProperties',
-- 'Graphics.Vulkan.Core11.Originally_Based_On_VK_KHR_subgroup.PhysicalDeviceSubgroupProperties',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.PhysicalDevicePointClippingProperties',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties',
-- 'Graphics.Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.PhysicalDeviceProtectedMemoryProperties',
-- and
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance3.PhysicalDeviceMaintenance3Properties'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'Graphics.Vulkan.Core11.Enums.PointClippingBehavior.PointClippingBehavior',
-- 'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Graphics.Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SubgroupFeatureFlags'
data PhysicalDeviceVulkan11Properties = PhysicalDeviceVulkan11Properties
  { -- | @deviceUUID@ is an array of
    -- 'Graphics.Vulkan.Core10.APIConstants.UUID_SIZE' @uint8_t@ values
    -- representing a universally unique identifier for the device.
    deviceUUID :: ByteString
  , -- | @driverUUID@ is an array of
    -- 'Graphics.Vulkan.Core10.APIConstants.UUID_SIZE' @uint8_t@ values
    -- representing a universally unique identifier for the driver build in use
    -- by the device.
    driverUUID :: ByteString
  , -- | @deviceLUID@ is an array of
    -- 'Graphics.Vulkan.Core10.APIConstants.LUID_SIZE' @uint8_t@ values
    -- representing a locally unique identifier for the device.
    deviceLUID :: ByteString
  , -- | @deviceNodeMask@ is a @uint32_t@ bitfield identifying the node within a
    -- linked device adapter corresponding to the device.
    deviceNodeMask :: Word32
  , -- | @deviceLUIDValid@ is a boolean value that will be
    -- 'Graphics.Vulkan.Core10.BaseType.TRUE' if @deviceLUID@ contains a valid
    -- LUID and @deviceNodeMask@ contains a valid node mask, and
    -- 'Graphics.Vulkan.Core10.BaseType.FALSE' if they do not.
    deviceLUIDValid :: Bool
  , -- | @subgroupSize@ is the default number of invocations in each subgroup.
    -- @subgroupSize@ is at least 1 if any of the physical device’s queues
    -- support 'Graphics.Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT'
    -- or 'Graphics.Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT'.
    -- @subgroupSize@ is a power-of-two.
    subgroupSize :: Word32
  , -- | @subgroupSupportedStages@ is a bitfield of
    -- 'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits'
    -- describing the shader stages that subgroup operations are supported in.
    -- @subgroupSupportedStages@ will have the
    -- 'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT'
    -- bit set if any of the physical device’s queues support
    -- 'Graphics.Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT'.
    subgroupSupportedStages :: ShaderStageFlags
  , -- | @subgroupSupportedOperations@ is a bitmask of
    -- 'Graphics.Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SubgroupFeatureFlagBits'
    -- specifying the sets of subgroup operations supported on this device.
    -- @subgroupSupportedOperations@ will have the
    -- 'Graphics.Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SUBGROUP_FEATURE_BASIC_BIT'
    -- bit set if any of the physical device’s queues support
    -- 'Graphics.Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
    -- 'Graphics.Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT'.
    subgroupSupportedOperations :: SubgroupFeatureFlags
  , -- | @subgroupQuadOperationsInAllStages@ is a boolean specifying whether
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-subgroup-quad quad subgroup operations>
    -- are available in all stages, or are restricted to fragment and compute
    -- stages.
    subgroupQuadOperationsInAllStages :: Bool
  , -- | @pointClippingBehavior@ /must/ be a valid
    -- 'Graphics.Vulkan.Core11.Enums.PointClippingBehavior.PointClippingBehavior'
    -- value
    pointClippingBehavior :: PointClippingBehavior
  , -- | @maxMultiviewViewCount@ is one greater than the maximum view index that
    -- /can/ be used in a subpass.
    maxMultiviewViewCount :: Word32
  , -- | @maxMultiviewInstanceIndex@ is the maximum valid value of instance index
    -- allowed to be generated by a drawing command recorded within a subpass
    -- of a multiview render pass instance.
    maxMultiviewInstanceIndex :: Word32
  , -- | @protectedNoFault@ specifies the behavior of the implementation when
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-protected-access-rules protected memory access rules>
    -- are broken. If @protectedNoFault@ is
    -- 'Graphics.Vulkan.Core10.BaseType.TRUE', breaking those rules will not
    -- result in process termination or device loss.
    protectedNoFault :: Bool
  , -- | @maxPerSetDescriptors@ is a maximum number of descriptors (summed over
    -- all descriptor types) in a single descriptor set that is guaranteed to
    -- satisfy any implementation-dependent constraints on the size of a
    -- descriptor set itself. Applications /can/ query whether a descriptor set
    -- that goes beyond this limit is supported using
    -- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance3.getDescriptorSetLayoutSupport'.
    maxPerSetDescriptors :: Word32
  , -- | @maxMemoryAllocationSize@ is the maximum size of a memory allocation
    -- that /can/ be created, even if there is more space available in the
    -- heap.
    maxMemoryAllocationSize :: DeviceSize
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceVulkan11Properties

instance ToCStruct PhysicalDeviceVulkan11Properties where
  withCStruct x f = allocaBytesAligned 112 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVulkan11Properties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (Data.Vector.Storable.Sized.Vector UUID_SIZE Word8))) (deviceUUID)
    pokeFixedLengthByteString ((p `plusPtr` 32 :: Ptr (Data.Vector.Storable.Sized.Vector UUID_SIZE Word8))) (driverUUID)
    pokeFixedLengthByteString ((p `plusPtr` 48 :: Ptr (Data.Vector.Storable.Sized.Vector LUID_SIZE Word8))) (deviceLUID)
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
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (Data.Vector.Storable.Sized.Vector UUID_SIZE Word8))) (mempty)
    pokeFixedLengthByteString ((p `plusPtr` 32 :: Ptr (Data.Vector.Storable.Sized.Vector UUID_SIZE Word8))) (mempty)
    pokeFixedLengthByteString ((p `plusPtr` 48 :: Ptr (Data.Vector.Storable.Sized.Vector LUID_SIZE Word8))) (mempty)
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
    deviceUUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 16 :: Ptr (Data.Vector.Storable.Sized.Vector UUID_SIZE Word8)))
    driverUUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 32 :: Ptr (Data.Vector.Storable.Sized.Vector UUID_SIZE Word8)))
    deviceLUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 48 :: Ptr (Data.Vector.Storable.Sized.Vector LUID_SIZE Word8)))
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
             deviceUUID driverUUID deviceLUID deviceNodeMask (bool32ToBool deviceLUIDValid) subgroupSize subgroupSupportedStages subgroupSupportedOperations (bool32ToBool subgroupQuadOperationsInAllStages) pointClippingBehavior maxMultiviewViewCount maxMultiviewInstanceIndex (bool32ToBool protectedNoFault) maxPerSetDescriptors maxMemoryAllocationSize

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
-- The members of the 'PhysicalDeviceVulkan12Features' structure describe
-- the following features:
--
-- = Description
--
-- -   @samplerMirrorClampToEdge@ indicates whether the implementation
--     supports the
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE'
--     sampler address mode. If this feature is not enabled, the
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE'
--     sampler address mode /must/ not be used.
--
-- -   @drawIndirectCount@ indicates whether the implementation supports
--     the
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndirectCount'
--     and
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCount'
--     functions. If this feature is not enabled, these functions /must/
--     not be used.
--
-- -   @storageBuffer8BitAccess@ indicates whether objects in the
--     @StorageBuffer@ or @PhysicalStorageBuffer@ storage class with the
--     @Block@ decoration /can/ have 8-bit integer members. If this feature
--     is not enabled, 8-bit integer members /must/ not be used in such
--     objects. This also indicates whether shader modules /can/ declare
--     the @StorageBuffer8BitAccess@ capability.
--
-- -   @uniformAndStorageBuffer8BitAccess@ indicates whether objects in the
--     @Uniform@ storage class with the @Block@ decoration and in the
--     @StorageBuffer@ or @PhysicalStorageBuffer@ storage class with the
--     same decoration /can/ have 8-bit integer members. If this feature is
--     not enabled, 8-bit integer members /must/ not be used in such
--     objects. This also indicates whether shader modules /can/ declare
--     the @UniformAndStorageBuffer8BitAccess@ capability.
--
-- -   @storagePushConstant8@ indicates whether objects in the
--     @PushConstant@ storage class /can/ have 8-bit integer members. If
--     this feature is not enabled, 8-bit integer members /must/ not be
--     used in such objects. This also indicates whether shader modules
--     /can/ declare the @StoragePushConstant8@ capability.
--
-- -   @shaderBufferInt64Atomics@ indicates whether shaders /can/ support
--     64-bit unsigned and signed integer atomic operations on buffers.
--
-- -   @shaderSharedInt64Atomics@ indicates whether shaders /can/ support
--     64-bit unsigned and signed integer atomic operations on shared
--     memory.
--
-- -   @shaderFloat16@ indicates whether 16-bit floats (halfs) are
--     supported in shader code. This also indicates whether shader modules
--     /can/ declare the @Float16@ capability. However, this only enables a
--     subset of the storage classes that SPIR-V allows for the @Float16@
--     SPIR-V capability: Declaring and using 16-bit floats in the
--     @Private@, @Workgroup@, and @Function@ storage classes is enabled,
--     while declaring them in the interface storage classes (e.g.,
--     @UniformConstant@, @Uniform@, @StorageBuffer@, @Input@, @Output@,
--     and @PushConstant@) is not enabled.
--
-- -   @shaderInt8@ indicates whether 8-bit integers (signed and unsigned)
--     are supported in shader code. This also indicates whether shader
--     modules /can/ declare the @Int8@ capability. However, this only
--     enables a subset of the storage classes that SPIR-V allows for the
--     @Int8@ SPIR-V capability: Declaring and using 8-bit integers in the
--     @Private@, @Workgroup@, and @Function@ storage classes is enabled,
--     while declaring them in the interface storage classes (e.g.,
--     @UniformConstant@, @Uniform@, @StorageBuffer@, @Input@, @Output@,
--     and @PushConstant@) is not enabled.
--
-- -   @descriptorIndexing@ indicates whether the implementation supports
--     the minimum set of descriptor indexing features as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-requirements Feature Requirements>
--     section. Enabling the @descriptorIndexing@ member when
--     'Graphics.Vulkan.Core10.Device.createDevice' is called does not
--     imply the other minimum descriptor indexing features are also
--     enabled. Those other descriptor indexing features /must/ be enabled
--     individually as needed by the application.
--
-- -   @shaderInputAttachmentArrayDynamicIndexing@ indicates whether arrays
--     of input attachments /can/ be indexed by dynamically uniform integer
--     expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     /must/ be indexed only by constant integral expressions when
--     aggregated into arrays in shader code. This also indicates whether
--     shader modules /can/ declare the
--     @InputAttachmentArrayDynamicIndexing@ capability.
--
-- -   @shaderUniformTexelBufferArrayDynamicIndexing@ indicates whether
--     arrays of uniform texel buffers /can/ be indexed by dynamically
--     uniform integer expressions in shader code. If this feature is not
--     enabled, resources with a descriptor type of
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     /must/ be indexed only by constant integral expressions when
--     aggregated into arrays in shader code. This also indicates whether
--     shader modules /can/ declare the
--     @UniformTexelBufferArrayDynamicIndexing@ capability.
--
-- -   @shaderStorageTexelBufferArrayDynamicIndexing@ indicates whether
--     arrays of storage texel buffers /can/ be indexed by dynamically
--     uniform integer expressions in shader code. If this feature is not
--     enabled, resources with a descriptor type of
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     /must/ be indexed only by constant integral expressions when
--     aggregated into arrays in shader code. This also indicates whether
--     shader modules /can/ declare the
--     @StorageTexelBufferArrayDynamicIndexing@ capability.
--
-- -   @shaderUniformBufferArrayNonUniformIndexing@ indicates whether
--     arrays of uniform buffers /can/ be indexed by non-uniform integer
--     expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     or
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     /must/ not be indexed by non-uniform integer expressions when
--     aggregated into arrays in shader code. This also indicates whether
--     shader modules /can/ declare the
--     @UniformBufferArrayNonUniformIndexing@ capability.
--
-- -   @shaderSampledImageArrayNonUniformIndexing@ indicates whether arrays
--     of samplers or sampled images /can/ be indexed by non-uniform
--     integer expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER',
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     or
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE'
--     /must/ not be indexed by non-uniform integer expressions when
--     aggregated into arrays in shader code. This also indicates whether
--     shader modules /can/ declare the
--     @SampledImageArrayNonUniformIndexing@ capability.
--
-- -   @shaderStorageBufferArrayNonUniformIndexing@ indicates whether
--     arrays of storage buffers /can/ be indexed by non-uniform integer
--     expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     or
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--     /must/ not be indexed by non-uniform integer expressions when
--     aggregated into arrays in shader code. This also indicates whether
--     shader modules /can/ declare the
--     @StorageBufferArrayNonUniformIndexing@ capability.
--
-- -   @shaderStorageImageArrayNonUniformIndexing@ indicates whether arrays
--     of storage images /can/ be indexed by non-uniform integer
--     expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'
--     /must/ not be indexed by non-uniform integer expressions when
--     aggregated into arrays in shader code. This also indicates whether
--     shader modules /can/ declare the
--     @StorageImageArrayNonUniformIndexing@ capability.
--
-- -   @shaderInputAttachmentArrayNonUniformIndexing@ indicates whether
--     arrays of input attachments /can/ be indexed by non-uniform integer
--     expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     /must/ not be indexed by non-uniform integer expressions when
--     aggregated into arrays in shader code. This also indicates whether
--     shader modules /can/ declare the
--     @InputAttachmentArrayNonUniformIndexing@ capability.
--
-- -   @shaderUniformTexelBufferArrayNonUniformIndexing@ indicates whether
--     arrays of uniform texel buffers /can/ be indexed by non-uniform
--     integer expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     /must/ not be indexed by non-uniform integer expressions when
--     aggregated into arrays in shader code. This also indicates whether
--     shader modules /can/ declare the
--     @UniformTexelBufferArrayNonUniformIndexing@ capability.
--
-- -   @shaderStorageTexelBufferArrayNonUniformIndexing@ indicates whether
--     arrays of storage texel buffers /can/ be indexed by non-uniform
--     integer expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     /must/ not be indexed by non-uniform integer expressions when
--     aggregated into arrays in shader code. This also indicates whether
--     shader modules /can/ declare the
--     @StorageTexelBufferArrayNonUniformIndexing@ capability.
--
-- -   @descriptorBindingUniformBufferUpdateAfterBind@ indicates whether
--     the implementation supports updating uniform buffer descriptors
--     after a set is bound. If this feature is not enabled,
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--     /must/ not be used with
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'.
--
-- -   @descriptorBindingSampledImageUpdateAfterBind@ indicates whether the
--     implementation supports updating sampled image descriptors after a
--     set is bound. If this feature is not enabled,
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--     /must/ not be used with
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER',
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     or
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE'.
--
-- -   @descriptorBindingStorageImageUpdateAfterBind@ indicates whether the
--     implementation supports updating storage image descriptors after a
--     set is bound. If this feature is not enabled,
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--     /must/ not be used with
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'.
--
-- -   @descriptorBindingStorageBufferUpdateAfterBind@ indicates whether
--     the implementation supports updating storage buffer descriptors
--     after a set is bound. If this feature is not enabled,
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--     /must/ not be used with
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'.
--
-- -   @descriptorBindingUniformTexelBufferUpdateAfterBind@ indicates
--     whether the implementation supports updating uniform texel buffer
--     descriptors after a set is bound. If this feature is not enabled,
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--     /must/ not be used with
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'.
--
-- -   @descriptorBindingStorageTexelBufferUpdateAfterBind@ indicates
--     whether the implementation supports updating storage texel buffer
--     descriptors after a set is bound. If this feature is not enabled,
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--     /must/ not be used with
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'.
--
-- -   @descriptorBindingUpdateUnusedWhilePending@ indicates whether the
--     implementation supports updating descriptors while the set is in
--     use. If this feature is not enabled,
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT'
--     /must/ not be used.
--
-- -   @descriptorBindingPartiallyBound@ indicates whether the
--     implementation supports statically using a descriptor set binding in
--     which some descriptors are not valid. If this feature is not
--     enabled,
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT'
--     /must/ not be used.
--
-- -   @descriptorBindingVariableDescriptorCount@ indicates whether the
--     implementation supports descriptor sets with a variable-sized last
--     binding. If this feature is not enabled,
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT'
--     /must/ not be used.
--
-- -   @runtimeDescriptorArray@ indicates whether the implementation
--     supports the SPIR-V @RuntimeDescriptorArray@ capability. If this
--     feature is not enabled, descriptors /must/ not be declared in
--     runtime arrays.
--
-- -   @samplerFilterMinmax@ indicates whether the implementation supports
--     a minimum set of required formats supporting min\/max filtering as
--     defined by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-filterMinmaxSingleComponentFormats-minimum-requirements filterMinmaxSingleComponentFormats>
--     property minimum requirements. If this feature is not enabled, then
--     no 'Graphics.Vulkan.Core10.Sampler.SamplerCreateInfo' @pNext@ chain
--     can include a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'
--     structure.
--
-- -   @scalarBlockLayout@ indicates that the implementation supports the
--     layout of resource blocks in shaders using
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-alignment-requirements scalar alignment>.
--
-- -   @imagelessFramebuffer@ indicates that the implementation supports
--     specifying the image view for attachments at render pass begin time
--     via
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'.
--
-- -   @uniformBufferStandardLayout@ indicates that the implementation
--     supports the same layouts for uniform buffers as for storage and
--     other kinds of buffers. See
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-resources-standard-layout Standard Buffer Layout>.
--
-- -   @shaderSubgroupExtendedTypes@ is a boolean that specifies whether
--     subgroup operations can use 8-bit integer, 16-bit integer, 64-bit
--     integer, 16-bit floating-point, and vectors of these types in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-group-operations group operations>
--     with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-scope-subgroup subgroup scope>if
--     the implementation supports the types.
--
-- -   @separateDepthStencilLayouts@ indicates whether the implementation
--     supports a 'Graphics.Vulkan.Core10.OtherTypes.ImageMemoryBarrier'
--     for a depth\/stencil image with only one of
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--     set, and whether
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--     can be used.
--
-- -   @hostQueryReset@ indicates that the implementation supports
--     resetting queries from the host with
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.resetQueryPool'.
--
-- -   @timelineSemaphore@ indicates whether semaphores created with a
--     'Graphics.Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Graphics.Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE'
--     are supported.
--
-- -   @bufferDeviceAddress@ indicates that the implementation supports
--     accessing buffer memory in shaders as storage buffers via an address
--     queried from
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'.
--
-- -   @bufferDeviceAddressCaptureReplay@ indicates that the implementation
--     supports saving and reusing buffer and device addresses, e.g. for
--     trace capture and replay.
--
-- -   @bufferDeviceAddressMultiDevice@ indicates that the implementation
--     supports the @bufferDeviceAddress@ feature for logical devices
--     created with multiple physical devices. If this feature is not
--     supported, buffer addresses /must/ not be queried on a logical
--     device created with more than one physical device.
--
-- -   @vulkanMemoryModel@ indicates whether the Vulkan Memory Model is
--     supported, as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-model Vulkan Memory Model>.
--     This also indicates whether shader modules /can/ declare the
--     @VulkanMemoryModel@ capability.
--
-- -   @vulkanMemoryModelDeviceScope@ indicates whether the Vulkan Memory
--     Model can use 'Graphics.Vulkan.Core10.Handles.Device' scope
--     synchronization. This also indicates whether shader modules /can/
--     declare the @VulkanMemoryModelDeviceScope@ capability.
--
-- -   @vulkanMemoryModelAvailabilityVisibilityChains@ indicates whether
--     the Vulkan Memory Model can use
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-model-availability-visibility availability and visibility chains>
--     with more than one element.
--
-- -   @shaderOutputViewportIndex@ indicates whether the implementation
--     supports the @ShaderViewportIndex@ SPIR-V capability enabling
--     variables decorated with the @ViewportIndex@ built-in to be exported
--     from vertex or tessellation evaluation shaders. If this feature is
--     not enabled, the @ViewportIndex@ built-in decoration /must/ not be
--     used on outputs in vertex or tessellation evaluation shaders.
--
-- -   @shaderOutputLayer@ indicates whether the implementation supports
--     the @ShaderLayer@ SPIR-V capability enabling variables decorated
--     with the @Layer@ built-in to be exported from vertex or tessellation
--     evaluation shaders. If this feature is not enabled, the @Layer@
--     built-in decoration /must/ not be used on outputs in vertex or
--     tessellation evaluation shaders.
--
-- -   If @subgroupBroadcastDynamicId@ is
--     'Graphics.Vulkan.Core10.BaseType.TRUE', the “Id” operand of
--     @OpGroupNonUniformBroadcast@ /can/ be dynamically uniform within a
--     subgroup, and the “Index” operand of
--     @OpGroupNonUniformQuadBroadcast@ /can/ be dynamically uniform within
--     the derivative group. If it is
--     'Graphics.Vulkan.Core10.BaseType.FALSE', these operands /must/ be
--     constants.
--
-- If the 'PhysicalDeviceVulkan12Features' structure is included in the
-- @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceVulkan12Features' /can/ also be used in the @pNext@ chain
-- of 'Graphics.Vulkan.Core10.Device.DeviceCreateInfo' to enable the
-- features.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
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
  deriving (Typeable)
deriving instance Show PhysicalDeviceVulkan12Features

instance ToCStruct PhysicalDeviceVulkan12Features where
  withCStruct x f = allocaBytesAligned 208 8 $ \p -> pokeCStruct p x (f p)
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
             (bool32ToBool samplerMirrorClampToEdge) (bool32ToBool drawIndirectCount) (bool32ToBool storageBuffer8BitAccess) (bool32ToBool uniformAndStorageBuffer8BitAccess) (bool32ToBool storagePushConstant8) (bool32ToBool shaderBufferInt64Atomics) (bool32ToBool shaderSharedInt64Atomics) (bool32ToBool shaderFloat16) (bool32ToBool shaderInt8) (bool32ToBool descriptorIndexing) (bool32ToBool shaderInputAttachmentArrayDynamicIndexing) (bool32ToBool shaderUniformTexelBufferArrayDynamicIndexing) (bool32ToBool shaderStorageTexelBufferArrayDynamicIndexing) (bool32ToBool shaderUniformBufferArrayNonUniformIndexing) (bool32ToBool shaderSampledImageArrayNonUniformIndexing) (bool32ToBool shaderStorageBufferArrayNonUniformIndexing) (bool32ToBool shaderStorageImageArrayNonUniformIndexing) (bool32ToBool shaderInputAttachmentArrayNonUniformIndexing) (bool32ToBool shaderUniformTexelBufferArrayNonUniformIndexing) (bool32ToBool shaderStorageTexelBufferArrayNonUniformIndexing) (bool32ToBool descriptorBindingUniformBufferUpdateAfterBind) (bool32ToBool descriptorBindingSampledImageUpdateAfterBind) (bool32ToBool descriptorBindingStorageImageUpdateAfterBind) (bool32ToBool descriptorBindingStorageBufferUpdateAfterBind) (bool32ToBool descriptorBindingUniformTexelBufferUpdateAfterBind) (bool32ToBool descriptorBindingStorageTexelBufferUpdateAfterBind) (bool32ToBool descriptorBindingUpdateUnusedWhilePending) (bool32ToBool descriptorBindingPartiallyBound) (bool32ToBool descriptorBindingVariableDescriptorCount) (bool32ToBool runtimeDescriptorArray) (bool32ToBool samplerFilterMinmax) (bool32ToBool scalarBlockLayout) (bool32ToBool imagelessFramebuffer) (bool32ToBool uniformBufferStandardLayout) (bool32ToBool shaderSubgroupExtendedTypes) (bool32ToBool separateDepthStencilLayouts) (bool32ToBool hostQueryReset) (bool32ToBool timelineSemaphore) (bool32ToBool bufferDeviceAddress) (bool32ToBool bufferDeviceAddressCaptureReplay) (bool32ToBool bufferDeviceAddressMultiDevice) (bool32ToBool vulkanMemoryModel) (bool32ToBool vulkanMemoryModelDeviceScope) (bool32ToBool vulkanMemoryModelAvailabilityVisibilityChains) (bool32ToBool shaderOutputViewportIndex) (bool32ToBool shaderOutputLayer) (bool32ToBool subgroupBroadcastDynamicId)

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
-- The members of 'PhysicalDeviceVulkan12Properties' /must/ have the same
-- values as the corresponding members of
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_driver_properties.PhysicalDeviceDriverProperties',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_float_controls.PhysicalDeviceFloatControlsProperties',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingProperties',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.PhysicalDeviceDepthStencilResolveProperties',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.PhysicalDeviceSamplerFilterMinmaxProperties',
-- and
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.PhysicalDeviceTimelineSemaphoreProperties'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_driver_properties.ConformanceVersion',
-- 'Graphics.Vulkan.Core12.Enums.DriverId.DriverId',
-- 'Graphics.Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlags',
-- 'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlags',
-- 'Graphics.Vulkan.Core12.Enums.ShaderFloatControlsIndependence.ShaderFloatControlsIndependence',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVulkan12Properties = PhysicalDeviceVulkan12Properties
  { -- | @driverID@ /must/ be a valid
    -- 'Graphics.Vulkan.Core12.Enums.DriverId.DriverId' value
    driverID :: DriverId
  , -- | @driverName@ /must/ be a null-terminated UTF-8 string whose length is
    -- less than or equal to VK_MAX_DRIVER_NAME_SIZE
    driverName :: ByteString
  , -- | @driverInfo@ /must/ be a null-terminated UTF-8 string whose length is
    -- less than or equal to VK_MAX_DRIVER_INFO_SIZE
    driverInfo :: ByteString
  , -- | @conformanceVersion@ is the version of the Vulkan conformance test this
    -- driver is conformant against (see
    -- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_driver_properties.ConformanceVersion').
    conformanceVersion :: ConformanceVersion
  , -- | @denormBehaviorIndependence@ /must/ be a valid
    -- 'Graphics.Vulkan.Core12.Enums.ShaderFloatControlsIndependence.ShaderFloatControlsIndependence'
    -- value
    denormBehaviorIndependence :: ShaderFloatControlsIndependence
  , -- | @roundingModeIndependence@ /must/ be a valid
    -- 'Graphics.Vulkan.Core12.Enums.ShaderFloatControlsIndependence.ShaderFloatControlsIndependence'
    -- value
    roundingModeIndependence :: ShaderFloatControlsIndependence
  , -- | @shaderSignedZeroInfNanPreserveFloat16@ is a boolean value indicating
    -- whether sign of a zero, Nans and \(\pm\infty\) /can/ be preserved in
    -- 16-bit floating-point computations. It also indicates whether the
    -- @SignedZeroInfNanPreserve@ execution mode /can/ be used for 16-bit
    -- floating-point types.
    shaderSignedZeroInfNanPreserveFloat16 :: Bool
  , -- | @shaderSignedZeroInfNanPreserveFloat32@ is a boolean value indicating
    -- whether sign of a zero, Nans and \(\pm\infty\) /can/ be preserved in
    -- 32-bit floating-point computations. It also indicates whether the
    -- @SignedZeroInfNanPreserve@ execution mode /can/ be used for 32-bit
    -- floating-point types.
    shaderSignedZeroInfNanPreserveFloat32 :: Bool
  , -- | @shaderSignedZeroInfNanPreserveFloat64@ is a boolean value indicating
    -- whether sign of a zero, Nans and \(\pm\infty\) /can/ be preserved in
    -- 64-bit floating-point computations. It also indicates whether the
    -- @SignedZeroInfNanPreserve@ execution mode /can/ be used for 64-bit
    -- floating-point types.
    shaderSignedZeroInfNanPreserveFloat64 :: Bool
  , -- | @shaderDenormPreserveFloat16@ is a boolean value indicating whether
    -- denormals /can/ be preserved in 16-bit floating-point computations. It
    -- also indicates whether the @DenormPreserve@ execution mode /can/ be used
    -- for 16-bit floating-point types.
    shaderDenormPreserveFloat16 :: Bool
  , -- | @shaderDenormPreserveFloat32@ is a boolean value indicating whether
    -- denormals /can/ be preserved in 32-bit floating-point computations. It
    -- also indicates whether the @DenormPreserve@ execution mode /can/ be used
    -- for 32-bit floating-point types.
    shaderDenormPreserveFloat32 :: Bool
  , -- | @shaderDenormPreserveFloat64@ is a boolean value indicating whether
    -- denormals /can/ be preserved in 64-bit floating-point computations. It
    -- also indicates whether the @DenormPreserve@ execution mode /can/ be used
    -- for 64-bit floating-point types.
    shaderDenormPreserveFloat64 :: Bool
  , -- | @shaderDenormFlushToZeroFloat16@ is a boolean value indicating whether
    -- denormals /can/ be flushed to zero in 16-bit floating-point
    -- computations. It also indicates whether the @DenormFlushToZero@
    -- execution mode /can/ be used for 16-bit floating-point types.
    shaderDenormFlushToZeroFloat16 :: Bool
  , -- | @shaderDenormFlushToZeroFloat32@ is a boolean value indicating whether
    -- denormals /can/ be flushed to zero in 32-bit floating-point
    -- computations. It also indicates whether the @DenormFlushToZero@
    -- execution mode /can/ be used for 32-bit floating-point types.
    shaderDenormFlushToZeroFloat32 :: Bool
  , -- | @shaderDenormFlushToZeroFloat64@ is a boolean value indicating whether
    -- denormals /can/ be flushed to zero in 64-bit floating-point
    -- computations. It also indicates whether the @DenormFlushToZero@
    -- execution mode /can/ be used for 64-bit floating-point types.
    shaderDenormFlushToZeroFloat64 :: Bool
  , -- | @shaderRoundingModeRTEFloat16@ is a boolean value indicating whether an
    -- implementation supports the round-to-nearest-even rounding mode for
    -- 16-bit floating-point arithmetic and conversion instructions. It also
    -- indicates whether the @RoundingModeRTE@ execution mode /can/ be used for
    -- 16-bit floating-point types.
    shaderRoundingModeRTEFloat16 :: Bool
  , -- | @shaderRoundingModeRTEFloat32@ is a boolean value indicating whether an
    -- implementation supports the round-to-nearest-even rounding mode for
    -- 32-bit floating-point arithmetic and conversion instructions. It also
    -- indicates whether the @RoundingModeRTE@ execution mode /can/ be used for
    -- 32-bit floating-point types.
    shaderRoundingModeRTEFloat32 :: Bool
  , -- | @shaderRoundingModeRTEFloat64@ is a boolean value indicating whether an
    -- implementation supports the round-to-nearest-even rounding mode for
    -- 64-bit floating-point arithmetic and conversion instructions. It also
    -- indicates whether the @RoundingModeRTE@ execution mode /can/ be used for
    -- 64-bit floating-point types.
    shaderRoundingModeRTEFloat64 :: Bool
  , -- | @shaderRoundingModeRTZFloat16@ is a boolean value indicating whether an
    -- implementation supports the round-towards-zero rounding mode for 16-bit
    -- floating-point arithmetic and conversion instructions. It also indicates
    -- whether the @RoundingModeRTZ@ execution mode /can/ be used for 16-bit
    -- floating-point types.
    shaderRoundingModeRTZFloat16 :: Bool
  , -- | @shaderRoundingModeRTZFloat32@ is a boolean value indicating whether an
    -- implementation supports the round-towards-zero rounding mode for 32-bit
    -- floating-point arithmetic and conversion instructions. It also indicates
    -- whether the @RoundingModeRTZ@ execution mode /can/ be used for 32-bit
    -- floating-point types.
    shaderRoundingModeRTZFloat32 :: Bool
  , -- | @shaderRoundingModeRTZFloat64@ is a boolean value indicating whether an
    -- implementation supports the round-towards-zero rounding mode for 64-bit
    -- floating-point arithmetic and conversion instructions. It also indicates
    -- whether the @RoundingModeRTZ@ execution mode /can/ be used for 64-bit
    -- floating-point types.
    shaderRoundingModeRTZFloat64 :: Bool
  , -- | @maxUpdateAfterBindDescriptorsInAllPools@ is the maximum number of
    -- descriptors (summed over all descriptor types) that /can/ be created
    -- across all pools that are created with the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT'
    -- bit set. Pool creation /may/ fail when this limit is exceeded, or when
    -- the space this limit represents is unable to satisfy a pool creation due
    -- to fragmentation.
    maxUpdateAfterBindDescriptorsInAllPools :: Word32
  , -- | @shaderUniformBufferArrayNonUniformIndexingNative@ is a boolean value
    -- indicating whether uniform buffer descriptors natively support
    -- nonuniform indexing. If this is 'Graphics.Vulkan.Core10.BaseType.FALSE',
    -- then a single dynamic instance of an instruction that nonuniformly
    -- indexes an array of uniform buffers /may/ execute multiple times in
    -- order to access all the descriptors.
    shaderUniformBufferArrayNonUniformIndexingNative :: Bool
  , -- | @shaderSampledImageArrayNonUniformIndexingNative@ is a boolean value
    -- indicating whether sampler and image descriptors natively support
    -- nonuniform indexing. If this is 'Graphics.Vulkan.Core10.BaseType.FALSE',
    -- then a single dynamic instance of an instruction that nonuniformly
    -- indexes an array of samplers or images /may/ execute multiple times in
    -- order to access all the descriptors.
    shaderSampledImageArrayNonUniformIndexingNative :: Bool
  , -- | @shaderStorageBufferArrayNonUniformIndexingNative@ is a boolean value
    -- indicating whether storage buffer descriptors natively support
    -- nonuniform indexing. If this is 'Graphics.Vulkan.Core10.BaseType.FALSE',
    -- then a single dynamic instance of an instruction that nonuniformly
    -- indexes an array of storage buffers /may/ execute multiple times in
    -- order to access all the descriptors.
    shaderStorageBufferArrayNonUniformIndexingNative :: Bool
  , -- | @shaderStorageImageArrayNonUniformIndexingNative@ is a boolean value
    -- indicating whether storage image descriptors natively support nonuniform
    -- indexing. If this is 'Graphics.Vulkan.Core10.BaseType.FALSE', then a
    -- single dynamic instance of an instruction that nonuniformly indexes an
    -- array of storage images /may/ execute multiple times in order to access
    -- all the descriptors.
    shaderStorageImageArrayNonUniformIndexingNative :: Bool
  , -- | @shaderInputAttachmentArrayNonUniformIndexingNative@ is a boolean value
    -- indicating whether input attachment descriptors natively support
    -- nonuniform indexing. If this is 'Graphics.Vulkan.Core10.BaseType.FALSE',
    -- then a single dynamic instance of an instruction that nonuniformly
    -- indexes an array of input attachments /may/ execute multiple times in
    -- order to access all the descriptors.
    shaderInputAttachmentArrayNonUniformIndexingNative :: Bool
  , -- | @robustBufferAccessUpdateAfterBind@ is a boolean value indicating
    -- whether
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
    -- /can/ be enabled in a device simultaneously with
    -- @descriptorBindingUniformBufferUpdateAfterBind@,
    -- @descriptorBindingStorageBufferUpdateAfterBind@,
    -- @descriptorBindingUniformTexelBufferUpdateAfterBind@, and\/or
    -- @descriptorBindingStorageTexelBufferUpdateAfterBind@. If this is
    -- 'Graphics.Vulkan.Core10.BaseType.FALSE', then either
    -- @robustBufferAccess@ /must/ be disabled or all of these
    -- update-after-bind features /must/ be disabled.
    robustBufferAccessUpdateAfterBind :: Bool
  , -- | @quadDivergentImplicitLod@ is a boolean value indicating whether
    -- implicit level of detail calculations for image operations have
    -- well-defined results when the image and\/or sampler objects used for the
    -- instruction are not uniform within a quad. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-derivative-image-operations Derivative Image Operations>.
    quadDivergentImplicitLod :: Bool
  , -- | @maxPerStageDescriptorUpdateAfterBindSamplers@ is similar to
    -- @maxPerStageDescriptorSamplers@ but counts descriptors from descriptor
    -- sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindSamplers :: Word32
  , -- | @maxPerStageDescriptorUpdateAfterBindUniformBuffers@ is similar to
    -- @maxPerStageDescriptorUniformBuffers@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindUniformBuffers :: Word32
  , -- | @maxPerStageDescriptorUpdateAfterBindStorageBuffers@ is similar to
    -- @maxPerStageDescriptorStorageBuffers@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindStorageBuffers :: Word32
  , -- | @maxPerStageDescriptorUpdateAfterBindSampledImages@ is similar to
    -- @maxPerStageDescriptorSampledImages@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindSampledImages :: Word32
  , -- | @maxPerStageDescriptorUpdateAfterBindStorageImages@ is similar to
    -- @maxPerStageDescriptorStorageImages@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindStorageImages :: Word32
  , -- | @maxPerStageDescriptorUpdateAfterBindInputAttachments@ is similar to
    -- @maxPerStageDescriptorInputAttachments@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindInputAttachments :: Word32
  , -- | @maxPerStageUpdateAfterBindResources@ is similar to
    -- @maxPerStageResources@ but counts descriptors from descriptor sets
    -- created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageUpdateAfterBindResources :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindSamplers@ is similar to
    -- @maxDescriptorSetSamplers@ but counts descriptors from descriptor sets
    -- created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindSamplers :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindUniformBuffers@ is similar to
    -- @maxDescriptorSetUniformBuffers@ but counts descriptors from descriptor
    -- sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindUniformBuffers :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindUniformBuffersDynamic@ is similar to
    -- @maxDescriptorSetUniformBuffersDynamic@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindUniformBuffersDynamic :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindStorageBuffers@ is similar to
    -- @maxDescriptorSetStorageBuffers@ but counts descriptors from descriptor
    -- sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindStorageBuffers :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindStorageBuffersDynamic@ is similar to
    -- @maxDescriptorSetStorageBuffersDynamic@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindStorageBuffersDynamic :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindSampledImages@ is similar to
    -- @maxDescriptorSetSampledImages@ but counts descriptors from descriptor
    -- sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindSampledImages :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindStorageImages@ is similar to
    -- @maxDescriptorSetStorageImages@ but counts descriptors from descriptor
    -- sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindStorageImages :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindInputAttachments@ is similar to
    -- @maxDescriptorSetInputAttachments@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindInputAttachments :: Word32
  , -- | @supportedDepthResolveModes@ /must/ not be @0@
    supportedDepthResolveModes :: ResolveModeFlags
  , -- | @supportedStencilResolveModes@ /must/ not be @0@
    supportedStencilResolveModes :: ResolveModeFlags
  , -- | @independentResolveNone@ is 'Graphics.Vulkan.Core10.BaseType.TRUE' if
    -- the implementation supports setting the depth and stencil resolve modes
    -- to different values when one of those modes is
    -- 'Graphics.Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE'.
    -- Otherwise the implementation only supports setting both modes to the
    -- same value.
    independentResolveNone :: Bool
  , -- | @independentResolve@ is 'Graphics.Vulkan.Core10.BaseType.TRUE' if the
    -- implementation supports all combinations of the supported depth and
    -- stencil resolve modes, including setting either depth or stencil resolve
    -- mode to
    -- 'Graphics.Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE'. An
    -- implementation that supports @independentResolve@ /must/ also support
    -- @independentResolveNone@.
    independentResolve :: Bool
  , -- | @filterMinmaxSingleComponentFormats@ is a boolean value indicating
    -- whether a minimum set of required formats support min\/max filtering.
    filterMinmaxSingleComponentFormats :: Bool
  , -- | @filterMinmaxImageComponentMapping@ is a boolean value indicating
    -- whether the implementation supports non-identity component mapping of
    -- the image when doing min\/max filtering.
    filterMinmaxImageComponentMapping :: Bool
  , -- | @maxTimelineSemaphoreValueDifference@ indicates the maximum difference
    -- allowed by the implementation between the current value of a timeline
    -- semaphore and any pending signal or wait operations.
    maxTimelineSemaphoreValueDifference :: Word64
  , -- | @framebufferIntegerColorSampleCounts@ /must/ be a valid combination of
    -- 'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits'
    -- values
    framebufferIntegerColorSampleCounts :: SampleCountFlags
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceVulkan12Properties

instance ToCStruct PhysicalDeviceVulkan12Properties where
  withCStruct x f = allocaBytesAligned 736 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVulkan12Properties{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DriverId)) (driverID)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 20 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DRIVER_NAME_SIZE CChar))) (driverName)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 276 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DRIVER_INFO_SIZE CChar))) (driverInfo)
    ContT $ pokeCStruct ((p `plusPtr` 532 :: Ptr ConformanceVersion)) (conformanceVersion) . ($ ())
    lift $ poke ((p `plusPtr` 536 :: Ptr ShaderFloatControlsIndependence)) (denormBehaviorIndependence)
    lift $ poke ((p `plusPtr` 540 :: Ptr ShaderFloatControlsIndependence)) (roundingModeIndependence)
    lift $ poke ((p `plusPtr` 544 :: Ptr Bool32)) (boolToBool32 (shaderSignedZeroInfNanPreserveFloat16))
    lift $ poke ((p `plusPtr` 548 :: Ptr Bool32)) (boolToBool32 (shaderSignedZeroInfNanPreserveFloat32))
    lift $ poke ((p `plusPtr` 552 :: Ptr Bool32)) (boolToBool32 (shaderSignedZeroInfNanPreserveFloat64))
    lift $ poke ((p `plusPtr` 556 :: Ptr Bool32)) (boolToBool32 (shaderDenormPreserveFloat16))
    lift $ poke ((p `plusPtr` 560 :: Ptr Bool32)) (boolToBool32 (shaderDenormPreserveFloat32))
    lift $ poke ((p `plusPtr` 564 :: Ptr Bool32)) (boolToBool32 (shaderDenormPreserveFloat64))
    lift $ poke ((p `plusPtr` 568 :: Ptr Bool32)) (boolToBool32 (shaderDenormFlushToZeroFloat16))
    lift $ poke ((p `plusPtr` 572 :: Ptr Bool32)) (boolToBool32 (shaderDenormFlushToZeroFloat32))
    lift $ poke ((p `plusPtr` 576 :: Ptr Bool32)) (boolToBool32 (shaderDenormFlushToZeroFloat64))
    lift $ poke ((p `plusPtr` 580 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTEFloat16))
    lift $ poke ((p `plusPtr` 584 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTEFloat32))
    lift $ poke ((p `plusPtr` 588 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTEFloat64))
    lift $ poke ((p `plusPtr` 592 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTZFloat16))
    lift $ poke ((p `plusPtr` 596 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTZFloat32))
    lift $ poke ((p `plusPtr` 600 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTZFloat64))
    lift $ poke ((p `plusPtr` 604 :: Ptr Word32)) (maxUpdateAfterBindDescriptorsInAllPools)
    lift $ poke ((p `plusPtr` 608 :: Ptr Bool32)) (boolToBool32 (shaderUniformBufferArrayNonUniformIndexingNative))
    lift $ poke ((p `plusPtr` 612 :: Ptr Bool32)) (boolToBool32 (shaderSampledImageArrayNonUniformIndexingNative))
    lift $ poke ((p `plusPtr` 616 :: Ptr Bool32)) (boolToBool32 (shaderStorageBufferArrayNonUniformIndexingNative))
    lift $ poke ((p `plusPtr` 620 :: Ptr Bool32)) (boolToBool32 (shaderStorageImageArrayNonUniformIndexingNative))
    lift $ poke ((p `plusPtr` 624 :: Ptr Bool32)) (boolToBool32 (shaderInputAttachmentArrayNonUniformIndexingNative))
    lift $ poke ((p `plusPtr` 628 :: Ptr Bool32)) (boolToBool32 (robustBufferAccessUpdateAfterBind))
    lift $ poke ((p `plusPtr` 632 :: Ptr Bool32)) (boolToBool32 (quadDivergentImplicitLod))
    lift $ poke ((p `plusPtr` 636 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindSamplers)
    lift $ poke ((p `plusPtr` 640 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindUniformBuffers)
    lift $ poke ((p `plusPtr` 644 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindStorageBuffers)
    lift $ poke ((p `plusPtr` 648 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindSampledImages)
    lift $ poke ((p `plusPtr` 652 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindStorageImages)
    lift $ poke ((p `plusPtr` 656 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindInputAttachments)
    lift $ poke ((p `plusPtr` 660 :: Ptr Word32)) (maxPerStageUpdateAfterBindResources)
    lift $ poke ((p `plusPtr` 664 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindSamplers)
    lift $ poke ((p `plusPtr` 668 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindUniformBuffers)
    lift $ poke ((p `plusPtr` 672 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindUniformBuffersDynamic)
    lift $ poke ((p `plusPtr` 676 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindStorageBuffers)
    lift $ poke ((p `plusPtr` 680 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindStorageBuffersDynamic)
    lift $ poke ((p `plusPtr` 684 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindSampledImages)
    lift $ poke ((p `plusPtr` 688 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindStorageImages)
    lift $ poke ((p `plusPtr` 692 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindInputAttachments)
    lift $ poke ((p `plusPtr` 696 :: Ptr ResolveModeFlags)) (supportedDepthResolveModes)
    lift $ poke ((p `plusPtr` 700 :: Ptr ResolveModeFlags)) (supportedStencilResolveModes)
    lift $ poke ((p `plusPtr` 704 :: Ptr Bool32)) (boolToBool32 (independentResolveNone))
    lift $ poke ((p `plusPtr` 708 :: Ptr Bool32)) (boolToBool32 (independentResolve))
    lift $ poke ((p `plusPtr` 712 :: Ptr Bool32)) (boolToBool32 (filterMinmaxSingleComponentFormats))
    lift $ poke ((p `plusPtr` 716 :: Ptr Bool32)) (boolToBool32 (filterMinmaxImageComponentMapping))
    lift $ poke ((p `plusPtr` 720 :: Ptr Word64)) (maxTimelineSemaphoreValueDifference)
    lift $ poke ((p `plusPtr` 728 :: Ptr SampleCountFlags)) (framebufferIntegerColorSampleCounts)
    lift $ f
  cStructSize = 736
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DriverId)) (zero)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 20 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DRIVER_NAME_SIZE CChar))) (mempty)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 276 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DRIVER_INFO_SIZE CChar))) (mempty)
    ContT $ pokeCStruct ((p `plusPtr` 532 :: Ptr ConformanceVersion)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 536 :: Ptr ShaderFloatControlsIndependence)) (zero)
    lift $ poke ((p `plusPtr` 540 :: Ptr ShaderFloatControlsIndependence)) (zero)
    lift $ poke ((p `plusPtr` 544 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 548 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 552 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 556 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 560 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 564 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 568 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 572 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 576 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 580 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 584 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 588 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 592 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 596 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 600 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 604 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 608 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 612 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 616 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 620 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 624 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 628 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 632 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 636 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 640 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 644 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 648 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 652 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 656 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 660 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 664 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 668 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 672 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 676 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 680 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 684 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 688 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 692 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 696 :: Ptr ResolveModeFlags)) (zero)
    lift $ poke ((p `plusPtr` 700 :: Ptr ResolveModeFlags)) (zero)
    lift $ poke ((p `plusPtr` 704 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 708 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 712 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 716 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 720 :: Ptr Word64)) (zero)
    lift $ f

instance FromCStruct PhysicalDeviceVulkan12Properties where
  peekCStruct p = do
    driverID <- peek @DriverId ((p `plusPtr` 16 :: Ptr DriverId))
    driverName <- packCString (lowerArrayPtr ((p `plusPtr` 20 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DRIVER_NAME_SIZE CChar))))
    driverInfo <- packCString (lowerArrayPtr ((p `plusPtr` 276 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DRIVER_INFO_SIZE CChar))))
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
             driverID driverName driverInfo conformanceVersion denormBehaviorIndependence roundingModeIndependence (bool32ToBool shaderSignedZeroInfNanPreserveFloat16) (bool32ToBool shaderSignedZeroInfNanPreserveFloat32) (bool32ToBool shaderSignedZeroInfNanPreserveFloat64) (bool32ToBool shaderDenormPreserveFloat16) (bool32ToBool shaderDenormPreserveFloat32) (bool32ToBool shaderDenormPreserveFloat64) (bool32ToBool shaderDenormFlushToZeroFloat16) (bool32ToBool shaderDenormFlushToZeroFloat32) (bool32ToBool shaderDenormFlushToZeroFloat64) (bool32ToBool shaderRoundingModeRTEFloat16) (bool32ToBool shaderRoundingModeRTEFloat32) (bool32ToBool shaderRoundingModeRTEFloat64) (bool32ToBool shaderRoundingModeRTZFloat16) (bool32ToBool shaderRoundingModeRTZFloat32) (bool32ToBool shaderRoundingModeRTZFloat64) maxUpdateAfterBindDescriptorsInAllPools (bool32ToBool shaderUniformBufferArrayNonUniformIndexingNative) (bool32ToBool shaderSampledImageArrayNonUniformIndexingNative) (bool32ToBool shaderStorageBufferArrayNonUniformIndexingNative) (bool32ToBool shaderStorageImageArrayNonUniformIndexingNative) (bool32ToBool shaderInputAttachmentArrayNonUniformIndexingNative) (bool32ToBool robustBufferAccessUpdateAfterBind) (bool32ToBool quadDivergentImplicitLod) maxPerStageDescriptorUpdateAfterBindSamplers maxPerStageDescriptorUpdateAfterBindUniformBuffers maxPerStageDescriptorUpdateAfterBindStorageBuffers maxPerStageDescriptorUpdateAfterBindSampledImages maxPerStageDescriptorUpdateAfterBindStorageImages maxPerStageDescriptorUpdateAfterBindInputAttachments maxPerStageUpdateAfterBindResources maxDescriptorSetUpdateAfterBindSamplers maxDescriptorSetUpdateAfterBindUniformBuffers maxDescriptorSetUpdateAfterBindUniformBuffersDynamic maxDescriptorSetUpdateAfterBindStorageBuffers maxDescriptorSetUpdateAfterBindStorageBuffersDynamic maxDescriptorSetUpdateAfterBindSampledImages maxDescriptorSetUpdateAfterBindStorageImages maxDescriptorSetUpdateAfterBindInputAttachments supportedDepthResolveModes supportedStencilResolveModes (bool32ToBool independentResolveNone) (bool32ToBool independentResolve) (bool32ToBool filterMinmaxSingleComponentFormats) (bool32ToBool filterMinmaxImageComponentMapping) maxTimelineSemaphoreValueDifference framebufferIntegerColorSampleCounts

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

