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
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCString)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
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
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Core10.APIConstants (UUID_SIZE)
import Vulkan.Zero (Zero(..))
import Vulkan.Version (pattern MAKE_VERSION)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))

pattern API_VERSION_1_2 :: Word32
pattern API_VERSION_1_2 = MAKE_VERSION 1 2 0



-- No documentation found for TopLevel "VkPhysicalDeviceVulkan11Features"
data PhysicalDeviceVulkan11Features = PhysicalDeviceVulkan11Features
  { -- No documentation found for Nested "VkPhysicalDeviceVulkan11Features" "storageBuffer16BitAccess"
    storageBuffer16BitAccess :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Features" "uniformAndStorageBuffer16BitAccess"
    uniformAndStorageBuffer16BitAccess :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Features" "storagePushConstant16"
    storagePushConstant16 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Features" "storageInputOutput16"
    storageInputOutput16 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Features" "multiview"
    multiview :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Features" "multiviewGeometryShader"
    multiviewGeometryShader :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Features" "multiviewTessellationShader"
    multiviewTessellationShader :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Features" "variablePointersStorageBuffer"
    variablePointersStorageBuffer :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Features" "variablePointers"
    variablePointers :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Features" "protectedMemory"
    protectedMemory :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Features" "samplerYcbcrConversion"
    samplerYcbcrConversion :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Features" "shaderDrawParameters"
    shaderDrawParameters :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVulkan11Features)
#endif
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



-- No documentation found for TopLevel "VkPhysicalDeviceVulkan11Properties"
data PhysicalDeviceVulkan11Properties = PhysicalDeviceVulkan11Properties
  { -- No documentation found for Nested "VkPhysicalDeviceVulkan11Properties" "deviceUUID"
    deviceUUID :: ByteString
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Properties" "driverUUID"
    driverUUID :: ByteString
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Properties" "deviceLUID"
    deviceLUID :: ByteString
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Properties" "deviceNodeMask"
    deviceNodeMask :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Properties" "deviceLUIDValid"
    deviceLUIDValid :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Properties" "subgroupSize"
    subgroupSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Properties" "subgroupSupportedStages"
    subgroupSupportedStages :: ShaderStageFlags
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Properties" "subgroupSupportedOperations"
    subgroupSupportedOperations :: SubgroupFeatureFlags
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Properties" "subgroupQuadOperationsInAllStages"
    subgroupQuadOperationsInAllStages :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Properties" "pointClippingBehavior"
    pointClippingBehavior :: PointClippingBehavior
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Properties" "maxMultiviewViewCount"
    maxMultiviewViewCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Properties" "maxMultiviewInstanceIndex"
    maxMultiviewInstanceIndex :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Properties" "protectedNoFault"
    protectedNoFault :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Properties" "maxPerSetDescriptors"
    maxPerSetDescriptors :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan11Properties" "maxMemoryAllocationSize"
    maxMemoryAllocationSize :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVulkan11Properties)
#endif
deriving instance Show PhysicalDeviceVulkan11Properties

instance ToCStruct PhysicalDeviceVulkan11Properties where
  withCStruct x f = allocaBytesAligned 112 8 $ \p -> pokeCStruct p x (f p)
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



-- No documentation found for TopLevel "VkPhysicalDeviceVulkan12Features"
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



-- No documentation found for TopLevel "VkPhysicalDeviceVulkan12Properties"
data PhysicalDeviceVulkan12Properties = PhysicalDeviceVulkan12Properties
  { -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "driverID"
    driverID :: DriverId
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "driverName"
    driverName :: ByteString
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "driverInfo"
    driverInfo :: ByteString
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "conformanceVersion"
    conformanceVersion :: ConformanceVersion
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "denormBehaviorIndependence"
    denormBehaviorIndependence :: ShaderFloatControlsIndependence
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "roundingModeIndependence"
    roundingModeIndependence :: ShaderFloatControlsIndependence
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "shaderSignedZeroInfNanPreserveFloat16"
    shaderSignedZeroInfNanPreserveFloat16 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "shaderSignedZeroInfNanPreserveFloat32"
    shaderSignedZeroInfNanPreserveFloat32 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "shaderSignedZeroInfNanPreserveFloat64"
    shaderSignedZeroInfNanPreserveFloat64 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "shaderDenormPreserveFloat16"
    shaderDenormPreserveFloat16 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "shaderDenormPreserveFloat32"
    shaderDenormPreserveFloat32 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "shaderDenormPreserveFloat64"
    shaderDenormPreserveFloat64 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "shaderDenormFlushToZeroFloat16"
    shaderDenormFlushToZeroFloat16 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "shaderDenormFlushToZeroFloat32"
    shaderDenormFlushToZeroFloat32 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "shaderDenormFlushToZeroFloat64"
    shaderDenormFlushToZeroFloat64 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "shaderRoundingModeRTEFloat16"
    shaderRoundingModeRTEFloat16 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "shaderRoundingModeRTEFloat32"
    shaderRoundingModeRTEFloat32 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "shaderRoundingModeRTEFloat64"
    shaderRoundingModeRTEFloat64 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "shaderRoundingModeRTZFloat16"
    shaderRoundingModeRTZFloat16 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "shaderRoundingModeRTZFloat32"
    shaderRoundingModeRTZFloat32 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "shaderRoundingModeRTZFloat64"
    shaderRoundingModeRTZFloat64 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "maxUpdateAfterBindDescriptorsInAllPools"
    maxUpdateAfterBindDescriptorsInAllPools :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "shaderUniformBufferArrayNonUniformIndexingNative"
    shaderUniformBufferArrayNonUniformIndexingNative :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "shaderSampledImageArrayNonUniformIndexingNative"
    shaderSampledImageArrayNonUniformIndexingNative :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "shaderStorageBufferArrayNonUniformIndexingNative"
    shaderStorageBufferArrayNonUniformIndexingNative :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "shaderStorageImageArrayNonUniformIndexingNative"
    shaderStorageImageArrayNonUniformIndexingNative :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "shaderInputAttachmentArrayNonUniformIndexingNative"
    shaderInputAttachmentArrayNonUniformIndexingNative :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "robustBufferAccessUpdateAfterBind"
    robustBufferAccessUpdateAfterBind :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "quadDivergentImplicitLod"
    quadDivergentImplicitLod :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "maxPerStageDescriptorUpdateAfterBindSamplers"
    maxPerStageDescriptorUpdateAfterBindSamplers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "maxPerStageDescriptorUpdateAfterBindUniformBuffers"
    maxPerStageDescriptorUpdateAfterBindUniformBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "maxPerStageDescriptorUpdateAfterBindStorageBuffers"
    maxPerStageDescriptorUpdateAfterBindStorageBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "maxPerStageDescriptorUpdateAfterBindSampledImages"
    maxPerStageDescriptorUpdateAfterBindSampledImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "maxPerStageDescriptorUpdateAfterBindStorageImages"
    maxPerStageDescriptorUpdateAfterBindStorageImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "maxPerStageDescriptorUpdateAfterBindInputAttachments"
    maxPerStageDescriptorUpdateAfterBindInputAttachments :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "maxPerStageUpdateAfterBindResources"
    maxPerStageUpdateAfterBindResources :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "maxDescriptorSetUpdateAfterBindSamplers"
    maxDescriptorSetUpdateAfterBindSamplers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "maxDescriptorSetUpdateAfterBindUniformBuffers"
    maxDescriptorSetUpdateAfterBindUniformBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "maxDescriptorSetUpdateAfterBindUniformBuffersDynamic"
    maxDescriptorSetUpdateAfterBindUniformBuffersDynamic :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "maxDescriptorSetUpdateAfterBindStorageBuffers"
    maxDescriptorSetUpdateAfterBindStorageBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "maxDescriptorSetUpdateAfterBindStorageBuffersDynamic"
    maxDescriptorSetUpdateAfterBindStorageBuffersDynamic :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "maxDescriptorSetUpdateAfterBindSampledImages"
    maxDescriptorSetUpdateAfterBindSampledImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "maxDescriptorSetUpdateAfterBindStorageImages"
    maxDescriptorSetUpdateAfterBindStorageImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "maxDescriptorSetUpdateAfterBindInputAttachments"
    maxDescriptorSetUpdateAfterBindInputAttachments :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "supportedDepthResolveModes"
    supportedDepthResolveModes :: ResolveModeFlags
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "supportedStencilResolveModes"
    supportedStencilResolveModes :: ResolveModeFlags
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "independentResolveNone"
    independentResolveNone :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "independentResolve"
    independentResolve :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "filterMinmaxSingleComponentFormats"
    filterMinmaxSingleComponentFormats :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "filterMinmaxImageComponentMapping"
    filterMinmaxImageComponentMapping :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "maxTimelineSemaphoreValueDifference"
    maxTimelineSemaphoreValueDifference :: Word64
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan12Properties" "framebufferIntegerColorSampleCounts"
    framebufferIntegerColorSampleCounts :: SampleCountFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVulkan12Properties)
#endif
deriving instance Show PhysicalDeviceVulkan12Properties

instance ToCStruct PhysicalDeviceVulkan12Properties where
  withCStruct x f = allocaBytesAligned 736 8 $ \p -> pokeCStruct p x (f p)
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
             driverID driverName driverInfo conformanceVersion denormBehaviorIndependence roundingModeIndependence (bool32ToBool shaderSignedZeroInfNanPreserveFloat16) (bool32ToBool shaderSignedZeroInfNanPreserveFloat32) (bool32ToBool shaderSignedZeroInfNanPreserveFloat64) (bool32ToBool shaderDenormPreserveFloat16) (bool32ToBool shaderDenormPreserveFloat32) (bool32ToBool shaderDenormPreserveFloat64) (bool32ToBool shaderDenormFlushToZeroFloat16) (bool32ToBool shaderDenormFlushToZeroFloat32) (bool32ToBool shaderDenormFlushToZeroFloat64) (bool32ToBool shaderRoundingModeRTEFloat16) (bool32ToBool shaderRoundingModeRTEFloat32) (bool32ToBool shaderRoundingModeRTEFloat64) (bool32ToBool shaderRoundingModeRTZFloat16) (bool32ToBool shaderRoundingModeRTZFloat32) (bool32ToBool shaderRoundingModeRTZFloat64) maxUpdateAfterBindDescriptorsInAllPools (bool32ToBool shaderUniformBufferArrayNonUniformIndexingNative) (bool32ToBool shaderSampledImageArrayNonUniformIndexingNative) (bool32ToBool shaderStorageBufferArrayNonUniformIndexingNative) (bool32ToBool shaderStorageImageArrayNonUniformIndexingNative) (bool32ToBool shaderInputAttachmentArrayNonUniformIndexingNative) (bool32ToBool robustBufferAccessUpdateAfterBind) (bool32ToBool quadDivergentImplicitLod) maxPerStageDescriptorUpdateAfterBindSamplers maxPerStageDescriptorUpdateAfterBindUniformBuffers maxPerStageDescriptorUpdateAfterBindStorageBuffers maxPerStageDescriptorUpdateAfterBindSampledImages maxPerStageDescriptorUpdateAfterBindStorageImages maxPerStageDescriptorUpdateAfterBindInputAttachments maxPerStageUpdateAfterBindResources maxDescriptorSetUpdateAfterBindSamplers maxDescriptorSetUpdateAfterBindUniformBuffers maxDescriptorSetUpdateAfterBindUniformBuffersDynamic maxDescriptorSetUpdateAfterBindStorageBuffers maxDescriptorSetUpdateAfterBindStorageBuffersDynamic maxDescriptorSetUpdateAfterBindSampledImages maxDescriptorSetUpdateAfterBindStorageImages maxDescriptorSetUpdateAfterBindInputAttachments supportedDepthResolveModes supportedStencilResolveModes (bool32ToBool independentResolveNone) (bool32ToBool independentResolve) (bool32ToBool filterMinmaxSingleComponentFormats) (bool32ToBool filterMinmaxImageComponentMapping) maxTimelineSemaphoreValueDifference framebufferIntegerColorSampleCounts


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

