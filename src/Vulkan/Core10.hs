{-# language CPP #-}
-- No documentation found for Chapter "Core10"
module Vulkan.Core10  ( pattern API_VERSION_1_0
                      , PhysicalDeviceVulkanSC10Properties(..)
                      , PhysicalDeviceVulkanSC10Features(..)
                      , StructureType(..)
                      , Result(..)
                      , module Vulkan.Core10.APIConstants
                      , module Vulkan.Core10.AllocationCallbacks
                      , module Vulkan.Core10.Buffer
                      , module Vulkan.Core10.BufferView
                      , module Vulkan.Core10.CommandBuffer
                      , module Vulkan.Core10.CommandBufferBuilding
                      , module Vulkan.Core10.CommandPool
                      , module Vulkan.Core10.DescriptorSet
                      , module Vulkan.Core10.Device
                      , module Vulkan.Core10.DeviceInitialization
                      , module Vulkan.Core10.Enums
                      , module Vulkan.Core10.Event
                      , module Vulkan.Core10.ExtensionDiscovery
                      , module Vulkan.Core10.FaultHandlingFunctionality
                      , module Vulkan.Core10.Fence
                      , module Vulkan.Core10.FuncPointers
                      , module Vulkan.Core10.FundamentalTypes
                      , module Vulkan.Core10.Handles
                      , module Vulkan.Core10.Image
                      , module Vulkan.Core10.ImageView
                      , module Vulkan.Core10.LayerDiscovery
                      , module Vulkan.Core10.Memory
                      , module Vulkan.Core10.MemoryManagement
                      , module Vulkan.Core10.OtherTypes
                      , module Vulkan.Core10.Pass
                      , module Vulkan.Core10.Pipeline
                      , module Vulkan.Core10.PipelineCache
                      , module Vulkan.Core10.PipelineCacheHeaderTheseTypesArePartOfThe
                      , module Vulkan.Core10.PipelineLayout
                      , module Vulkan.Core10.PipelineOfflineCreateInfo
                      , module Vulkan.Core10.Query
                      , module Vulkan.Core10.Queue
                      , module Vulkan.Core10.QueueSemaphore
                      , module Vulkan.Core10.Sampler
                      , module Vulkan.Core10.Shader
                      , module Vulkan.Core10.SparseResourceMemoryManagement
                      , module Vulkan.Core10.StaticMemoryFunctionality
                      ) where
import Vulkan.Core10.APIConstants
import Vulkan.Core10.AllocationCallbacks
import Vulkan.Core10.Buffer
import Vulkan.Core10.BufferView
import Vulkan.Core10.CommandBuffer
import Vulkan.Core10.CommandBufferBuilding
import Vulkan.Core10.CommandPool
import Vulkan.Core10.DescriptorSet
import Vulkan.Core10.Device
import Vulkan.Core10.DeviceInitialization
import Vulkan.Core10.Enums
import Vulkan.Core10.Event
import Vulkan.Core10.ExtensionDiscovery
import Vulkan.Core10.FaultHandlingFunctionality
import Vulkan.Core10.Fence
import Vulkan.Core10.FuncPointers
import Vulkan.Core10.FundamentalTypes
import Vulkan.Core10.Handles
import Vulkan.Core10.Image
import Vulkan.Core10.ImageView
import Vulkan.Core10.LayerDiscovery
import Vulkan.Core10.Memory
import Vulkan.Core10.MemoryManagement
import Vulkan.Core10.OtherTypes
import Vulkan.Core10.Pass
import Vulkan.Core10.Pipeline
import Vulkan.Core10.PipelineCache
import Vulkan.Core10.PipelineCacheHeaderTheseTypesArePartOfThe
import Vulkan.Core10.PipelineLayout
import Vulkan.Core10.PipelineOfflineCreateInfo
import Vulkan.Core10.Query
import Vulkan.Core10.Queue
import Vulkan.Core10.QueueSemaphore
import Vulkan.Core10.Sampler
import Vulkan.Core10.Shader
import Vulkan.Core10.SparseResourceMemoryManagement
import Vulkan.Core10.StaticMemoryFunctionality
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
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Version (pattern MAKE_API_VERSION)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_SC_1_0_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_SC_1_0_PROPERTIES))
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
pattern API_VERSION_1_0 :: Word32
pattern API_VERSION_1_0 = MAKE_API_VERSION 1 0 0


pattern API_VERSION_1_0 :: Word32
pattern API_VERSION_1_0 = MAKE_API_VERSION 1 0 0


-- No documentation found for TopLevel "VkPhysicalDeviceVulkanSC10Properties"
data PhysicalDeviceVulkanSC10Properties = PhysicalDeviceVulkanSC10Properties
  { -- No documentation found for Nested "VkPhysicalDeviceVulkanSC10Properties" "deviceNoDynamicHostAllocations"
    deviceNoDynamicHostAllocations :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanSC10Properties" "deviceDestroyFreesMemory"
    deviceDestroyFreesMemory :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanSC10Properties" "commandPoolMultipleCommandBuffersRecording"
    commandPoolMultipleCommandBuffersRecording :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanSC10Properties" "commandPoolResetCommandBuffer"
    commandPoolResetCommandBuffer :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanSC10Properties" "commandBufferSimultaneousUse"
    commandBufferSimultaneousUse :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanSC10Properties" "secondaryCommandBufferNullOrImagelessFramebuffer"
    secondaryCommandBufferNullOrImagelessFramebuffer :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanSC10Properties" "recycleDescriptorSetMemory"
    recycleDescriptorSetMemory :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanSC10Properties" "recyclePipelineMemory"
    recyclePipelineMemory :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanSC10Properties" "maxRenderPassSubpasses"
    maxRenderPassSubpasses :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanSC10Properties" "maxRenderPassDependencies"
    maxRenderPassDependencies :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanSC10Properties" "maxSubpassInputAttachments"
    maxSubpassInputAttachments :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanSC10Properties" "maxSubpassPreserveAttachments"
    maxSubpassPreserveAttachments :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanSC10Properties" "maxFramebufferAttachments"
    maxFramebufferAttachments :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanSC10Properties" "maxDescriptorSetLayoutBindings"
    maxDescriptorSetLayoutBindings :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanSC10Properties" "maxQueryFaultCount"
    maxQueryFaultCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanSC10Properties" "maxCallbackFaultCount"
    maxCallbackFaultCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanSC10Properties" "maxCommandPoolCommandBuffers"
    maxCommandPoolCommandBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanSC10Properties" "maxCommandBufferSize"
    maxCommandBufferSize :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVulkanSC10Properties)
#endif
deriving instance Show PhysicalDeviceVulkanSC10Properties

instance ToCStruct PhysicalDeviceVulkanSC10Properties where
  withCStruct x f = allocaBytes 96 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVulkanSC10Properties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_SC_1_0_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (deviceNoDynamicHostAllocations))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (deviceDestroyFreesMemory))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (commandPoolMultipleCommandBuffersRecording))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (commandPoolResetCommandBuffer))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (commandBufferSimultaneousUse))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (secondaryCommandBufferNullOrImagelessFramebuffer))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (recycleDescriptorSetMemory))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (recyclePipelineMemory))
    poke ((p `plusPtr` 48 :: Ptr Word32)) (maxRenderPassSubpasses)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (maxRenderPassDependencies)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (maxSubpassInputAttachments)
    poke ((p `plusPtr` 60 :: Ptr Word32)) (maxSubpassPreserveAttachments)
    poke ((p `plusPtr` 64 :: Ptr Word32)) (maxFramebufferAttachments)
    poke ((p `plusPtr` 68 :: Ptr Word32)) (maxDescriptorSetLayoutBindings)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (maxQueryFaultCount)
    poke ((p `plusPtr` 76 :: Ptr Word32)) (maxCallbackFaultCount)
    poke ((p `plusPtr` 80 :: Ptr Word32)) (maxCommandPoolCommandBuffers)
    poke ((p `plusPtr` 88 :: Ptr DeviceSize)) (maxCommandBufferSize)
    f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_SC_1_0_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 64 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 68 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 76 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 80 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 88 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct PhysicalDeviceVulkanSC10Properties where
  peekCStruct p = do
    deviceNoDynamicHostAllocations <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    deviceDestroyFreesMemory <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    commandPoolMultipleCommandBuffersRecording <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    commandPoolResetCommandBuffer <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    commandBufferSimultaneousUse <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    secondaryCommandBufferNullOrImagelessFramebuffer <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    recycleDescriptorSetMemory <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    recyclePipelineMemory <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    maxRenderPassSubpasses <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    maxRenderPassDependencies <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    maxSubpassInputAttachments <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    maxSubpassPreserveAttachments <- peek @Word32 ((p `plusPtr` 60 :: Ptr Word32))
    maxFramebufferAttachments <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    maxDescriptorSetLayoutBindings <- peek @Word32 ((p `plusPtr` 68 :: Ptr Word32))
    maxQueryFaultCount <- peek @Word32 ((p `plusPtr` 72 :: Ptr Word32))
    maxCallbackFaultCount <- peek @Word32 ((p `plusPtr` 76 :: Ptr Word32))
    maxCommandPoolCommandBuffers <- peek @Word32 ((p `plusPtr` 80 :: Ptr Word32))
    maxCommandBufferSize <- peek @DeviceSize ((p `plusPtr` 88 :: Ptr DeviceSize))
    pure $ PhysicalDeviceVulkanSC10Properties
             (bool32ToBool deviceNoDynamicHostAllocations)
             (bool32ToBool deviceDestroyFreesMemory)
             (bool32ToBool commandPoolMultipleCommandBuffersRecording)
             (bool32ToBool commandPoolResetCommandBuffer)
             (bool32ToBool commandBufferSimultaneousUse)
             (bool32ToBool secondaryCommandBufferNullOrImagelessFramebuffer)
             (bool32ToBool recycleDescriptorSetMemory)
             (bool32ToBool recyclePipelineMemory)
             maxRenderPassSubpasses
             maxRenderPassDependencies
             maxSubpassInputAttachments
             maxSubpassPreserveAttachments
             maxFramebufferAttachments
             maxDescriptorSetLayoutBindings
             maxQueryFaultCount
             maxCallbackFaultCount
             maxCommandPoolCommandBuffers
             maxCommandBufferSize

instance Storable PhysicalDeviceVulkanSC10Properties where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVulkanSC10Properties where
  zero = PhysicalDeviceVulkanSC10Properties
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- No documentation found for TopLevel "VkPhysicalDeviceVulkanSC10Features"
data PhysicalDeviceVulkanSC10Features = PhysicalDeviceVulkanSC10Features
  { -- No documentation found for Nested "VkPhysicalDeviceVulkanSC10Features" "shaderAtomicInstructions"
    shaderAtomicInstructions :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVulkanSC10Features)
#endif
deriving instance Show PhysicalDeviceVulkanSC10Features

instance ToCStruct PhysicalDeviceVulkanSC10Features where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVulkanSC10Features{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_SC_1_0_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderAtomicInstructions))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_SC_1_0_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceVulkanSC10Features where
  peekCStruct p = do
    shaderAtomicInstructions <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceVulkanSC10Features
             (bool32ToBool shaderAtomicInstructions)

instance Storable PhysicalDeviceVulkanSC10Features where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVulkanSC10Features where
  zero = PhysicalDeviceVulkanSC10Features
           zero

