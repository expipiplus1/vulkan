{-# language CPP #-}
-- No documentation found for Chapter "Handles"
module Vulkan.Core10.Handles  ( Instance(..)
                              , Instance_T
                              , PhysicalDevice(..)
                              , PhysicalDevice_T
                              , Device(..)
                              , Device_T
                              , Queue(..)
                              , Queue_T
                              , CommandBuffer(..)
                              , CommandBuffer_T
                              , DeviceMemory(..)
                              , CommandPool(..)
                              , Buffer(..)
                              , BufferView(..)
                              , Image(..)
                              , ImageView(..)
                              , ShaderModule(..)
                              , Pipeline(..)
                              , PipelineLayout(..)
                              , Sampler(..)
                              , DescriptorSet(..)
                              , DescriptorSetLayout(..)
                              , DescriptorPool(..)
                              , Fence(..)
                              , Semaphore(..)
                              , Event(..)
                              , QueryPool(..)
                              , Framebuffer(..)
                              , RenderPass(..)
                              , PipelineCache(..)
                              ) where

import Foreign.Ptr (ptrToWordPtr)
import GHC.Show (showParen)
import Numeric (showHex)
import Foreign.Ptr (pattern WordPtr)
import Foreign.Storable (Storable)
import Foreign.Ptr (Ptr)
import Data.Word (Word64)
import Vulkan.Dynamic (DeviceCmds)
import Vulkan.Core10.APIConstants (HasObjectType(..))
import Vulkan.Dynamic (InstanceCmds)
import Vulkan.Core10.APIConstants (IsHandle)
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_BUFFER))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_BUFFER_VIEW))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_COMMAND_BUFFER))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_COMMAND_POOL))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DESCRIPTOR_POOL))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DESCRIPTOR_SET))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DEVICE))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DEVICE_MEMORY))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_EVENT))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_FENCE))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_FRAMEBUFFER))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_IMAGE))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_IMAGE_VIEW))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_INSTANCE))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_PHYSICAL_DEVICE))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_PIPELINE))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_PIPELINE_CACHE))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_PIPELINE_LAYOUT))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_QUERY_POOL))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_QUEUE))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_RENDER_PASS))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_SAMPLER))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_SEMAPHORE))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_SHADER_MODULE))
-- | An opaque type for representing pointers to VkInstance handles
data Instance_T
-- No documentation found for TopLevel "VkInstance"
data Instance = Instance
  { instanceHandle :: Ptr Instance_T
  , instanceCmds :: InstanceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero Instance where
  zero = Instance zero zero
instance HasObjectType Instance where
  objectTypeAndHandle (Instance (ptrToWordPtr -> WordPtr h) _) = (OBJECT_TYPE_INSTANCE, fromIntegral h)


-- | An opaque type for representing pointers to VkPhysicalDevice handles
data PhysicalDevice_T
-- No documentation found for TopLevel "VkPhysicalDevice"
data PhysicalDevice = PhysicalDevice
  { physicalDeviceHandle :: Ptr PhysicalDevice_T
  , instanceCmds :: InstanceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero PhysicalDevice where
  zero = PhysicalDevice zero zero
instance HasObjectType PhysicalDevice where
  objectTypeAndHandle (PhysicalDevice (ptrToWordPtr -> WordPtr h) _) = (OBJECT_TYPE_PHYSICAL_DEVICE, fromIntegral h)


-- | An opaque type for representing pointers to VkDevice handles
data Device_T
-- No documentation found for TopLevel "VkDevice"
data Device = Device
  { deviceHandle :: Ptr Device_T
  , deviceCmds :: DeviceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero Device where
  zero = Device zero zero
instance HasObjectType Device where
  objectTypeAndHandle (Device (ptrToWordPtr -> WordPtr h) _) = (OBJECT_TYPE_DEVICE, fromIntegral h)


-- | An opaque type for representing pointers to VkQueue handles
data Queue_T
-- No documentation found for TopLevel "VkQueue"
data Queue = Queue
  { queueHandle :: Ptr Queue_T
  , deviceCmds :: DeviceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero Queue where
  zero = Queue zero zero
instance HasObjectType Queue where
  objectTypeAndHandle (Queue (ptrToWordPtr -> WordPtr h) _) = (OBJECT_TYPE_QUEUE, fromIntegral h)


-- | An opaque type for representing pointers to VkCommandBuffer handles
data CommandBuffer_T
-- No documentation found for TopLevel "VkCommandBuffer"
data CommandBuffer = CommandBuffer
  { commandBufferHandle :: Ptr CommandBuffer_T
  , deviceCmds :: DeviceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero CommandBuffer where
  zero = CommandBuffer zero zero
instance HasObjectType CommandBuffer where
  objectTypeAndHandle (CommandBuffer (ptrToWordPtr -> WordPtr h) _) = (OBJECT_TYPE_COMMAND_BUFFER, fromIntegral h)


-- No documentation found for TopLevel "VkDeviceMemory"
newtype DeviceMemory = DeviceMemory Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType DeviceMemory where
  objectTypeAndHandle (DeviceMemory h) = (OBJECT_TYPE_DEVICE_MEMORY, h)
instance Show DeviceMemory where
  showsPrec p (DeviceMemory x) = showParen (p >= 11) (showString "DeviceMemory 0x" . showHex x)


-- No documentation found for TopLevel "VkCommandPool"
newtype CommandPool = CommandPool Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType CommandPool where
  objectTypeAndHandle (CommandPool h) = (OBJECT_TYPE_COMMAND_POOL, h)
instance Show CommandPool where
  showsPrec p (CommandPool x) = showParen (p >= 11) (showString "CommandPool 0x" . showHex x)


-- No documentation found for TopLevel "VkBuffer"
newtype Buffer = Buffer Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType Buffer where
  objectTypeAndHandle (Buffer h) = (OBJECT_TYPE_BUFFER, h)
instance Show Buffer where
  showsPrec p (Buffer x) = showParen (p >= 11) (showString "Buffer 0x" . showHex x)


-- No documentation found for TopLevel "VkBufferView"
newtype BufferView = BufferView Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType BufferView where
  objectTypeAndHandle (BufferView h) = (OBJECT_TYPE_BUFFER_VIEW, h)
instance Show BufferView where
  showsPrec p (BufferView x) = showParen (p >= 11) (showString "BufferView 0x" . showHex x)


-- No documentation found for TopLevel "VkImage"
newtype Image = Image Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType Image where
  objectTypeAndHandle (Image h) = (OBJECT_TYPE_IMAGE, h)
instance Show Image where
  showsPrec p (Image x) = showParen (p >= 11) (showString "Image 0x" . showHex x)


-- No documentation found for TopLevel "VkImageView"
newtype ImageView = ImageView Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType ImageView where
  objectTypeAndHandle (ImageView h) = (OBJECT_TYPE_IMAGE_VIEW, h)
instance Show ImageView where
  showsPrec p (ImageView x) = showParen (p >= 11) (showString "ImageView 0x" . showHex x)


-- No documentation found for TopLevel "VkShaderModule"
newtype ShaderModule = ShaderModule Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType ShaderModule where
  objectTypeAndHandle (ShaderModule h) = (OBJECT_TYPE_SHADER_MODULE, h)
instance Show ShaderModule where
  showsPrec p (ShaderModule x) = showParen (p >= 11) (showString "ShaderModule 0x" . showHex x)


-- No documentation found for TopLevel "VkPipeline"
newtype Pipeline = Pipeline Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType Pipeline where
  objectTypeAndHandle (Pipeline h) = (OBJECT_TYPE_PIPELINE, h)
instance Show Pipeline where
  showsPrec p (Pipeline x) = showParen (p >= 11) (showString "Pipeline 0x" . showHex x)


-- No documentation found for TopLevel "VkPipelineLayout"
newtype PipelineLayout = PipelineLayout Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType PipelineLayout where
  objectTypeAndHandle (PipelineLayout h) = (OBJECT_TYPE_PIPELINE_LAYOUT, h)
instance Show PipelineLayout where
  showsPrec p (PipelineLayout x) = showParen (p >= 11) (showString "PipelineLayout 0x" . showHex x)


-- No documentation found for TopLevel "VkSampler"
newtype Sampler = Sampler Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType Sampler where
  objectTypeAndHandle (Sampler h) = (OBJECT_TYPE_SAMPLER, h)
instance Show Sampler where
  showsPrec p (Sampler x) = showParen (p >= 11) (showString "Sampler 0x" . showHex x)


-- No documentation found for TopLevel "VkDescriptorSet"
newtype DescriptorSet = DescriptorSet Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType DescriptorSet where
  objectTypeAndHandle (DescriptorSet h) = (OBJECT_TYPE_DESCRIPTOR_SET, h)
instance Show DescriptorSet where
  showsPrec p (DescriptorSet x) = showParen (p >= 11) (showString "DescriptorSet 0x" . showHex x)


-- No documentation found for TopLevel "VkDescriptorSetLayout"
newtype DescriptorSetLayout = DescriptorSetLayout Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType DescriptorSetLayout where
  objectTypeAndHandle (DescriptorSetLayout h) = (OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT, h)
instance Show DescriptorSetLayout where
  showsPrec p (DescriptorSetLayout x) = showParen (p >= 11) (showString "DescriptorSetLayout 0x" . showHex x)


-- No documentation found for TopLevel "VkDescriptorPool"
newtype DescriptorPool = DescriptorPool Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType DescriptorPool where
  objectTypeAndHandle (DescriptorPool h) = (OBJECT_TYPE_DESCRIPTOR_POOL, h)
instance Show DescriptorPool where
  showsPrec p (DescriptorPool x) = showParen (p >= 11) (showString "DescriptorPool 0x" . showHex x)


-- No documentation found for TopLevel "VkFence"
newtype Fence = Fence Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType Fence where
  objectTypeAndHandle (Fence h) = (OBJECT_TYPE_FENCE, h)
instance Show Fence where
  showsPrec p (Fence x) = showParen (p >= 11) (showString "Fence 0x" . showHex x)


-- No documentation found for TopLevel "VkSemaphore"
newtype Semaphore = Semaphore Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType Semaphore where
  objectTypeAndHandle (Semaphore h) = (OBJECT_TYPE_SEMAPHORE, h)
instance Show Semaphore where
  showsPrec p (Semaphore x) = showParen (p >= 11) (showString "Semaphore 0x" . showHex x)


-- No documentation found for TopLevel "VkEvent"
newtype Event = Event Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType Event where
  objectTypeAndHandle (Event h) = (OBJECT_TYPE_EVENT, h)
instance Show Event where
  showsPrec p (Event x) = showParen (p >= 11) (showString "Event 0x" . showHex x)


-- No documentation found for TopLevel "VkQueryPool"
newtype QueryPool = QueryPool Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType QueryPool where
  objectTypeAndHandle (QueryPool h) = (OBJECT_TYPE_QUERY_POOL, h)
instance Show QueryPool where
  showsPrec p (QueryPool x) = showParen (p >= 11) (showString "QueryPool 0x" . showHex x)


-- No documentation found for TopLevel "VkFramebuffer"
newtype Framebuffer = Framebuffer Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType Framebuffer where
  objectTypeAndHandle (Framebuffer h) = (OBJECT_TYPE_FRAMEBUFFER, h)
instance Show Framebuffer where
  showsPrec p (Framebuffer x) = showParen (p >= 11) (showString "Framebuffer 0x" . showHex x)


-- No documentation found for TopLevel "VkRenderPass"
newtype RenderPass = RenderPass Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType RenderPass where
  objectTypeAndHandle (RenderPass h) = (OBJECT_TYPE_RENDER_PASS, h)
instance Show RenderPass where
  showsPrec p (RenderPass x) = showParen (p >= 11) (showString "RenderPass 0x" . showHex x)


-- No documentation found for TopLevel "VkPipelineCache"
newtype PipelineCache = PipelineCache Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType PipelineCache where
  objectTypeAndHandle (PipelineCache h) = (OBJECT_TYPE_PIPELINE_CACHE, h)
instance Show PipelineCache where
  showsPrec p (PipelineCache x) = showParen (p >= 11) (showString "PipelineCache 0x" . showHex x)

