{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory
  ( pattern VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2
  , pattern VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT
  , pattern VK_QUEUE_PROTECTED_BIT
  , pattern VK_MEMORY_PROPERTY_PROTECTED_BIT
  , pattern VK_BUFFER_CREATE_PROTECTED_BIT
  , pattern VK_IMAGE_CREATE_PROTECTED_BIT
  , pattern VK_COMMAND_POOL_CREATE_PROTECTED_BIT
  , vkGetDeviceQueue2
  , VkProtectedSubmitInfo(..)
  , VkPhysicalDeviceProtectedMemoryFeatures(..)
  , VkPhysicalDeviceProtectedMemoryProperties(..)
  , VkDeviceQueueInfo2(..)
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Buffer
  ( VkBufferCreateFlagBits(..)
  )
import Graphics.Vulkan.Core10.CommandPool
  ( VkCommandPoolCreateFlagBits(..)
  )
import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.Device
  ( VkDeviceQueueCreateFlagBits(..)
  , VkDeviceQueueCreateFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkImageCreateFlagBits(..)
  , VkMemoryPropertyFlagBits(..)
  , VkQueueFlagBits(..)
  , VkDevice
  )
import Graphics.Vulkan.Core10.Queue
  ( VkQueue
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO"
pattern VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO = VkStructureType 1000145000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES = VkStructureType 1000145001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES = VkStructureType 1000145002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2"
pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2 = VkStructureType 1000145003
-- | @VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT@ specifies that the device queue
-- is a protected-capable queue. If the protected memory feature is not
-- enabled, the @VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT@ bit of @flags@
-- /must/ not be set.
pattern VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT :: VkDeviceQueueCreateFlagBits
pattern VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT = VkDeviceQueueCreateFlagBits 0x00000001
-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_PROTECTED_BIT"
pattern VK_QUEUE_PROTECTED_BIT :: VkQueueFlagBits
pattern VK_QUEUE_PROTECTED_BIT = VkQueueFlagBits 0x00000010
-- No documentation found for Nested "VkMemoryPropertyFlagBits" "VK_MEMORY_PROPERTY_PROTECTED_BIT"
pattern VK_MEMORY_PROPERTY_PROTECTED_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_PROTECTED_BIT = VkMemoryPropertyFlagBits 0x00000020
-- No documentation found for Nested "VkBufferCreateFlagBits" "VK_BUFFER_CREATE_PROTECTED_BIT"
pattern VK_BUFFER_CREATE_PROTECTED_BIT :: VkBufferCreateFlagBits
pattern VK_BUFFER_CREATE_PROTECTED_BIT = VkBufferCreateFlagBits 0x00000008
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_PROTECTED_BIT"
pattern VK_IMAGE_CREATE_PROTECTED_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_PROTECTED_BIT = VkImageCreateFlagBits 0x00000800
-- No documentation found for Nested "VkCommandPoolCreateFlagBits" "VK_COMMAND_POOL_CREATE_PROTECTED_BIT"
pattern VK_COMMAND_POOL_CREATE_PROTECTED_BIT :: VkCommandPoolCreateFlagBits
pattern VK_COMMAND_POOL_CREATE_PROTECTED_BIT = VkCommandPoolCreateFlagBits 0x00000004
-- | vkGetDeviceQueue2 - Get a queue handle from a device
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the queue.
--
-- -   @pQueueInfo@ points to an instance of the 'VkDeviceQueueInfo2'
--     structure, describing the parameters used to create the device
--     queue.
--
-- -   @pQueue@ is a pointer to a @VkQueue@ object that will be filled with
--     the handle for the requested queue.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pQueueInfo@ /must/ be a valid pointer to a valid
--     @VkDeviceQueueInfo2@ structure
--
-- -   @pQueue@ /must/ be a valid pointer to a @VkQueue@ handle
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkDeviceQueueInfo2', 'Graphics.Vulkan.Core10.Queue.VkQueue'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDeviceQueue2" vkGetDeviceQueue2 :: ("device" ::: VkDevice) -> ("pQueueInfo" ::: Ptr VkDeviceQueueInfo2) -> ("pQueue" ::: Ptr VkQueue) -> IO ()
-- | VkProtectedSubmitInfo - Structure indicating whether the submission is
-- protected
--
-- == Valid Usage
--
-- -   If the protected memory feature is not enabled, @protectedSubmit@
--     /must/ not be @VK_TRUE@.
--
-- -   If @protectedSubmit@ is @VK_TRUE@, then each element of the
--     @pCommandBuffers@ array /must/ be a protected command buffer.
--
-- -   If @protectedSubmit@ is @VK_FALSE@, then each element of the
--     @pCommandBuffers@ array /must/ be an unprotected command buffer.
--
-- -   If the @VkSubmitInfo@::@pNext@ chain does not include a
--     @VkProtectedSubmitInfo@ structure, then each element of the command
--     buffer of the @pCommandBuffers@ array /must/ be an unprotected
--     command buffer.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO@
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkProtectedSubmitInfo = VkProtectedSubmitInfo
  { -- No documentation found for Nested "VkProtectedSubmitInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkProtectedSubmitInfo" "pNext"
  vkPNext :: Ptr ()
  , -- | @protectedSubmit@ specifies whether the batch is protected. If
  -- @protectedSubmit@ is @VK_TRUE@, the batch is protected. If
  -- @protectedSubmit@ is @VK_FALSE@, the batch is unprotected. If the
  -- @VkSubmitInfo@::@pNext@ chain does not contain this structure, the batch
  -- is unprotected.
  vkProtectedSubmit :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkProtectedSubmitInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkProtectedSubmitInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkProtectedSubmitInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkProtectedSubmitInfo))
                *> poke (ptr `plusPtr` 16) (vkProtectedSubmit (poked :: VkProtectedSubmitInfo))
-- | VkPhysicalDeviceProtectedMemoryFeatures - Structure describing protected
-- memory features that can be supported by an implementation
--
-- = Description
--
-- If the @VkPhysicalDeviceProtectedMemoryFeatures@ structure is included
-- in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with a value indicating whether the feature is supported.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES@
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDeviceProtectedMemoryFeatures = VkPhysicalDeviceProtectedMemoryFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceProtectedMemoryFeatures" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceProtectedMemoryFeatures" "pNext"
  vkPNext :: Ptr ()
  , -- | @protectedMemory@ specifies whether protected memory is supported.
  vkProtectedMemory :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceProtectedMemoryFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceProtectedMemoryFeatures <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceProtectedMemoryFeatures))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceProtectedMemoryFeatures))
                *> poke (ptr `plusPtr` 16) (vkProtectedMemory (poked :: VkPhysicalDeviceProtectedMemoryFeatures))
-- | VkPhysicalDeviceProtectedMemoryProperties - Structure describing
-- protected memory properties that can be supported by an implementation
--
-- = Description
--
-- If the @VkPhysicalDeviceProtectedMemoryProperties@ structure is included
-- in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with a value indicating the implementation-dependent
-- behavior.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES@
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDeviceProtectedMemoryProperties = VkPhysicalDeviceProtectedMemoryProperties
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @protectedNoFault@ specifies whether the undefined behavior will not
  -- include process termination or device loss. If @protectedNoFault@ is
  -- @VK_FALSE@, undefined behavior /may/ include process termination or
  -- device loss. If @protectedNoFault@ is @VK_TRUE@, undefined behavior will
  -- not include process termination or device loss.
  vkProtectedNoFault :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceProtectedMemoryProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceProtectedMemoryProperties <$> peek (ptr `plusPtr` 0)
                                                       <*> peek (ptr `plusPtr` 8)
                                                       <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceProtectedMemoryProperties))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceProtectedMemoryProperties))
                *> poke (ptr `plusPtr` 16) (vkProtectedNoFault (poked :: VkPhysicalDeviceProtectedMemoryProperties))
-- | VkDeviceQueueInfo2 - Structure specifying the parameters used for device
-- queue creation
--
-- = Description
--
-- The queue returned by @vkGetDeviceQueue2@ /must/ have the same @flags@
-- value from this structure as that used at device creation time in a
-- @VkDeviceQueueCreateInfo@ instance. If no matching @flags@ were
-- specified at device creation time then @pQueue@ will return
-- @VK_NULL_HANDLE@.
--
-- == Valid Usage
--
-- -   @queueFamilyIndex@ /must/ be one of the queue family indices
--     specified when @device@ was created, via the
--     @VkDeviceQueueCreateInfo@ structure
--
-- -   @queueIndex@ /must/ be less than the number of queues created for
--     the specified queue family index and @VkDeviceQueueCreateFlags@
--     member @flags@ equal to this @flags@ value when @device@ was
--     created, via the @queueCount@ member of the
--     @VkDeviceQueueCreateInfo@ structure
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Device.VkDeviceQueueCreateFlagBits' values
--
-- -   @flags@ /must/ not be @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Device.VkDeviceQueueCreateFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkGetDeviceQueue2'
data VkDeviceQueueInfo2 = VkDeviceQueueInfo2
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure. The
  -- @pNext@ chain of @VkDeviceQueueInfo2@ is used to provide additional
  -- image parameters to @vkGetDeviceQueue2@.
  vkPNext :: Ptr ()
  , -- | @flags@ is a 'Graphics.Vulkan.Core10.Device.VkDeviceQueueCreateFlags'
  -- value indicating the flags used to create the device queue.
  vkFlags :: VkDeviceQueueCreateFlags
  , -- | @queueFamilyIndex@ is the index of the queue family to which the queue
  -- belongs.
  vkQueueFamilyIndex :: Word32
  , -- | @queueIndex@ is the index within this queue family of the queue to
  -- retrieve.
  vkQueueIndex :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDeviceQueueInfo2 where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDeviceQueueInfo2 <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 20)
                                <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceQueueInfo2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceQueueInfo2))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDeviceQueueInfo2))
                *> poke (ptr `plusPtr` 20) (vkQueueFamilyIndex (poked :: VkDeviceQueueInfo2))
                *> poke (ptr `plusPtr` 24) (vkQueueIndex (poked :: VkDeviceQueueInfo2))
