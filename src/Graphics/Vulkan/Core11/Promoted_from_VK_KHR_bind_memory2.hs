{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2
  ( withCStructBindBufferMemoryInfo
  , fromCStructBindBufferMemoryInfo
  , BindBufferMemoryInfo(..)
  , withCStructBindImageMemoryInfo
  , fromCStructBindImageMemoryInfo
  , BindImageMemoryInfo(..)
  , bindBufferMemory2
  , bindImageMemory2
  , pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  , pattern IMAGE_CREATE_ALIAS_BIT
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2
  ( VkBindBufferMemoryInfo(..)
  , VkBindImageMemoryInfo(..)
  , vkBindBufferMemory2
  , vkBindImageMemory2
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , DeviceSize
  )
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  , Image
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( pattern IMAGE_CREATE_ALIAS_BIT
  )



-- | VkBindBufferMemoryInfo - Structure specifying how to bind a buffer to
-- memory
--
-- == Valid Usage
--
-- -   @buffer@ /must/ not already be backed by a memory object
--
-- -   @buffer@ /must/ not have been created with any sparse memory binding
--     flags
--
-- -   @memoryOffset@ /must/ be less than the size of @memory@
--
-- -   @memory@ /must/ have been allocated using one of the memory types
--     allowed in the @memoryTypeBits@ member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetBufferMemoryRequirements'
--     with @buffer@
--
-- -   @memoryOffset@ /must/ be an integer multiple of the @alignment@
--     member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetBufferMemoryRequirements'
--     with @buffer@
--
-- -   The @size@ member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetBufferMemoryRequirements'
--     with @buffer@ /must/ be less than or equal to the size of @memory@
--     minus @memoryOffset@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindBufferMemoryDeviceGroupInfo'
--
-- -   @buffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @memory@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' handle
--
-- -   Both of @buffer@, and @memory@ /must/ have been created, allocated,
--     or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindBufferMemory2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_bind_memory2.vkBindBufferMemory2KHR'
data BindBufferMemoryInfo = BindBufferMemoryInfo
  { -- Univalued member elided
  -- No documentation found for Nested "BindBufferMemoryInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BindBufferMemoryInfo" "buffer"
  buffer :: Buffer
  , -- No documentation found for Nested "BindBufferMemoryInfo" "memory"
  memory :: DeviceMemory
  , -- No documentation found for Nested "BindBufferMemoryInfo" "memoryOffset"
  memoryOffset :: DeviceSize
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkBindBufferMemoryInfo' and
-- marshal a 'BindBufferMemoryInfo' into it. The 'VkBindBufferMemoryInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructBindBufferMemoryInfo :: BindBufferMemoryInfo -> (VkBindBufferMemoryInfo -> IO a) -> IO a
withCStructBindBufferMemoryInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: BindBufferMemoryInfo)) (\pPNext -> cont (VkBindBufferMemoryInfo VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO pPNext (buffer (marshalled :: BindBufferMemoryInfo)) (memory (marshalled :: BindBufferMemoryInfo)) (memoryOffset (marshalled :: BindBufferMemoryInfo))))

-- | A function to read a 'VkBindBufferMemoryInfo' and all additional
-- structures in the pointer chain into a 'BindBufferMemoryInfo'.
fromCStructBindBufferMemoryInfo :: VkBindBufferMemoryInfo -> IO BindBufferMemoryInfo
fromCStructBindBufferMemoryInfo c = BindBufferMemoryInfo <$> -- Univalued Member elided
                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBindBufferMemoryInfo)))
                                                         <*> pure (vkBuffer (c :: VkBindBufferMemoryInfo))
                                                         <*> pure (vkMemory (c :: VkBindBufferMemoryInfo))
                                                         <*> pure (vkMemoryOffset (c :: VkBindBufferMemoryInfo))

instance Zero BindBufferMemoryInfo where
  zero = BindBufferMemoryInfo Nothing
                              zero
                              zero
                              zero



-- | VkBindImageMemoryInfo - Structure specifying how to bind an image to
-- memory
--
-- == Valid Usage
--
-- -   @image@ /must/ not already be backed by a memory object
--
-- -   @image@ /must/ not have been created with any sparse memory binding
--     flags
--
-- -   @memoryOffset@ /must/ be less than the size of @memory@
--
-- -   @memory@ /must/ have been allocated using one of the memory types
--     allowed in the @memoryTypeBits@ member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetImageMemoryRequirements'
--     with @image@
--
-- -   @memoryOffset@ /must/ be an integer multiple of the @alignment@
--     member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetImageMemoryRequirements'
--     with @image@
--
-- -   The difference of the size of @memory@ and @memoryOffset@ /must/ be
--     greater than or equal to the @size@ member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetImageMemoryRequirements'
--     with the same @image@
--
-- -   @memory@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' handle
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo',
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkBindImageMemorySwapchainInfoKHR',
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkBindImagePlaneMemoryInfo'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @image@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle
--
-- -   Both of @image@, and @memory@ that are valid handles /must/ have
--     been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindImageMemory2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_bind_memory2.vkBindImageMemory2KHR'
data BindImageMemoryInfo = BindImageMemoryInfo
  { -- Univalued member elided
  -- No documentation found for Nested "BindImageMemoryInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BindImageMemoryInfo" "image"
  image :: Image
  , -- No documentation found for Nested "BindImageMemoryInfo" "memory"
  memory :: DeviceMemory
  , -- No documentation found for Nested "BindImageMemoryInfo" "memoryOffset"
  memoryOffset :: DeviceSize
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkBindImageMemoryInfo' and
-- marshal a 'BindImageMemoryInfo' into it. The 'VkBindImageMemoryInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructBindImageMemoryInfo :: BindImageMemoryInfo -> (VkBindImageMemoryInfo -> IO a) -> IO a
withCStructBindImageMemoryInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: BindImageMemoryInfo)) (\pPNext -> cont (VkBindImageMemoryInfo VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO pPNext (image (marshalled :: BindImageMemoryInfo)) (memory (marshalled :: BindImageMemoryInfo)) (memoryOffset (marshalled :: BindImageMemoryInfo))))

-- | A function to read a 'VkBindImageMemoryInfo' and all additional
-- structures in the pointer chain into a 'BindImageMemoryInfo'.
fromCStructBindImageMemoryInfo :: VkBindImageMemoryInfo -> IO BindImageMemoryInfo
fromCStructBindImageMemoryInfo c = BindImageMemoryInfo <$> -- Univalued Member elided
                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBindImageMemoryInfo)))
                                                       <*> pure (vkImage (c :: VkBindImageMemoryInfo))
                                                       <*> pure (vkMemory (c :: VkBindImageMemoryInfo))
                                                       <*> pure (vkMemoryOffset (c :: VkBindImageMemoryInfo))

instance Zero BindImageMemoryInfo where
  zero = BindImageMemoryInfo Nothing
                             zero
                             zero
                             zero



-- | vkBindBufferMemory2 - Bind device memory to buffer objects
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the buffers and memory.
--
-- -   @bindInfoCount@ is the number of elements in @pBindInfos@.
--
-- -   @pBindInfos@ is a pointer to an array of structures of type
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo',
--     describing buffers and memory to bind.
--
-- = Description
--
-- On some implementations, it /may/ be more efficient to batch memory
-- bindings into a single command.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
bindBufferMemory2 :: Device ->  Vector BindBufferMemoryInfo ->  IO ()
bindBufferMemory2 = \(Device device' commandTable) -> \bindInfos' -> withVec withCStructBindBufferMemoryInfo bindInfos' (\pBindInfos' -> vkBindBufferMemory2 commandTable device' (fromIntegral $ Data.Vector.length bindInfos') pBindInfos' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))


-- | vkBindImageMemory2 - Bind device memory to image objects
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the images and memory.
--
-- -   @bindInfoCount@ is the number of elements in @pBindInfos@.
--
-- -   @pBindInfos@ is a pointer to an array of structures of type
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo',
--     describing images and memory to bind.
--
-- = Description
--
-- On some implementations, it /may/ be more efficient to batch memory
-- bindings into a single command.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
bindImageMemory2 :: Device ->  Vector BindImageMemoryInfo ->  IO ()
bindImageMemory2 = \(Device device' commandTable) -> \bindInfos' -> withVec withCStructBindImageMemoryInfo bindInfos' (\pBindInfos' -> vkBindImageMemory2 commandTable device' (fromIntegral $ Data.Vector.length bindInfos') pBindInfos' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))
