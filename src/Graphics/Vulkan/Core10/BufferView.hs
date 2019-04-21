{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.BufferView
  ( BufferView
  , BufferViewCreateFlags
  , withCStructBufferViewCreateInfo
  , fromCStructBufferViewCreateInfo
  , BufferViewCreateInfo(..)
  , createBufferView
  , destroyBufferView
  , withBufferView
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( when
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.BufferView
  ( VkBufferViewCreateFlags(..)
  , VkBufferViewCreateInfo(..)
  , VkBufferView
  , vkCreateBufferView
  , vkDestroyBufferView
  )
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , DeviceSize
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- | VkBufferView - Opaque handle to a buffer view object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet',
-- 'Graphics.Vulkan.C.Core10.BufferView.vkCreateBufferView',
-- 'Graphics.Vulkan.C.Core10.BufferView.vkDestroyBufferView'
type BufferView = VkBufferView

-- | VkBufferViewCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferViewCreateFlags' is a
-- bitmask type for setting a mask, but is currently reserved for future
-- use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferViewCreateInfo'
type BufferViewCreateFlags = VkBufferViewCreateFlags


-- | VkBufferViewCreateInfo - Structure specifying parameters of a newly
-- created buffer view
--
-- == Valid Usage
--
-- -   @offset@ /must/ be less than the size of @buffer@
--
-- -   @offset@ /must/ be a multiple of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@minTexelBufferOffsetAlignment@
--
-- -   If @range@ is not equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', @range@ /must/
--     be greater than @0@
--
-- -   If @range@ is not equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', @range@ /must/
--     be an integer multiple of the texel block size of @format@
--
-- -   If @range@ is not equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', @range@ divided
--     by the texel block size of @format@, multiplied by the number of
--     texels per texel block for that format (as defined in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-compatibility Compatible Formats>
--     table), /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxTexelBufferElements@
--
-- -   If @range@ is not equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', the sum of
--     @offset@ and @range@ /must/ be less than or equal to the size of
--     @buffer@
--
-- -   @buffer@ /must/ have been created with a @usage@ value containing at
--     least one of
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT'
--
-- -   If @buffer@ was created with @usage@ containing
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT',
--     @format@ /must/ be supported for uniform texel buffers, as specified
--     by the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT'
--     flag in
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkFormatProperties'::@bufferFeatures@
--     returned by
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties'
--
-- -   If @buffer@ was created with @usage@ containing
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT',
--     @format@ /must/ be supported for storage texel buffers, as specified
--     by the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT'
--     flag in
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkFormatProperties'::@bufferFeatures@
--     returned by
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties'
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- Unresolved directive in VkBufferViewCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkBufferViewCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferViewCreateFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.BufferView.vkCreateBufferView'
data BufferViewCreateInfo = BufferViewCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "BufferViewCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BufferViewCreateInfo" "flags"
  flags :: BufferViewCreateFlags
  , -- No documentation found for Nested "BufferViewCreateInfo" "buffer"
  buffer :: Buffer
  , -- No documentation found for Nested "BufferViewCreateInfo" "format"
  format :: Format
  , -- No documentation found for Nested "BufferViewCreateInfo" "offset"
  offset :: DeviceSize
  , -- No documentation found for Nested "BufferViewCreateInfo" "range"
  range :: DeviceSize
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkBufferViewCreateInfo' and
-- marshal a 'BufferViewCreateInfo' into it. The 'VkBufferViewCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructBufferViewCreateInfo :: BufferViewCreateInfo -> (VkBufferViewCreateInfo -> IO a) -> IO a
withCStructBufferViewCreateInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: BufferViewCreateInfo)) (\pPNext -> cont (VkBufferViewCreateInfo VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO pPNext (flags (marshalled :: BufferViewCreateInfo)) (buffer (marshalled :: BufferViewCreateInfo)) (format (marshalled :: BufferViewCreateInfo)) (offset (marshalled :: BufferViewCreateInfo)) (range (marshalled :: BufferViewCreateInfo))))

-- | A function to read a 'VkBufferViewCreateInfo' and all additional
-- structures in the pointer chain into a 'BufferViewCreateInfo'.
fromCStructBufferViewCreateInfo :: VkBufferViewCreateInfo -> IO BufferViewCreateInfo
fromCStructBufferViewCreateInfo c = BufferViewCreateInfo <$> -- Univalued Member elided
                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBufferViewCreateInfo)))
                                                         <*> pure (vkFlags (c :: VkBufferViewCreateInfo))
                                                         <*> pure (vkBuffer (c :: VkBufferViewCreateInfo))
                                                         <*> pure (vkFormat (c :: VkBufferViewCreateInfo))
                                                         <*> pure (vkOffset (c :: VkBufferViewCreateInfo))
                                                         <*> pure (vkRange (c :: VkBufferViewCreateInfo))

instance Zero BufferViewCreateInfo where
  zero = BufferViewCreateInfo Nothing
                              zero
                              zero
                              zero
                              zero
                              zero



-- | vkCreateBufferView - Create a new buffer view object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the buffer view.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core10.BufferView.VkBufferViewCreateInfo'
--     structure containing parameters to be used to create the buffer.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pView@ points to a
--     'Graphics.Vulkan.C.Core10.BufferView.VkBufferView' handle in which
--     the resulting buffer view object is returned.
--
-- = Description
--
-- Unresolved directive in vkCreateBufferView.txt -
-- include::{generated}\/validity\/protos\/vkCreateBufferView.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferView',
-- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferViewCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
createBufferView :: Device ->  BufferViewCreateInfo ->  Maybe AllocationCallbacks ->  IO (BufferView)
createBufferView = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pView' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructBufferViewCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateBufferView commandTable device' pCreateInfo' pAllocator pView' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pView')))))


-- | vkDestroyBufferView - Destroy a buffer view object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the buffer view.
--
-- -   @bufferView@ is the buffer view to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @bufferView@ /must/ have
--     completed execution
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @bufferView@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @bufferView@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- Unresolved directive in vkDestroyBufferView.txt -
-- include::{generated}\/validity\/protos\/vkDestroyBufferView.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferView',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
destroyBufferView :: Device ->  BufferView ->  Maybe AllocationCallbacks ->  IO ()
destroyBufferView = \(Device device' commandTable) -> \bufferView' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyBufferView commandTable device' bufferView' pAllocator *> (pure ()))

-- | A safe wrapper for 'createBufferView' and 'destroyBufferView' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withBufferView
  :: Device -> BufferViewCreateInfo -> Maybe (AllocationCallbacks) -> (BufferView -> IO a) -> IO a
withBufferView device bufferViewCreateInfo allocationCallbacks = bracket
  (createBufferView device bufferViewCreateInfo allocationCallbacks)
  (\o -> destroyBufferView device o allocationCallbacks)
