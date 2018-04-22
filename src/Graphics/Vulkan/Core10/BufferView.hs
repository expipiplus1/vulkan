{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.BufferView
  ( VkBufferViewCreateFlags(..)
  , VkBufferView
  , vkCreateBufferView
  , vkDestroyBufferView
  , VkBufferViewCreateInfo(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  , VkDeviceSize
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkBuffer
  )


-- ** VkBufferViewCreateFlags

-- | VkBufferViewCreateFlags - Reserved for future use
--
-- = Description
--
-- @VkBufferViewCreateFlags@ is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'VkBufferViewCreateInfo'
newtype VkBufferViewCreateFlags = VkBufferViewCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkBufferViewCreateFlags where
  
  showsPrec p (VkBufferViewCreateFlags x) = showParen (p >= 11) (showString "VkBufferViewCreateFlags " . showsPrec 11 x)

instance Read VkBufferViewCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBufferViewCreateFlags")
                        v <- step readPrec
                        pure (VkBufferViewCreateFlags v)
                        )
                    )


-- | Dummy data to tag the 'Ptr' with
data VkBufferView_T
-- | VkBufferView - Opaque handle to a buffer view object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkWriteDescriptorSet',
-- 'vkCreateBufferView', 'vkDestroyBufferView'
type VkBufferView = Ptr VkBufferView_T
-- | vkCreateBufferView - Create a new buffer view object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the buffer view.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     @VkBufferViewCreateInfo@ structure containing parameters to be used
--     to create the buffer.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pView@ points to a @VkBufferView@ handle in which the resulting
--     buffer view object is returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkBufferViewCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pView@ /must/ be a valid pointer to a @VkBufferView@ handle
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'VkBufferView', 'VkBufferViewCreateInfo',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateBufferView" vkCreateBufferView :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkBufferView) -> IO VkResult
-- | vkDestroyBufferView - Destroy a buffer view object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the buffer view.
--
-- -   @bufferView@ is the buffer view to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @bufferView@ /must/ have
--     completed execution
--
-- -   If @VkAllocationCallbacks@ were provided when @bufferView@ was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @bufferView@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @bufferView@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @bufferView@
--     /must/ be a valid @VkBufferView@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   If @bufferView@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @bufferView@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'VkBufferView', 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyBufferView" vkDestroyBufferView :: ("device" ::: VkDevice) -> ("bufferView" ::: VkBufferView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | VkBufferViewCreateInfo - Structure specifying parameters of a newly
-- created buffer view
--
-- == Valid Usage
--
-- -   @offset@ /must/ be less than the size of @buffer@
--
-- -   @offset@ /must/ be a multiple of
--     @VkPhysicalDeviceLimits@::@minTexelBufferOffsetAlignment@
--
-- -   If @range@ is not equal to @VK_WHOLE_SIZE@, @range@ /must/ be
--     greater than @0@
--
-- -   If @range@ is not equal to @VK_WHOLE_SIZE@, @range@ /must/ be a
--     multiple of the element size of @format@
--
-- -   If @range@ is not equal to @VK_WHOLE_SIZE@, @range@ divided by the
--     element size of @format@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxTexelBufferElements@
--
-- -   If @range@ is not equal to @VK_WHOLE_SIZE@, the sum of @offset@ and
--     @range@ /must/ be less than or equal to the size of @buffer@
--
-- -   @buffer@ /must/ have been created with a @usage@ value containing at
--     least one of @VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT@ or
--     @VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT@
--
-- -   If @buffer@ was created with @usage@ containing
--     @VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT@, @format@ /must/ be
--     supported for uniform texel buffers, as specified by the
--     @VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT@ flag in
--     @VkFormatProperties@::@bufferFeatures@ returned by
--     @vkGetPhysicalDeviceFormatProperties@
--
-- -   If @buffer@ was created with @usage@ containing
--     @VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT@, @format@ /must/ be
--     supported for storage texel buffers, as specified by the
--     @VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT@ flag in
--     @VkFormatProperties@::@bufferFeatures@ returned by
--     @vkGetPhysicalDeviceFormatProperties@
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   @buffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @format@ /must/ be a valid 'Graphics.Vulkan.Core10.Core.VkFormat'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'VkBufferViewCreateFlags', @VkDeviceSize@,
-- 'Graphics.Vulkan.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCreateBufferView'
data VkBufferViewCreateInfo = VkBufferViewCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkBufferViewCreateFlags
  , -- | @buffer@ is a @VkBuffer@ on which the view will be created.
  vkBuffer :: VkBuffer
  , -- | @format@ is a 'Graphics.Vulkan.Core10.Core.VkFormat' describing the
  -- format of the data elements in the buffer.
  vkFormat :: VkFormat
  , -- | @offset@ is an offset in bytes from the base address of the buffer.
  -- Accesses to the buffer view from shaders use addressing that is relative
  -- to this starting offset.
  vkOffset :: VkDeviceSize
  , -- | @range@ is a size in bytes of the buffer view. If @range@ is equal to
  -- @VK_WHOLE_SIZE@, the range from @offset@ to the end of the buffer is
  -- used. If @VK_WHOLE_SIZE@ is used and the remaining size of the buffer is
  -- not a multiple of the element size of @format@, then the nearest smaller
  -- multiple is used.
  vkRange :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkBufferViewCreateInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkBufferViewCreateInfo <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 32)
                                    <*> peek (ptr `plusPtr` 40)
                                    <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkBuffer (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkFormat (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkOffset (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkRange (poked :: VkBufferViewCreateInfo))
