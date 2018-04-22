{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Buffer
  ( VkSharingMode(..)
  , pattern VK_SHARING_MODE_EXCLUSIVE
  , pattern VK_SHARING_MODE_CONCURRENT
  , VkBufferUsageFlagBits(..)
  , pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT
  , pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT
  , pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT
  , VkBufferCreateFlagBits(..)
  , pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT
  , pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT
  , pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT
  , vkCreateBuffer
  , vkDestroyBuffer
  , VkBufferCreateInfo(..)
  , VkBufferUsageFlags
  , VkBufferCreateFlags
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
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
  ( VkResult(..)
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


-- ** VkSharingMode

-- | VkSharingMode - Buffer and image sharing modes
--
-- = Description
--
-- -   @VK_SHARING_MODE_EXCLUSIVE@ specifies that access to any range or
--     image subresource of the object will be exclusive to a single queue
--     family at a time.
--
-- -   @VK_SHARING_MODE_CONCURRENT@ specifies that concurrent access to any
--     range or image subresource of the object from multiple queue
--     families is supported.
--
-- __Note__
--
-- @VK_SHARING_MODE_CONCURRENT@ /may/ result in lower performance access to
-- the buffer or image than @VK_SHARING_MODE_EXCLUSIVE@.
--
-- Ranges of buffers and image subresources of image objects created using
-- @VK_SHARING_MODE_EXCLUSIVE@ /must/ only be accessed by queues in the
-- queue family that has /ownership/ of the resource. Upon creation, such
-- resources are not owned by any queue family; ownership is implicitly
-- acquired upon first use within a queue. Once a resource using
-- @VK_SHARING_MODE_EXCLUSIVE@ is owned by some queue family, the
-- application /must/ perform a [queue family ownership
-- transfer](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-queue-transfers)
-- to make the memory contents of a range or image subresource accessible
-- to a different queue family.
--
-- __Note__
--
-- Images still require a [layout
-- transition](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-layouts)
-- from @VK_IMAGE_LAYOUT_UNDEFINED@ or @VK_IMAGE_LAYOUT_PREINITIALIZED@
-- before being used on the first queue.
--
-- A queue family /can/ take ownership of an image subresource or buffer
-- range of a resource created with @VK_SHARING_MODE_EXCLUSIVE@, without an
-- ownership transfer, in the same way as for a resource that was just
-- created; however, taking ownership in this way has the effect that the
-- contents of the image subresource or buffer range are undefined.
--
-- Ranges of buffers and image subresources of image objects created using
-- @VK_SHARING_MODE_CONCURRENT@ /must/ only be accessed by queues from the
-- queue families specified through the @queueFamilyIndexCount@ and
-- @pQueueFamilyIndices@ members of the corresponding create info
-- structures.
--
-- = See Also
--
-- 'VkBufferCreateInfo', 'Graphics.Vulkan.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'
newtype VkSharingMode = VkSharingMode Int32
  deriving (Eq, Ord, Storable)

instance Show VkSharingMode where
  showsPrec _ VK_SHARING_MODE_EXCLUSIVE = showString "VK_SHARING_MODE_EXCLUSIVE"
  showsPrec _ VK_SHARING_MODE_CONCURRENT = showString "VK_SHARING_MODE_CONCURRENT"
  showsPrec p (VkSharingMode x) = showParen (p >= 11) (showString "VkSharingMode " . showsPrec 11 x)

instance Read VkSharingMode where
  readPrec = parens ( choose [ ("VK_SHARING_MODE_EXCLUSIVE",  pure VK_SHARING_MODE_EXCLUSIVE)
                             , ("VK_SHARING_MODE_CONCURRENT", pure VK_SHARING_MODE_CONCURRENT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSharingMode")
                        v <- step readPrec
                        pure (VkSharingMode v)
                        )
                    )

-- No documentation found for Nested "VkSharingMode" "VK_SHARING_MODE_EXCLUSIVE"
pattern VK_SHARING_MODE_EXCLUSIVE :: VkSharingMode
pattern VK_SHARING_MODE_EXCLUSIVE = VkSharingMode 0

-- No documentation found for Nested "VkSharingMode" "VK_SHARING_MODE_CONCURRENT"
pattern VK_SHARING_MODE_CONCURRENT :: VkSharingMode
pattern VK_SHARING_MODE_CONCURRENT = VkSharingMode 1
-- ** VkBufferUsageFlagBits

-- | VkBufferUsageFlagBits - Bitmask specifying allowed usage of a buffer
--
-- = See Also
--
-- 'VkBufferUsageFlags'
newtype VkBufferUsageFlagBits = VkBufferUsageFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkBufferUsageFlagBits where
  showsPrec _ VK_BUFFER_USAGE_TRANSFER_SRC_BIT = showString "VK_BUFFER_USAGE_TRANSFER_SRC_BIT"
  showsPrec _ VK_BUFFER_USAGE_TRANSFER_DST_BIT = showString "VK_BUFFER_USAGE_TRANSFER_DST_BIT"
  showsPrec _ VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT = showString "VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT = showString "VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT = showString "VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_STORAGE_BUFFER_BIT = showString "VK_BUFFER_USAGE_STORAGE_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_INDEX_BUFFER_BIT = showString "VK_BUFFER_USAGE_INDEX_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_VERTEX_BUFFER_BIT = showString "VK_BUFFER_USAGE_VERTEX_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT = showString "VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT"
  showsPrec p (VkBufferUsageFlagBits x) = showParen (p >= 11) (showString "VkBufferUsageFlagBits " . showsPrec 11 x)

instance Read VkBufferUsageFlagBits where
  readPrec = parens ( choose [ ("VK_BUFFER_USAGE_TRANSFER_SRC_BIT",         pure VK_BUFFER_USAGE_TRANSFER_SRC_BIT)
                             , ("VK_BUFFER_USAGE_TRANSFER_DST_BIT",         pure VK_BUFFER_USAGE_TRANSFER_DST_BIT)
                             , ("VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT", pure VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT", pure VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT",       pure VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_STORAGE_BUFFER_BIT",       pure VK_BUFFER_USAGE_STORAGE_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_INDEX_BUFFER_BIT",         pure VK_BUFFER_USAGE_INDEX_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_VERTEX_BUFFER_BIT",        pure VK_BUFFER_USAGE_VERTEX_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT",      pure VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBufferUsageFlagBits")
                        v <- step readPrec
                        pure (VkBufferUsageFlagBits v)
                        )
                    )

-- | @VK_BUFFER_USAGE_TRANSFER_SRC_BIT@ specifies that the buffer /can/ be
-- used as the source of a /transfer command/ (see the definition of
-- [@VK_PIPELINE_STAGE_TRANSFER_BIT@](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-transfer)).
pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT = VkBufferUsageFlagBits 0x00000001

-- | @VK_BUFFER_USAGE_TRANSFER_DST_BIT@ specifies that the buffer /can/ be
-- used as the destination of a transfer command.
pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT = VkBufferUsageFlagBits 0x00000002

-- | @VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT@ specifies that the buffer
-- /can/ be used to create a @VkBufferView@ suitable for occupying a
-- @VkDescriptorSet@ slot of type
-- @VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER@.
pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT = VkBufferUsageFlagBits 0x00000004

-- | @VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT@ specifies that the buffer
-- /can/ be used to create a @VkBufferView@ suitable for occupying a
-- @VkDescriptorSet@ slot of type
-- @VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER@.
pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT = VkBufferUsageFlagBits 0x00000008

-- | @VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT@ specifies that the buffer /can/ be
-- used in a @VkDescriptorBufferInfo@ suitable for occupying a
-- @VkDescriptorSet@ slot either of type
-- @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER@ or
-- @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@.
pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT = VkBufferUsageFlagBits 0x00000010

-- | @VK_BUFFER_USAGE_STORAGE_BUFFER_BIT@ specifies that the buffer /can/ be
-- used in a @VkDescriptorBufferInfo@ suitable for occupying a
-- @VkDescriptorSet@ slot either of type
-- @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER@ or
-- @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@.
pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT = VkBufferUsageFlagBits 0x00000020

-- | @VK_BUFFER_USAGE_INDEX_BUFFER_BIT@ specifies that the buffer is suitable
-- for passing as the @buffer@ parameter to @vkCmdBindIndexBuffer@.
pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT = VkBufferUsageFlagBits 0x00000040

-- | @VK_BUFFER_USAGE_VERTEX_BUFFER_BIT@ specifies that the buffer is
-- suitable for passing as an element of the @pBuffers@ array to
-- @vkCmdBindVertexBuffers@.
pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT = VkBufferUsageFlagBits 0x00000080

-- | @VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT@ specifies that the buffer is
-- suitable for passing as the @buffer@ parameter to @vkCmdDrawIndirect@,
-- @vkCmdDrawIndexedIndirect@, or @vkCmdDispatchIndirect@. It is also
-- suitable for passing as the @buffer@ member of
-- @VkIndirectCommandsTokenNVX@, or @sequencesCountBuffer@ or
-- @sequencesIndexBuffer@ member of @VkCmdProcessCommandsInfoNVX@
pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT = VkBufferUsageFlagBits 0x00000100
-- ** VkBufferCreateFlagBits

-- | VkBufferCreateFlagBits - Bitmask specifying additional parameters of a
-- buffer
--
-- = Description
--
-- -   @VK_BUFFER_CREATE_SPARSE_BINDING_BIT@ specifies that the buffer will
--     be backed using sparse memory binding.
--
-- -   @VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT@ specifies that the buffer
--     /can/ be partially backed using sparse memory binding. Buffers
--     created with this flag /must/ also be created with the
--     @VK_BUFFER_CREATE_SPARSE_BINDING_BIT@ flag.
--
-- -   @VK_BUFFER_CREATE_SPARSE_ALIASED_BIT@ specifies that the buffer will
--     be backed using sparse memory binding with memory ranges that might
--     also simultaneously be backing another buffer (or another portion of
--     the same buffer). Buffers created with this flag /must/ also be
--     created with the @VK_BUFFER_CREATE_SPARSE_BINDING_BIT@ flag.
--
-- See [Sparse Resource
-- Features](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#sparsememory-sparseresourcefeatures)
-- and [Physical Device
-- Features](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features)
-- for details of the sparse memory features supported on a device.
--
-- = See Also
--
-- 'VkBufferCreateFlags'
newtype VkBufferCreateFlagBits = VkBufferCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkBufferCreateFlagBits where
  showsPrec _ VK_BUFFER_CREATE_SPARSE_BINDING_BIT = showString "VK_BUFFER_CREATE_SPARSE_BINDING_BIT"
  showsPrec _ VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT = showString "VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT"
  showsPrec _ VK_BUFFER_CREATE_SPARSE_ALIASED_BIT = showString "VK_BUFFER_CREATE_SPARSE_ALIASED_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkBufferCreateFlagBits 0x00000008) = showString "VK_BUFFER_CREATE_PROTECTED_BIT"
  showsPrec p (VkBufferCreateFlagBits x) = showParen (p >= 11) (showString "VkBufferCreateFlagBits " . showsPrec 11 x)

instance Read VkBufferCreateFlagBits where
  readPrec = parens ( choose [ ("VK_BUFFER_CREATE_SPARSE_BINDING_BIT",   pure VK_BUFFER_CREATE_SPARSE_BINDING_BIT)
                             , ("VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT", pure VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT)
                             , ("VK_BUFFER_CREATE_SPARSE_ALIASED_BIT",   pure VK_BUFFER_CREATE_SPARSE_ALIASED_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_BUFFER_CREATE_PROTECTED_BIT", pure (VkBufferCreateFlagBits 0x00000008))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBufferCreateFlagBits")
                        v <- step readPrec
                        pure (VkBufferCreateFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkBufferCreateFlagBits" "VK_BUFFER_CREATE_SPARSE_BINDING_BIT"
pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT :: VkBufferCreateFlagBits
pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT = VkBufferCreateFlagBits 0x00000001

-- No documentation found for Nested "VkBufferCreateFlagBits" "VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT"
pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT :: VkBufferCreateFlagBits
pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT = VkBufferCreateFlagBits 0x00000002

-- No documentation found for Nested "VkBufferCreateFlagBits" "VK_BUFFER_CREATE_SPARSE_ALIASED_BIT"
pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT :: VkBufferCreateFlagBits
pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT = VkBufferCreateFlagBits 0x00000004
-- | vkCreateBuffer - Create a new buffer object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the buffer object.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     @VkBufferCreateInfo@ structure containing parameters affecting
--     creation of the buffer.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pBuffer@ points to a @VkBuffer@ handle in which the resulting
--     buffer object is returned.
--
-- == Valid Usage
--
-- -   If the @flags@ member of @pCreateInfo@ includes
--     @VK_BUFFER_CREATE_SPARSE_BINDING_BIT@, creating this @VkBuffer@
--     /must/ not cause the total required sparse memory for all currently
--     valid sparse resources on the device to exceed
--     @VkPhysicalDeviceLimits@::@sparseAddressSpaceSize@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkBufferCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pBuffer@ /must/ be a valid pointer to a @VkBuffer@ handle
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
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'VkBufferCreateInfo',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateBuffer" vkCreateBuffer :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pBuffer" ::: Ptr VkBuffer) -> IO VkResult
-- | vkDestroyBuffer - Destroy a buffer object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the buffer.
--
-- -   @buffer@ is the buffer to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @buffer@, either directly or
--     via a @VkBufferView@, /must/ have completed execution
--
-- -   If @VkAllocationCallbacks@ were provided when @buffer@ was created,
--     a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @buffer@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @buffer@ /must/
--     be a valid @VkBuffer@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   If @buffer@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @buffer@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyBuffer" vkDestroyBuffer :: ("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | VkBufferCreateInfo - Structure specifying the parameters of a newly
-- created buffer object
--
-- == Valid Usage
--
-- -   @size@ /must/ be greater than @0@
--
-- -   If @sharingMode@ is @VK_SHARING_MODE_CONCURRENT@,
--     @pQueueFamilyIndices@ /must/ be a valid pointer to an array of
--     @queueFamilyIndexCount@ @uint32_t@ values
--
-- -   If @sharingMode@ is @VK_SHARING_MODE_CONCURRENT@,
--     @queueFamilyIndexCount@ /must/ be greater than @1@
--
-- -   If @sharingMode@ is @VK_SHARING_MODE_CONCURRENT@, each element of
--     @pQueueFamilyIndices@ /must/ be unique and /must/ be less than
--     @pQueueFamilyPropertyCount@ returned by either
--     'Graphics.Vulkan.Core10.DeviceInitialization.vkGetPhysicalDeviceQueueFamilyProperties'
--     or
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceQueueFamilyProperties2'
--     for the @physicalDevice@ that was used to create @device@
--
-- -   If the [sparse
--     bindings](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-sparseBinding)
--     feature is not enabled, @flags@ /must/ not contain
--     @VK_BUFFER_CREATE_SPARSE_BINDING_BIT@
--
-- -   If the [sparse buffer
--     residency](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-sparseResidencyBuffer)
--     feature is not enabled, @flags@ /must/ not contain
--     @VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If the [sparse aliased
--     residency](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-sparseResidencyAliased)
--     feature is not enabled, @flags@ /must/ not contain
--     @VK_BUFFER_CREATE_SPARSE_ALIASED_BIT@
--
-- -   If @flags@ contains @VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT@ or
--     @VK_BUFFER_CREATE_SPARSE_ALIASED_BIT@, it /must/ also contain
--     @VK_BUFFER_CREATE_SPARSE_BINDING_BIT@
--
-- -   If the @pNext@ chain contains an instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryBufferCreateInfo',
--     its @handleTypes@ member /must/ only contain bits that are also in
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalBufferProperties'::@externalMemoryProperties.pname@:compatibleHandleTypes,
--     as returned by
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.vkGetPhysicalDeviceExternalBufferProperties'
--     with @pExternalBufferInfo@->@handleType@ equal to any one of the
--     handle types specified in
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryBufferCreateInfo'::@handleTypes@
--
-- -   If the @pNext@ chain contains an instance of
--     'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationBufferCreateInfoNV',
--     and the @dedicatedAllocation@ member of the chained structure is
--     @VK_TRUE@, then @flags@ /must/ not include
--     @VK_BUFFER_CREATE_SPARSE_BINDING_BIT@,
--     @VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT@, or
--     @VK_BUFFER_CREATE_SPARSE_ALIASED_BIT@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO@
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationBufferCreateInfoNV'
--     or
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryBufferCreateInfo'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @flags@ /must/ be a valid combination of 'VkBufferCreateFlagBits'
--     values
--
-- -   @usage@ /must/ be a valid combination of 'VkBufferUsageFlagBits'
--     values
--
-- -   @usage@ /must/ not be @0@
--
-- -   @sharingMode@ /must/ be a valid 'VkSharingMode' value
--
-- = See Also
--
-- 'VkBufferCreateFlags', 'VkBufferUsageFlags', @VkDeviceSize@,
-- 'VkSharingMode', 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkCreateBuffer'
data VkBufferCreateInfo = VkBufferCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is a bitmask of 'VkBufferCreateFlagBits' specifying additional
  -- parameters of the buffer.
  vkFlags :: VkBufferCreateFlags
  , -- | @size@ is the size in bytes of the buffer to be created.
  vkSize :: VkDeviceSize
  , -- | @usage@ is a bitmask of 'VkBufferUsageFlagBits' specifying allowed
  -- usages of the buffer.
  vkUsage :: VkBufferUsageFlags
  , -- | @sharingMode@ is a 'VkSharingMode' value specifying the sharing mode of
  -- the buffer when it will be accessed by multiple queue families.
  vkSharingMode :: VkSharingMode
  , -- | @queueFamilyIndexCount@ is the number of entries in the
  -- @pQueueFamilyIndices@ array.
  vkQueueFamilyIndexCount :: Word32
  , -- | @pQueueFamilyIndices@ is a list of queue families that will access this
  -- buffer (ignored if @sharingMode@ is not @VK_SHARING_MODE_CONCURRENT@).
  vkPQueueFamilyIndices :: Ptr Word32
  }
  deriving (Eq, Show)

instance Storable VkBufferCreateInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkBufferCreateInfo <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 24)
                                <*> peek (ptr `plusPtr` 32)
                                <*> peek (ptr `plusPtr` 36)
                                <*> peek (ptr `plusPtr` 40)
                                <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkSize (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkUsage (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 36) (vkSharingMode (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkQueueFamilyIndexCount (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkPQueueFamilyIndices (poked :: VkBufferCreateInfo))
-- | VkBufferUsageFlags - Bitmask of VkBufferUsageFlagBits
--
-- = Description
--
-- @VkBufferUsageFlags@ is a bitmask type for setting a mask of zero or
-- more 'VkBufferUsageFlagBits'.
--
-- = See Also
--
-- 'VkBufferCreateInfo', 'VkBufferUsageFlagBits',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalBufferInfo'
type VkBufferUsageFlags = VkBufferUsageFlagBits
-- | VkBufferCreateFlags - Bitmask of VkBufferCreateFlagBits
--
-- = Description
--
-- @VkBufferCreateFlags@ is a bitmask type for setting a mask of zero or
-- more 'VkBufferCreateFlagBits'.
--
-- = See Also
--
-- 'VkBufferCreateFlagBits', 'VkBufferCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalBufferInfo'
type VkBufferCreateFlags = VkBufferCreateFlagBits
