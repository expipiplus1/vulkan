{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2
  , vkGetBufferMemoryRequirements2
  , vkGetImageMemoryRequirements2
  , vkGetImageSparseMemoryRequirements2
  , VkBufferMemoryRequirementsInfo2(..)
  , VkImageMemoryRequirementsInfo2(..)
  , VkImageSparseMemoryRequirementsInfo2(..)
  , VkMemoryRequirements2(..)
  , VkSparseImageMemoryRequirements2(..)
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


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkMemoryRequirements(..)
  , VkBuffer
  , VkImage
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( VkSparseImageMemoryRequirements(..)
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2"
pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2 = VkStructureType 1000146000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2"
pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2 = VkStructureType 1000146001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2"
pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2 = VkStructureType 1000146002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2"
pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2 = VkStructureType 1000146003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2"
pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2 = VkStructureType 1000146004
-- | vkGetBufferMemoryRequirements2 - Returns the memory requirements for
-- specified Vulkan object
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the buffer.
--
-- -   @pInfo@ is a pointer to an instance of the
--     @VkBufferMemoryRequirementsInfo2@ structure containing parameters
--     required for the memory requirements query.
--
-- -   @pMemoryRequirements@ points to an instance of the
--     'VkMemoryRequirements2' structure in which the memory requirements
--     of the buffer object are returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     @VkBufferMemoryRequirementsInfo2@ structure
--
-- -   @pMemoryRequirements@ /must/ be a valid pointer to a
--     @VkMemoryRequirements2@ structure
--
-- = See Also
--
-- 'VkBufferMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkMemoryRequirements2'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetBufferMemoryRequirements2" vkGetBufferMemoryRequirements2 :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ()
-- | vkGetImageMemoryRequirements2 - Returns the memory requirements for
-- specified Vulkan object
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image.
--
-- -   @pInfo@ is a pointer to an instance of the
--     @VkImageMemoryRequirementsInfo2@ structure containing parameters
--     required for the memory requirements query.
--
-- -   @pMemoryRequirements@ points to an instance of the
--     'VkMemoryRequirements2' structure in which the memory requirements
--     of the image object are returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     @VkImageMemoryRequirementsInfo2@ structure
--
-- -   @pMemoryRequirements@ /must/ be a valid pointer to a
--     @VkMemoryRequirements2@ structure
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkImageMemoryRequirementsInfo2', 'VkMemoryRequirements2'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetImageMemoryRequirements2" vkGetImageMemoryRequirements2 :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ()
-- | vkGetImageSparseMemoryRequirements2 - Query the memory requirements for
-- a sparse image
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image.
--
-- -   @pInfo@ is a pointer to an instance of the
--     @VkImageSparseMemoryRequirementsInfo2@ structure containing
--     parameters required for the memory requirements query.
--
-- -   @pSparseMemoryRequirementCount@ is a pointer to an integer related
--     to the number of sparse memory requirements available or queried, as
--     described below.
--
-- -   @pSparseMemoryRequirements@ is either @NULL@ or a pointer to an
--     array of @VkSparseImageMemoryRequirements2@ structures.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     @VkImageSparseMemoryRequirementsInfo2@ structure
--
-- -   @pSparseMemoryRequirementCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   If the value referenced by @pSparseMemoryRequirementCount@ is not
--     @0@, and @pSparseMemoryRequirements@ is not @NULL@,
--     @pSparseMemoryRequirements@ /must/ be a valid pointer to an array of
--     @pSparseMemoryRequirementCount@ @VkSparseImageMemoryRequirements2@
--     structures
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkImageSparseMemoryRequirementsInfo2',
-- 'VkSparseImageMemoryRequirements2'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetImageSparseMemoryRequirements2" vkGetImageSparseMemoryRequirements2 :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageSparseMemoryRequirementsInfo2) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements2) -> IO ()
-- | VkBufferMemoryRequirementsInfo2 - (None)
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @buffer@ /must/ be a valid @VkBuffer@ handle
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkGetBufferMemoryRequirements2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2.vkGetBufferMemoryRequirements2KHR'
data VkBufferMemoryRequirementsInfo2 = VkBufferMemoryRequirementsInfo2
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @buffer@ is the buffer to query.
  vkBuffer :: VkBuffer
  }
  deriving (Eq, Show)

instance Storable VkBufferMemoryRequirementsInfo2 where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkBufferMemoryRequirementsInfo2 <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBufferMemoryRequirementsInfo2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBufferMemoryRequirementsInfo2))
                *> poke (ptr `plusPtr` 16) (vkBuffer (poked :: VkBufferMemoryRequirementsInfo2))
-- | VkImageMemoryRequirementsInfo2 - (None)
--
-- == Valid Usage
--
-- -   If @image@ was created with a /multi-planar/ format and the
--     @VK_IMAGE_CREATE_DISJOINT_BIT@ flag, there /must/ be a
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkImagePlaneMemoryRequirementsInfo'
--     in the @pNext@ chain of the 'VkImageMemoryRequirementsInfo2'
--     structure
--
-- -   If @image@ was not created with the @VK_IMAGE_CREATE_DISJOINT_BIT@
--     flag, there /must/ not be a
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkImagePlaneMemoryRequirementsInfo'
--     in the @pNext@ chain of the 'VkImageMemoryRequirementsInfo2'
--     structure
--
-- -   If @image@ was created with a single-plane format, there /must/ not
--     be a
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkImagePlaneMemoryRequirementsInfo'
--     in the @pNext@ chain of the 'VkImageMemoryRequirementsInfo2'
--     structure
--
-- -   If @image@ was created with the
--     VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID
--     external memory handle type, then @image@ /must/ be bound to memory.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2@
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkImagePlaneMemoryRequirementsInfo'
--
-- -   @image@ /must/ be a valid @VkImage@ handle
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkGetImageMemoryRequirements2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2KHR'
data VkImageMemoryRequirementsInfo2 = VkImageMemoryRequirementsInfo2
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @image@ is the image to query.
  vkImage :: VkImage
  }
  deriving (Eq, Show)

instance Storable VkImageMemoryRequirementsInfo2 where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImageMemoryRequirementsInfo2 <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageMemoryRequirementsInfo2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageMemoryRequirementsInfo2))
                *> poke (ptr `plusPtr` 16) (vkImage (poked :: VkImageMemoryRequirementsInfo2))
-- | VkImageSparseMemoryRequirementsInfo2 - (None)
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @image@ /must/ be a valid @VkImage@ handle
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkGetImageSparseMemoryRequirements2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2.vkGetImageSparseMemoryRequirements2KHR'
data VkImageSparseMemoryRequirementsInfo2 = VkImageSparseMemoryRequirementsInfo2
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @image@ is the image to query.
  vkImage :: VkImage
  }
  deriving (Eq, Show)

instance Storable VkImageSparseMemoryRequirementsInfo2 where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImageSparseMemoryRequirementsInfo2 <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageSparseMemoryRequirementsInfo2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageSparseMemoryRequirementsInfo2))
                *> poke (ptr `plusPtr` 16) (vkImage (poked :: VkImageSparseMemoryRequirementsInfo2))
-- | VkMemoryRequirements2 - Structure specifying memory requirements
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2@
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedRequirements'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkMemoryRequirements',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkGetBufferMemoryRequirements2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2.vkGetBufferMemoryRequirements2KHR',
-- 'vkGetImageMemoryRequirements2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2KHR'
data VkMemoryRequirements2 = VkMemoryRequirements2
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @memoryRequirements@ is a structure of type
  -- 'Graphics.Vulkan.Core10.MemoryManagement.VkMemoryRequirements'
  -- describing the memory requirements of the resource.
  vkMemoryRequirements :: VkMemoryRequirements
  }
  deriving (Eq, Show)

instance Storable VkMemoryRequirements2 where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkMemoryRequirements2 <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryRequirements2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryRequirements2))
                *> poke (ptr `plusPtr` 16) (vkMemoryRequirements (poked :: VkMemoryRequirements2))
-- | VkSparseImageMemoryRequirements2 - (None)
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2@
--
-- -   @pNext@ /must/ be @NULL@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryRequirements',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkGetImageSparseMemoryRequirements2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2.vkGetImageSparseMemoryRequirements2KHR'
data VkSparseImageMemoryRequirements2 = VkSparseImageMemoryRequirements2
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @memoryRequirements@ is a structure of type
  -- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryRequirements'
  -- describing the memory requirements of the sparse image.
  vkMemoryRequirements :: VkSparseImageMemoryRequirements
  }
  deriving (Eq, Show)

instance Storable VkSparseImageMemoryRequirements2 where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkSparseImageMemoryRequirements2 <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSparseImageMemoryRequirements2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSparseImageMemoryRequirements2))
                *> poke (ptr `plusPtr` 16) (vkMemoryRequirements (poked :: VkSparseImageMemoryRequirements2))
