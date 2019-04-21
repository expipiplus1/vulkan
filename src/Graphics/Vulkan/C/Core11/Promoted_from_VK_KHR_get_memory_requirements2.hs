{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( VkBufferMemoryRequirementsInfo2(..)
  , VkImageMemoryRequirementsInfo2(..)
  , VkImageSparseMemoryRequirementsInfo2(..)
  , VkMemoryRequirements2(..)
  , VkMemoryRequirements2KHR
  , pattern VkMemoryRequirements2KHR
  , VkSparseImageMemoryRequirements2(..)
  , FN_vkGetBufferMemoryRequirements2
  , PFN_vkGetBufferMemoryRequirements2
  , vkGetBufferMemoryRequirements2
  , FN_vkGetImageMemoryRequirements2
  , PFN_vkGetImageMemoryRequirements2
  , vkGetImageMemoryRequirements2
  , FN_vkGetImageSparseMemoryRequirements2
  , PFN_vkGetImageSparseMemoryRequirements2
  , vkGetImageSparseMemoryRequirements2
  , pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkMemoryRequirements(..)
  , VkBuffer
  , VkImage
  )
import Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkSparseImageMemoryRequirements(..)
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkBufferMemoryRequirementsInfo2 - (None)
--
-- = Description
--
-- Unresolved directive in VkBufferMemoryRequirementsInfo2.txt -
-- include::{generated}\/validity\/structs\/VkBufferMemoryRequirementsInfo2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetBufferMemoryRequirements2'
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

instance Zero VkBufferMemoryRequirementsInfo2 where
  zero = VkBufferMemoryRequirementsInfo2 VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2
                                         zero
                                         zero

-- | VkImageMemoryRequirementsInfo2 - (None)
--
-- == Valid Usage
--
-- -   If @image@ was created with a /multi-planar/ format and the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_CREATE_DISJOINT_BIT'
--     flag, there /must/ be a
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkImagePlaneMemoryRequirementsInfo'
--     in the @pNext@ chain of the 'VkImageMemoryRequirementsInfo2'
--     structure
--
-- -   If @image@ was created with
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_CREATE_DISJOINT_BIT'
--     and with
--     'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--     then there /must/ be a
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkImagePlaneMemoryRequirementsInfo'
--     in the @pNext@ chain of the 'VkImageMemoryRequirementsInfo2'
--     structure
--
-- -   If @image@ was not created with the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_CREATE_DISJOINT_BIT'
--     flag, there /must/ not be a
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkImagePlaneMemoryRequirementsInfo'
--     in the @pNext@ chain of the 'VkImageMemoryRequirementsInfo2'
--     structure
--
-- -   If @image@ was created with a single-plane format and with any
--     @tiling@ other than
--     'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--     then there /must/ not be a
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkImagePlaneMemoryRequirementsInfo'
--     in the @pNext@ chain of the 'VkImageMemoryRequirementsInfo2'
--     structure
--
-- -   If @image@ was created with the
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID'
--     external memory handle type, then @image@ /must/ be bound to memory.
--
-- Unresolved directive in VkImageMemoryRequirementsInfo2.txt -
-- include::{generated}\/validity\/structs\/VkImageMemoryRequirementsInfo2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetImageMemoryRequirements2'
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

instance Zero VkImageMemoryRequirementsInfo2 where
  zero = VkImageMemoryRequirementsInfo2 VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2
                                        zero
                                        zero

-- | VkImageSparseMemoryRequirementsInfo2 - (None)
--
-- = Description
--
-- Unresolved directive in VkImageSparseMemoryRequirementsInfo2.txt -
-- include::{generated}\/validity\/structs\/VkImageSparseMemoryRequirementsInfo2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetImageSparseMemoryRequirements2'
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

instance Zero VkImageSparseMemoryRequirementsInfo2 where
  zero = VkImageSparseMemoryRequirementsInfo2 VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2
                                              zero
                                              zero

-- | VkMemoryRequirements2 - Structure specifying memory requirements
--
-- = Description
--
-- Unresolved directive in VkMemoryRequirements2.txt -
-- include::{generated}\/validity\/structs\/VkMemoryRequirements2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetBufferMemoryRequirements2', 'vkGetImageMemoryRequirements2'
data VkMemoryRequirements2 = VkMemoryRequirements2
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @memoryRequirements@ is a structure of type
  -- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
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

instance Zero VkMemoryRequirements2 where
  zero = VkMemoryRequirements2 VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2
                               zero
                               zero

-- No documentation found for TopLevel "VkMemoryRequirements2KHR"
type VkMemoryRequirements2KHR = VkMemoryRequirements2


-- No documentation found for TopLevel "VkMemoryRequirements2KHR"
pattern VkMemoryRequirements2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("memoryRequirements" ::: VkMemoryRequirements) -> VkMemoryRequirements2KHR
pattern VkMemoryRequirements2KHR vkSType vkPNext vkMemoryRequirements = VkMemoryRequirements2 vkSType vkPNext vkMemoryRequirements

-- | VkSparseImageMemoryRequirements2 - (None)
--
-- = Description
--
-- Unresolved directive in VkSparseImageMemoryRequirements2.txt -
-- include::{generated}\/validity\/structs\/VkSparseImageMemoryRequirements2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryRequirements',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetImageSparseMemoryRequirements2'
data VkSparseImageMemoryRequirements2 = VkSparseImageMemoryRequirements2
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @memoryRequirements@ is a structure of type
  -- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryRequirements'
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

instance Zero VkSparseImageMemoryRequirements2 where
  zero = VkSparseImageMemoryRequirements2 VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2
                                          zero
                                          zero

-- | vkGetBufferMemoryRequirements2 - Returns the memory requirements for
-- specified Vulkan object
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the buffer.
--
-- -   @pInfo@ is a pointer to an instance of the
--     'VkBufferMemoryRequirementsInfo2' structure containing parameters
--     required for the memory requirements query.
--
-- -   @pMemoryRequirements@ points to an instance of the
--     'VkMemoryRequirements2' structure in which the memory requirements
--     of the buffer object are returned.
--
-- = Description
--
-- Unresolved directive in vkGetBufferMemoryRequirements2.txt -
-- include::{generated}\/validity\/protos\/vkGetBufferMemoryRequirements2.txt[]
--
-- = See Also
--
-- 'VkBufferMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'VkMemoryRequirements2'
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetBufferMemoryRequirements2" vkGetBufferMemoryRequirements2 :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ()
#else
vkGetBufferMemoryRequirements2 :: DeviceCmds -> ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ()
vkGetBufferMemoryRequirements2 deviceCmds = mkVkGetBufferMemoryRequirements2 (pVkGetBufferMemoryRequirements2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetBufferMemoryRequirements2
  :: FunPtr (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ()) -> (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ())
#endif

type FN_vkGetBufferMemoryRequirements2 = ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ()
type PFN_vkGetBufferMemoryRequirements2 = FunPtr FN_vkGetBufferMemoryRequirements2

-- | vkGetImageMemoryRequirements2 - Returns the memory requirements for
-- specified Vulkan object
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image.
--
-- -   @pInfo@ is a pointer to an instance of the
--     'VkImageMemoryRequirementsInfo2' structure containing parameters
--     required for the memory requirements query.
--
-- -   @pMemoryRequirements@ points to an instance of the
--     'VkMemoryRequirements2' structure in which the memory requirements
--     of the image object are returned.
--
-- = Description
--
-- Unresolved directive in vkGetImageMemoryRequirements2.txt -
-- include::{generated}\/validity\/protos\/vkGetImageMemoryRequirements2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'VkImageMemoryRequirementsInfo2', 'VkMemoryRequirements2'
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetImageMemoryRequirements2" vkGetImageMemoryRequirements2 :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ()
#else
vkGetImageMemoryRequirements2 :: DeviceCmds -> ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ()
vkGetImageMemoryRequirements2 deviceCmds = mkVkGetImageMemoryRequirements2 (pVkGetImageMemoryRequirements2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageMemoryRequirements2
  :: FunPtr (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ()) -> (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ())
#endif

type FN_vkGetImageMemoryRequirements2 = ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ()
type PFN_vkGetImageMemoryRequirements2 = FunPtr FN_vkGetImageMemoryRequirements2

-- | vkGetImageSparseMemoryRequirements2 - Query the memory requirements for
-- a sparse image
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image.
--
-- -   @pInfo@ is a pointer to an instance of the
--     'VkImageSparseMemoryRequirementsInfo2' structure containing
--     parameters required for the memory requirements query.
--
-- -   @pSparseMemoryRequirementCount@ is a pointer to an integer related
--     to the number of sparse memory requirements available or queried, as
--     described below.
--
-- -   @pSparseMemoryRequirements@ is either @NULL@ or a pointer to an
--     array of 'VkSparseImageMemoryRequirements2' structures.
--
-- = Description
--
-- Unresolved directive in vkGetImageSparseMemoryRequirements2.txt -
-- include::{generated}\/validity\/protos\/vkGetImageSparseMemoryRequirements2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'VkImageSparseMemoryRequirementsInfo2',
-- 'VkSparseImageMemoryRequirements2'
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetImageSparseMemoryRequirements2" vkGetImageSparseMemoryRequirements2 :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageSparseMemoryRequirementsInfo2) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements2) -> IO ()
#else
vkGetImageSparseMemoryRequirements2 :: DeviceCmds -> ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageSparseMemoryRequirementsInfo2) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements2) -> IO ()
vkGetImageSparseMemoryRequirements2 deviceCmds = mkVkGetImageSparseMemoryRequirements2 (pVkGetImageSparseMemoryRequirements2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageSparseMemoryRequirements2
  :: FunPtr (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageSparseMemoryRequirementsInfo2) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements2) -> IO ()) -> (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageSparseMemoryRequirementsInfo2) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements2) -> IO ())
#endif

type FN_vkGetImageSparseMemoryRequirements2 = ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageSparseMemoryRequirementsInfo2) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements2) -> IO ()
type PFN_vkGetImageSparseMemoryRequirements2 = FunPtr FN_vkGetImageSparseMemoryRequirements2

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
