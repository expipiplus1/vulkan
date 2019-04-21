{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation
  ( withCStructDedicatedAllocationBufferCreateInfoNV
  , fromCStructDedicatedAllocationBufferCreateInfoNV
  , DedicatedAllocationBufferCreateInfoNV(..)
  , withCStructDedicatedAllocationImageCreateInfoNV
  , fromCStructDedicatedAllocationImageCreateInfoNV
  , DedicatedAllocationImageCreateInfoNV(..)
  , withCStructDedicatedAllocationMemoryAllocateInfoNV
  , fromCStructDedicatedAllocationMemoryAllocateInfoNV
  , DedicatedAllocationMemoryAllocateInfoNV(..)
  , pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION
  , pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation
  ( VkDedicatedAllocationBufferCreateInfoNV(..)
  , VkDedicatedAllocationImageCreateInfoNV(..)
  , VkDedicatedAllocationMemoryAllocateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  , Image
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation
  ( pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME
  , pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION
  )



-- | VkDedicatedAllocationBufferCreateInfoNV - Specify that a buffer is bound
-- to a dedicated memory resource
--
-- = Description
--
-- Unresolved directive in VkDedicatedAllocationBufferCreateInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkDedicatedAllocationBufferCreateInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data DedicatedAllocationBufferCreateInfoNV = DedicatedAllocationBufferCreateInfoNV
  { -- Univalued member elided
  -- No documentation found for Nested "DedicatedAllocationBufferCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DedicatedAllocationBufferCreateInfoNV" "dedicatedAllocation"
  dedicatedAllocation :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDedicatedAllocationBufferCreateInfoNV' and
-- marshal a 'DedicatedAllocationBufferCreateInfoNV' into it. The 'VkDedicatedAllocationBufferCreateInfoNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDedicatedAllocationBufferCreateInfoNV :: DedicatedAllocationBufferCreateInfoNV -> (VkDedicatedAllocationBufferCreateInfoNV -> IO a) -> IO a
withCStructDedicatedAllocationBufferCreateInfoNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: DedicatedAllocationBufferCreateInfoNV)) (\pPNext -> cont (VkDedicatedAllocationBufferCreateInfoNV VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV pPNext (boolToBool32 (dedicatedAllocation (marshalled :: DedicatedAllocationBufferCreateInfoNV)))))

-- | A function to read a 'VkDedicatedAllocationBufferCreateInfoNV' and all additional
-- structures in the pointer chain into a 'DedicatedAllocationBufferCreateInfoNV'.
fromCStructDedicatedAllocationBufferCreateInfoNV :: VkDedicatedAllocationBufferCreateInfoNV -> IO DedicatedAllocationBufferCreateInfoNV
fromCStructDedicatedAllocationBufferCreateInfoNV c = DedicatedAllocationBufferCreateInfoNV <$> -- Univalued Member elided
                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDedicatedAllocationBufferCreateInfoNV)))
                                                                                           <*> pure (bool32ToBool (vkDedicatedAllocation (c :: VkDedicatedAllocationBufferCreateInfoNV)))

instance Zero DedicatedAllocationBufferCreateInfoNV where
  zero = DedicatedAllocationBufferCreateInfoNV Nothing
                                               False



-- | VkDedicatedAllocationImageCreateInfoNV - Specify that an image is bound
-- to a dedicated memory resource
--
-- = Description
--
-- __Note__
--
-- Using a dedicated allocation for color and depth\/stencil attachments or
-- other large images /may/ improve performance on some devices.
--
-- == Valid Usage
--
-- -   If @dedicatedAllocation@ is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE',
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@flags@ /must/
--     not include
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_BINDING_BIT',
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT',
--     or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_ALIASED_BIT'
--
-- Unresolved directive in VkDedicatedAllocationImageCreateInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkDedicatedAllocationImageCreateInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data DedicatedAllocationImageCreateInfoNV = DedicatedAllocationImageCreateInfoNV
  { -- Univalued member elided
  -- No documentation found for Nested "DedicatedAllocationImageCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DedicatedAllocationImageCreateInfoNV" "dedicatedAllocation"
  dedicatedAllocation :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDedicatedAllocationImageCreateInfoNV' and
-- marshal a 'DedicatedAllocationImageCreateInfoNV' into it. The 'VkDedicatedAllocationImageCreateInfoNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDedicatedAllocationImageCreateInfoNV :: DedicatedAllocationImageCreateInfoNV -> (VkDedicatedAllocationImageCreateInfoNV -> IO a) -> IO a
withCStructDedicatedAllocationImageCreateInfoNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: DedicatedAllocationImageCreateInfoNV)) (\pPNext -> cont (VkDedicatedAllocationImageCreateInfoNV VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV pPNext (boolToBool32 (dedicatedAllocation (marshalled :: DedicatedAllocationImageCreateInfoNV)))))

-- | A function to read a 'VkDedicatedAllocationImageCreateInfoNV' and all additional
-- structures in the pointer chain into a 'DedicatedAllocationImageCreateInfoNV'.
fromCStructDedicatedAllocationImageCreateInfoNV :: VkDedicatedAllocationImageCreateInfoNV -> IO DedicatedAllocationImageCreateInfoNV
fromCStructDedicatedAllocationImageCreateInfoNV c = DedicatedAllocationImageCreateInfoNV <$> -- Univalued Member elided
                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDedicatedAllocationImageCreateInfoNV)))
                                                                                         <*> pure (bool32ToBool (vkDedicatedAllocation (c :: VkDedicatedAllocationImageCreateInfoNV)))

instance Zero DedicatedAllocationImageCreateInfoNV where
  zero = DedicatedAllocationImageCreateInfoNV Nothing
                                              False



-- | VkDedicatedAllocationMemoryAllocateInfoNV - Specify a dedicated memory
-- allocation resource
--
-- == Valid Usage
--
-- -   At least one of @image@ and @buffer@ /must/ be
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   If @image@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', the image
--     /must/ have been created with
--     'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationImageCreateInfoNV'::@dedicatedAllocation@
--     equal to 'Graphics.Vulkan.C.Core10.Core.VK_TRUE'
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', the buffer
--     /must/ have been created with
--     'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationBufferCreateInfoNV'::@dedicatedAllocation@
--     equal to 'Graphics.Vulkan.C.Core10.Core.VK_TRUE'
--
-- -   If @image@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo'::@allocationSize@
--     /must/ equal the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'::@size@
--     of the image
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo'::@allocationSize@
--     /must/ equal the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'::@size@
--     of the buffer
--
-- -   If @image@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' and
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo' defines a
--     memory import operation, the memory being imported /must/ also be a
--     dedicated image allocation and @image@ /must/ be identical to the
--     image associated with the imported memory.
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' and
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo' defines a
--     memory import operation, the memory being imported /must/ also be a
--     dedicated buffer allocation and @buffer@ /must/ be identical to the
--     buffer associated with the imported memory.
--
-- Unresolved directive in VkDedicatedAllocationMemoryAllocateInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkDedicatedAllocationMemoryAllocateInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data DedicatedAllocationMemoryAllocateInfoNV = DedicatedAllocationMemoryAllocateInfoNV
  { -- Univalued member elided
  -- No documentation found for Nested "DedicatedAllocationMemoryAllocateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DedicatedAllocationMemoryAllocateInfoNV" "image"
  image :: Image
  , -- No documentation found for Nested "DedicatedAllocationMemoryAllocateInfoNV" "buffer"
  buffer :: Buffer
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDedicatedAllocationMemoryAllocateInfoNV' and
-- marshal a 'DedicatedAllocationMemoryAllocateInfoNV' into it. The 'VkDedicatedAllocationMemoryAllocateInfoNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDedicatedAllocationMemoryAllocateInfoNV :: DedicatedAllocationMemoryAllocateInfoNV -> (VkDedicatedAllocationMemoryAllocateInfoNV -> IO a) -> IO a
withCStructDedicatedAllocationMemoryAllocateInfoNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: DedicatedAllocationMemoryAllocateInfoNV)) (\pPNext -> cont (VkDedicatedAllocationMemoryAllocateInfoNV VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV pPNext (image (marshalled :: DedicatedAllocationMemoryAllocateInfoNV)) (buffer (marshalled :: DedicatedAllocationMemoryAllocateInfoNV))))

-- | A function to read a 'VkDedicatedAllocationMemoryAllocateInfoNV' and all additional
-- structures in the pointer chain into a 'DedicatedAllocationMemoryAllocateInfoNV'.
fromCStructDedicatedAllocationMemoryAllocateInfoNV :: VkDedicatedAllocationMemoryAllocateInfoNV -> IO DedicatedAllocationMemoryAllocateInfoNV
fromCStructDedicatedAllocationMemoryAllocateInfoNV c = DedicatedAllocationMemoryAllocateInfoNV <$> -- Univalued Member elided
                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDedicatedAllocationMemoryAllocateInfoNV)))
                                                                                               <*> pure (vkImage (c :: VkDedicatedAllocationMemoryAllocateInfoNV))
                                                                                               <*> pure (vkBuffer (c :: VkDedicatedAllocationMemoryAllocateInfoNV))

instance Zero DedicatedAllocationMemoryAllocateInfoNV where
  zero = DedicatedAllocationMemoryAllocateInfoNV Nothing
                                                 zero
                                                 zero

