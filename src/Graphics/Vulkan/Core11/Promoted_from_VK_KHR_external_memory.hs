{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory
  ( withCStructExportMemoryAllocateInfo
  , fromCStructExportMemoryAllocateInfo
  , ExportMemoryAllocateInfo(..)
  , withCStructExternalMemoryBufferCreateInfo
  , fromCStructExternalMemoryBufferCreateInfo
  , ExternalMemoryBufferCreateInfo(..)
  , withCStructExternalMemoryImageCreateInfo
  , fromCStructExternalMemoryImageCreateInfo
  , ExternalMemoryImageCreateInfo(..)
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO
  , pattern VK_ERROR_INVALID_EXTERNAL_HANDLE
  , pattern VK_QUEUE_FAMILY_EXTERNAL
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
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory
  ( VkExportMemoryAllocateInfo(..)
  , VkExternalMemoryBufferCreateInfo(..)
  , VkExternalMemoryImageCreateInfo(..)
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( ExternalMemoryHandleTypeFlags
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory
  ( pattern VK_ERROR_INVALID_EXTERNAL_HANDLE
  , pattern VK_QUEUE_FAMILY_EXTERNAL
  )



-- | VkExportMemoryAllocateInfo - Specify exportable handle types for a
-- device memory object
--
-- == Valid Usage
--
-- -   The bits in @handleTypes@ /must/ be supported and compatible, as
--     reported by
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalImageFormatProperties'
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalBufferProperties'.
--
-- Unresolved directive in VkExportMemoryAllocateInfo.txt -
-- include::{generated}\/validity\/structs\/VkExportMemoryAllocateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data ExportMemoryAllocateInfo = ExportMemoryAllocateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "ExportMemoryAllocateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportMemoryAllocateInfo" "handleTypes"
  handleTypes :: ExternalMemoryHandleTypeFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkExportMemoryAllocateInfo' and
-- marshal a 'ExportMemoryAllocateInfo' into it. The 'VkExportMemoryAllocateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructExportMemoryAllocateInfo :: ExportMemoryAllocateInfo -> (VkExportMemoryAllocateInfo -> IO a) -> IO a
withCStructExportMemoryAllocateInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ExportMemoryAllocateInfo)) (\pPNext -> cont (VkExportMemoryAllocateInfo VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO pPNext (handleTypes (marshalled :: ExportMemoryAllocateInfo))))

-- | A function to read a 'VkExportMemoryAllocateInfo' and all additional
-- structures in the pointer chain into a 'ExportMemoryAllocateInfo'.
fromCStructExportMemoryAllocateInfo :: VkExportMemoryAllocateInfo -> IO ExportMemoryAllocateInfo
fromCStructExportMemoryAllocateInfo c = ExportMemoryAllocateInfo <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExportMemoryAllocateInfo)))
                                                                 <*> pure (vkHandleTypes (c :: VkExportMemoryAllocateInfo))

instance Zero ExportMemoryAllocateInfo where
  zero = ExportMemoryAllocateInfo Nothing
                                  zero



-- | VkExternalMemoryBufferCreateInfo - Specify that a buffer may be backed
-- by external memory
--
-- = Description
--
-- Unresolved directive in VkExternalMemoryBufferCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkExternalMemoryBufferCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data ExternalMemoryBufferCreateInfo = ExternalMemoryBufferCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "ExternalMemoryBufferCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalMemoryBufferCreateInfo" "handleTypes"
  handleTypes :: ExternalMemoryHandleTypeFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkExternalMemoryBufferCreateInfo' and
-- marshal a 'ExternalMemoryBufferCreateInfo' into it. The 'VkExternalMemoryBufferCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructExternalMemoryBufferCreateInfo :: ExternalMemoryBufferCreateInfo -> (VkExternalMemoryBufferCreateInfo -> IO a) -> IO a
withCStructExternalMemoryBufferCreateInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ExternalMemoryBufferCreateInfo)) (\pPNext -> cont (VkExternalMemoryBufferCreateInfo VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO pPNext (handleTypes (marshalled :: ExternalMemoryBufferCreateInfo))))

-- | A function to read a 'VkExternalMemoryBufferCreateInfo' and all additional
-- structures in the pointer chain into a 'ExternalMemoryBufferCreateInfo'.
fromCStructExternalMemoryBufferCreateInfo :: VkExternalMemoryBufferCreateInfo -> IO ExternalMemoryBufferCreateInfo
fromCStructExternalMemoryBufferCreateInfo c = ExternalMemoryBufferCreateInfo <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExternalMemoryBufferCreateInfo)))
                                                                             <*> pure (vkHandleTypes (c :: VkExternalMemoryBufferCreateInfo))

instance Zero ExternalMemoryBufferCreateInfo where
  zero = ExternalMemoryBufferCreateInfo Nothing
                                        zero



-- | VkExternalMemoryImageCreateInfo - Specify that an image may be backed by
-- external memory
--
-- = Description
--
-- Unresolved directive in VkExternalMemoryImageCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkExternalMemoryImageCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data ExternalMemoryImageCreateInfo = ExternalMemoryImageCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "ExternalMemoryImageCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalMemoryImageCreateInfo" "handleTypes"
  handleTypes :: ExternalMemoryHandleTypeFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkExternalMemoryImageCreateInfo' and
-- marshal a 'ExternalMemoryImageCreateInfo' into it. The 'VkExternalMemoryImageCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructExternalMemoryImageCreateInfo :: ExternalMemoryImageCreateInfo -> (VkExternalMemoryImageCreateInfo -> IO a) -> IO a
withCStructExternalMemoryImageCreateInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ExternalMemoryImageCreateInfo)) (\pPNext -> cont (VkExternalMemoryImageCreateInfo VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO pPNext (handleTypes (marshalled :: ExternalMemoryImageCreateInfo))))

-- | A function to read a 'VkExternalMemoryImageCreateInfo' and all additional
-- structures in the pointer chain into a 'ExternalMemoryImageCreateInfo'.
fromCStructExternalMemoryImageCreateInfo :: VkExternalMemoryImageCreateInfo -> IO ExternalMemoryImageCreateInfo
fromCStructExternalMemoryImageCreateInfo c = ExternalMemoryImageCreateInfo <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExternalMemoryImageCreateInfo)))
                                                                           <*> pure (vkHandleTypes (c :: VkExternalMemoryImageCreateInfo))

instance Zero ExternalMemoryImageCreateInfo where
  zero = ExternalMemoryImageCreateInfo Nothing
                                       zero

