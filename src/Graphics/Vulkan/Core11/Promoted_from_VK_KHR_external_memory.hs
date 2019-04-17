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


-- No documentation found for TopLevel "ExportMemoryAllocateInfo"
data ExportMemoryAllocateInfo = ExportMemoryAllocateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "ExportMemoryAllocateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportMemoryAllocateInfo" "handleTypes"
  vkHandleTypes :: ExternalMemoryHandleTypeFlags
  }
  deriving (Show, Eq)
withCStructExportMemoryAllocateInfo :: ExportMemoryAllocateInfo -> (VkExportMemoryAllocateInfo -> IO a) -> IO a
withCStructExportMemoryAllocateInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: ExportMemoryAllocateInfo)) (\pPNext -> cont (VkExportMemoryAllocateInfo VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO pPNext (vkHandleTypes (from :: ExportMemoryAllocateInfo))))
fromCStructExportMemoryAllocateInfo :: VkExportMemoryAllocateInfo -> IO ExportMemoryAllocateInfo
fromCStructExportMemoryAllocateInfo c = ExportMemoryAllocateInfo <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExportMemoryAllocateInfo)))
                                                                 <*> pure (vkHandleTypes (c :: VkExportMemoryAllocateInfo))
instance Zero ExportMemoryAllocateInfo where
  zero = ExportMemoryAllocateInfo Nothing
                                  zero
-- No documentation found for TopLevel "ExternalMemoryBufferCreateInfo"
data ExternalMemoryBufferCreateInfo = ExternalMemoryBufferCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "ExternalMemoryBufferCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalMemoryBufferCreateInfo" "handleTypes"
  vkHandleTypes :: ExternalMemoryHandleTypeFlags
  }
  deriving (Show, Eq)
withCStructExternalMemoryBufferCreateInfo :: ExternalMemoryBufferCreateInfo -> (VkExternalMemoryBufferCreateInfo -> IO a) -> IO a
withCStructExternalMemoryBufferCreateInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: ExternalMemoryBufferCreateInfo)) (\pPNext -> cont (VkExternalMemoryBufferCreateInfo VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO pPNext (vkHandleTypes (from :: ExternalMemoryBufferCreateInfo))))
fromCStructExternalMemoryBufferCreateInfo :: VkExternalMemoryBufferCreateInfo -> IO ExternalMemoryBufferCreateInfo
fromCStructExternalMemoryBufferCreateInfo c = ExternalMemoryBufferCreateInfo <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExternalMemoryBufferCreateInfo)))
                                                                             <*> pure (vkHandleTypes (c :: VkExternalMemoryBufferCreateInfo))
instance Zero ExternalMemoryBufferCreateInfo where
  zero = ExternalMemoryBufferCreateInfo Nothing
                                        zero
-- No documentation found for TopLevel "ExternalMemoryImageCreateInfo"
data ExternalMemoryImageCreateInfo = ExternalMemoryImageCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "ExternalMemoryImageCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalMemoryImageCreateInfo" "handleTypes"
  vkHandleTypes :: ExternalMemoryHandleTypeFlags
  }
  deriving (Show, Eq)
withCStructExternalMemoryImageCreateInfo :: ExternalMemoryImageCreateInfo -> (VkExternalMemoryImageCreateInfo -> IO a) -> IO a
withCStructExternalMemoryImageCreateInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: ExternalMemoryImageCreateInfo)) (\pPNext -> cont (VkExternalMemoryImageCreateInfo VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO pPNext (vkHandleTypes (from :: ExternalMemoryImageCreateInfo))))
fromCStructExternalMemoryImageCreateInfo :: VkExternalMemoryImageCreateInfo -> IO ExternalMemoryImageCreateInfo
fromCStructExternalMemoryImageCreateInfo c = ExternalMemoryImageCreateInfo <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExternalMemoryImageCreateInfo)))
                                                                           <*> pure (vkHandleTypes (c :: VkExternalMemoryImageCreateInfo))
instance Zero ExternalMemoryImageCreateInfo where
  zero = ExternalMemoryImageCreateInfo Nothing
                                       zero
