{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_external_memory
  ( withCStructExportMemoryAllocateInfoNV
  , fromCStructExportMemoryAllocateInfoNV
  , ExportMemoryAllocateInfoNV(..)
  , withCStructExternalMemoryImageCreateInfoNV
  , fromCStructExternalMemoryImageCreateInfoNV
  , ExternalMemoryImageCreateInfoNV(..)
  , pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION
  , pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Extensions.VK_NV_external_memory
  ( VkExportMemoryAllocateInfoNV(..)
  , VkExternalMemoryImageCreateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV
  )
import Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities
  ( ExternalMemoryHandleTypeFlagsNV
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory
  ( pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME
  , pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION
  )


-- No documentation found for TopLevel "ExportMemoryAllocateInfoNV"
data ExportMemoryAllocateInfoNV = ExportMemoryAllocateInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "ExportMemoryAllocateInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportMemoryAllocateInfoNV" "handleTypes"
  vkHandleTypes :: ExternalMemoryHandleTypeFlagsNV
  }
  deriving (Show, Eq)
withCStructExportMemoryAllocateInfoNV :: ExportMemoryAllocateInfoNV -> (VkExportMemoryAllocateInfoNV -> IO a) -> IO a
withCStructExportMemoryAllocateInfoNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: ExportMemoryAllocateInfoNV)) (\pPNext -> cont (VkExportMemoryAllocateInfoNV VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV pPNext (vkHandleTypes (from :: ExportMemoryAllocateInfoNV))))
fromCStructExportMemoryAllocateInfoNV :: VkExportMemoryAllocateInfoNV -> IO ExportMemoryAllocateInfoNV
fromCStructExportMemoryAllocateInfoNV c = ExportMemoryAllocateInfoNV <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExportMemoryAllocateInfoNV)))
                                                                     <*> pure (vkHandleTypes (c :: VkExportMemoryAllocateInfoNV))
-- No documentation found for TopLevel "ExternalMemoryImageCreateInfoNV"
data ExternalMemoryImageCreateInfoNV = ExternalMemoryImageCreateInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "ExternalMemoryImageCreateInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalMemoryImageCreateInfoNV" "handleTypes"
  vkHandleTypes :: ExternalMemoryHandleTypeFlagsNV
  }
  deriving (Show, Eq)
withCStructExternalMemoryImageCreateInfoNV :: ExternalMemoryImageCreateInfoNV -> (VkExternalMemoryImageCreateInfoNV -> IO a) -> IO a
withCStructExternalMemoryImageCreateInfoNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: ExternalMemoryImageCreateInfoNV)) (\pPNext -> cont (VkExternalMemoryImageCreateInfoNV VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV pPNext (vkHandleTypes (from :: ExternalMemoryImageCreateInfoNV))))
fromCStructExternalMemoryImageCreateInfoNV :: VkExternalMemoryImageCreateInfoNV -> IO ExternalMemoryImageCreateInfoNV
fromCStructExternalMemoryImageCreateInfoNV c = ExternalMemoryImageCreateInfoNV <$> -- Univalued Member elided
                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExternalMemoryImageCreateInfoNV)))
                                                                               <*> pure (vkHandleTypes (c :: VkExternalMemoryImageCreateInfoNV))
