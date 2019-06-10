{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  ExportMemoryAllocateInfo(..)
  , 
  ExternalMemoryBufferCreateInfo(..)
  , ExternalMemoryImageCreateInfo(..)
#endif
  , pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO
  , pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO
  , pattern STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO
  , pattern ERROR_INVALID_EXTERNAL_HANDLE
  ) where





#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( ExternalMemoryHandleTypeFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern ERROR_INVALID_EXTERNAL_HANDLE
  , pattern STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO
  , pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO
  , pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkExportMemoryAllocateInfo"
data ExportMemoryAllocateInfo = ExportMemoryAllocateInfo
  { -- No documentation found for Nested "ExportMemoryAllocateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportMemoryAllocateInfo" "handleTypes"
  handleTypes :: ExternalMemoryHandleTypeFlags
  }
  deriving (Show, Eq)

instance Zero ExportMemoryAllocateInfo where
  zero = ExportMemoryAllocateInfo Nothing
                                  zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkExternalMemoryBufferCreateInfo"
data ExternalMemoryBufferCreateInfo = ExternalMemoryBufferCreateInfo
  { -- No documentation found for Nested "ExternalMemoryBufferCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalMemoryBufferCreateInfo" "handleTypes"
  handleTypes :: ExternalMemoryHandleTypeFlags
  }
  deriving (Show, Eq)

instance Zero ExternalMemoryBufferCreateInfo where
  zero = ExternalMemoryBufferCreateInfo Nothing
                                        zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkExternalMemoryImageCreateInfo"
data ExternalMemoryImageCreateInfo = ExternalMemoryImageCreateInfo
  { -- No documentation found for Nested "ExternalMemoryImageCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalMemoryImageCreateInfo" "handleTypes"
  handleTypes :: ExternalMemoryHandleTypeFlags
  }
  deriving (Show, Eq)

instance Zero ExternalMemoryImageCreateInfo where
  zero = ExternalMemoryImageCreateInfo Nothing
                                       zero

#endif
