{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_external_memory
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  ExportMemoryAllocateInfoNV(..)
  , 
  ExternalMemoryImageCreateInfoNV(..)
#endif
  , pattern NV_EXTERNAL_MEMORY_EXTENSION_NAME
  , pattern NV_EXTERNAL_MEMORY_SPEC_VERSION
  , pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory
  ( pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME
  , pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities
  ( ExternalMemoryHandleTypeFlagsNV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV
  , pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkExportMemoryAllocateInfoNV"
data ExportMemoryAllocateInfoNV = ExportMemoryAllocateInfoNV
  { -- No documentation found for Nested "ExportMemoryAllocateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportMemoryAllocateInfoNV" "handleTypes"
  handleTypes :: ExternalMemoryHandleTypeFlagsNV
  }
  deriving (Show, Eq)

instance Zero ExportMemoryAllocateInfoNV where
  zero = ExportMemoryAllocateInfoNV Nothing
                                    zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkExternalMemoryImageCreateInfoNV"
data ExternalMemoryImageCreateInfoNV = ExternalMemoryImageCreateInfoNV
  { -- No documentation found for Nested "ExternalMemoryImageCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalMemoryImageCreateInfoNV" "handleTypes"
  handleTypes :: ExternalMemoryHandleTypeFlagsNV
  }
  deriving (Show, Eq)

instance Zero ExternalMemoryImageCreateInfoNV where
  zero = ExternalMemoryImageCreateInfoNV Nothing
                                         zero

#endif

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME"
pattern NV_EXTERNAL_MEMORY_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_EXTERNAL_MEMORY_EXTENSION_NAME = VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_SPEC_VERSION"
pattern NV_EXTERNAL_MEMORY_SPEC_VERSION :: Integral a => a
pattern NV_EXTERNAL_MEMORY_SPEC_VERSION = VK_NV_EXTERNAL_MEMORY_SPEC_VERSION
