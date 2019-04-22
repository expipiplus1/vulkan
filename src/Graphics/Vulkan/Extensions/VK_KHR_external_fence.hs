{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_fence
  ( ExportFenceCreateInfoKHR
  , pattern FENCE_IMPORT_TEMPORARY_BIT_KHR
  , pattern KHR_EXTERNAL_FENCE_EXTENSION_NAME
  , pattern KHR_EXTERNAL_FENCE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR
  , FenceImportFlagsKHR
  , FenceImportFlagBitsKHR
  , pattern STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
  , pattern FENCE_IMPORT_TEMPORARY_BIT
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence
  ( VkFenceImportFlagBits(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence
  ( pattern VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_FENCE_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence
  ( ExportFenceCreateInfo(..)
  , pattern FENCE_IMPORT_TEMPORARY_BIT
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence
  ( FenceImportFlagBitsKHR
  , FenceImportFlagsKHR
  )


type ExportFenceCreateInfoKHR = ExportFenceCreateInfo
-- TODO: Pattern constructor alias)

-- No documentation found for TopLevel "FENCE_IMPORT_TEMPORARY_BIT_KHR"
pattern FENCE_IMPORT_TEMPORARY_BIT_KHR :: VkFenceImportFlagBits
pattern FENCE_IMPORT_TEMPORARY_BIT_KHR = FENCE_IMPORT_TEMPORARY_BIT

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME"
pattern KHR_EXTERNAL_FENCE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_EXTERNAL_FENCE_EXTENSION_NAME = VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_SPEC_VERSION"
pattern KHR_EXTERNAL_FENCE_SPEC_VERSION :: Integral a => a
pattern KHR_EXTERNAL_FENCE_SPEC_VERSION = VK_KHR_EXTERNAL_FENCE_SPEC_VERSION

-- No documentation found for TopLevel "STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR :: VkStructureType
pattern STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR = STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
