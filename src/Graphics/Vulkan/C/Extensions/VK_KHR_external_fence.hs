{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_external_fence
  ( VkExportFenceCreateInfoKHR
  , pattern VkExportFenceCreateInfoKHR
  , VkFenceImportFlagBitsKHR
  , VkFenceImportFlagsKHR
  , pattern VK_FENCE_IMPORT_TEMPORARY_BIT_KHR
  , pattern VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_FENCE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
  , pattern VK_FENCE_IMPORT_TEMPORARY_BIT
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence
  ( VkExportFenceCreateInfo(..)
  , VkFenceImportFlagBits(..)
  , VkFenceImportFlags
  , pattern VK_FENCE_IMPORT_TEMPORARY_BIT
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceHandleTypeFlags
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkExportFenceCreateInfoKHR"
type VkExportFenceCreateInfoKHR = VkExportFenceCreateInfo


-- No documentation found for TopLevel "VkExportFenceCreateInfoKHR"
pattern VkExportFenceCreateInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("handleTypes" ::: VkExternalFenceHandleTypeFlags) -> VkExportFenceCreateInfoKHR
pattern VkExportFenceCreateInfoKHR vkSType vkPNext vkHandleTypes = VkExportFenceCreateInfo vkSType vkPNext vkHandleTypes

-- No documentation found for TopLevel "VkFenceImportFlagBitsKHR"
type VkFenceImportFlagBitsKHR = VkFenceImportFlagBits

-- No documentation found for TopLevel "VkFenceImportFlagsKHR"
type VkFenceImportFlagsKHR = VkFenceImportFlags

-- No documentation found for TopLevel "VK_FENCE_IMPORT_TEMPORARY_BIT_KHR"
pattern VK_FENCE_IMPORT_TEMPORARY_BIT_KHR :: VkFenceImportFlagBits
pattern VK_FENCE_IMPORT_TEMPORARY_BIT_KHR = VK_FENCE_IMPORT_TEMPORARY_BIT

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME = "VK_KHR_external_fence"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_FENCE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_FENCE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR = VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
