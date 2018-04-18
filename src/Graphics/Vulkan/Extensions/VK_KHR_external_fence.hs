{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_fence
  ( pattern VK_KHR_EXTERNAL_FENCE_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME
  , VkFenceImportFlagBitsKHR
  , VkFenceImportFlagsKHR
  , VkExportFenceCreateInfoKHR
  , pattern VkExportFenceCreateInfoKHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR
  , pattern VK_FENCE_IMPORT_TEMPORARY_BIT_KHR
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence
  ( pattern VK_FENCE_IMPORT_TEMPORARY_BIT
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
  , VkExportFenceCreateInfo(..)
  , VkFenceImportFlags
  , VkFenceImportFlagBits(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceHandleTypeFlags
  )


pattern VK_KHR_EXTERNAL_FENCE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_FENCE_SPEC_VERSION = 1
pattern VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME = "VK_KHR_external_fence"
type VkFenceImportFlagBitsKHR = VkFenceImportFlagBits
type VkFenceImportFlagsKHR = VkFenceImportFlags
type VkExportFenceCreateInfoKHR = VkExportFenceCreateInfo


pattern VkExportFenceCreateInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("handleTypes" ::: VkExternalFenceHandleTypeFlags) -> VkExportFenceCreateInfoKHR
pattern VkExportFenceCreateInfoKHR vkSType vkPNext vkHandleTypes = VkExportFenceCreateInfo vkSType vkPNext vkHandleTypes
pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR = VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
pattern VK_FENCE_IMPORT_TEMPORARY_BIT_KHR :: VkFenceImportFlagBits
pattern VK_FENCE_IMPORT_TEMPORARY_BIT_KHR = VK_FENCE_IMPORT_TEMPORARY_BIT
