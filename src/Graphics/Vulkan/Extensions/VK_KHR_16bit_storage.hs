{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_KHR_16bit_storage
  ( pattern VK_KHR_16BIT_STORAGE_SPEC_VERSION
  , pattern VK_KHR_16BIT_STORAGE_EXTENSION_NAME
  , VkPhysicalDevice16BitStorageFeaturesKHR
  , pattern VkPhysicalDevice16BitStorageFeaturesKHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR
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
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_16bit_storage
  ( VkPhysicalDevice16BitStorageFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
  )


-- No documentation found for TopLevel "VK_KHR_16BIT_STORAGE_SPEC_VERSION"
pattern VK_KHR_16BIT_STORAGE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_16BIT_STORAGE_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_16BIT_STORAGE_EXTENSION_NAME"
pattern VK_KHR_16BIT_STORAGE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_16BIT_STORAGE_EXTENSION_NAME = "VK_KHR_16bit_storage"
-- No documentation found for TopLevel "VkPhysicalDevice16BitStorageFeaturesKHR"
type VkPhysicalDevice16BitStorageFeaturesKHR = VkPhysicalDevice16BitStorageFeatures


-- No documentation found for TopLevel "VkPhysicalDevice16BitStorageFeaturesKHR"
pattern VkPhysicalDevice16BitStorageFeaturesKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("storageBuffer16BitAccess" ::: VkBool32) -> ("uniformAndStorageBuffer16BitAccess" ::: VkBool32) -> ("storagePushConstant16" ::: VkBool32) -> ("storageInputOutput16" ::: VkBool32) -> VkPhysicalDevice16BitStorageFeaturesKHR
pattern VkPhysicalDevice16BitStorageFeaturesKHR vkSType vkPNext vkStorageBuffer16BitAccess vkUniformAndStorageBuffer16BitAccess vkStoragePushConstant16 vkStorageInputOutput16 = VkPhysicalDevice16BitStorageFeatures vkSType vkPNext vkStorageBuffer16BitAccess vkUniformAndStorageBuffer16BitAccess vkStoragePushConstant16 vkStorageInputOutput16
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
