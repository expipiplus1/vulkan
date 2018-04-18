{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_KHR_variable_pointers
  ( pattern VK_KHR_VARIABLE_POINTERS_SPEC_VERSION
  , pattern VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME
  , VkPhysicalDeviceVariablePointerFeaturesKHR
  , pattern VkPhysicalDeviceVariablePointerFeaturesKHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR
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
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_variable_pointers
  ( pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
  , VkPhysicalDeviceVariablePointerFeatures(..)
  )


pattern VK_KHR_VARIABLE_POINTERS_SPEC_VERSION :: Integral a => a
pattern VK_KHR_VARIABLE_POINTERS_SPEC_VERSION = 1
pattern VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME = "VK_KHR_variable_pointers"
type VkPhysicalDeviceVariablePointerFeaturesKHR = VkPhysicalDeviceVariablePointerFeatures


pattern VkPhysicalDeviceVariablePointerFeaturesKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("variablePointersStorageBuffer" ::: VkBool32) -> ("variablePointers" ::: VkBool32) -> VkPhysicalDeviceVariablePointerFeaturesKHR
pattern VkPhysicalDeviceVariablePointerFeaturesKHR vkSType vkNext vkVariablePointersStorageBuffer vkVariablePointers = VkPhysicalDeviceVariablePointerFeatures vkSType vkNext vkVariablePointersStorageBuffer vkVariablePointers
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
