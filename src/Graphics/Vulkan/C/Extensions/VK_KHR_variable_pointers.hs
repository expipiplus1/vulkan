{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_variable_pointers
  ( VkPhysicalDeviceVariablePointerFeaturesKHR
  , pattern VkPhysicalDeviceVariablePointerFeaturesKHR
  , VkPhysicalDeviceVariablePointersFeaturesKHR
  , pattern VkPhysicalDeviceVariablePointersFeaturesKHR
  , pattern VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME
  , pattern VK_KHR_VARIABLE_POINTERS_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_variable_pointers
  ( VkPhysicalDeviceVariablePointersFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkPhysicalDeviceVariablePointerFeaturesKHR"
type VkPhysicalDeviceVariablePointerFeaturesKHR = VkPhysicalDeviceVariablePointersFeatures


-- No documentation found for TopLevel "VkPhysicalDeviceVariablePointerFeaturesKHR"
pattern VkPhysicalDeviceVariablePointerFeaturesKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("variablePointersStorageBuffer" ::: VkBool32) -> ("variablePointers" ::: VkBool32) -> VkPhysicalDeviceVariablePointerFeaturesKHR
pattern VkPhysicalDeviceVariablePointerFeaturesKHR vkSType vkPNext vkVariablePointersStorageBuffer vkVariablePointers = VkPhysicalDeviceVariablePointersFeatures vkSType vkPNext vkVariablePointersStorageBuffer vkVariablePointers

-- No documentation found for TopLevel "VkPhysicalDeviceVariablePointersFeaturesKHR"
type VkPhysicalDeviceVariablePointersFeaturesKHR = VkPhysicalDeviceVariablePointersFeatures


-- No documentation found for TopLevel "VkPhysicalDeviceVariablePointersFeaturesKHR"
pattern VkPhysicalDeviceVariablePointersFeaturesKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("variablePointersStorageBuffer" ::: VkBool32) -> ("variablePointers" ::: VkBool32) -> VkPhysicalDeviceVariablePointersFeaturesKHR
pattern VkPhysicalDeviceVariablePointersFeaturesKHR vkSType vkPNext vkVariablePointersStorageBuffer vkVariablePointers = VkPhysicalDeviceVariablePointersFeatures vkSType vkPNext vkVariablePointersStorageBuffer vkVariablePointers

-- No documentation found for TopLevel "VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME"
pattern VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME = "VK_KHR_variable_pointers"

-- No documentation found for TopLevel "VK_KHR_VARIABLE_POINTERS_SPEC_VERSION"
pattern VK_KHR_VARIABLE_POINTERS_SPEC_VERSION :: Integral a => a
pattern VK_KHR_VARIABLE_POINTERS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES

-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
