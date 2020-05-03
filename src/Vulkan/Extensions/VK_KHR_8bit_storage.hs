{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_8bit_storage  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR
                                              , PhysicalDevice8BitStorageFeaturesKHR
                                              , KHR_8BIT_STORAGE_SPEC_VERSION
                                              , pattern KHR_8BIT_STORAGE_SPEC_VERSION
                                              , KHR_8BIT_STORAGE_EXTENSION_NAME
                                              , pattern KHR_8BIT_STORAGE_EXTENSION_NAME
                                              ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage (PhysicalDevice8BitStorageFeatures)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES


-- No documentation found for TopLevel "VkPhysicalDevice8BitStorageFeaturesKHR"
type PhysicalDevice8BitStorageFeaturesKHR = PhysicalDevice8BitStorageFeatures


type KHR_8BIT_STORAGE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_8BIT_STORAGE_SPEC_VERSION"
pattern KHR_8BIT_STORAGE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_8BIT_STORAGE_SPEC_VERSION = 1


type KHR_8BIT_STORAGE_EXTENSION_NAME = "VK_KHR_8bit_storage"

-- No documentation found for TopLevel "VK_KHR_8BIT_STORAGE_EXTENSION_NAME"
pattern KHR_8BIT_STORAGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_8BIT_STORAGE_EXTENSION_NAME = "VK_KHR_8bit_storage"

