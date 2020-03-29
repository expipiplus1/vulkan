{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_16bit_storage  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR
                                                        , PhysicalDevice16BitStorageFeaturesKHR
                                                        , KHR_16BIT_STORAGE_SPEC_VERSION
                                                        , pattern KHR_16BIT_STORAGE_SPEC_VERSION
                                                        , KHR_16BIT_STORAGE_EXTENSION_NAME
                                                        , pattern KHR_16BIT_STORAGE_EXTENSION_NAME
                                                        ) where

import Data.String (IsString)
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage (PhysicalDevice16BitStorageFeatures)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES


-- No documentation found for TopLevel "VkPhysicalDevice16BitStorageFeaturesKHR"
type PhysicalDevice16BitStorageFeaturesKHR = PhysicalDevice16BitStorageFeatures


type KHR_16BIT_STORAGE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_16BIT_STORAGE_SPEC_VERSION"
pattern KHR_16BIT_STORAGE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_16BIT_STORAGE_SPEC_VERSION = 1


type KHR_16BIT_STORAGE_EXTENSION_NAME = "VK_KHR_16bit_storage"

-- No documentation found for TopLevel "VK_KHR_16BIT_STORAGE_EXTENSION_NAME"
pattern KHR_16BIT_STORAGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_16BIT_STORAGE_EXTENSION_NAME = "VK_KHR_16bit_storage"

