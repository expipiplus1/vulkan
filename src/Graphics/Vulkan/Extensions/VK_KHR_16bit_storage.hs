{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_16bit_storage
  ( PhysicalDevice16BitStorageFeaturesKHR
  , pattern KHR_16BIT_STORAGE_EXTENSION_NAME
  , pattern KHR_16BIT_STORAGE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_16bit_storage
  ( pattern VK_KHR_16BIT_STORAGE_EXTENSION_NAME
  , pattern VK_KHR_16BIT_STORAGE_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_16bit_storage
  ( PhysicalDevice16BitStorageFeatures(..)
  )


type PhysicalDevice16BitStorageFeaturesKHR = PhysicalDevice16BitStorageFeatures
-- TODO: Pattern constructor alias)

-- No documentation found for TopLevel "VK_KHR_16BIT_STORAGE_EXTENSION_NAME"
pattern KHR_16BIT_STORAGE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_16BIT_STORAGE_EXTENSION_NAME = VK_KHR_16BIT_STORAGE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_16BIT_STORAGE_SPEC_VERSION"
pattern KHR_16BIT_STORAGE_SPEC_VERSION :: Integral a => a
pattern KHR_16BIT_STORAGE_SPEC_VERSION = VK_KHR_16BIT_STORAGE_SPEC_VERSION

-- No documentation found for TopLevel "STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR :: VkStructureType
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
