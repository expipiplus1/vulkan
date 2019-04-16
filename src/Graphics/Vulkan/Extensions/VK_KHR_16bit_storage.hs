{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_16bit_storage
  ( PhysicalDevice16BitStorageFeaturesKHR
  , pattern VK_KHR_16BIT_STORAGE_SPEC_VERSION
  , pattern VK_KHR_16BIT_STORAGE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
  ) where




import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_16bit_storage
  ( PhysicalDevice16BitStorageFeatures(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_16bit_storage
  ( pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_16bit_storage
  ( pattern VK_KHR_16BIT_STORAGE_EXTENSION_NAME
  , pattern VK_KHR_16BIT_STORAGE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR
  )


type PhysicalDevice16BitStorageFeaturesKHR = PhysicalDevice16BitStorageFeatures
-- TODO: Pattern constructor alias)
