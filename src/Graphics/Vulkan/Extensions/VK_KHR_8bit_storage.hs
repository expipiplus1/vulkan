{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_8bit_storage
  ( withCStructPhysicalDevice8BitStorageFeaturesKHR
  , fromCStructPhysicalDevice8BitStorageFeaturesKHR
  , PhysicalDevice8BitStorageFeaturesKHR(..)
  , pattern VK_KHR_8BIT_STORAGE_SPEC_VERSION
  , pattern VK_KHR_8BIT_STORAGE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Extensions.VK_KHR_8bit_storage
  ( VkPhysicalDevice8BitStorageFeaturesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_8bit_storage
  ( pattern VK_KHR_8BIT_STORAGE_EXTENSION_NAME
  , pattern VK_KHR_8BIT_STORAGE_SPEC_VERSION
  )


-- No documentation found for TopLevel "PhysicalDevice8BitStorageFeaturesKHR"
data PhysicalDevice8BitStorageFeaturesKHR = PhysicalDevice8BitStorageFeaturesKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDevice8BitStorageFeaturesKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDevice8BitStorageFeaturesKHR" "storageBuffer8BitAccess"
  vkStorageBuffer8BitAccess :: Bool
  , -- No documentation found for Nested "PhysicalDevice8BitStorageFeaturesKHR" "uniformAndStorageBuffer8BitAccess"
  vkUniformAndStorageBuffer8BitAccess :: Bool
  , -- No documentation found for Nested "PhysicalDevice8BitStorageFeaturesKHR" "storagePushConstant8"
  vkStoragePushConstant8 :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDevice8BitStorageFeaturesKHR :: PhysicalDevice8BitStorageFeaturesKHR -> (VkPhysicalDevice8BitStorageFeaturesKHR -> IO a) -> IO a
withCStructPhysicalDevice8BitStorageFeaturesKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDevice8BitStorageFeaturesKHR)) (\pPNext -> cont (VkPhysicalDevice8BitStorageFeaturesKHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR pPNext (boolToBool32 (vkStorageBuffer8BitAccess (from :: PhysicalDevice8BitStorageFeaturesKHR))) (boolToBool32 (vkUniformAndStorageBuffer8BitAccess (from :: PhysicalDevice8BitStorageFeaturesKHR))) (boolToBool32 (vkStoragePushConstant8 (from :: PhysicalDevice8BitStorageFeaturesKHR)))))
fromCStructPhysicalDevice8BitStorageFeaturesKHR :: VkPhysicalDevice8BitStorageFeaturesKHR -> IO PhysicalDevice8BitStorageFeaturesKHR
fromCStructPhysicalDevice8BitStorageFeaturesKHR c = PhysicalDevice8BitStorageFeaturesKHR <$> -- Univalued Member elided
                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDevice8BitStorageFeaturesKHR)))
                                                                                         <*> pure (bool32ToBool (vkStorageBuffer8BitAccess (c :: VkPhysicalDevice8BitStorageFeaturesKHR)))
                                                                                         <*> pure (bool32ToBool (vkUniformAndStorageBuffer8BitAccess (c :: VkPhysicalDevice8BitStorageFeaturesKHR)))
                                                                                         <*> pure (bool32ToBool (vkStoragePushConstant8 (c :: VkPhysicalDevice8BitStorageFeaturesKHR)))
