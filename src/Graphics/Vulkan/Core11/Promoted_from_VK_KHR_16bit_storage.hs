{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_16bit_storage
  ( withCStructPhysicalDevice16BitStorageFeatures
  , fromCStructPhysicalDevice16BitStorageFeatures
  , PhysicalDevice16BitStorageFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_16bit_storage
  ( VkPhysicalDevice16BitStorageFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
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


-- No documentation found for TopLevel "PhysicalDevice16BitStorageFeatures"
data PhysicalDevice16BitStorageFeatures = PhysicalDevice16BitStorageFeatures
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDevice16BitStorageFeatures" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDevice16BitStorageFeatures" "storageBuffer16BitAccess"
  vkStorageBuffer16BitAccess :: Bool
  , -- No documentation found for Nested "PhysicalDevice16BitStorageFeatures" "uniformAndStorageBuffer16BitAccess"
  vkUniformAndStorageBuffer16BitAccess :: Bool
  , -- No documentation found for Nested "PhysicalDevice16BitStorageFeatures" "storagePushConstant16"
  vkStoragePushConstant16 :: Bool
  , -- No documentation found for Nested "PhysicalDevice16BitStorageFeatures" "storageInputOutput16"
  vkStorageInputOutput16 :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDevice16BitStorageFeatures :: PhysicalDevice16BitStorageFeatures -> (VkPhysicalDevice16BitStorageFeatures -> IO a) -> IO a
withCStructPhysicalDevice16BitStorageFeatures from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDevice16BitStorageFeatures)) (\pPNext -> cont (VkPhysicalDevice16BitStorageFeatures VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES pPNext (boolToBool32 (vkStorageBuffer16BitAccess (from :: PhysicalDevice16BitStorageFeatures))) (boolToBool32 (vkUniformAndStorageBuffer16BitAccess (from :: PhysicalDevice16BitStorageFeatures))) (boolToBool32 (vkStoragePushConstant16 (from :: PhysicalDevice16BitStorageFeatures))) (boolToBool32 (vkStorageInputOutput16 (from :: PhysicalDevice16BitStorageFeatures)))))
fromCStructPhysicalDevice16BitStorageFeatures :: VkPhysicalDevice16BitStorageFeatures -> IO PhysicalDevice16BitStorageFeatures
fromCStructPhysicalDevice16BitStorageFeatures c = PhysicalDevice16BitStorageFeatures <$> -- Univalued Member elided
                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDevice16BitStorageFeatures)))
                                                                                     <*> pure (bool32ToBool (vkStorageBuffer16BitAccess (c :: VkPhysicalDevice16BitStorageFeatures)))
                                                                                     <*> pure (bool32ToBool (vkUniformAndStorageBuffer16BitAccess (c :: VkPhysicalDevice16BitStorageFeatures)))
                                                                                     <*> pure (bool32ToBool (vkStoragePushConstant16 (c :: VkPhysicalDevice16BitStorageFeatures)))
                                                                                     <*> pure (bool32ToBool (vkStorageInputOutput16 (c :: VkPhysicalDevice16BitStorageFeatures)))
instance Zero PhysicalDevice16BitStorageFeatures where
  zero = PhysicalDevice16BitStorageFeatures Nothing
                                            False
                                            False
                                            False
                                            False
