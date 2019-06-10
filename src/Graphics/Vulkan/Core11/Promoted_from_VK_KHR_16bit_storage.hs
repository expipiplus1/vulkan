{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_16bit_storage
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDevice16BitStorageFeatures(..)
  , 
#endif
  pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
  ) where





#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDevice16BitStorageFeatures"
data PhysicalDevice16BitStorageFeatures = PhysicalDevice16BitStorageFeatures
  { -- No documentation found for Nested "PhysicalDevice16BitStorageFeatures" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDevice16BitStorageFeatures" "storageBuffer16BitAccess"
  storageBuffer16BitAccess :: Bool
  , -- No documentation found for Nested "PhysicalDevice16BitStorageFeatures" "uniformAndStorageBuffer16BitAccess"
  uniformAndStorageBuffer16BitAccess :: Bool
  , -- No documentation found for Nested "PhysicalDevice16BitStorageFeatures" "storagePushConstant16"
  storagePushConstant16 :: Bool
  , -- No documentation found for Nested "PhysicalDevice16BitStorageFeatures" "storageInputOutput16"
  storageInputOutput16 :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDevice16BitStorageFeatures where
  zero = PhysicalDevice16BitStorageFeatures Nothing
                                            False
                                            False
                                            False
                                            False

#endif
