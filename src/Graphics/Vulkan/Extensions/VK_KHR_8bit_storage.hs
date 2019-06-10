{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_8bit_storage
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDevice8BitStorageFeaturesKHR(..)
  , 
#endif
  pattern KHR_8BIT_STORAGE_EXTENSION_NAME
  , pattern KHR_8BIT_STORAGE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_8bit_storage
  ( pattern VK_KHR_8BIT_STORAGE_EXTENSION_NAME
  , pattern VK_KHR_8BIT_STORAGE_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDevice8BitStorageFeaturesKHR"
data PhysicalDevice8BitStorageFeaturesKHR = PhysicalDevice8BitStorageFeaturesKHR
  { -- No documentation found for Nested "PhysicalDevice8BitStorageFeaturesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDevice8BitStorageFeaturesKHR" "storageBuffer8BitAccess"
  storageBuffer8BitAccess :: Bool
  , -- No documentation found for Nested "PhysicalDevice8BitStorageFeaturesKHR" "uniformAndStorageBuffer8BitAccess"
  uniformAndStorageBuffer8BitAccess :: Bool
  , -- No documentation found for Nested "PhysicalDevice8BitStorageFeaturesKHR" "storagePushConstant8"
  storagePushConstant8 :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDevice8BitStorageFeaturesKHR where
  zero = PhysicalDevice8BitStorageFeaturesKHR Nothing
                                              False
                                              False
                                              False

#endif

-- No documentation found for TopLevel "VK_KHR_8BIT_STORAGE_EXTENSION_NAME"
pattern KHR_8BIT_STORAGE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_8BIT_STORAGE_EXTENSION_NAME = VK_KHR_8BIT_STORAGE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_8BIT_STORAGE_SPEC_VERSION"
pattern KHR_8BIT_STORAGE_SPEC_VERSION :: Integral a => a
pattern KHR_8BIT_STORAGE_SPEC_VERSION = VK_KHR_8BIT_STORAGE_SPEC_VERSION
