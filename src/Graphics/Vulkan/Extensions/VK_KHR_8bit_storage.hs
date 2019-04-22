{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_8bit_storage
  ( withCStructPhysicalDevice8BitStorageFeaturesKHR
  , fromCStructPhysicalDevice8BitStorageFeaturesKHR
  , PhysicalDevice8BitStorageFeaturesKHR(..)
  , pattern KHR_8BIT_STORAGE_EXTENSION_NAME
  , pattern KHR_8BIT_STORAGE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR
  ) where

import Data.String
  ( IsString
  )
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
import Graphics.Vulkan.C.Extensions.VK_KHR_8bit_storage
  ( VkPhysicalDevice8BitStorageFeaturesKHR(..)
  , pattern VK_KHR_8BIT_STORAGE_EXTENSION_NAME
  , pattern VK_KHR_8BIT_STORAGE_SPEC_VERSION
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR
  )



-- | VkPhysicalDevice8BitStorageFeaturesKHR - Structure describing features
-- supported by VK_KHR_8bit_storage
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDevice8BitStorageFeaturesKHR = PhysicalDevice8BitStorageFeaturesKHR
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDevice8BitStorageFeaturesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDevice8BitStorageFeaturesKHR" "storageBuffer8BitAccess"
  storageBuffer8BitAccess :: Bool
  , -- No documentation found for Nested "PhysicalDevice8BitStorageFeaturesKHR" "uniformAndStorageBuffer8BitAccess"
  uniformAndStorageBuffer8BitAccess :: Bool
  , -- No documentation found for Nested "PhysicalDevice8BitStorageFeaturesKHR" "storagePushConstant8"
  storagePushConstant8 :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDevice8BitStorageFeaturesKHR' and
-- marshal a 'PhysicalDevice8BitStorageFeaturesKHR' into it. The 'VkPhysicalDevice8BitStorageFeaturesKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDevice8BitStorageFeaturesKHR :: PhysicalDevice8BitStorageFeaturesKHR -> (VkPhysicalDevice8BitStorageFeaturesKHR -> IO a) -> IO a
withCStructPhysicalDevice8BitStorageFeaturesKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDevice8BitStorageFeaturesKHR)) (\pPNext -> cont (VkPhysicalDevice8BitStorageFeaturesKHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR pPNext (boolToBool32 (storageBuffer8BitAccess (marshalled :: PhysicalDevice8BitStorageFeaturesKHR))) (boolToBool32 (uniformAndStorageBuffer8BitAccess (marshalled :: PhysicalDevice8BitStorageFeaturesKHR))) (boolToBool32 (storagePushConstant8 (marshalled :: PhysicalDevice8BitStorageFeaturesKHR)))))

-- | A function to read a 'VkPhysicalDevice8BitStorageFeaturesKHR' and all additional
-- structures in the pointer chain into a 'PhysicalDevice8BitStorageFeaturesKHR'.
fromCStructPhysicalDevice8BitStorageFeaturesKHR :: VkPhysicalDevice8BitStorageFeaturesKHR -> IO PhysicalDevice8BitStorageFeaturesKHR
fromCStructPhysicalDevice8BitStorageFeaturesKHR c = PhysicalDevice8BitStorageFeaturesKHR <$> -- Univalued Member elided
                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDevice8BitStorageFeaturesKHR)))
                                                                                         <*> pure (bool32ToBool (vkStorageBuffer8BitAccess (c :: VkPhysicalDevice8BitStorageFeaturesKHR)))
                                                                                         <*> pure (bool32ToBool (vkUniformAndStorageBuffer8BitAccess (c :: VkPhysicalDevice8BitStorageFeaturesKHR)))
                                                                                         <*> pure (bool32ToBool (vkStoragePushConstant8 (c :: VkPhysicalDevice8BitStorageFeaturesKHR)))

instance Zero PhysicalDevice8BitStorageFeaturesKHR where
  zero = PhysicalDevice8BitStorageFeaturesKHR Nothing
                                              False
                                              False
                                              False


-- No documentation found for TopLevel "VK_KHR_8BIT_STORAGE_EXTENSION_NAME"
pattern KHR_8BIT_STORAGE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_8BIT_STORAGE_EXTENSION_NAME = VK_KHR_8BIT_STORAGE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_8BIT_STORAGE_SPEC_VERSION"
pattern KHR_8BIT_STORAGE_SPEC_VERSION :: Integral a => a
pattern KHR_8BIT_STORAGE_SPEC_VERSION = VK_KHR_8BIT_STORAGE_SPEC_VERSION
