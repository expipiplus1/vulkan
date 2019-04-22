{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_16bit_storage
  ( withCStructPhysicalDevice16BitStorageFeatures
  , fromCStructPhysicalDevice16BitStorageFeatures
  , PhysicalDevice16BitStorageFeatures(..)
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
  )



-- | VkPhysicalDevice16BitStorageFeatures - Structure describing features
-- supported by VK_KHR_16bit_storage
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDevice16BitStorageFeatures = PhysicalDevice16BitStorageFeatures
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDevice16BitStorageFeatures" "pNext"
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

-- | A function to temporarily allocate memory for a 'VkPhysicalDevice16BitStorageFeatures' and
-- marshal a 'PhysicalDevice16BitStorageFeatures' into it. The 'VkPhysicalDevice16BitStorageFeatures' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDevice16BitStorageFeatures :: PhysicalDevice16BitStorageFeatures -> (VkPhysicalDevice16BitStorageFeatures -> IO a) -> IO a
withCStructPhysicalDevice16BitStorageFeatures marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDevice16BitStorageFeatures)) (\pPNext -> cont (VkPhysicalDevice16BitStorageFeatures VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES pPNext (boolToBool32 (storageBuffer16BitAccess (marshalled :: PhysicalDevice16BitStorageFeatures))) (boolToBool32 (uniformAndStorageBuffer16BitAccess (marshalled :: PhysicalDevice16BitStorageFeatures))) (boolToBool32 (storagePushConstant16 (marshalled :: PhysicalDevice16BitStorageFeatures))) (boolToBool32 (storageInputOutput16 (marshalled :: PhysicalDevice16BitStorageFeatures)))))

-- | A function to read a 'VkPhysicalDevice16BitStorageFeatures' and all additional
-- structures in the pointer chain into a 'PhysicalDevice16BitStorageFeatures'.
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

