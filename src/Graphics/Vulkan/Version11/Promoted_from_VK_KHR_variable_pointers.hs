{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Version11.Promoted_from_VK_KHR_variable_pointers
  ( pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
  , VkPhysicalDeviceVariablePointerFeatures(..)
  ) where

import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )


import Graphics.Vulkan.Version10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )


-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES = VkStructureType 1000120000
-- | TODO: Struct comments
data VkPhysicalDeviceVariablePointerFeatures = VkPhysicalDeviceVariablePointerFeatures
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkVariablePointersStorageBuffer :: VkBool32
  , vkVariablePointers :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceVariablePointerFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceVariablePointerFeatures <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
                                                     <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceVariablePointerFeatures))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPhysicalDeviceVariablePointerFeatures))
                *> poke (ptr `plusPtr` 16) (vkVariablePointersStorageBuffer (poked :: VkPhysicalDeviceVariablePointerFeatures))
                *> poke (ptr `plusPtr` 20) (vkVariablePointers (poked :: VkPhysicalDeviceVariablePointerFeatures))
