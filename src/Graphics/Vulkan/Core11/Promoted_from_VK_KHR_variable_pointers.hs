{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_variable_pointers
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


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES = VkStructureType 1000120000
-- | VkPhysicalDeviceVariablePointerFeatures - Structure describing variable
-- pointers features that can be supported by an implementation
data VkPhysicalDeviceVariablePointerFeatures = VkPhysicalDeviceVariablePointerFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceVariablePointerFeatures" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceVariablePointerFeatures" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceVariablePointerFeatures" "vkVariablePointersStorageBuffer"
  vkVariablePointersStorageBuffer :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceVariablePointerFeatures" "vkVariablePointers"
  vkVariablePointers :: VkBool32
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceVariablePointerFeatures))
                *> poke (ptr `plusPtr` 16) (vkVariablePointersStorageBuffer (poked :: VkPhysicalDeviceVariablePointerFeatures))
                *> poke (ptr `plusPtr` 20) (vkVariablePointers (poked :: VkPhysicalDeviceVariablePointerFeatures))
