{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_variable_pointers
  ( VkPhysicalDeviceVariablePointerFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
  ) where

import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )


-- No documentation found for TopLevel "VkPhysicalDeviceVariablePointerFeatures"
data VkPhysicalDeviceVariablePointerFeatures = VkPhysicalDeviceVariablePointerFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceVariablePointerFeatures" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceVariablePointerFeatures" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceVariablePointerFeatures" "variablePointersStorageBuffer"
  vkVariablePointersStorageBuffer :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceVariablePointerFeatures" "variablePointers"
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
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES = VkStructureType 1000120000
