{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_variable_pointers
  ( VkPhysicalDeviceVariablePointerFeatures
  , pattern VkPhysicalDeviceVariablePointerFeatures
  , VkPhysicalDeviceVariablePointersFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES
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
  , Zero(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkPhysicalDeviceVariablePointerFeatures"
type VkPhysicalDeviceVariablePointerFeatures = VkPhysicalDeviceVariablePointersFeatures


-- No documentation found for TopLevel "VkPhysicalDeviceVariablePointerFeatures"
pattern VkPhysicalDeviceVariablePointerFeatures :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("variablePointersStorageBuffer" ::: VkBool32) -> ("variablePointers" ::: VkBool32) -> VkPhysicalDeviceVariablePointerFeatures
pattern VkPhysicalDeviceVariablePointerFeatures vkSType vkPNext vkVariablePointersStorageBuffer vkVariablePointers = VkPhysicalDeviceVariablePointersFeatures vkSType vkPNext vkVariablePointersStorageBuffer vkVariablePointers

-- No documentation found for TopLevel "VkPhysicalDeviceVariablePointersFeatures"
data VkPhysicalDeviceVariablePointersFeatures = VkPhysicalDeviceVariablePointersFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceVariablePointersFeatures" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceVariablePointersFeatures" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceVariablePointersFeatures" "variablePointersStorageBuffer"
  vkVariablePointersStorageBuffer :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceVariablePointersFeatures" "variablePointers"
  vkVariablePointers :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceVariablePointersFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceVariablePointersFeatures <$> peek (ptr `plusPtr` 0)
                                                      <*> peek (ptr `plusPtr` 8)
                                                      <*> peek (ptr `plusPtr` 16)
                                                      <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceVariablePointersFeatures))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceVariablePointersFeatures))
                *> poke (ptr `plusPtr` 16) (vkVariablePointersStorageBuffer (poked :: VkPhysicalDeviceVariablePointersFeatures))
                *> poke (ptr `plusPtr` 20) (vkVariablePointers (poked :: VkPhysicalDeviceVariablePointersFeatures))

instance Zero VkPhysicalDeviceVariablePointersFeatures where
  zero = VkPhysicalDeviceVariablePointersFeatures VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES
                                                  zero
                                                  zero
                                                  zero

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES = VkStructureType 1000120000

-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES
