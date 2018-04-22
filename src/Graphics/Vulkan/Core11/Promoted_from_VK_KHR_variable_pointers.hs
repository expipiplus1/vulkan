{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_variable_pointers
  ( pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
  , VkPhysicalDeviceVariablePointerFeatures(..)
  ) where

import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
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
--
-- = Members
--
-- The members of the @VkPhysicalDeviceVariablePointerFeatures@ structure
-- describe the following features:
--
-- = Description
--
-- -   @variablePointersStorageBuffer@ specifies whether the implementation
--     supports the SPIR-V @VariablePointersStorageBuffer@ capability. When
--     this feature is not enabled, shader modules /must/ not declare the
--     @SPV_KHR_variable_pointers@ extension or the
--     @VariablePointersStorageBuffer@ capability.
--
-- -   @variablePointers@ specifies whether the implementation supports the
--     SPIR-V @VariablePointers@ capability. When this feature is not
--     enabled, shader modules /must/ not declare the @VariablePointers@
--     capability.
--
-- If the @VkPhysicalDeviceVariablePointerFeatures@ structure is included
-- in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- @VkPhysicalDeviceVariablePointerFeatures@ /can/ also be used in the
-- @pNext@ chain of 'Graphics.Vulkan.Core10.Device.VkDeviceCreateInfo' to
-- enable the features.
--
-- == Valid Usage
--
-- -   If @variablePointers@ is enabled then
--     @variablePointersStorageBuffer@ /must/ also be enabled.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES@
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
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
