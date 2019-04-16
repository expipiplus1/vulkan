{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_memory_budget
  ( VkPhysicalDeviceMemoryBudgetPropertiesEXT(..)
  , pattern VK_EXT_MEMORY_BUDGET_EXTENSION_NAME
  , pattern VK_EXT_MEMORY_BUDGET_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT
  ) where

import Data.String
  ( IsString
  )
import Data.Vector.Storable.Sized
  ( Vector
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VK_MAX_MEMORY_HEAPS
  , VkDeviceSize
  )


-- No documentation found for TopLevel "VkPhysicalDeviceMemoryBudgetPropertiesEXT"
data VkPhysicalDeviceMemoryBudgetPropertiesEXT = VkPhysicalDeviceMemoryBudgetPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceMemoryBudgetPropertiesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceMemoryBudgetPropertiesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceMemoryBudgetPropertiesEXT" "heapBudget"
  vkHeapBudget :: Vector VK_MAX_MEMORY_HEAPS VkDeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceMemoryBudgetPropertiesEXT" "heapUsage"
  vkHeapUsage :: Vector VK_MAX_MEMORY_HEAPS VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceMemoryBudgetPropertiesEXT where
  sizeOf ~_ = 272
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMemoryBudgetPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                       <*> peek (ptr `plusPtr` 8)
                                                       <*> peek (ptr `plusPtr` 16)
                                                       <*> peek (ptr `plusPtr` 144)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceMemoryBudgetPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceMemoryBudgetPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkHeapBudget (poked :: VkPhysicalDeviceMemoryBudgetPropertiesEXT))
                *> poke (ptr `plusPtr` 144) (vkHeapUsage (poked :: VkPhysicalDeviceMemoryBudgetPropertiesEXT))
-- No documentation found for TopLevel "VK_EXT_MEMORY_BUDGET_EXTENSION_NAME"
pattern VK_EXT_MEMORY_BUDGET_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_MEMORY_BUDGET_EXTENSION_NAME = "VK_EXT_memory_budget"
-- No documentation found for TopLevel "VK_EXT_MEMORY_BUDGET_SPEC_VERSION"
pattern VK_EXT_MEMORY_BUDGET_SPEC_VERSION :: Integral a => a
pattern VK_EXT_MEMORY_BUDGET_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT = VkStructureType 1000237000
