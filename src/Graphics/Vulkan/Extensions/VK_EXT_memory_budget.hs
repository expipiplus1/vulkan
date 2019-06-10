{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_memory_budget
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceMemoryBudgetPropertiesEXT(..)
  , 
#endif
  pattern EXT_MEMORY_BUDGET_EXTENSION_NAME
  , pattern EXT_MEMORY_BUDGET_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT
  ) where

import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_memory_budget
  ( pattern VK_EXT_MEMORY_BUDGET_EXTENSION_NAME
  , pattern VK_EXT_MEMORY_BUDGET_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( DeviceSize
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceMemoryBudgetPropertiesEXT"
data PhysicalDeviceMemoryBudgetPropertiesEXT = PhysicalDeviceMemoryBudgetPropertiesEXT
  { -- No documentation found for Nested "PhysicalDeviceMemoryBudgetPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMemoryBudgetPropertiesEXT" "heapBudget"
  heapBudget :: Vector DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceMemoryBudgetPropertiesEXT" "heapUsage"
  heapUsage :: Vector DeviceSize
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceMemoryBudgetPropertiesEXT where
  zero = PhysicalDeviceMemoryBudgetPropertiesEXT Nothing
                                                 mempty
                                                 mempty

#endif

-- No documentation found for TopLevel "VK_EXT_MEMORY_BUDGET_EXTENSION_NAME"
pattern EXT_MEMORY_BUDGET_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_MEMORY_BUDGET_EXTENSION_NAME = VK_EXT_MEMORY_BUDGET_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_MEMORY_BUDGET_SPEC_VERSION"
pattern EXT_MEMORY_BUDGET_SPEC_VERSION :: Integral a => a
pattern EXT_MEMORY_BUDGET_SPEC_VERSION = VK_EXT_MEMORY_BUDGET_SPEC_VERSION
