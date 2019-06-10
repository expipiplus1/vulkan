{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_memory_priority
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  MemoryPriorityAllocateInfoEXT(..)
  , 
  PhysicalDeviceMemoryPriorityFeaturesEXT(..)
#endif
  , pattern EXT_MEMORY_PRIORITY_EXTENSION_NAME
  , pattern EXT_MEMORY_PRIORITY_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT
  , pattern STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_memory_priority
  ( pattern VK_EXT_MEMORY_PRIORITY_EXTENSION_NAME
  , pattern VK_EXT_MEMORY_PRIORITY_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkMemoryPriorityAllocateInfoEXT"
data MemoryPriorityAllocateInfoEXT = MemoryPriorityAllocateInfoEXT
  { -- No documentation found for Nested "MemoryPriorityAllocateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryPriorityAllocateInfoEXT" "priority"
  priority :: Float
  }
  deriving (Show, Eq)

instance Zero MemoryPriorityAllocateInfoEXT where
  zero = MemoryPriorityAllocateInfoEXT Nothing
                                       zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceMemoryPriorityFeaturesEXT"
data PhysicalDeviceMemoryPriorityFeaturesEXT = PhysicalDeviceMemoryPriorityFeaturesEXT
  { -- No documentation found for Nested "PhysicalDeviceMemoryPriorityFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMemoryPriorityFeaturesEXT" "memoryPriority"
  memoryPriority :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceMemoryPriorityFeaturesEXT where
  zero = PhysicalDeviceMemoryPriorityFeaturesEXT Nothing
                                                 False

#endif

-- No documentation found for TopLevel "VK_EXT_MEMORY_PRIORITY_EXTENSION_NAME"
pattern EXT_MEMORY_PRIORITY_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_MEMORY_PRIORITY_EXTENSION_NAME = VK_EXT_MEMORY_PRIORITY_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_MEMORY_PRIORITY_SPEC_VERSION"
pattern EXT_MEMORY_PRIORITY_SPEC_VERSION :: Integral a => a
pattern EXT_MEMORY_PRIORITY_SPEC_VERSION = VK_EXT_MEMORY_PRIORITY_SPEC_VERSION
