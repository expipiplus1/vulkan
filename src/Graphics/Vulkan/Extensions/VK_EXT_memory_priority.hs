{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_memory_priority
  ( withCStructMemoryPriorityAllocateInfoEXT
  , fromCStructMemoryPriorityAllocateInfoEXT
  , MemoryPriorityAllocateInfoEXT(..)
  , withCStructPhysicalDeviceMemoryPriorityFeaturesEXT
  , fromCStructPhysicalDeviceMemoryPriorityFeaturesEXT
  , PhysicalDeviceMemoryPriorityFeaturesEXT(..)
  , pattern VK_EXT_MEMORY_PRIORITY_SPEC_VERSION
  , pattern VK_EXT_MEMORY_PRIORITY_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT
  ) where

import Foreign.C.Types
  ( CFloat(..)
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Extensions.VK_EXT_memory_priority
  ( VkMemoryPriorityAllocateInfoEXT(..)
  , VkPhysicalDeviceMemoryPriorityFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT
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
import Graphics.Vulkan.C.Extensions.VK_EXT_memory_priority
  ( pattern VK_EXT_MEMORY_PRIORITY_EXTENSION_NAME
  , pattern VK_EXT_MEMORY_PRIORITY_SPEC_VERSION
  )


-- No documentation found for TopLevel "MemoryPriorityAllocateInfoEXT"
data MemoryPriorityAllocateInfoEXT = MemoryPriorityAllocateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "MemoryPriorityAllocateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryPriorityAllocateInfoEXT" "priority"
  vkPriority :: CFloat
  }
  deriving (Show, Eq)
withCStructMemoryPriorityAllocateInfoEXT :: MemoryPriorityAllocateInfoEXT -> (VkMemoryPriorityAllocateInfoEXT -> IO a) -> IO a
withCStructMemoryPriorityAllocateInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: MemoryPriorityAllocateInfoEXT)) (\pPNext -> cont (VkMemoryPriorityAllocateInfoEXT VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT pPNext (vkPriority (from :: MemoryPriorityAllocateInfoEXT))))
fromCStructMemoryPriorityAllocateInfoEXT :: VkMemoryPriorityAllocateInfoEXT -> IO MemoryPriorityAllocateInfoEXT
fromCStructMemoryPriorityAllocateInfoEXT c = MemoryPriorityAllocateInfoEXT <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryPriorityAllocateInfoEXT)))
                                                                           <*> pure (vkPriority (c :: VkMemoryPriorityAllocateInfoEXT))
-- No documentation found for TopLevel "PhysicalDeviceMemoryPriorityFeaturesEXT"
data PhysicalDeviceMemoryPriorityFeaturesEXT = PhysicalDeviceMemoryPriorityFeaturesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceMemoryPriorityFeaturesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMemoryPriorityFeaturesEXT" "memoryPriority"
  vkMemoryPriority :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceMemoryPriorityFeaturesEXT :: PhysicalDeviceMemoryPriorityFeaturesEXT -> (VkPhysicalDeviceMemoryPriorityFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceMemoryPriorityFeaturesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceMemoryPriorityFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceMemoryPriorityFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT pPNext (boolToBool32 (vkMemoryPriority (from :: PhysicalDeviceMemoryPriorityFeaturesEXT)))))
fromCStructPhysicalDeviceMemoryPriorityFeaturesEXT :: VkPhysicalDeviceMemoryPriorityFeaturesEXT -> IO PhysicalDeviceMemoryPriorityFeaturesEXT
fromCStructPhysicalDeviceMemoryPriorityFeaturesEXT c = PhysicalDeviceMemoryPriorityFeaturesEXT <$> -- Univalued Member elided
                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceMemoryPriorityFeaturesEXT)))
                                                                                               <*> pure (bool32ToBool (vkMemoryPriority (c :: VkPhysicalDeviceMemoryPriorityFeaturesEXT)))
