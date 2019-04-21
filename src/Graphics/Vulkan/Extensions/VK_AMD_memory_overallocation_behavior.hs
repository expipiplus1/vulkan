{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_AMD_memory_overallocation_behavior
  ( withCStructDeviceMemoryOverallocationCreateInfoAMD
  , fromCStructDeviceMemoryOverallocationCreateInfoAMD
  , DeviceMemoryOverallocationCreateInfoAMD(..)
  , MemoryOverallocationBehaviorAMD
  , pattern MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD
  , pattern MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD
  , pattern MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD
  , pattern VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION
  , pattern VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD
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
import Graphics.Vulkan.C.Extensions.VK_AMD_memory_overallocation_behavior
  ( VkDeviceMemoryOverallocationCreateInfoAMD(..)
  , VkMemoryOverallocationBehaviorAMD(..)
  , pattern VK_MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD
  , pattern VK_MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD
  , pattern VK_MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD
  , pattern VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_memory_overallocation_behavior
  ( pattern VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME
  , pattern VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION
  )



-- | VkDeviceMemoryOverallocationCreateInfoAMD - Specify memory
-- overallocation behavior for a Vulkan device
--
-- = Description
--
-- Unresolved directive in VkDeviceMemoryOverallocationCreateInfoAMD.txt -
-- include::{generated}\/validity\/structs\/VkDeviceMemoryOverallocationCreateInfoAMD.txt[]
--
-- = See Also
--
-- No cross-references are available
data DeviceMemoryOverallocationCreateInfoAMD = DeviceMemoryOverallocationCreateInfoAMD
  { -- Univalued member elided
  -- No documentation found for Nested "DeviceMemoryOverallocationCreateInfoAMD" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceMemoryOverallocationCreateInfoAMD" "overallocationBehavior"
  overallocationBehavior :: MemoryOverallocationBehaviorAMD
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDeviceMemoryOverallocationCreateInfoAMD' and
-- marshal a 'DeviceMemoryOverallocationCreateInfoAMD' into it. The 'VkDeviceMemoryOverallocationCreateInfoAMD' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDeviceMemoryOverallocationCreateInfoAMD :: DeviceMemoryOverallocationCreateInfoAMD -> (VkDeviceMemoryOverallocationCreateInfoAMD -> IO a) -> IO a
withCStructDeviceMemoryOverallocationCreateInfoAMD marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: DeviceMemoryOverallocationCreateInfoAMD)) (\pPNext -> cont (VkDeviceMemoryOverallocationCreateInfoAMD VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD pPNext (overallocationBehavior (marshalled :: DeviceMemoryOverallocationCreateInfoAMD))))

-- | A function to read a 'VkDeviceMemoryOverallocationCreateInfoAMD' and all additional
-- structures in the pointer chain into a 'DeviceMemoryOverallocationCreateInfoAMD'.
fromCStructDeviceMemoryOverallocationCreateInfoAMD :: VkDeviceMemoryOverallocationCreateInfoAMD -> IO DeviceMemoryOverallocationCreateInfoAMD
fromCStructDeviceMemoryOverallocationCreateInfoAMD c = DeviceMemoryOverallocationCreateInfoAMD <$> -- Univalued Member elided
                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceMemoryOverallocationCreateInfoAMD)))
                                                                                               <*> pure (vkOverallocationBehavior (c :: VkDeviceMemoryOverallocationCreateInfoAMD))

instance Zero DeviceMemoryOverallocationCreateInfoAMD where
  zero = DeviceMemoryOverallocationCreateInfoAMD Nothing
                                                 zero


-- | VkMemoryOverallocationBehaviorAMD - Specify memory overallocation
-- behavior
--
-- = See Also
--
-- No cross-references are available
type MemoryOverallocationBehaviorAMD = VkMemoryOverallocationBehaviorAMD


-- | 'Graphics.Vulkan.C.Extensions.VK_AMD_memory_overallocation_behavior.VK_MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD'
-- lets the implementation decide if overallocation should be allowed.
pattern MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD :: (a ~ MemoryOverallocationBehaviorAMD) => a
pattern MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD = VK_MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD


-- | 'Graphics.Vulkan.C.Extensions.VK_AMD_memory_overallocation_behavior.VK_MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD'
-- specifies overallocation is allowed if platform permits.
pattern MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD :: (a ~ MemoryOverallocationBehaviorAMD) => a
pattern MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD = VK_MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD


-- | 'Graphics.Vulkan.C.Extensions.VK_AMD_memory_overallocation_behavior.VK_MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD'
-- specifies the application is not allowed to allocate device memory
-- beyond the heap sizes reported by
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceMemoryProperties'.
-- Allocations that are not explicitly made by the application within the
-- scope of the Vulkan instance are not accounted for.
pattern MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD :: (a ~ MemoryOverallocationBehaviorAMD) => a
pattern MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD = VK_MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD
