{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_AMD_memory_overallocation_behavior
  ( withCStructDeviceMemoryOverallocationCreateInfoAMD
  , fromCStructDeviceMemoryOverallocationCreateInfoAMD
  , DeviceMemoryOverallocationCreateInfoAMD(..)
  , MemoryOverallocationBehaviorAMD
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


-- No documentation found for TopLevel "DeviceMemoryOverallocationCreateInfoAMD"
data DeviceMemoryOverallocationCreateInfoAMD = DeviceMemoryOverallocationCreateInfoAMD
  { -- Univalued Member elided
  -- No documentation found for Nested "DeviceMemoryOverallocationCreateInfoAMD" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceMemoryOverallocationCreateInfoAMD" "overallocationBehavior"
  vkOverallocationBehavior :: MemoryOverallocationBehaviorAMD
  }
  deriving (Show, Eq)
withCStructDeviceMemoryOverallocationCreateInfoAMD :: DeviceMemoryOverallocationCreateInfoAMD -> (VkDeviceMemoryOverallocationCreateInfoAMD -> IO a) -> IO a
withCStructDeviceMemoryOverallocationCreateInfoAMD from cont = maybeWith withSomeVkStruct (vkPNext (from :: DeviceMemoryOverallocationCreateInfoAMD)) (\pPNext -> cont (VkDeviceMemoryOverallocationCreateInfoAMD VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD pPNext (vkOverallocationBehavior (from :: DeviceMemoryOverallocationCreateInfoAMD))))
fromCStructDeviceMemoryOverallocationCreateInfoAMD :: VkDeviceMemoryOverallocationCreateInfoAMD -> IO DeviceMemoryOverallocationCreateInfoAMD
fromCStructDeviceMemoryOverallocationCreateInfoAMD c = DeviceMemoryOverallocationCreateInfoAMD <$> -- Univalued Member elided
                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceMemoryOverallocationCreateInfoAMD)))
                                                                                               <*> pure (vkOverallocationBehavior (c :: VkDeviceMemoryOverallocationCreateInfoAMD))
instance Zero DeviceMemoryOverallocationCreateInfoAMD where
  zero = DeviceMemoryOverallocationCreateInfoAMD Nothing
                                                 zero
-- No documentation found for TopLevel "MemoryOverallocationBehaviorAMD"
type MemoryOverallocationBehaviorAMD = VkMemoryOverallocationBehaviorAMD
