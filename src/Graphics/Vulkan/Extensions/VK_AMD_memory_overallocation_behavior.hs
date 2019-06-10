{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_AMD_memory_overallocation_behavior
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  DeviceMemoryOverallocationCreateInfoAMD(..)
  , 
#endif
  MemoryOverallocationBehaviorAMD
  , pattern MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD
  , pattern MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD
  , pattern MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD
  , pattern AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME
  , pattern AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION
  , pattern STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_AMD_memory_overallocation_behavior
  ( VkMemoryOverallocationBehaviorAMD(..)
  , pattern VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME
  , pattern VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION
  , pattern VK_MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD
  , pattern VK_MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD
  , pattern VK_MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDeviceMemoryOverallocationCreateInfoAMD"
data DeviceMemoryOverallocationCreateInfoAMD = DeviceMemoryOverallocationCreateInfoAMD
  { -- No documentation found for Nested "DeviceMemoryOverallocationCreateInfoAMD" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceMemoryOverallocationCreateInfoAMD" "overallocationBehavior"
  overallocationBehavior :: MemoryOverallocationBehaviorAMD
  }
  deriving (Show, Eq)

instance Zero DeviceMemoryOverallocationCreateInfoAMD where
  zero = DeviceMemoryOverallocationCreateInfoAMD Nothing
                                                 zero

#endif

-- No documentation found for TopLevel "MemoryOverallocationBehaviorAMD"
type MemoryOverallocationBehaviorAMD = VkMemoryOverallocationBehaviorAMD


{-# complete MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD, MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD, MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD :: MemoryOverallocationBehaviorAMD #-}


-- No documentation found for Nested "MemoryOverallocationBehaviorAMD" "MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD"
pattern MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD :: (a ~ MemoryOverallocationBehaviorAMD) => a
pattern MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD = VK_MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD


-- No documentation found for Nested "MemoryOverallocationBehaviorAMD" "MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD"
pattern MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD :: (a ~ MemoryOverallocationBehaviorAMD) => a
pattern MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD = VK_MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD


-- No documentation found for Nested "MemoryOverallocationBehaviorAMD" "MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD"
pattern MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD :: (a ~ MemoryOverallocationBehaviorAMD) => a
pattern MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD = VK_MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD

-- No documentation found for TopLevel "VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME"
pattern AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME = VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME

-- No documentation found for TopLevel "VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION"
pattern AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION :: Integral a => a
pattern AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION = VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION
