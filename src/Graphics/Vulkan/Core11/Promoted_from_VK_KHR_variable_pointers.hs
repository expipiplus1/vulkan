{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_variable_pointers
  ( PhysicalDeviceVariablePointerFeatures
#if defined(VK_USE_PLATFORM_GGP)
  , PhysicalDeviceVariablePointersFeatures(..)
#endif
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES
  ) where




import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif


type PhysicalDeviceVariablePointerFeatures = PhysicalDeviceVariablePointersFeatures
-- TODO: Pattern constructor alias)


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceVariablePointersFeatures"
data PhysicalDeviceVariablePointersFeatures = PhysicalDeviceVariablePointersFeatures
  { -- No documentation found for Nested "PhysicalDeviceVariablePointersFeatures" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceVariablePointersFeatures" "variablePointersStorageBuffer"
  variablePointersStorageBuffer :: Bool
  , -- No documentation found for Nested "PhysicalDeviceVariablePointersFeatures" "variablePointers"
  variablePointers :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceVariablePointersFeatures where
  zero = PhysicalDeviceVariablePointersFeatures Nothing
                                                False
                                                False

#endif

-- No documentation found for TopLevel "STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES :: VkStructureType
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES = STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES
