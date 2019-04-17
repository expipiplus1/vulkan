{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_variable_pointers
  ( PhysicalDeviceVariablePointerFeatures
  , withCStructPhysicalDeviceVariablePointersFeatures
  , fromCStructPhysicalDeviceVariablePointersFeatures
  , PhysicalDeviceVariablePointersFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
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
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_variable_pointers
  ( VkPhysicalDeviceVariablePointersFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES
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
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_variable_pointers
  ( pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
  )


type PhysicalDeviceVariablePointerFeatures = PhysicalDeviceVariablePointersFeatures
-- TODO: Pattern constructor alias)
-- No documentation found for TopLevel "PhysicalDeviceVariablePointersFeatures"
data PhysicalDeviceVariablePointersFeatures = PhysicalDeviceVariablePointersFeatures
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceVariablePointersFeatures" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceVariablePointersFeatures" "variablePointersStorageBuffer"
  vkVariablePointersStorageBuffer :: Bool
  , -- No documentation found for Nested "PhysicalDeviceVariablePointersFeatures" "variablePointers"
  vkVariablePointers :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceVariablePointersFeatures :: PhysicalDeviceVariablePointersFeatures -> (VkPhysicalDeviceVariablePointersFeatures -> IO a) -> IO a
withCStructPhysicalDeviceVariablePointersFeatures from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceVariablePointersFeatures)) (\pPNext -> cont (VkPhysicalDeviceVariablePointersFeatures VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES pPNext (boolToBool32 (vkVariablePointersStorageBuffer (from :: PhysicalDeviceVariablePointersFeatures))) (boolToBool32 (vkVariablePointers (from :: PhysicalDeviceVariablePointersFeatures)))))
fromCStructPhysicalDeviceVariablePointersFeatures :: VkPhysicalDeviceVariablePointersFeatures -> IO PhysicalDeviceVariablePointersFeatures
fromCStructPhysicalDeviceVariablePointersFeatures c = PhysicalDeviceVariablePointersFeatures <$> -- Univalued Member elided
                                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceVariablePointersFeatures)))
                                                                                             <*> pure (bool32ToBool (vkVariablePointersStorageBuffer (c :: VkPhysicalDeviceVariablePointersFeatures)))
                                                                                             <*> pure (bool32ToBool (vkVariablePointers (c :: VkPhysicalDeviceVariablePointersFeatures)))
instance Zero PhysicalDeviceVariablePointersFeatures where
  zero = PhysicalDeviceVariablePointersFeatures Nothing
                                                False
                                                False
