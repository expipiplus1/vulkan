{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_variable_pointers
  ( PhysicalDeviceVariablePointerFeatures
  , withCStructPhysicalDeviceVariablePointersFeatures
  , fromCStructPhysicalDeviceVariablePointersFeatures
  , PhysicalDeviceVariablePointersFeatures(..)
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_variable_pointers
  ( VkPhysicalDeviceVariablePointersFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


type PhysicalDeviceVariablePointerFeatures = PhysicalDeviceVariablePointersFeatures
-- TODO: Pattern constructor alias)


-- | VkPhysicalDeviceVariablePointersFeatures - Structure describing variable
-- pointers features that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_variable_pointers.VkPhysicalDeviceVariablePointersFeatures'
-- structure describe the following features:
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
-- If the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_variable_pointers.VkPhysicalDeviceVariablePointersFeatures'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_variable_pointers.VkPhysicalDeviceVariablePointersFeatures'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable the
-- features.
--
-- == Valid Usage
--
-- -   If @variablePointers@ is enabled then
--     @variablePointersStorageBuffer@ /must/ also be enabled.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_variable_pointers.VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceVariablePointersFeatures = PhysicalDeviceVariablePointersFeatures
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceVariablePointersFeatures" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceVariablePointersFeatures" "variablePointersStorageBuffer"
  variablePointersStorageBuffer :: Bool
  , -- No documentation found for Nested "PhysicalDeviceVariablePointersFeatures" "variablePointers"
  variablePointers :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceVariablePointersFeatures' and
-- marshal a 'PhysicalDeviceVariablePointersFeatures' into it. The 'VkPhysicalDeviceVariablePointersFeatures' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceVariablePointersFeatures :: PhysicalDeviceVariablePointersFeatures -> (VkPhysicalDeviceVariablePointersFeatures -> IO a) -> IO a
withCStructPhysicalDeviceVariablePointersFeatures marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceVariablePointersFeatures)) (\pPNext -> cont (VkPhysicalDeviceVariablePointersFeatures VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES pPNext (boolToBool32 (variablePointersStorageBuffer (marshalled :: PhysicalDeviceVariablePointersFeatures))) (boolToBool32 (variablePointers (marshalled :: PhysicalDeviceVariablePointersFeatures)))))

-- | A function to read a 'VkPhysicalDeviceVariablePointersFeatures' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceVariablePointersFeatures'.
fromCStructPhysicalDeviceVariablePointersFeatures :: VkPhysicalDeviceVariablePointersFeatures -> IO PhysicalDeviceVariablePointersFeatures
fromCStructPhysicalDeviceVariablePointersFeatures c = PhysicalDeviceVariablePointersFeatures <$> -- Univalued Member elided
                                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceVariablePointersFeatures)))
                                                                                             <*> pure (bool32ToBool (vkVariablePointersStorageBuffer (c :: VkPhysicalDeviceVariablePointersFeatures)))
                                                                                             <*> pure (bool32ToBool (vkVariablePointers (c :: VkPhysicalDeviceVariablePointersFeatures)))

instance Zero PhysicalDeviceVariablePointersFeatures where
  zero = PhysicalDeviceVariablePointersFeatures Nothing
                                                False
                                                False


-- No documentation found for TopLevel "STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES :: VkStructureType
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES = STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES
