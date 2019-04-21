{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_variable_pointers
  ( PhysicalDeviceVariablePointerFeaturesKHR
  , PhysicalDeviceVariablePointersFeaturesKHR
  , pattern VK_KHR_VARIABLE_POINTERS_SPEC_VERSION
  , pattern VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR
  ) where




import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_variable_pointers
  ( PhysicalDeviceVariablePointersFeatures(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_variable_pointers
  ( pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_variable_pointers
  ( pattern VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME
  , pattern VK_KHR_VARIABLE_POINTERS_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR
  )


type PhysicalDeviceVariablePointerFeaturesKHR = PhysicalDeviceVariablePointersFeatures
-- TODO: Pattern constructor alias)

type PhysicalDeviceVariablePointersFeaturesKHR = PhysicalDeviceVariablePointersFeatures
-- TODO: Pattern constructor alias)
