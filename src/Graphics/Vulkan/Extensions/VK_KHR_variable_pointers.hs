{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_variable_pointers
  ( PhysicalDeviceVariablePointerFeaturesKHR
  , PhysicalDeviceVariablePointersFeaturesKHR
  , pattern KHR_VARIABLE_POINTERS_EXTENSION_NAME
  , pattern KHR_VARIABLE_POINTERS_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_variable_pointers
  ( pattern VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME
  , pattern VK_KHR_VARIABLE_POINTERS_SPEC_VERSION
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_variable_pointers
  ( PhysicalDeviceVariablePointersFeatures(..)
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
  )


type PhysicalDeviceVariablePointerFeaturesKHR = PhysicalDeviceVariablePointersFeatures
-- TODO: Pattern constructor alias)

type PhysicalDeviceVariablePointersFeaturesKHR = PhysicalDeviceVariablePointersFeatures
-- TODO: Pattern constructor alias)

-- No documentation found for TopLevel "VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME"
pattern KHR_VARIABLE_POINTERS_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_VARIABLE_POINTERS_EXTENSION_NAME = VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_VARIABLE_POINTERS_SPEC_VERSION"
pattern KHR_VARIABLE_POINTERS_SPEC_VERSION :: Integral a => a
pattern KHR_VARIABLE_POINTERS_SPEC_VERSION = VK_KHR_VARIABLE_POINTERS_SPEC_VERSION

-- No documentation found for TopLevel "STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR :: VkStructureType
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES

-- No documentation found for TopLevel "STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR :: VkStructureType
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
