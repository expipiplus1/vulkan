{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_multiview
  ( PhysicalDeviceMultiviewFeaturesKHR
  , PhysicalDeviceMultiviewPropertiesKHR
  , RenderPassMultiviewCreateInfoKHR
  , pattern DEPENDENCY_VIEW_LOCAL_BIT_KHR
  , pattern KHR_MULTIVIEW_EXTENSION_NAME
  , pattern KHR_MULTIVIEW_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHR
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHR
  , pattern STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES
  , pattern DEPENDENCY_VIEW_LOCAL_BIT
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkDependencyFlagBits(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_multiview
  ( pattern VK_KHR_MULTIVIEW_EXTENSION_NAME
  , pattern VK_KHR_MULTIVIEW_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES
  , pattern STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO
  )
import Graphics.Vulkan.Core10.Pass
  ( pattern DEPENDENCY_VIEW_LOCAL_BIT
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview
  ( PhysicalDeviceMultiviewFeatures(..)
  , PhysicalDeviceMultiviewProperties(..)
  , RenderPassMultiviewCreateInfo(..)
  )


type PhysicalDeviceMultiviewFeaturesKHR = PhysicalDeviceMultiviewFeatures
-- TODO: Pattern constructor alias)

type PhysicalDeviceMultiviewPropertiesKHR = PhysicalDeviceMultiviewProperties
-- TODO: Pattern constructor alias)

type RenderPassMultiviewCreateInfoKHR = RenderPassMultiviewCreateInfo
-- TODO: Pattern constructor alias)

-- No documentation found for TopLevel "DEPENDENCY_VIEW_LOCAL_BIT_KHR"
pattern DEPENDENCY_VIEW_LOCAL_BIT_KHR :: VkDependencyFlagBits
pattern DEPENDENCY_VIEW_LOCAL_BIT_KHR = DEPENDENCY_VIEW_LOCAL_BIT

-- No documentation found for TopLevel "VK_KHR_MULTIVIEW_EXTENSION_NAME"
pattern KHR_MULTIVIEW_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_MULTIVIEW_EXTENSION_NAME = VK_KHR_MULTIVIEW_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_MULTIVIEW_SPEC_VERSION"
pattern KHR_MULTIVIEW_SPEC_VERSION :: Integral a => a
pattern KHR_MULTIVIEW_SPEC_VERSION = VK_KHR_MULTIVIEW_SPEC_VERSION

-- No documentation found for TopLevel "STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHR :: VkStructureType
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES

-- No documentation found for TopLevel "STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHR :: VkStructureType
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES

-- No documentation found for TopLevel "STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHR :: VkStructureType
pattern STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHR = STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO
