{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_multiview
  ( PhysicalDeviceMultiviewFeaturesKHR
  , PhysicalDeviceMultiviewPropertiesKHR
  , RenderPassMultiviewCreateInfoKHR
  , pattern VK_KHR_MULTIVIEW_SPEC_VERSION
  , pattern VK_KHR_MULTIVIEW_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES
  , pattern VK_DEPENDENCY_VIEW_LOCAL_BIT_KHR
  , pattern VK_DEPENDENCY_VIEW_LOCAL_BIT
  ) where




import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview
  ( PhysicalDeviceMultiviewFeatures(..)
  , PhysicalDeviceMultiviewProperties(..)
  , RenderPassMultiviewCreateInfo(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview
  ( pattern VK_DEPENDENCY_VIEW_LOCAL_BIT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_multiview
  ( pattern VK_DEPENDENCY_VIEW_LOCAL_BIT_KHR
  , pattern VK_KHR_MULTIVIEW_EXTENSION_NAME
  , pattern VK_KHR_MULTIVIEW_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHR
  )


type PhysicalDeviceMultiviewFeaturesKHR = PhysicalDeviceMultiviewFeatures
-- TODO: Pattern constructor alias)

type PhysicalDeviceMultiviewPropertiesKHR = PhysicalDeviceMultiviewProperties
-- TODO: Pattern constructor alias)

type RenderPassMultiviewCreateInfoKHR = RenderPassMultiviewCreateInfo
-- TODO: Pattern constructor alias)
