{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_multiview  ( pattern STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHR
                                           , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHR
                                           , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHR
                                           , pattern DEPENDENCY_VIEW_LOCAL_BIT_KHR
                                           , PhysicalDeviceMultiviewFeaturesKHR
                                           , PhysicalDeviceMultiviewPropertiesKHR
                                           , RenderPassMultiviewCreateInfoKHR
                                           , KHR_MULTIVIEW_SPEC_VERSION
                                           , pattern KHR_MULTIVIEW_SPEC_VERSION
                                           , KHR_MULTIVIEW_EXTENSION_NAME
                                           , pattern KHR_MULTIVIEW_EXTENSION_NAME
                                           ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_multiview (PhysicalDeviceMultiviewFeatures)
import Vulkan.Core11.Promoted_From_VK_KHR_multiview (PhysicalDeviceMultiviewProperties)
import Vulkan.Core11.Promoted_From_VK_KHR_multiview (RenderPassMultiviewCreateInfo)
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlags)
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlagBits(DEPENDENCY_VIEW_LOCAL_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHR = STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES


-- No documentation found for TopLevel "VK_DEPENDENCY_VIEW_LOCAL_BIT_KHR"
pattern DEPENDENCY_VIEW_LOCAL_BIT_KHR = DEPENDENCY_VIEW_LOCAL_BIT


-- No documentation found for TopLevel "VkPhysicalDeviceMultiviewFeaturesKHR"
type PhysicalDeviceMultiviewFeaturesKHR = PhysicalDeviceMultiviewFeatures


-- No documentation found for TopLevel "VkPhysicalDeviceMultiviewPropertiesKHR"
type PhysicalDeviceMultiviewPropertiesKHR = PhysicalDeviceMultiviewProperties


-- No documentation found for TopLevel "VkRenderPassMultiviewCreateInfoKHR"
type RenderPassMultiviewCreateInfoKHR = RenderPassMultiviewCreateInfo


type KHR_MULTIVIEW_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_MULTIVIEW_SPEC_VERSION"
pattern KHR_MULTIVIEW_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_MULTIVIEW_SPEC_VERSION = 1


type KHR_MULTIVIEW_EXTENSION_NAME = "VK_KHR_multiview"

-- No documentation found for TopLevel "VK_KHR_MULTIVIEW_EXTENSION_NAME"
pattern KHR_MULTIVIEW_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_MULTIVIEW_EXTENSION_NAME = "VK_KHR_multiview"

