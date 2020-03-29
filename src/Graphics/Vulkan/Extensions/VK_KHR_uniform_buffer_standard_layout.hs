{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_uniform_buffer_standard_layout  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES_KHR
                                                                         , PhysicalDeviceUniformBufferStandardLayoutFeaturesKHR
                                                                         , KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_SPEC_VERSION
                                                                         , pattern KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_SPEC_VERSION
                                                                         , KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME
                                                                         , pattern KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME
                                                                         ) where

import Data.String (IsString)
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout (PhysicalDeviceUniformBufferStandardLayoutFeatures)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES


-- No documentation found for TopLevel "VkPhysicalDeviceUniformBufferStandardLayoutFeaturesKHR"
type PhysicalDeviceUniformBufferStandardLayoutFeaturesKHR = PhysicalDeviceUniformBufferStandardLayoutFeatures


type KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_SPEC_VERSION"
pattern KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_SPEC_VERSION = 1


type KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME = "VK_KHR_uniform_buffer_standard_layout"

-- No documentation found for TopLevel "VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME"
pattern KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME = "VK_KHR_uniform_buffer_standard_layout"

