{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_shader_float_controls  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR
                                                                , pattern SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY_KHR
                                                                , pattern SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL_KHR
                                                                , pattern SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE_KHR
                                                                , ShaderFloatControlsIndependenceKHR
                                                                , PhysicalDeviceFloatControlsPropertiesKHR
                                                                , KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION
                                                                , pattern KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION
                                                                , KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
                                                                , pattern KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
                                                                ) where

import Data.String (IsString)
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_float_controls (PhysicalDeviceFloatControlsProperties)
import Graphics.Vulkan.Core12.Enums.ShaderFloatControlsIndependence (ShaderFloatControlsIndependence)
import Graphics.Vulkan.Core12.Enums.ShaderFloatControlsIndependence (ShaderFloatControlsIndependence(SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY))
import Graphics.Vulkan.Core12.Enums.ShaderFloatControlsIndependence (ShaderFloatControlsIndependence(SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL))
import Graphics.Vulkan.Core12.Enums.ShaderFloatControlsIndependence (ShaderFloatControlsIndependence(SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES


-- No documentation found for TopLevel "VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY_KHR"
pattern SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY_KHR = SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY


-- No documentation found for TopLevel "VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL_KHR"
pattern SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL_KHR = SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL


-- No documentation found for TopLevel "VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE_KHR"
pattern SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE_KHR = SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE


-- No documentation found for TopLevel "VkShaderFloatControlsIndependenceKHR"
type ShaderFloatControlsIndependenceKHR = ShaderFloatControlsIndependence


-- No documentation found for TopLevel "VkPhysicalDeviceFloatControlsPropertiesKHR"
type PhysicalDeviceFloatControlsPropertiesKHR = PhysicalDeviceFloatControlsProperties


type KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION = 4

-- No documentation found for TopLevel "VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION"
pattern KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION = 4


type KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME = "VK_KHR_shader_float_controls"

-- No documentation found for TopLevel "VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME"
pattern KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME = "VK_KHR_shader_float_controls"

