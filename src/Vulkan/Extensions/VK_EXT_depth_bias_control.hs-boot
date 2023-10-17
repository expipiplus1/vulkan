{-# language CPP #-}
-- | = Name
--
-- VK_EXT_depth_bias_control - device extension
--
-- == VK_EXT_depth_bias_control
--
-- [__Name String__]
--     @VK_EXT_depth_bias_control@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     284
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
-- [__Contact__]
--
--     -   Joshua Ashton
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_depth_bias_control] @Joshua-Ashton%0A*Here describe the issue or question you have about the VK_EXT_depth_bias_control extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_depth_bias_control.adoc VK_EXT_depth_bias_control>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-02-15
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Joshua Ashton, VALVE
--
--     -   Hans-Kristian Arntzen, VALVE
--
--     -   Mike Blumenkrantz, VALVE
--
--     -   Georg Lehmann, VALVE
--
--     -   Piers Daniell, NVIDIA
--
--     -   Lionel Landwerlin, INTEL
--
--     -   Tobias Hector, AMD
--
--     -   Ricardo Garcia, IGALIA
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Shahbaz Youssefi, GOOGLE
--
--     -   Tom Olson, ARM
--
-- == Description
--
-- This extension adds a new structure, 'DepthBiasRepresentationInfoEXT',
-- that can be added to a @pNext@ chain of
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo' and allows
-- setting the scaling and representation of depth bias for a pipeline.
--
-- This state can also be set dynamically by using the new structure
-- mentioned above in combination with the new 'cmdSetDepthBias2EXT'
-- command.
--
-- == New Commands
--
-- -   'cmdSetDepthBias2EXT'
--
-- == New Structures
--
-- -   'DepthBiasInfoEXT'
--
-- -   Extending 'DepthBiasInfoEXT',
--     'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo':
--
--     -   'DepthBiasRepresentationInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDepthBiasControlFeaturesEXT'
--
-- == New Enums
--
-- -   'DepthBiasRepresentationEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DEPTH_BIAS_CONTROL_EXTENSION_NAME'
--
-- -   'EXT_DEPTH_BIAS_CONTROL_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEPTH_BIAS_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEPTH_BIAS_REPRESENTATION_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_BIAS_CONTROL_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-09-22 (Joshua Ashton)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'DepthBiasInfoEXT', 'DepthBiasRepresentationEXT',
-- 'DepthBiasRepresentationInfoEXT',
-- 'PhysicalDeviceDepthBiasControlFeaturesEXT', 'cmdSetDepthBias2EXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_depth_bias_control Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_depth_bias_control  ( DepthBiasInfoEXT
                                                    , DepthBiasRepresentationInfoEXT
                                                    , PhysicalDeviceDepthBiasControlFeaturesEXT
                                                    ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
type role DepthBiasInfoEXT nominal
data DepthBiasInfoEXT (es :: [Type])

instance ( Extendss DepthBiasInfoEXT es
         , PokeChain es ) => ToCStruct (DepthBiasInfoEXT es)
instance Show (Chain es) => Show (DepthBiasInfoEXT es)

instance ( Extendss DepthBiasInfoEXT es
         , PeekChain es ) => FromCStruct (DepthBiasInfoEXT es)


data DepthBiasRepresentationInfoEXT

instance ToCStruct DepthBiasRepresentationInfoEXT
instance Show DepthBiasRepresentationInfoEXT

instance FromCStruct DepthBiasRepresentationInfoEXT


data PhysicalDeviceDepthBiasControlFeaturesEXT

instance ToCStruct PhysicalDeviceDepthBiasControlFeaturesEXT
instance Show PhysicalDeviceDepthBiasControlFeaturesEXT

instance FromCStruct PhysicalDeviceDepthBiasControlFeaturesEXT

