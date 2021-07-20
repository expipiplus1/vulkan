{-# language CPP #-}
-- | = Name
--
-- VK_EXT_provoking_vertex - device extension
--
-- == VK_EXT_provoking_vertex
--
-- [__Name String__]
--     @VK_EXT_provoking_vertex@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     255
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   Jesse Hall
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_provoking_vertex:%20&body=@jessehall%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-02-22
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Alexis Hétu, Google
--
--     -   Bill Licea-Kane, Qualcomm
--
--     -   Daniel Koch, Nvidia
--
--     -   Jamie Madill, Google
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Jason Ekstrand, Intel
--
--     -   Jeff Bolz, Nvidia
--
--     -   Jeff Leger, Qualcomm
--
--     -   Jesse Hall, Google
--
--     -   Jörg Wagner, Arm
--
--     -   Matthew Netsch, Qualcomm
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Piers Daniell, Nvidia
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- This extension allows changing the provoking vertex convention between
-- Vulkan’s default convention (first vertex) and OpenGL’s convention (last
-- vertex).
--
-- This extension is intended for use by API-translation layers that
-- implement APIs like OpenGL on top of Vulkan, and need to match the
-- source API’s provoking vertex convention. Applications using Vulkan
-- directly should use Vulkan’s default convention.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceProvokingVertexFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceProvokingVertexPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo':
--
--     -   'PipelineRasterizationProvokingVertexStateCreateInfoEXT'
--
-- == New Enums
--
-- -   'ProvokingVertexModeEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PROVOKING_VERTEX_EXTENSION_NAME'
--
-- -   'EXT_PROVOKING_VERTEX_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PROVOKING_VERTEX_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PROVOKING_VERTEX_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_PROVOKING_VERTEX_STATE_CREATE_INFO_EXT'
--
-- == Issues
--
-- 1) At what granularity should this state be set?
--
-- __RESOLVED__: At pipeline bind, with an optional per-renderpass
-- restriction.
--
-- The most natural place to put this state is in the graphics pipeline
-- object. Some implementations require it to be known when creating the
-- pipeline, and pipeline state is convenient for implementing OpenGL 3.2’s
-- glProvokingVertex, which can change the state between draw calls.
-- However, some implementations can only change it approximately
-- renderpass granularity. To accommodate both, provoking vertex will be
-- pipeline state, but implementations can require that only one mode is
-- used within a renderpass instance; the renderpass’s mode is chosen
-- implicitly when the first pipeline is bound.
--
-- 2) Does the provoking vertex mode affect the order that vertices are
-- written to transform feedback buffers?
--
-- __RESOLVED__: Yes, to enable layered implementations of OpenGL and D3D.
--
-- All of OpenGL, OpenGL ES, and Direct3D 11 require that vertices are
-- written to transform feedback buffers such that flat-shaded attributes
-- have the same value when drawing the contents of the transform feedback
-- buffer as they did in the original drawing when the transform feedback
-- buffer was written (assuming the provoking vertex mode has not changed,
-- in APIs that support more than one mode).
--
-- == Version History
--
-- -   Revision 1(c), 2021-02-22 (Jesse Hall)
--
--     -   Added
--         VkPhysicalDeviceProvokingVertexPropertiesEXT::transformFeedbackPreservesTriangleFanProvokingVertex
--         to accommodate implementations that cannot change the transform
--         feedback vertex order for triangle fans.
--
-- -   Revision 1(b), 2020-06-14 (Jesse Hall)
--
--     -   Added
--         VkPhysicalDeviceProvokingVertexFeaturesEXT::transformFeedbackPreservesProvokingVertex
--         and required that transform feedback write vertices so as to
--         preserve the provoking vertex of each primitive.
--
-- -   Revision 1(a), 2019-10-23 (Jesse Hall)
--
--     -   Initial draft, based on a proposal by Alexis Hétu
--
-- = See Also
--
-- 'PhysicalDeviceProvokingVertexFeaturesEXT',
-- 'PhysicalDeviceProvokingVertexPropertiesEXT',
-- 'PipelineRasterizationProvokingVertexStateCreateInfoEXT',
-- 'ProvokingVertexModeEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_provoking_vertex Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_provoking_vertex  ( PhysicalDeviceProvokingVertexFeaturesEXT
                                                  , PhysicalDeviceProvokingVertexPropertiesEXT
                                                  , PipelineRasterizationProvokingVertexStateCreateInfoEXT
                                                  ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceProvokingVertexFeaturesEXT

instance ToCStruct PhysicalDeviceProvokingVertexFeaturesEXT
instance Show PhysicalDeviceProvokingVertexFeaturesEXT

instance FromCStruct PhysicalDeviceProvokingVertexFeaturesEXT


data PhysicalDeviceProvokingVertexPropertiesEXT

instance ToCStruct PhysicalDeviceProvokingVertexPropertiesEXT
instance Show PhysicalDeviceProvokingVertexPropertiesEXT

instance FromCStruct PhysicalDeviceProvokingVertexPropertiesEXT


data PipelineRasterizationProvokingVertexStateCreateInfoEXT

instance ToCStruct PipelineRasterizationProvokingVertexStateCreateInfoEXT
instance Show PipelineRasterizationProvokingVertexStateCreateInfoEXT

instance FromCStruct PipelineRasterizationProvokingVertexStateCreateInfoEXT

