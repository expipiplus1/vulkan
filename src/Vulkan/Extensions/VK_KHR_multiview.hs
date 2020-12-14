{-# language CPP #-}
-- | = Name
--
-- VK_KHR_multiview - device extension
--
-- == VK_KHR_multiview
--
-- [__Name String__]
--     @VK_KHR_multiview@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     54
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
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_multiview:%20&body=@jeffbolznv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-10-28
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.1 Core
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_multiview.html SPV_KHR_multiview>
--
--     -   This extension provides API support for
--         <https://raw.githubusercontent.com/KhronosGroup/GLSL/master/extensions/ext/GL_EXT_multiview.txt GL_EXT_multiview>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension has the same goal as the OpenGL ES @GL_OVR_multiview@
-- extension. Multiview is a rendering technique originally designed for VR
-- where it is more efficient to record a single set of commands to be
-- executed with slightly different behavior for each \"view\".
--
-- It includes a concise way to declare a render pass with multiple views,
-- and gives implementations freedom to render the views in the most
-- efficient way possible. This is done with a multiview configuration
-- specified during
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass render pass>
-- creation with the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'
-- passed into 'Vulkan.Core10.Pass.RenderPassCreateInfo'::@pNext@.
--
-- This extension enables the use of the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_multiview.html SPV_KHR_multiview>
-- shader extension which adds a new @ViewIndex@ built-in type to shaders
-- that allow shaders to control what to do for each view. If using GLSL
-- there is also a
-- <https://raw.githubusercontent.com/KhronosGroup/GLSL/master/extensions/ext/GL_EXT_multiview.txt GL_EXT_multiview>
-- extension that introduces a @highp int gl_ViewIndex;@ built-in variable
-- for vertex, tessellation, geometry, and fragment shaders.
--
-- == Promotion to Vulkan 1.1
--
-- All functionality in this extension is included in core Vulkan 1.1, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMultiviewFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMultiviewPropertiesKHR'
--
-- -   Extending 'Vulkan.Core10.Pass.RenderPassCreateInfo':
--
--     -   'RenderPassMultiviewCreateInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_MULTIVIEW_EXTENSION_NAME'
--
-- -   'KHR_MULTIVIEW_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits':
--
--     -   'DEPENDENCY_VIEW_LOCAL_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHR'
--
--     -   'STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHR'
--
-- == New Built-In Variables
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-viewindex ViewIndex>
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-MultiView MultiView>
--
-- == Version History
--
-- -   Revision 1, 2016-10-28 (Jeff Bolz)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'PhysicalDeviceMultiviewFeaturesKHR',
-- 'PhysicalDeviceMultiviewPropertiesKHR',
-- 'RenderPassMultiviewCreateInfoKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_multiview Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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

