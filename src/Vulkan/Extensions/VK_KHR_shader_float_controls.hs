{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_float_controls - device extension
--
-- == VK_KHR_shader_float_controls
--
-- [__Name String__]
--     @VK_KHR_shader_float_controls@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     198
--
-- [__Revision__]
--     4
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
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Alexander Galazin
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_shader_float_controls:%20&body=@alegal-arm%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-09-11
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.2 Core
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_float_controls.html SPV_KHR_float_controls>
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Alexander Galazin, Arm
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Graeme Leese, Broadcom
--
--     -   Daniel Rakos, AMD
--
-- == Description
--
-- The @VK_KHR_shader_float_controls@ extension enables efficient use of
-- floating-point computations through the ability to query and override
-- the implementation’s default behavior for rounding modes, denormals,
-- signed zero, and infinity.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceFloatControlsPropertiesKHR'
--
-- == New Enums
--
-- -   'ShaderFloatControlsIndependenceKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME'
--
-- -   'KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core12.Enums.ShaderFloatControlsIndependence.ShaderFloatControlsIndependence':
--
--     -   'SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY_KHR'
--
--     -   'SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL_KHR'
--
--     -   'SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-DenormPreserve DenormPreserve>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-DenormFlushToZero DenormFlushToZero>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-SignedZeroInfNanPreserve SignedZeroInfNanPreserve>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-RoundingModeRTE RoundingModeRTE>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-RoundingModeRTZ RoundingModeRTZ>
--
-- == Issues
--
-- 1) Which instructions must flush denorms?
--
-- __RESOLVED__: Only floating-point conversion, floating-point arithmetic,
-- floating-point relational (except @OpIsNaN@, @OpIsInf@), and
-- floating-point GLSL.std.450 extended instructions must flush denormals.
--
-- 2) What is the denorm behavior for intermediate results?
--
-- __RESOLVED__: When a SPIR-V instruction is implemented as a sequence of
-- other instructions: - in the @DenormFlushToZero@ execution mode the
-- intermediate instructions may flush denormals, the final result of the
-- sequence /must/ not be denormal. - in the @DenormPreserve@ execution
-- mode denormals must be preserved throughout the whole sequence.
--
-- 3) Do denorm and rounding mode controls apply to @OpSpecConstantOp@?
--
-- __RESOLVED__: Yes, except when the opcode is @OpQuantizeToF16@.
--
-- 4) The SPIR-V specification says that @OpConvertFToU@ and
-- @OpConvertFToS@ unconditionally round towards zero. Do the rounding mode
-- controls specified through the execution modes apply to them?
--
-- __RESOLVED__: No, these instructions unconditionally round towards zero.
--
-- 5) Do any of the \"Pack\" GLSL.std.450 instructions count as conversion
-- instructions and have the rounding mode apply?
--
-- __RESOLVED__: No, only instructions listed in the section \"3.32.11.
-- Conversion Instructions\" of the SPIR-V specification count as
-- conversion instructions.
--
-- 6) When using inf\/nan-ignore mode, what is expected of @OpIsNan@ and
-- @OpIsInf@?
--
-- __RESOLVED__: These instructions must always accurately detect inf\/nan
-- if it is passed to them.
--
-- == Version 4 API incompatibility
--
-- The original versions of @VK_KHR_shader_float_controls@ shipped with
-- booleans named “separateDenormSettings” and
-- “separateRoundingModeSettings”, which at first glance could have
-- indicated “they can all independently set, or not”. However the spec
-- language as written indicated that the 32-bit value could always be set
-- independently, and only the 16- and 64-bit controls needed to be the
-- same if these values were 'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- As a result of this slight disparity, and lack of test coverage for this
-- facet of the extension, we ended up with two different behaviors in the
-- wild, where some implementations worked as written, and others worked
-- based on the naming. As these are hard limits in hardware with reasons
-- for exposure as written, it was not possible to standardise on a single
-- way to make this work within the existing API.
--
-- No known users of this part of the extension exist in the wild, and as
-- such the Vulkan WG took the unusual step of retroactively changing the
-- once boolean value into a tri-state enum, breaking source compatibility.
-- This was however done in such a way as to retain ABI compatibility, in
-- case any code using this did exist; with the numerical values 0 and 1
-- retaining their original specified meaning, and a new value signifying
-- the additional “all need to be set together” state. If any applications
-- exist today, compiled binaries will continue to work as written in most
-- cases, but will need changes before the code can be recompiled.
--
-- == Version History
--
-- -   Revision 4, 2019-06-18 (Tobias Hector)
--
--     -   Modified settings restrictions, see
--         <VK_KHR_shader_controls_v4_incompatibility.html VK_KHR_shader_controls_v4_incompatibility>
--
-- -   Revision 3, 2018-09-11 (Alexander Galazin)
--
--     -   Minor restructuring
--
-- -   Revision 2, 2018-04-17 (Alexander Galazin)
--
--     -   Added issues and resolutions
--
-- -   Revision 1, 2018-04-11 (Alexander Galazin)
--
--     -   Initial draft
--
-- = See Also
--
-- 'PhysicalDeviceFloatControlsPropertiesKHR',
-- 'ShaderFloatControlsIndependenceKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_float_controls Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_float_controls  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR
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
import Vulkan.Core12.Promoted_From_VK_KHR_shader_float_controls (PhysicalDeviceFloatControlsProperties)
import Vulkan.Core12.Enums.ShaderFloatControlsIndependence (ShaderFloatControlsIndependence)
import Vulkan.Core12.Enums.ShaderFloatControlsIndependence (ShaderFloatControlsIndependence(SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY))
import Vulkan.Core12.Enums.ShaderFloatControlsIndependence (ShaderFloatControlsIndependence(SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL))
import Vulkan.Core12.Enums.ShaderFloatControlsIndependence (ShaderFloatControlsIndependence(SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES))
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

