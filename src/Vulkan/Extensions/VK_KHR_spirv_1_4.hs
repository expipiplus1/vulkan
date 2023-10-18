{-# language CPP #-}
-- | = Name
--
-- VK_KHR_spirv_1_4 - device extension
--
-- == VK_KHR_spirv_1_4
--
-- [__Name String__]
--     @VK_KHR_spirv_1_4@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     237
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_float_controls VK_KHR_shader_float_controls>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Jesse Hall
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_spirv_1_4] @critsec%0A*Here describe the issue or question you have about the VK_KHR_spirv_1_4 extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-04-01
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Requires SPIR-V 1.4.
--
--     -   Promoted to Vulkan 1.2 Core
--
-- [__Contributors__]
--
--     -   Alexander Galazin, Arm
--
--     -   David Neto, Google
--
--     -   Jesse Hall, Google
--
--     -   John Kessenich, Google
--
--     -   Neil Henning, AMD
--
--     -   Tom Olson, Arm
--
-- == Description
--
-- This extension allows the use of SPIR-V 1.4 shader modules. SPIR-V 1.4’s
-- new features primarily make it an easier target for compilers from
-- high-level languages, rather than exposing new hardware functionality.
--
-- SPIR-V 1.4 incorporates features that are also available separately as
-- extensions. SPIR-V 1.4 shader modules do not need to enable those
-- extensions with the @OpExtension@ opcode, since they are integral parts
-- of SPIR-V 1.4.
--
-- SPIR-V 1.4 introduces new floating point execution mode capabilities,
-- also available via @SPV_KHR_float_controls@. Implementations are not
-- required to support all of these new capabilities; support can be
-- queried using
-- 'Vulkan.Extensions.VK_KHR_shader_float_controls.PhysicalDeviceFloatControlsPropertiesKHR'
-- from the @VK_KHR_shader_float_controls@ extension.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Enum Constants
--
-- -   'KHR_SPIRV_1_4_EXTENSION_NAME'
--
-- -   'KHR_SPIRV_1_4_SPEC_VERSION'
--
-- == Issues
--
-- 1. Should we have an extension specific to this SPIR-V version, or add a
-- version-generic query for SPIR-V version? SPIR-V 1.4 does not need any
-- other API changes.
--
-- __RESOLVED__: Just expose SPIR-V 1.4.
--
-- Most new SPIR-V versions introduce optionally-required capabilities or
-- have implementation-defined limits, and would need more API and
-- specification changes specific to that version to make them available in
-- Vulkan. For example, to support the subgroup capabilities added in
-- SPIR-V 1.3 required introducing
-- 'Vulkan.Core11.Originally_Based_On_VK_KHR_subgroup.PhysicalDeviceSubgroupProperties'
-- to allow querying the supported group operation categories, maximum
-- supported subgroup size, etc. While we could expose the parts of a new
-- SPIR-V version that do not need accompanying changes generically, we
-- will still end up writing extensions specific to each version for the
-- remaining parts. Thus the generic mechanism will not reduce future
-- spec-writing effort. In addition, making it clear which parts of a
-- future version are supported by the generic mechanism and which cannot
-- be used without specific support would be difficult to get right ahead
-- of time.
--
-- 2. Can different stages of the same pipeline use shaders with different
-- SPIR-V versions?
--
-- __RESOLVED__: Yes.
--
-- Mixing SPIR-V versions 1.0-1.3 in the same pipeline has not been
-- disallowed, so it would be inconsistent to disallow mixing 1.4 with
-- previous versions. SPIR-V 1.4 does not introduce anything that should
-- cause new difficulties here.
--
-- 3. Must Vulkan extensions corresponding to SPIR-V extensions that were
-- promoted to core in 1.4 be enabled in order to use that functionality in
-- a SPIR-V 1.4 module?
--
-- __RESOLVED__: No, with caveats.
--
-- The SPIR-V 1.4 module does not need to declare the SPIR-V extensions,
-- since the functionality is now part of core, so there is no need to
-- enable the Vulkan extension that allows SPIR-V modules to declare the
-- SPIR-V extension. However, when the functionality that is now core in
-- SPIR-V 1.4 is optionally supported, the query for support is provided by
-- a Vulkan extension, and that query can only be used if the extension is
-- enabled.
--
-- This applies to any SPIR-V version; specifically for SPIR-V 1.4 this
-- only applies to the functionality from @SPV_KHR_float_controls@, which
-- was made available in Vulkan by @VK_KHR_shader_float_controls@. Even
-- though the extension was promoted in SPIR-V 1.4, the capabilities are
-- still optional in implementations that support @VK_KHR_spirv_1_4@.
--
-- A SPIR-V 1.4 module does not need to enable @SPV_KHR_float_controls@ in
-- order to use the capabilities, so if the application has /a priori/
-- knowledge that the implementation supports the capabilities, it does not
-- need to enable @VK_KHR_shader_float_controls@. However, if it does not
-- have this knowledge and has to query for support at runtime, it must
-- enable @VK_KHR_shader_float_controls@ in order to use
-- 'Vulkan.Extensions.VK_KHR_shader_float_controls.PhysicalDeviceFloatControlsPropertiesKHR'.
--
-- == Version History
--
-- -   Revision 1, 2019-04-01 (Jesse Hall)
--
--     -   Internal draft versions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_spirv_1_4 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_spirv_1_4  ( KHR_SPIRV_1_4_SPEC_VERSION
                                           , pattern KHR_SPIRV_1_4_SPEC_VERSION
                                           , KHR_SPIRV_1_4_EXTENSION_NAME
                                           , pattern KHR_SPIRV_1_4_EXTENSION_NAME
                                           ) where

import Data.String (IsString)

type KHR_SPIRV_1_4_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SPIRV_1_4_SPEC_VERSION"
pattern KHR_SPIRV_1_4_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SPIRV_1_4_SPEC_VERSION = 1


type KHR_SPIRV_1_4_EXTENSION_NAME = "VK_KHR_spirv_1_4"

-- No documentation found for TopLevel "VK_KHR_SPIRV_1_4_EXTENSION_NAME"
pattern KHR_SPIRV_1_4_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SPIRV_1_4_EXTENSION_NAME = "VK_KHR_spirv_1_4"

