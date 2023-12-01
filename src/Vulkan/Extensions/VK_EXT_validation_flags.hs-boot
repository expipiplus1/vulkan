{-# language CPP #-}
-- | = Name
--
-- VK_EXT_validation_flags - instance extension
--
-- == VK_EXT_validation_flags
--
-- [__Name String__]
--     @VK_EXT_validation_flags@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     62
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Deprecation State__]
--
--     -   /Deprecated/ by
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_validation_features VK_EXT_validation_features>
--         extension
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Debugging tools>
--
-- [__Contact__]
--
--     -   Tobin Ehlis
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_validation_flags] @tobine%0A*Here describe the issue or question you have about the VK_EXT_validation_flags extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-08-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Tobin Ehlis, Google
--
--     -   Courtney Goeltzenleuchter, Google
--
-- == Description
--
-- This extension provides the 'ValidationFlagsEXT' struct that can be
-- included in the @pNext@ chain of the
-- 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo' structure passed
-- as the @pCreateInfo@ parameter of
-- 'Vulkan.Core10.DeviceInitialization.createInstance'. The structure
-- contains an array of 'ValidationCheckEXT' values that will be disabled
-- by the validation layers.
--
-- == Deprecation by @VK_EXT_validation_features@
--
-- Functionality in this extension is subsumed into the
-- @VK_EXT_validation_features@ extension.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo':
--
--     -   'ValidationFlagsEXT'
--
-- == New Enums
--
-- -   'ValidationCheckEXT'
--
-- == New Enum Constants
--
-- -   'EXT_VALIDATION_FLAGS_EXTENSION_NAME'
--
-- -   'EXT_VALIDATION_FLAGS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_VALIDATION_FLAGS_EXT'
--
-- == Version History
--
-- -   Revision 2, 2019-08-19 (Mark Lobodzinski)
--
--     -   Marked as deprecated
--
-- -   Revision 1, 2016-08-26 (Courtney Goeltzenleuchter)
--
--     -   Initial draft
--
-- == See Also
--
-- 'ValidationCheckEXT', 'ValidationFlagsEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_validation_flags Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_validation_flags  (ValidationFlagsEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ValidationFlagsEXT

instance ToCStruct ValidationFlagsEXT
instance Show ValidationFlagsEXT

instance FromCStruct ValidationFlagsEXT

