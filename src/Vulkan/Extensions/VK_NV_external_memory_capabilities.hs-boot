{-# language CPP #-}
-- | = Name
--
-- VK_NV_external_memory_capabilities - instance extension
--
-- == VK_NV_external_memory_capabilities
--
-- [__Name String__]
--     @VK_NV_external_memory_capabilities@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     56
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Deprecation state__]
--
--     -   /Deprecated/ by @VK_KHR_external_memory_capabilities@ extension
--
--         -   Which in turn was /promoted/ to
--             <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_external_memory_capabilities] @cubanismo%0A<<Here describe the issue or question you have about the VK_NV_external_memory_capabilities extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-08-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with Vulkan 1.1.
--
--     -   Interacts with @VK_KHR_dedicated_allocation@.
--
--     -   Interacts with @VK_NV_dedicated_allocation@.
--
-- [__Contributors__]
--
--     -   James Jones, NVIDIA
--
-- == Description
--
-- Applications may wish to import memory from the Direct 3D API, or export
-- memory to other Vulkan instances. This extension provides a set of
-- capability queries that allow applications determine what types of win32
-- memory handles an implementation supports for a given set of use cases.
--
-- == New Commands
--
-- -   'getPhysicalDeviceExternalImageFormatPropertiesNV'
--
-- == New Structures
--
-- -   'ExternalImageFormatPropertiesNV'
--
-- == New Enums
--
-- -   'ExternalMemoryFeatureFlagBitsNV'
--
-- -   'ExternalMemoryHandleTypeFlagBitsNV'
--
-- == New Bitmasks
--
-- -   'ExternalMemoryFeatureFlagsNV'
--
-- -   'ExternalMemoryHandleTypeFlagsNV'
--
-- == New Enum Constants
--
-- -   'NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME'
--
-- -   'NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION'
--
-- == Issues
--
-- 1) Why do so many external memory capabilities need to be queried on a
-- per-memory-handle-type basis?
--
-- __RESOLVED__: This is because some handle types are based on OS-native
-- objects that have far more limited capabilities than the very generic
-- Vulkan memory objects. Not all memory handle types can name memory
-- objects that support 3D images, for example. Some handle types cannot
-- even support the deferred image and memory binding behavior of Vulkan
-- and require specifying the image when allocating or importing the memory
-- object.
--
-- 2) Does the 'ExternalImageFormatPropertiesNV' struct need to include a
-- list of memory type bits that support the given handle type?
--
-- __RESOLVED__: No. The memory types that do not support the handle types
-- will simply be filtered out of the results returned by
-- 'Vulkan.Core10.MemoryManagement.getImageMemoryRequirements' when a set
-- of handle types was specified at image creation time.
--
-- 3) Should the non-opaque handle types be moved to their own extension?
--
-- __RESOLVED__: Perhaps. However, defining the handle type bits does very
-- little and does not require any platform-specific types on its own, and
-- it is easier to maintain the bitmask values in a single extension for
-- now. Presumably more handle types could be added by separate extensions
-- though, and it would be midly weird to have some platform-specific ones
-- defined in the core spec and some in extensions
--
-- == Version History
--
-- -   Revision 1, 2016-08-19 (James Jones)
--
--     -   Initial version
--
-- = See Also
--
-- 'ExternalImageFormatPropertiesNV', 'ExternalMemoryFeatureFlagBitsNV',
-- 'ExternalMemoryFeatureFlagsNV', 'ExternalMemoryHandleTypeFlagBitsNV',
-- 'ExternalMemoryHandleTypeFlagsNV',
-- 'getPhysicalDeviceExternalImageFormatPropertiesNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_external_memory_capabilities Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_external_memory_capabilities  ( ExternalImageFormatPropertiesNV
                                                             , ExternalMemoryHandleTypeFlagsNV
                                                             , ExternalMemoryHandleTypeFlagBitsNV
                                                             ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ExternalImageFormatPropertiesNV

instance ToCStruct ExternalImageFormatPropertiesNV
instance Show ExternalImageFormatPropertiesNV

instance FromCStruct ExternalImageFormatPropertiesNV


type ExternalMemoryHandleTypeFlagsNV = ExternalMemoryHandleTypeFlagBitsNV

data ExternalMemoryHandleTypeFlagBitsNV

