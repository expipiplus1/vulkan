{-# language CPP #-}
-- | = Name
--
-- VK_NV_low_latency - device extension
--
-- == VK_NV_low_latency
--
-- [__Name String__]
--     @VK_NV_low_latency@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     311
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__; __Contact__]
--
--     -   Charles Hansen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_low_latency] @cshansen%0A*Here describe the issue or question you have about the VK_NV_low_latency extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-02-10
--
-- [__Contributors__]
--
--     -   Charles Hansen, NVIDIA
--
-- == Description
--
-- This extension adds the 'QueryLowLatencySupportNV' structure, a
-- structure used to query support for NVIDIA Reflex.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.QueueSemaphore.SemaphoreCreateInfo':
--
--     -   'QueryLowLatencySupportNV'
--
-- == New Enum Constants
--
-- -   'NV_LOW_LATENCY_EXTENSION_NAME'
--
-- -   'NV_LOW_LATENCY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUERY_LOW_LATENCY_SUPPORT_NV'
--
-- == Issues
--
-- 1) Why does 'QueryLowLatencySupportNV' have output parameters in an
-- input chain?
--
-- __RESOLVED__: We are stuck with this for legacy reasons - we are aware
-- this is bad behavior and this should not be used as a precedent for
-- future extensions.
--
-- == Version History
--
-- -   Revision 1, 2023-02-10 (Charles Hansen)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'QueryLowLatencySupportNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_low_latency Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_low_latency  (QueryLowLatencySupportNV) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data QueryLowLatencySupportNV

instance ToCStruct QueryLowLatencySupportNV
instance Show QueryLowLatencySupportNV

instance FromCStruct QueryLowLatencySupportNV

