{-# language CPP #-}
-- | = Name
--
-- VK_KHR_calibrated_timestamps - device extension
--
-- == VK_KHR_calibrated_timestamps
--
-- [__Name String__]
--     @VK_KHR_calibrated_timestamps@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     544
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Contact__]
--
--     -   Daniel Rakos
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_calibrated_timestamps] @aqnuep%0A*Here describe the issue or question you have about the VK_KHR_calibrated_timestamps extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_calibrated_timestamps.adoc VK_EXT_calibrated_timestamps>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-07-12
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Alan Harrison, AMD
--
--     -   Derrick Owens, AMD
--
--     -   Daniel Rakos, RasterGrid
--
--     -   Faith Ekstrand, Intel
--
--     -   Keith Packard, Valve
--
-- == Description
--
-- This extension provides an interface to query calibrated timestamps
-- obtained quasi simultaneously from two time domains.
--
-- == New Commands
--
-- -   'getCalibratedTimestampsKHR'
--
-- -   'getPhysicalDeviceCalibrateableTimeDomainsKHR'
--
-- == New Structures
--
-- -   'CalibratedTimestampInfoKHR'
--
-- == New Enums
--
-- -   'TimeDomainKHR'
--
-- == New Enum Constants
--
-- -   'KHR_CALIBRATED_TIMESTAMPS_EXTENSION_NAME'
--
-- -   'KHR_CALIBRATED_TIMESTAMPS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_KHR'
--
-- == Version History
--
-- -   Revision 1, 2023-07-12 (Daniel Rakos)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'CalibratedTimestampInfoKHR', 'TimeDomainKHR',
-- 'getCalibratedTimestampsKHR',
-- 'getPhysicalDeviceCalibrateableTimeDomainsKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_calibrated_timestamps Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_calibrated_timestamps  ( CalibratedTimestampInfoKHR
                                                       , TimeDomainKHR
                                                       ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data CalibratedTimestampInfoKHR

instance ToCStruct CalibratedTimestampInfoKHR
instance Show CalibratedTimestampInfoKHR

instance FromCStruct CalibratedTimestampInfoKHR


data TimeDomainKHR

