{-# language CPP #-}
-- | = Name
--
-- VK_EXT_calibrated_timestamps - device extension
--
-- == VK_EXT_calibrated_timestamps
--
-- [__Name String__]
--     @VK_EXT_calibrated_timestamps@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     185
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Contact__]
--
--     -   Daniel Rakos
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_calibrated_timestamps] @drakos-amd%0A*Here describe the issue or question you have about the VK_EXT_calibrated_timestamps extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_calibrated_timestamps.adoc VK_EXT_calibrated_timestamps>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-10-04
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
--     -   Daniel Rakos, AMD
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
-- -   'getCalibratedTimestampsEXT'
--
-- -   'getPhysicalDeviceCalibrateableTimeDomainsEXT'
--
-- == New Structures
--
-- -   'CalibratedTimestampInfoEXT'
--
-- == New Enums
--
-- -   'TimeDomainEXT'
--
-- == New Enum Constants
--
-- -   'EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME'
--
-- -   'EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT'
--
-- == Version History
--
-- -   Revision 2, 2021-03-16 (Lionel Landwerlin)
--
--     -   Specify requirement on device timestamps
--
-- -   Revision 1, 2018-10-04 (Daniel Rakos)
--
--     -   Internal revisions.
--
-- == See Also
--
-- 'CalibratedTimestampInfoEXT', 'TimeDomainEXT',
-- 'getCalibratedTimestampsEXT',
-- 'getPhysicalDeviceCalibrateableTimeDomainsEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_calibrated_timestamps Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_calibrated_timestamps  ( CalibratedTimestampInfoEXT
                                                       , TimeDomainEXT
                                                       ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data CalibratedTimestampInfoEXT

instance ToCStruct CalibratedTimestampInfoEXT
instance Show CalibratedTimestampInfoEXT

instance FromCStruct CalibratedTimestampInfoEXT


data TimeDomainEXT

