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
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_calibrated_timestamps VK_KHR_calibrated_timestamps>
--         extension
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
-- == Promotion to @VK_KHR_calibrated_timestamps@
--
-- All functionality in this extension is included in
-- @VK_KHR_calibrated_timestamps@, with the suffix changed to KHR. The
-- original enum names are still available as aliases of the KHR
-- functionality.
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
--     -   'STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_calibrated_timestamps.TimeDomainKHR':
--
--     -   'TIME_DOMAIN_CLOCK_MONOTONIC_EXT'
--
--     -   'TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT'
--
--     -   'TIME_DOMAIN_DEVICE_EXT'
--
--     -   'TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT'
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
module Vulkan.Extensions.VK_EXT_calibrated_timestamps  ( pattern STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT
                                                       , pattern TIME_DOMAIN_DEVICE_EXT
                                                       , pattern TIME_DOMAIN_CLOCK_MONOTONIC_EXT
                                                       , pattern TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT
                                                       , pattern TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT
                                                       , getPhysicalDeviceCalibrateableTimeDomainsEXT
                                                       , getCalibratedTimestampsEXT
                                                       , TimeDomainEXT
                                                       , CalibratedTimestampInfoEXT
                                                       , EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION
                                                       , pattern EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION
                                                       , EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME
                                                       , pattern EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME
                                                       , CalibratedTimestampInfoKHR(..)
                                                       , getPhysicalDeviceCalibrateableTimeDomainsKHR
                                                       , getCalibratedTimestampsKHR
                                                       , TimeDomainKHR(..)
                                                       ) where

import Data.String (IsString)
import Vulkan.Extensions.VK_KHR_calibrated_timestamps (getCalibratedTimestampsKHR)
import Vulkan.Extensions.VK_KHR_calibrated_timestamps (getPhysicalDeviceCalibrateableTimeDomainsKHR)
import Vulkan.Extensions.VK_KHR_calibrated_timestamps (CalibratedTimestampInfoKHR)
import Vulkan.Extensions.VK_KHR_calibrated_timestamps (TimeDomainKHR)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_KHR))
import Vulkan.Extensions.VK_KHR_calibrated_timestamps (TimeDomainKHR(TIME_DOMAIN_CLOCK_MONOTONIC_KHR))
import Vulkan.Extensions.VK_KHR_calibrated_timestamps (TimeDomainKHR(TIME_DOMAIN_CLOCK_MONOTONIC_RAW_KHR))
import Vulkan.Extensions.VK_KHR_calibrated_timestamps (TimeDomainKHR(TIME_DOMAIN_DEVICE_KHR))
import Vulkan.Extensions.VK_KHR_calibrated_timestamps (TimeDomainKHR(TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_KHR))
import Vulkan.Extensions.VK_KHR_calibrated_timestamps (getCalibratedTimestampsKHR)
import Vulkan.Extensions.VK_KHR_calibrated_timestamps (getPhysicalDeviceCalibrateableTimeDomainsKHR)
import Vulkan.Extensions.VK_KHR_calibrated_timestamps (CalibratedTimestampInfoKHR(..))
import Vulkan.Extensions.VK_KHR_calibrated_timestamps (TimeDomainKHR(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT"
pattern STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT = STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_KHR


-- No documentation found for TopLevel "VK_TIME_DOMAIN_DEVICE_EXT"
pattern TIME_DOMAIN_DEVICE_EXT = TIME_DOMAIN_DEVICE_KHR


-- No documentation found for TopLevel "VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT"
pattern TIME_DOMAIN_CLOCK_MONOTONIC_EXT = TIME_DOMAIN_CLOCK_MONOTONIC_KHR


-- No documentation found for TopLevel "VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT"
pattern TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT = TIME_DOMAIN_CLOCK_MONOTONIC_RAW_KHR


-- No documentation found for TopLevel "VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT"
pattern TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT = TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_KHR


-- No documentation found for TopLevel "vkGetPhysicalDeviceCalibrateableTimeDomainsEXT"
getPhysicalDeviceCalibrateableTimeDomainsEXT = getPhysicalDeviceCalibrateableTimeDomainsKHR


-- No documentation found for TopLevel "vkGetCalibratedTimestampsEXT"
getCalibratedTimestampsEXT = getCalibratedTimestampsKHR


-- No documentation found for TopLevel "VkTimeDomainEXT"
type TimeDomainEXT = TimeDomainKHR


-- No documentation found for TopLevel "VkCalibratedTimestampInfoEXT"
type CalibratedTimestampInfoEXT = CalibratedTimestampInfoKHR


type EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION"
pattern EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION = 2


type EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME = "VK_EXT_calibrated_timestamps"

-- No documentation found for TopLevel "VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME"
pattern EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME = "VK_EXT_calibrated_timestamps"

