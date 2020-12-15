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
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Daniel Rakos
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_calibrated_timestamps:%20&body=@drakos-amd%20 >
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
--     -   Jason Ekstrand, Intel
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
-- == Issues
--
-- 1) Is the device timestamp value returned in the same time domain as the
-- timestamp values written by
-- 'Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp'?
--
-- __RESOLVED__: Yes.
--
-- 2) What time domain is the host timestamp returned in?
--
-- __RESOLVED__: A query is provided to determine the calibrateable time
-- domains. The expected host time domain used on Windows is that of
-- QueryPerformanceCounter, and on Linux that of CLOCK_MONOTONIC.
--
-- 3) Should we support other time domain combinations than just one host
-- and the device time domain?
--
-- __RESOLVED__: Supporting that would need the application to query the
-- set of supported time domains, while supporting only one host and the
-- device time domain would only need a query for the host time domain
-- type. The proposed API chooses the general approach for the sake of
-- extensibility.
--
-- 4) Shouldn’t we use CLOCK_MONOTONIC_RAW instead of CLOCK_MONOTONIC?
--
-- __RESOLVED__: CLOCK_MONOTONIC is usable in a wider set of situations,
-- however, it is subject to NTP adjustments so some use cases may prefer
-- CLOCK_MONOTONIC_RAW. Thus this extension allows both to be exposed.
--
-- 5) How can the application extrapolate future device timestamp values
-- from the calibrated timestamp value?
--
-- __RESOLVED__:
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@timestampPeriod@
-- makes it possible to calculate future device timestamps as follows:
--
-- > futureTimestamp = calibratedTimestamp + deltaNanoseconds / timestampPeriod
--
-- 6) Can the host and device timestamp values drift apart over longer
-- periods of time?
--
-- __RESOLVED__: Yes, especially as some time domains by definition allow
-- for that to happen (e.g. CLOCK_MONOTONIC is subject to NTP adjustments).
-- Thus it’s recommended that applications re-calibrate from time to time.
--
-- 7) Should we add a query for reporting the maximum deviation of the
-- timestamp values returned by calibrated timestamp queries?
--
-- __RESOLVED__: A global query seems inappropriate and difficult to
-- enforce. However, it’s possible to return the maximum deviation any
-- single calibrated timestamp query can have by sampling one of the time
-- domains twice as follows:
--
-- > timestampX = timestampX_before = SampleTimeDomain(X)
-- > for each time domain Y != X
-- >     timestampY = SampleTimeDomain(Y)
-- > timestampX_after = SampleTimeDomain(X)
-- > maxDeviation = timestampX_after - timestampX_before
--
-- 8) Can the maximum deviation reported ever be zero?
--
-- __RESOLVED__: Unless the tick of each clock corresponding to the set of
-- time domains coincides and all clocks can literally be sampled
-- simutaneously, there isn’t really a possibility for the maximum
-- deviation to be zero, so by convention the maximum deviation is always
-- at least the maximum of the length of the ticks of the set of time
-- domains calibrated and thus can never be zero.
--
-- == Version History
--
-- -   Revision 1, 2018-10-04 (Daniel Rakos)
--
--     -   Internal revisions.
--
-- = See Also
--
-- 'CalibratedTimestampInfoEXT', 'TimeDomainEXT',
-- 'getCalibratedTimestampsEXT',
-- 'getPhysicalDeviceCalibrateableTimeDomainsEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_calibrated_timestamps Vulkan Specification>
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

