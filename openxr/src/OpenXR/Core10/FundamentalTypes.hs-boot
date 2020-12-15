{-# language CPP #-}
-- No documentation found for Chapter "FundamentalTypes"
module OpenXR.Core10.FundamentalTypes  ( Extent2Df
                                       , Extent2Di
                                       , Offset2Df
                                       , Offset2Di
                                       , Rect2Df
                                       , Rect2Di
                                       , Time
                                       ) where

import Data.Int (Int64)
import Data.Kind (Type)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
data Extent2Df

instance ToCStruct Extent2Df
instance Show Extent2Df

instance FromCStruct Extent2Df


data Extent2Di

instance ToCStruct Extent2Di
instance Show Extent2Di

instance FromCStruct Extent2Di


data Offset2Df

instance ToCStruct Offset2Df
instance Show Offset2Df

instance FromCStruct Offset2Df


data Offset2Di

instance ToCStruct Offset2Di
instance Show Offset2Di

instance FromCStruct Offset2Di


data Rect2Df

instance ToCStruct Rect2Df
instance Show Rect2Df

instance FromCStruct Rect2Df


data Rect2Di

instance ToCStruct Rect2Di
instance Show Rect2Di

instance FromCStruct Rect2Di


-- | XrTime - Basic type for time
--
-- = Description
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
-- is a base value type that represents time as a signed 64-bit integer,
-- representing the monotonically-increasing count of nanoseconds that have
-- elapsed since a runtime-chosen epoch.
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
-- always represents the time elasped since that constant epoch, rather
-- than a duration or a time point relative to some moving epoch such as
-- vsync time, etc. Durations are instead represented by
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrDuration >.
--
-- A single runtime /must/ use the same epoch for all simultaneous
-- applications. Time /must/ be represented the same regardless of multiple
-- processors or threads present in the system.
--
-- The period precision of time reported by the runtime is
-- runtime-dependent, and /may/ change. One nanosecond is the finest
-- possible period precision. A runtime /may/, for example, report time
-- progression with only microsecond-level granularity.
--
-- Time /must/ not be assumed to correspond to a system clock time.
--
-- Unless specified otherwise, zero or a negative value is not a valid
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >,
-- and related functions /must/ return error
-- 'OpenXR.Core10.Enums.Result.ERROR_TIME_INVALID'. Applications /must/ not
-- initialize such
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
-- fields to a zero value. Instead, applications /should/ always assign
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
-- fields to the meaningful point in time they are choosing to reason
-- about, such as a frame’s predicted display time, or an action’s last
-- change time.
--
-- The behavior of a runtime is undefined when time overflows beyond the
-- maximum positive value that can be represented by an
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >.
-- Runtimes /should/ choose an epoch that minimizes the chance of overflow.
-- Runtimes /should/ also choose an epoch that minimizes the chance of
-- underflow below 0 for applications performing a reasonable amount of
-- historical pose lookback. For example, if the runtime chooses an epoch
-- relative to its startup time, it /should/ push the epoch into the past
-- by enough time to avoid applications performing reasonable pose lookback
-- from reaching a negative
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
-- value.
--
-- An application cannot assume that the system’s clock and the runtime’s
-- clock will maintain a constant relationship across frames and /should/
-- avoid storing such an offset, as this may cause time drift. Applications
-- /should/ instead always use time interop functions to convert a relevant
-- time point across the system’s clock and the runtime’s clock using
-- extensions, for example,
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_win32_convert_performance_counter_time>
-- or
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_convert_timespec_time>.
--
-- = See Also
--
-- 'OpenXR.Core10.Input.ActionStateBoolean',
-- 'OpenXR.Core10.Input.ActionStateFloat',
-- 'OpenXR.Core10.Input.ActionStateVector2f',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrDuration >,
-- 'OpenXR.Core10.OtherTypes.EventDataInstanceLossPending',
-- 'OpenXR.Core10.OtherTypes.EventDataReferenceSpaceChangePending',
-- 'OpenXR.Core10.OtherTypes.EventDataSessionStateChanged',
-- 'OpenXR.Extensions.XR_EXT_eye_gaze_interaction.EyeGazeSampleTimeEXT',
-- 'OpenXR.Core10.DisplayTiming.FrameEndInfo',
-- 'OpenXR.Core10.DisplayTiming.FrameState',
-- 'OpenXR.Extensions.XR_EXT_hand_tracking.HandJointsLocateInfoEXT',
-- 'OpenXR.Extensions.XR_MSFT_hand_tracking_mesh.HandMeshUpdateInfoMSFT',
-- 'OpenXR.Extensions.XR_MSFT_hand_tracking_mesh.HandMeshVertexBufferMSFT',
-- 'OpenXR.Extensions.XR_MSFT_spatial_anchor.SpatialAnchorCreateInfoMSFT',
-- 'OpenXR.Core10.DisplayTiming.ViewLocateInfo',
-- 'OpenXR.Extensions.XR_KHR_convert_timespec_time.convertTimeToTimespecTimeKHR',
-- 'OpenXR.Extensions.XR_KHR_win32_convert_performance_counter_time.convertTimeToWin32PerformanceCounterKHR',
-- 'OpenXR.Extensions.XR_KHR_convert_timespec_time.convertTimespecTimeToTimeKHR',
-- 'OpenXR.Extensions.XR_KHR_win32_convert_performance_counter_time.convertWin32PerformanceCounterToTimeKHR',
-- 'OpenXR.Core10.Space.locateSpace'
type Time = Int64

