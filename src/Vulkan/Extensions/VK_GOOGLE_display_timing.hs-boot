{-# language CPP #-}
-- | = Name
--
-- VK_GOOGLE_display_timing - device extension
--
-- == VK_GOOGLE_display_timing
--
-- [__Name String__]
--     @VK_GOOGLE_display_timing@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     93
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_swapchain@
--
-- [__Contact__]
--
--     -   Ian Elliott
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_GOOGLE_display_timing:%20&body=@ianelliottus%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-02-14
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Ian Elliott, Google
--
--     -   Jesse Hall, Google
--
-- == Description
--
-- This device extension allows an application that uses the
-- @VK_KHR_swapchain@ extension to obtain information about the
-- presentation engine’s display, to obtain timing information about each
-- present, and to schedule a present to happen no earlier than a desired
-- time. An application can use this to minimize various visual anomalies
-- (e.g. stuttering).
--
-- Traditional game and real-time animation applications need to correctly
-- position their geometry for when the presentable image will be presented
-- to the user. To accomplish this, applications need various timing
-- information about the presentation engine’s display. They need to know
-- when presentable images were actually presented, and when they could
-- have been presented. Applications also need to tell the presentation
-- engine to display an image no sooner than a given time. This allows the
-- application to avoid stuttering, so the animation looks smooth to the
-- user.
--
-- This extension treats variable-refresh-rate (VRR) displays as if they
-- are fixed-refresh-rate (FRR) displays.
--
-- == New Commands
--
-- -   'getPastPresentationTimingGOOGLE'
--
-- -   'getRefreshCycleDurationGOOGLE'
--
-- == New Structures
--
-- -   'PastPresentationTimingGOOGLE'
--
-- -   'PresentTimeGOOGLE'
--
-- -   'RefreshCycleDurationGOOGLE'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR':
--
--     -   'PresentTimesInfoGOOGLE'
--
-- == New Enum Constants
--
-- -   'GOOGLE_DISPLAY_TIMING_EXTENSION_NAME'
--
-- -   'GOOGLE_DISPLAY_TIMING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE'
--
-- == Examples
--
-- Note
--
-- The example code for the this extension (like the @VK_KHR_surface@ and
-- @VK_GOOGLE_display_timing@ extensions) is contained in the cube demo
-- that is shipped with the official Khronos SDK, and is being kept
-- up-to-date in that location (see:
-- <https://github.com/KhronosGroup/Vulkan-Tools/blob/master/cube/cube.c>
-- ).
--
-- == Version History
--
-- -   Revision 1, 2017-02-14 (Ian Elliott)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'PastPresentationTimingGOOGLE', 'PresentTimeGOOGLE',
-- 'PresentTimesInfoGOOGLE', 'RefreshCycleDurationGOOGLE',
-- 'getPastPresentationTimingGOOGLE', 'getRefreshCycleDurationGOOGLE'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_GOOGLE_display_timing Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_GOOGLE_display_timing  ( PastPresentationTimingGOOGLE
                                                   , PresentTimeGOOGLE
                                                   , PresentTimesInfoGOOGLE
                                                   , RefreshCycleDurationGOOGLE
                                                   ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PastPresentationTimingGOOGLE

instance ToCStruct PastPresentationTimingGOOGLE
instance Show PastPresentationTimingGOOGLE

instance FromCStruct PastPresentationTimingGOOGLE


data PresentTimeGOOGLE

instance ToCStruct PresentTimeGOOGLE
instance Show PresentTimeGOOGLE

instance FromCStruct PresentTimeGOOGLE


data PresentTimesInfoGOOGLE

instance ToCStruct PresentTimesInfoGOOGLE
instance Show PresentTimesInfoGOOGLE

instance FromCStruct PresentTimesInfoGOOGLE


data RefreshCycleDurationGOOGLE

instance ToCStruct RefreshCycleDurationGOOGLE
instance Show RefreshCycleDurationGOOGLE

instance FromCStruct RefreshCycleDurationGOOGLE

