{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing
  ( VkPastPresentationTimingGOOGLE(..)
  , VkPresentTimeGOOGLE(..)
  , VkPresentTimesInfoGOOGLE(..)
  , VkRefreshCycleDurationGOOGLE(..)
  , FN_vkGetPastPresentationTimingGOOGLE
  , PFN_vkGetPastPresentationTimingGOOGLE
  , vkGetPastPresentationTimingGOOGLE
  , FN_vkGetRefreshCycleDurationGOOGLE
  , PFN_vkGetRefreshCycleDurationGOOGLE
  , vkGetRefreshCycleDurationGOOGLE
  , pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME
  , pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  , Word64
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkSwapchainKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkPastPresentationTimingGOOGLE - Structure containing timing information
-- about a previously-presented image
--
-- = Description
--
-- The results for a given @swapchain@ and @presentID@ are only returned
-- once from 'vkGetPastPresentationTimingGOOGLE'.
--
-- The application /can/ use the 'VkPastPresentationTimingGOOGLE' values to
-- occasionally adjust its timing. For example, if @actualPresentTime@ is
-- later than expected (e.g. one @refreshDuration@ late), the application
-- may increase its target IPD to a higher multiple of @refreshDuration@
-- (e.g. decrease its frame rate from 60Hz to 30Hz). If @actualPresentTime@
-- and @earliestPresentTime@ are consistently different, and if
-- @presentMargin@ is consistently large enough, the application may
-- decrease its target IPD to a smaller multiple of @refreshDuration@ (e.g.
-- increase its frame rate from 30Hz to 60Hz). If @actualPresentTime@ and
-- @earliestPresentTime@ are same, and if @presentMargin@ is consistently
-- high, the application may delay the start of its input-render-present
-- loop in order to decrease the latency between user input and the
-- corresponding present (always leaving some margin in case a new image
-- takes longer to render than the previous image). An application that
-- desires its target IPD to always be the same as @refreshDuration@, can
-- also adjust features until @actualPresentTime@ is never late and
-- @presentMargin@ is satisfactory.
--
-- = See Also
--
-- 'vkGetPastPresentationTimingGOOGLE'
data VkPastPresentationTimingGOOGLE = VkPastPresentationTimingGOOGLE
  { -- | @presentID@ is an application-provided value that was given to a
  -- previous
  -- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkQueuePresentKHR'
  -- command via 'VkPresentTimeGOOGLE'::@presentID@ (see below). It /can/ be
  -- used to uniquely identify a previous present with the
  -- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkQueuePresentKHR'
  -- command.
  vkPresentID :: Word32
  , -- | @desiredPresentTime@ is an application-provided value that was given to
  -- a previous
  -- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkQueuePresentKHR'
  -- command via 'VkPresentTimeGOOGLE'::@desiredPresentTime@. If non-zero, it
  -- was used by the application to indicate that an image not be presented
  -- any sooner than @desiredPresentTime@.
  vkDesiredPresentTime :: Word64
  , -- | @actualPresentTime@ is the time when the image of the @swapchain@ was
  -- actually displayed.
  vkActualPresentTime :: Word64
  , -- | @earliestPresentTime@ is the time when the image of the @swapchain@
  -- could have been displayed. This /may/ differ from @actualPresentTime@ if
  -- the application requested that the image be presented no sooner than
  -- 'VkPresentTimeGOOGLE'::@desiredPresentTime@.
  vkEarliestPresentTime :: Word64
  , -- | @presentMargin@ is an indication of how early the
  -- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkQueuePresentKHR'
  -- command was processed compared to how soon it needed to be processed,
  -- and still be presented at @earliestPresentTime@.
  vkPresentMargin :: Word64
  }
  deriving (Eq, Show)

instance Storable VkPastPresentationTimingGOOGLE where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPastPresentationTimingGOOGLE <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
                                            <*> peek (ptr `plusPtr` 24)
                                            <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkPresentID (poked :: VkPastPresentationTimingGOOGLE))
                *> poke (ptr `plusPtr` 8) (vkDesiredPresentTime (poked :: VkPastPresentationTimingGOOGLE))
                *> poke (ptr `plusPtr` 16) (vkActualPresentTime (poked :: VkPastPresentationTimingGOOGLE))
                *> poke (ptr `plusPtr` 24) (vkEarliestPresentTime (poked :: VkPastPresentationTimingGOOGLE))
                *> poke (ptr `plusPtr` 32) (vkPresentMargin (poked :: VkPastPresentationTimingGOOGLE))

instance Zero VkPastPresentationTimingGOOGLE where
  zero = VkPastPresentationTimingGOOGLE zero
                                        zero
                                        zero
                                        zero
                                        zero

-- | VkPresentTimeGOOGLE - The earliest time image should be presented
--
-- = See Also
--
-- 'VkPresentTimesInfoGOOGLE'
data VkPresentTimeGOOGLE = VkPresentTimeGOOGLE
  { -- | @presentID@ is an application-provided identification value, that /can/
  -- be used with the results of 'vkGetPastPresentationTimingGOOGLE', in
  -- order to uniquely identify this present. In order to be useful to the
  -- application, it /should/ be unique within some period of time that is
  -- meaningful to the application.
  vkPresentID :: Word32
  , -- | @desiredPresentTime@ specifies that the image given /should/ not be
  -- displayed to the user any earlier than this time. @desiredPresentTime@
  -- is a time in nanoseconds, relative to a monotonically-increasing clock
  -- (e.g. @CLOCK_MONOTONIC@ (see clock_gettime(2)) on Android and Linux). A
  -- value of zero specifies that the presentation engine /may/ display the
  -- image at any time. This is useful when the application desires to
  -- provide @presentID@, but does not need a specific @desiredPresentTime@.
  vkDesiredPresentTime :: Word64
  }
  deriving (Eq, Show)

instance Storable VkPresentTimeGOOGLE where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkPresentTimeGOOGLE <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkPresentID (poked :: VkPresentTimeGOOGLE))
                *> poke (ptr `plusPtr` 8) (vkDesiredPresentTime (poked :: VkPresentTimeGOOGLE))

instance Zero VkPresentTimeGOOGLE where
  zero = VkPresentTimeGOOGLE zero
                             zero

-- | VkPresentTimesInfoGOOGLE - The earliest time each image should be
-- presented
--
-- == Valid Usage
--
-- -   @swapchainCount@ /must/ be the same value as
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkPresentInfoKHR'::@swapchainCount@,
--     where
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkPresentInfoKHR' is
--     in the @pNext@ chain of this 'VkPresentTimesInfoGOOGLE' structure.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be 'VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE'
--
-- -   If @pTimes@ is not @NULL@, @pTimes@ /must/ be a valid pointer to an
--     array of @swapchainCount@ 'VkPresentTimeGOOGLE' structures
--
-- -   @swapchainCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'VkPresentTimeGOOGLE', 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkPresentTimesInfoGOOGLE = VkPresentTimesInfoGOOGLE
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @swapchainCount@ is the number of swapchains being presented to by this
  -- command.
  vkSwapchainCount :: Word32
  , -- | @pTimes@ is @NULL@ or a pointer to an array of 'VkPresentTimeGOOGLE'
  -- elements with @swapchainCount@ entries. If not @NULL@, each element of
  -- @pTimes@ contains the earliest time to present the image corresponding
  -- to the entry in the
  -- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkPresentInfoKHR'::@pImageIndices@
  -- array.
  vkPTimes :: Ptr VkPresentTimeGOOGLE
  }
  deriving (Eq, Show)

instance Storable VkPresentTimesInfoGOOGLE where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPresentTimesInfoGOOGLE <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPresentTimesInfoGOOGLE))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPresentTimesInfoGOOGLE))
                *> poke (ptr `plusPtr` 16) (vkSwapchainCount (poked :: VkPresentTimesInfoGOOGLE))
                *> poke (ptr `plusPtr` 24) (vkPTimes (poked :: VkPresentTimesInfoGOOGLE))

instance Zero VkPresentTimesInfoGOOGLE where
  zero = VkPresentTimesInfoGOOGLE VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE
                                  zero
                                  zero
                                  zero

-- | VkRefreshCycleDurationGOOGLE - Structure containing the RC duration of a
-- display
--
-- = See Also
--
-- 'vkGetRefreshCycleDurationGOOGLE'
data VkRefreshCycleDurationGOOGLE = VkRefreshCycleDurationGOOGLE
  { -- | @refreshDuration@ is the number of nanoseconds from the start of one
  -- refresh cycle to the next.
  vkRefreshDuration :: Word64
  }
  deriving (Eq, Show)

instance Storable VkRefreshCycleDurationGOOGLE where
  sizeOf ~_ = 8
  alignment ~_ = 8
  peek ptr = VkRefreshCycleDurationGOOGLE <$> peek (ptr `plusPtr` 0)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkRefreshDuration (poked :: VkRefreshCycleDurationGOOGLE))

instance Zero VkRefreshCycleDurationGOOGLE where
  zero = VkRefreshCycleDurationGOOGLE zero

-- | vkGetPastPresentationTimingGOOGLE - Obtain timing of a
-- previously-presented image
--
-- = Parameters
--
-- -   @device@ is the device associated with @swapchain@.
--
-- -   @swapchain@ is the swapchain to obtain presentation timing
--     information duration for.
--
-- -   @pPresentationTimingCount@ is a pointer to an integer related to the
--     number of 'VkPastPresentationTimingGOOGLE' structures to query, as
--     described below.
--
-- -   @pPresentationTimings@ is either @NULL@ or a pointer to an array of
--     'VkPastPresentationTimingGOOGLE' structures.
--
-- = Description
--
-- If @pPresentationTimings@ is @NULL@, then the number of newly-available
-- timing records for the given @swapchain@ is returned in
-- @pPresentationTimingCount@. Otherwise, @pPresentationTimingCount@ /must/
-- point to a variable set by the user to the number of elements in the
-- @pPresentationTimings@ array, and on return the variable is overwritten
-- with the number of structures actually written to
-- @pPresentationTimings@. If the value of @pPresentationTimingCount@ is
-- less than the number of newly-available timing records, at most
-- @pPresentationTimingCount@ structures will be written. If
-- @pPresentationTimingCount@ is smaller than the number of newly-available
-- timing records for the given @swapchain@,
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' to indicate that not all
-- the available values were returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @swapchain@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
--     handle
--
-- -   @pPresentationTimingCount@ /must/ be a valid pointer to a @uint32_t@
--     value
--
-- -   If the value referenced by @pPresentationTimingCount@ is not @0@,
--     and @pPresentationTimings@ is not @NULL@, @pPresentationTimings@
--     /must/ be a valid pointer to an array of @pPresentationTimingCount@
--     'VkPastPresentationTimingGOOGLE' structures
--
-- -   Both of @device@, and @swapchain@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance'
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_DEVICE_LOST'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_ERROR_OUT_OF_DATE_KHR'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'VkPastPresentationTimingGOOGLE',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPastPresentationTimingGOOGLE" vkGetPastPresentationTimingGOOGLE :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pPresentationTimingCount" ::: Ptr Word32) -> ("pPresentationTimings" ::: Ptr VkPastPresentationTimingGOOGLE) -> IO VkResult
#else
vkGetPastPresentationTimingGOOGLE :: DeviceCmds -> ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pPresentationTimingCount" ::: Ptr Word32) -> ("pPresentationTimings" ::: Ptr VkPastPresentationTimingGOOGLE) -> IO VkResult
vkGetPastPresentationTimingGOOGLE deviceCmds = mkVkGetPastPresentationTimingGOOGLE (pVkGetPastPresentationTimingGOOGLE deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPastPresentationTimingGOOGLE
  :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pPresentationTimingCount" ::: Ptr Word32) -> ("pPresentationTimings" ::: Ptr VkPastPresentationTimingGOOGLE) -> IO VkResult) -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pPresentationTimingCount" ::: Ptr Word32) -> ("pPresentationTimings" ::: Ptr VkPastPresentationTimingGOOGLE) -> IO VkResult)
#endif

type FN_vkGetPastPresentationTimingGOOGLE = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pPresentationTimingCount" ::: Ptr Word32) -> ("pPresentationTimings" ::: Ptr VkPastPresentationTimingGOOGLE) -> IO VkResult
type PFN_vkGetPastPresentationTimingGOOGLE = FunPtr FN_vkGetPastPresentationTimingGOOGLE

-- | vkGetRefreshCycleDurationGOOGLE - Obtain the RC duration of the PE’s
-- display
--
-- = Parameters
--
-- -   @device@ is the device associated with @swapchain@.
--
-- -   @swapchain@ is the swapchain to obtain the refresh duration for.
--
-- -   @pDisplayTimingProperties@ is a pointer to an instance of the
--     'VkRefreshCycleDurationGOOGLE' structure.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @swapchain@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
--     handle
--
-- -   @pDisplayTimingProperties@ /must/ be a valid pointer to a
--     'VkRefreshCycleDurationGOOGLE' structure
--
-- -   Both of @device@, and @swapchain@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance'
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_DEVICE_LOST'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'VkRefreshCycleDurationGOOGLE',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetRefreshCycleDurationGOOGLE" vkGetRefreshCycleDurationGOOGLE :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pDisplayTimingProperties" ::: Ptr VkRefreshCycleDurationGOOGLE) -> IO VkResult
#else
vkGetRefreshCycleDurationGOOGLE :: DeviceCmds -> ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pDisplayTimingProperties" ::: Ptr VkRefreshCycleDurationGOOGLE) -> IO VkResult
vkGetRefreshCycleDurationGOOGLE deviceCmds = mkVkGetRefreshCycleDurationGOOGLE (pVkGetRefreshCycleDurationGOOGLE deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRefreshCycleDurationGOOGLE
  :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pDisplayTimingProperties" ::: Ptr VkRefreshCycleDurationGOOGLE) -> IO VkResult) -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pDisplayTimingProperties" ::: Ptr VkRefreshCycleDurationGOOGLE) -> IO VkResult)
#endif

type FN_vkGetRefreshCycleDurationGOOGLE = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pDisplayTimingProperties" ::: Ptr VkRefreshCycleDurationGOOGLE) -> IO VkResult
type PFN_vkGetRefreshCycleDurationGOOGLE = FunPtr FN_vkGetRefreshCycleDurationGOOGLE

-- No documentation found for TopLevel "VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME"
pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME = "VK_GOOGLE_display_timing"

-- No documentation found for TopLevel "VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION"
pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION :: Integral a => a
pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE"
pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE :: VkStructureType
pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE = VkStructureType 1000092000
