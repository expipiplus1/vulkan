{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_GOOGLE_display_timing
  ( pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE
  , pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION
  , pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME
  , vkGetRefreshCycleDurationGOOGLE
  , vkGetPastPresentationTimingGOOGLE
  , VkRefreshCycleDurationGOOGLE(..)
  , VkPastPresentationTimingGOOGLE(..)
  , VkPresentTimesInfoGOOGLE(..)
  , VkPresentTimeGOOGLE(..)
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word64
  , Word32
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( VkSwapchainKHR
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE"
pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE :: VkStructureType
pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE = VkStructureType 1000092000
-- No documentation found for TopLevel "VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION"
pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION :: Integral a => a
pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME"
pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME = "VK_GOOGLE_display_timing"
-- | vkGetRefreshCycleDurationGOOGLE - Obtain the RC duration of the PE’s
-- display
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the device associated with @swapchain@.
--
-- -   @swapchain@ is the swapchain to obtain the refresh duration for.
--
-- -   @pDisplayTimingProperties@ is a pointer to an instance of the
--     @VkRefreshCycleDurationGOOGLE@ structure.
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @swapchain@ /must/ be a valid @VkSwapchainKHR@ handle
--
-- -   @pDisplayTimingProperties@ /must/ be a valid pointer to a
--     @VkRefreshCycleDurationGOOGLE@ structure
--
-- -   Both of @device@, and @swapchain@ /must/ have been created,
--     allocated, or retrieved from the same @VkInstance@
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_DEVICE_LOST@
--
--     -   @VK_ERROR_SURFACE_LOST_KHR@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkRefreshCycleDurationGOOGLE',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
foreign import ccall "vkGetRefreshCycleDurationGOOGLE" vkGetRefreshCycleDurationGOOGLE :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pDisplayTimingProperties" ::: Ptr VkRefreshCycleDurationGOOGLE) -> IO VkResult
-- | vkGetPastPresentationTimingGOOGLE - Obtain timing of a
-- previously-presented image
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the device associated with @swapchain@.
--
-- -   @swapchain@ is the swapchain to obtain presentation timing
--     information duration for.
--
-- -   @pPresentationTimingCount@ is a pointer to an integer related to the
--     number of @VkPastPresentationTimingGOOGLE@ structures to query, as
--     described below.
--
-- -   @pPresentationTimings@ is either @NULL@ or a pointer to an an array
--     of @VkPastPresentationTimingGOOGLE@ structures.
--
-- = Description
-- #_description#
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
-- timing records for the given @swapchain@, @VK_INCOMPLETE@ will be
-- returned instead of @VK_SUCCESS@ to indicate that not all the available
-- values were returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @swapchain@ /must/ be a valid @VkSwapchainKHR@ handle
--
-- -   @pPresentationTimingCount@ /must/ be a valid pointer to a @uint32_t@
--     value
--
-- -   If the value referenced by @pPresentationTimingCount@ is not @0@,
--     and @pPresentationTimings@ is not @NULL@, @pPresentationTimings@
--     /must/ be a valid pointer to an array of @pPresentationTimingCount@
--     @VkPastPresentationTimingGOOGLE@ structures
--
-- -   Both of @device@, and @swapchain@ /must/ have been created,
--     allocated, or retrieved from the same @VkInstance@
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
--     -   @VK_INCOMPLETE@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_DEVICE_LOST@
--
--     -   @VK_ERROR_OUT_OF_DATE_KHR@
--
--     -   @VK_ERROR_SURFACE_LOST_KHR@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkPastPresentationTimingGOOGLE',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
foreign import ccall "vkGetPastPresentationTimingGOOGLE" vkGetPastPresentationTimingGOOGLE :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pPresentationTimingCount" ::: Ptr Word32) -> ("pPresentationTimings" ::: Ptr VkPastPresentationTimingGOOGLE) -> IO VkResult
-- | VkRefreshCycleDurationGOOGLE - Structure containing the RC duration of a
-- display
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- 'vkGetRefreshCycleDurationGOOGLE'
data VkRefreshCycleDurationGOOGLE = VkRefreshCycleDurationGOOGLE
  { -- No documentation found for Nested "VkRefreshCycleDurationGOOGLE" "vkRefreshDuration"
  vkRefreshDuration :: Word64
  }
  deriving (Eq, Show)

instance Storable VkRefreshCycleDurationGOOGLE where
  sizeOf ~_ = 8
  alignment ~_ = 8
  peek ptr = VkRefreshCycleDurationGOOGLE <$> peek (ptr `plusPtr` 0)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkRefreshDuration (poked :: VkRefreshCycleDurationGOOGLE))
-- | VkPastPresentationTimingGOOGLE - Structure containing timing information
-- about a previously-presented image
--
-- = Description
-- #_description#
--
-- The results for a given @swapchain@ and @presentID@ are only returned
-- once from @vkGetPastPresentationTimingGOOGLE@.
--
-- The application /can/ use the @VkPastPresentationTimingGOOGLE@ values to
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
-- #_see_also#
--
-- 'vkGetPastPresentationTimingGOOGLE'
data VkPastPresentationTimingGOOGLE = VkPastPresentationTimingGOOGLE
  { -- No documentation found for Nested "VkPastPresentationTimingGOOGLE" "vkPresentID"
  vkPresentID :: Word32
  , -- No documentation found for Nested "VkPastPresentationTimingGOOGLE" "vkDesiredPresentTime"
  vkDesiredPresentTime :: Word64
  , -- No documentation found for Nested "VkPastPresentationTimingGOOGLE" "vkActualPresentTime"
  vkActualPresentTime :: Word64
  , -- No documentation found for Nested "VkPastPresentationTimingGOOGLE" "vkEarliestPresentTime"
  vkEarliestPresentTime :: Word64
  , -- No documentation found for Nested "VkPastPresentationTimingGOOGLE" "vkPresentMargin"
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
-- | VkPresentTimesInfoGOOGLE - The earliest time each image should be
-- presented
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   @swapchainCount@ /must/ be the same value as
--     @VkPresentInfoKHR@::@swapchainCount@, where @VkPresentInfoKHR@ is in
--     the @pNext@ chain of this @VkPresentTimesInfoGOOGLE@ structure.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE@
--
-- -   If @pTimes@ is not @NULL@, @pTimes@ /must/ be a valid pointer to an
--     array of @swapchainCount@ @VkPresentTimeGOOGLE@ structures
--
-- -   @swapchainCount@ /must/ be greater than @0@
--
-- = See Also
-- #_see_also#
--
-- 'VkPresentTimeGOOGLE', 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPresentTimesInfoGOOGLE = VkPresentTimesInfoGOOGLE
  { -- No documentation found for Nested "VkPresentTimesInfoGOOGLE" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPresentTimesInfoGOOGLE" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPresentTimesInfoGOOGLE" "vkSwapchainCount"
  vkSwapchainCount :: Word32
  , -- No documentation found for Nested "VkPresentTimesInfoGOOGLE" "vkPTimes"
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
-- | VkPresentTimeGOOGLE - The earliest time image should be presented
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- 'VkPresentTimesInfoGOOGLE'
data VkPresentTimeGOOGLE = VkPresentTimeGOOGLE
  { -- No documentation found for Nested "VkPresentTimeGOOGLE" "vkPresentID"
  vkPresentID :: Word32
  , -- No documentation found for Nested "VkPresentTimeGOOGLE" "vkDesiredPresentTime"
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
