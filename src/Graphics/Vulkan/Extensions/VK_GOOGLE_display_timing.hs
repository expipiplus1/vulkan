{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_GOOGLE_display_timing
  ( withCStructPastPresentationTimingGOOGLE
  , fromCStructPastPresentationTimingGOOGLE
  , PastPresentationTimingGOOGLE(..)
  , withCStructPresentTimeGOOGLE
  , fromCStructPresentTimeGOOGLE
  , PresentTimeGOOGLE(..)
  , withCStructPresentTimesInfoGOOGLE
  , fromCStructPresentTimesInfoGOOGLE
  , PresentTimesInfoGOOGLE(..)
  , withCStructRefreshCycleDurationGOOGLE
  , fromCStructRefreshCycleDurationGOOGLE
  , RefreshCycleDurationGOOGLE(..)
  , getNumPastPresentationTimingGOOGLE
  , getPastPresentationTimingGOOGLE
  , getAllPastPresentationTimingGOOGLE
  , getRefreshCycleDurationGOOGLE
  , pattern GOOGLE_DISPLAY_TIMING_EXTENSION_NAME
  , pattern GOOGLE_DISPLAY_TIMING_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Maybe
  ( maybe
  )
import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  , length
  )
import Data.Word
  ( Word32
  , Word64
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  , nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing
  ( VkPastPresentationTimingGOOGLE(..)
  , VkPresentTimeGOOGLE(..)
  , VkPresentTimesInfoGOOGLE(..)
  , VkRefreshCycleDurationGOOGLE(..)
  , vkGetPastPresentationTimingGOOGLE
  , vkGetRefreshCycleDurationGOOGLE
  , pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME
  , pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( SwapchainKHR
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE
  )



-- | VkPastPresentationTimingGOOGLE - Structure containing timing information
-- about a previously-presented image
--
-- = Description
--
-- The results for a given @swapchain@ and @presentID@ are only returned
-- once from
-- 'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.vkGetPastPresentationTimingGOOGLE'.
--
-- The application /can/ use the
-- 'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.VkPastPresentationTimingGOOGLE'
-- values to occasionally adjust its timing. For example, if
-- @actualPresentTime@ is later than expected (e.g. one @refreshDuration@
-- late), the application may increase its target IPD to a higher multiple
-- of @refreshDuration@ (e.g. decrease its frame rate from 60Hz to 30Hz).
-- If @actualPresentTime@ and @earliestPresentTime@ are consistently
-- different, and if @presentMargin@ is consistently large enough, the
-- application may decrease its target IPD to a smaller multiple of
-- @refreshDuration@ (e.g. increase its frame rate from 30Hz to 60Hz). If
-- @actualPresentTime@ and @earliestPresentTime@ are same, and if
-- @presentMargin@ is consistently high, the application may delay the
-- start of its input-render-present loop in order to decrease the latency
-- between user input and the corresponding present (always leaving some
-- margin in case a new image takes longer to render than the previous
-- image). An application that desires its target IPD to always be the same
-- as @refreshDuration@, can also adjust features until @actualPresentTime@
-- is never late and @presentMargin@ is satisfactory.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.vkGetPastPresentationTimingGOOGLE'
data PastPresentationTimingGOOGLE = PastPresentationTimingGOOGLE
  { -- No documentation found for Nested "PastPresentationTimingGOOGLE" "presentID"
  presentID :: Word32
  , -- No documentation found for Nested "PastPresentationTimingGOOGLE" "desiredPresentTime"
  desiredPresentTime :: Word64
  , -- No documentation found for Nested "PastPresentationTimingGOOGLE" "actualPresentTime"
  actualPresentTime :: Word64
  , -- No documentation found for Nested "PastPresentationTimingGOOGLE" "earliestPresentTime"
  earliestPresentTime :: Word64
  , -- No documentation found for Nested "PastPresentationTimingGOOGLE" "presentMargin"
  presentMargin :: Word64
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPastPresentationTimingGOOGLE' and
-- marshal a 'PastPresentationTimingGOOGLE' into it. The 'VkPastPresentationTimingGOOGLE' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPastPresentationTimingGOOGLE :: PastPresentationTimingGOOGLE -> (VkPastPresentationTimingGOOGLE -> IO a) -> IO a
withCStructPastPresentationTimingGOOGLE marshalled cont = cont (VkPastPresentationTimingGOOGLE (presentID (marshalled :: PastPresentationTimingGOOGLE)) (desiredPresentTime (marshalled :: PastPresentationTimingGOOGLE)) (actualPresentTime (marshalled :: PastPresentationTimingGOOGLE)) (earliestPresentTime (marshalled :: PastPresentationTimingGOOGLE)) (presentMargin (marshalled :: PastPresentationTimingGOOGLE)))

-- | A function to read a 'VkPastPresentationTimingGOOGLE' and all additional
-- structures in the pointer chain into a 'PastPresentationTimingGOOGLE'.
fromCStructPastPresentationTimingGOOGLE :: VkPastPresentationTimingGOOGLE -> IO PastPresentationTimingGOOGLE
fromCStructPastPresentationTimingGOOGLE c = PastPresentationTimingGOOGLE <$> pure (vkPresentID (c :: VkPastPresentationTimingGOOGLE))
                                                                         <*> pure (vkDesiredPresentTime (c :: VkPastPresentationTimingGOOGLE))
                                                                         <*> pure (vkActualPresentTime (c :: VkPastPresentationTimingGOOGLE))
                                                                         <*> pure (vkEarliestPresentTime (c :: VkPastPresentationTimingGOOGLE))
                                                                         <*> pure (vkPresentMargin (c :: VkPastPresentationTimingGOOGLE))

instance Zero PastPresentationTimingGOOGLE where
  zero = PastPresentationTimingGOOGLE zero
                                      zero
                                      zero
                                      zero
                                      zero



-- | VkPresentTimeGOOGLE - The earliest time image should be presented
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.VkPresentTimesInfoGOOGLE'
data PresentTimeGOOGLE = PresentTimeGOOGLE
  { -- No documentation found for Nested "PresentTimeGOOGLE" "presentID"
  presentID :: Word32
  , -- No documentation found for Nested "PresentTimeGOOGLE" "desiredPresentTime"
  desiredPresentTime :: Word64
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPresentTimeGOOGLE' and
-- marshal a 'PresentTimeGOOGLE' into it. The 'VkPresentTimeGOOGLE' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPresentTimeGOOGLE :: PresentTimeGOOGLE -> (VkPresentTimeGOOGLE -> IO a) -> IO a
withCStructPresentTimeGOOGLE marshalled cont = cont (VkPresentTimeGOOGLE (presentID (marshalled :: PresentTimeGOOGLE)) (desiredPresentTime (marshalled :: PresentTimeGOOGLE)))

-- | A function to read a 'VkPresentTimeGOOGLE' and all additional
-- structures in the pointer chain into a 'PresentTimeGOOGLE'.
fromCStructPresentTimeGOOGLE :: VkPresentTimeGOOGLE -> IO PresentTimeGOOGLE
fromCStructPresentTimeGOOGLE c = PresentTimeGOOGLE <$> pure (vkPresentID (c :: VkPresentTimeGOOGLE))
                                                   <*> pure (vkDesiredPresentTime (c :: VkPresentTimeGOOGLE))

instance Zero PresentTimeGOOGLE where
  zero = PresentTimeGOOGLE zero
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
--     in the @pNext@ chain of this
--     'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.VkPresentTimesInfoGOOGLE'
--     structure.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE'
--
-- -   If @pTimes@ is not @NULL@, @pTimes@ /must/ be a valid pointer to an
--     array of @swapchainCount@
--     'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.VkPresentTimeGOOGLE'
--     structures
--
-- -   @swapchainCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.VkPresentTimeGOOGLE',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PresentTimesInfoGOOGLE = PresentTimesInfoGOOGLE
  { -- Univalued member elided
  -- No documentation found for Nested "PresentTimesInfoGOOGLE" "pNext"
  next :: Maybe SomeVkStruct
  -- Optional length valued member elided
  , -- No documentation found for Nested "PresentTimesInfoGOOGLE" "pTimes"
  times :: Maybe (Vector PresentTimeGOOGLE)
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPresentTimesInfoGOOGLE' and
-- marshal a 'PresentTimesInfoGOOGLE' into it. The 'VkPresentTimesInfoGOOGLE' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPresentTimesInfoGOOGLE :: PresentTimesInfoGOOGLE -> (VkPresentTimesInfoGOOGLE -> IO a) -> IO a
withCStructPresentTimesInfoGOOGLE marshalled cont = maybeWith (withVec withCStructPresentTimeGOOGLE) (times (marshalled :: PresentTimesInfoGOOGLE)) (\pPTimes -> maybeWith withSomeVkStruct (next (marshalled :: PresentTimesInfoGOOGLE)) (\pPNext -> cont (VkPresentTimesInfoGOOGLE VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE pPNext (maybe 0 (fromIntegral . Data.Vector.length) (times (marshalled :: PresentTimesInfoGOOGLE))) pPTimes)))

-- | A function to read a 'VkPresentTimesInfoGOOGLE' and all additional
-- structures in the pointer chain into a 'PresentTimesInfoGOOGLE'.
fromCStructPresentTimesInfoGOOGLE :: VkPresentTimesInfoGOOGLE -> IO PresentTimesInfoGOOGLE
fromCStructPresentTimesInfoGOOGLE c = PresentTimesInfoGOOGLE <$> -- Univalued Member elided
                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPresentTimesInfoGOOGLE)))
                                                             -- Optional length valued member elided
                                                             <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkSwapchainCount (c :: VkPresentTimesInfoGOOGLE))) (((fromCStructPresentTimeGOOGLE <=<) . peekElemOff) p)) (vkPTimes (c :: VkPresentTimesInfoGOOGLE))

instance Zero PresentTimesInfoGOOGLE where
  zero = PresentTimesInfoGOOGLE Nothing
                                Nothing



-- | VkRefreshCycleDurationGOOGLE - Structure containing the RC duration of a
-- display
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.vkGetRefreshCycleDurationGOOGLE'
data RefreshCycleDurationGOOGLE = RefreshCycleDurationGOOGLE
  { -- No documentation found for Nested "RefreshCycleDurationGOOGLE" "refreshDuration"
  refreshDuration :: Word64
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkRefreshCycleDurationGOOGLE' and
-- marshal a 'RefreshCycleDurationGOOGLE' into it. The 'VkRefreshCycleDurationGOOGLE' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructRefreshCycleDurationGOOGLE :: RefreshCycleDurationGOOGLE -> (VkRefreshCycleDurationGOOGLE -> IO a) -> IO a
withCStructRefreshCycleDurationGOOGLE marshalled cont = cont (VkRefreshCycleDurationGOOGLE (refreshDuration (marshalled :: RefreshCycleDurationGOOGLE)))

-- | A function to read a 'VkRefreshCycleDurationGOOGLE' and all additional
-- structures in the pointer chain into a 'RefreshCycleDurationGOOGLE'.
fromCStructRefreshCycleDurationGOOGLE :: VkRefreshCycleDurationGOOGLE -> IO RefreshCycleDurationGOOGLE
fromCStructRefreshCycleDurationGOOGLE c = RefreshCycleDurationGOOGLE <$> pure (vkRefreshDuration (c :: VkRefreshCycleDurationGOOGLE))

instance Zero RefreshCycleDurationGOOGLE where
  zero = RefreshCycleDurationGOOGLE zero



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
--     number of
--     'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.VkPastPresentationTimingGOOGLE'
--     structures to query, as described below.
--
-- -   @pPresentationTimings@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.VkPastPresentationTimingGOOGLE'
--     structures.
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
--     'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.VkPastPresentationTimingGOOGLE'
--     structures
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
-- 'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.VkPastPresentationTimingGOOGLE',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
getNumPastPresentationTimingGOOGLE :: Device ->  SwapchainKHR ->  IO (VkResult, Word32)
getNumPastPresentationTimingGOOGLE = \(Device device' commandTable) -> \swapchain' -> alloca (\pPresentationTimingCount' -> vkGetPastPresentationTimingGOOGLE commandTable device' swapchain' pPresentationTimingCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pPresentationTimingCount')))

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
--     number of
--     'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.VkPastPresentationTimingGOOGLE'
--     structures to query, as described below.
--
-- -   @pPresentationTimings@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.VkPastPresentationTimingGOOGLE'
--     structures.
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
--     'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.VkPastPresentationTimingGOOGLE'
--     structures
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
-- 'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.VkPastPresentationTimingGOOGLE',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
getPastPresentationTimingGOOGLE :: Device ->  SwapchainKHR ->  Word32 ->  IO (VkResult, Vector PastPresentationTimingGOOGLE)
getPastPresentationTimingGOOGLE = \(Device device' commandTable) -> \swapchain' -> \presentationTimingCount' -> allocaArray (fromIntegral presentationTimingCount') (\pPresentationTimings' -> with presentationTimingCount' (\pPresentationTimingCount' -> vkGetPastPresentationTimingGOOGLE commandTable device' swapchain' pPresentationTimingCount' pPresentationTimings' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM ((\p -> fromCStructPastPresentationTimingGOOGLE <=< peekElemOff p) pPresentationTimings') =<< (fromIntegral <$> (peek pPresentationTimingCount')))))))
-- | Returns all the values available from 'getPastPresentationTimingGOOGLE'.
getAllPastPresentationTimingGOOGLE :: Device ->  SwapchainKHR ->  IO (Vector PastPresentationTimingGOOGLE)
getAllPastPresentationTimingGOOGLE device' swapchain' =
  snd <$> getNumPastPresentationTimingGOOGLE device' swapchain'
    >>= \num -> snd <$> getPastPresentationTimingGOOGLE device' swapchain' num



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
--     'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.VkRefreshCycleDurationGOOGLE'
--     structure.
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
--     'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.VkRefreshCycleDurationGOOGLE'
--     structure
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
-- 'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.VkRefreshCycleDurationGOOGLE',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
getRefreshCycleDurationGOOGLE :: Device ->  SwapchainKHR ->  IO (RefreshCycleDurationGOOGLE)
getRefreshCycleDurationGOOGLE = \(Device device' commandTable) -> \swapchain' -> alloca (\pDisplayTimingProperties' -> vkGetRefreshCycleDurationGOOGLE commandTable device' swapchain' pDisplayTimingProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((fromCStructRefreshCycleDurationGOOGLE <=< peek) pDisplayTimingProperties')))

-- No documentation found for TopLevel "VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME"
pattern GOOGLE_DISPLAY_TIMING_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern GOOGLE_DISPLAY_TIMING_EXTENSION_NAME = VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME

-- No documentation found for TopLevel "VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION"
pattern GOOGLE_DISPLAY_TIMING_SPEC_VERSION :: Integral a => a
pattern GOOGLE_DISPLAY_TIMING_SPEC_VERSION = VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION
