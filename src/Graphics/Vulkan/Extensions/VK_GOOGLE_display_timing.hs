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
  , pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION
  , pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE
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
import qualified Graphics.Vulkan.C.Dynamic
  ( getPastPresentationTimingGOOGLE
  , getRefreshCycleDurationGOOGLE
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing
  ( VkPastPresentationTimingGOOGLE(..)
  , VkPresentTimeGOOGLE(..)
  , VkPresentTimesInfoGOOGLE(..)
  , VkRefreshCycleDurationGOOGLE(..)
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
import Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing
  ( pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME
  , pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION
  )


-- No documentation found for TopLevel "PastPresentationTimingGOOGLE"
data PastPresentationTimingGOOGLE = PastPresentationTimingGOOGLE
  { -- No documentation found for Nested "PastPresentationTimingGOOGLE" "presentID"
  vkPresentID :: Word32
  , -- No documentation found for Nested "PastPresentationTimingGOOGLE" "desiredPresentTime"
  vkDesiredPresentTime :: Word64
  , -- No documentation found for Nested "PastPresentationTimingGOOGLE" "actualPresentTime"
  vkActualPresentTime :: Word64
  , -- No documentation found for Nested "PastPresentationTimingGOOGLE" "earliestPresentTime"
  vkEarliestPresentTime :: Word64
  , -- No documentation found for Nested "PastPresentationTimingGOOGLE" "presentMargin"
  vkPresentMargin :: Word64
  }
  deriving (Show, Eq)
withCStructPastPresentationTimingGOOGLE :: PastPresentationTimingGOOGLE -> (VkPastPresentationTimingGOOGLE -> IO a) -> IO a
withCStructPastPresentationTimingGOOGLE from cont = cont (VkPastPresentationTimingGOOGLE (vkPresentID (from :: PastPresentationTimingGOOGLE)) (vkDesiredPresentTime (from :: PastPresentationTimingGOOGLE)) (vkActualPresentTime (from :: PastPresentationTimingGOOGLE)) (vkEarliestPresentTime (from :: PastPresentationTimingGOOGLE)) (vkPresentMargin (from :: PastPresentationTimingGOOGLE)))
fromCStructPastPresentationTimingGOOGLE :: VkPastPresentationTimingGOOGLE -> IO PastPresentationTimingGOOGLE
fromCStructPastPresentationTimingGOOGLE c = PastPresentationTimingGOOGLE <$> pure (vkPresentID (c :: VkPastPresentationTimingGOOGLE))
                                                                         <*> pure (vkDesiredPresentTime (c :: VkPastPresentationTimingGOOGLE))
                                                                         <*> pure (vkActualPresentTime (c :: VkPastPresentationTimingGOOGLE))
                                                                         <*> pure (vkEarliestPresentTime (c :: VkPastPresentationTimingGOOGLE))
                                                                         <*> pure (vkPresentMargin (c :: VkPastPresentationTimingGOOGLE))
-- No documentation found for TopLevel "PresentTimeGOOGLE"
data PresentTimeGOOGLE = PresentTimeGOOGLE
  { -- No documentation found for Nested "PresentTimeGOOGLE" "presentID"
  vkPresentID :: Word32
  , -- No documentation found for Nested "PresentTimeGOOGLE" "desiredPresentTime"
  vkDesiredPresentTime :: Word64
  }
  deriving (Show, Eq)
withCStructPresentTimeGOOGLE :: PresentTimeGOOGLE -> (VkPresentTimeGOOGLE -> IO a) -> IO a
withCStructPresentTimeGOOGLE from cont = cont (VkPresentTimeGOOGLE (vkPresentID (from :: PresentTimeGOOGLE)) (vkDesiredPresentTime (from :: PresentTimeGOOGLE)))
fromCStructPresentTimeGOOGLE :: VkPresentTimeGOOGLE -> IO PresentTimeGOOGLE
fromCStructPresentTimeGOOGLE c = PresentTimeGOOGLE <$> pure (vkPresentID (c :: VkPresentTimeGOOGLE))
                                                   <*> pure (vkDesiredPresentTime (c :: VkPresentTimeGOOGLE))
-- No documentation found for TopLevel "PresentTimesInfoGOOGLE"
data PresentTimesInfoGOOGLE = PresentTimesInfoGOOGLE
  { -- Univalued Member elided
  -- No documentation found for Nested "PresentTimesInfoGOOGLE" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Optional length valued member elided
  , -- No documentation found for Nested "PresentTimesInfoGOOGLE" "pTimes"
  vkPTimes :: Maybe (Vector PresentTimeGOOGLE)
  }
  deriving (Show, Eq)
withCStructPresentTimesInfoGOOGLE :: PresentTimesInfoGOOGLE -> (VkPresentTimesInfoGOOGLE -> IO a) -> IO a
withCStructPresentTimesInfoGOOGLE from cont = maybeWith (withVec withCStructPresentTimeGOOGLE) (vkPTimes (from :: PresentTimesInfoGOOGLE)) (\pTimes -> maybeWith withSomeVkStruct (vkPNext (from :: PresentTimesInfoGOOGLE)) (\pPNext -> cont (VkPresentTimesInfoGOOGLE VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE pPNext (maybe 0 (fromIntegral . Data.Vector.length) (vkPTimes (from :: PresentTimesInfoGOOGLE))) pTimes)))
fromCStructPresentTimesInfoGOOGLE :: VkPresentTimesInfoGOOGLE -> IO PresentTimesInfoGOOGLE
fromCStructPresentTimesInfoGOOGLE c = PresentTimesInfoGOOGLE <$> -- Univalued Member elided
                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPresentTimesInfoGOOGLE)))
                                                             -- Optional length valued member elided
                                                             <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkSwapchainCount (c :: VkPresentTimesInfoGOOGLE))) (((fromCStructPresentTimeGOOGLE <=<) . peekElemOff) p)) (vkPTimes (c :: VkPresentTimesInfoGOOGLE))
-- No documentation found for TopLevel "RefreshCycleDurationGOOGLE"
data RefreshCycleDurationGOOGLE = RefreshCycleDurationGOOGLE
  { -- No documentation found for Nested "RefreshCycleDurationGOOGLE" "refreshDuration"
  vkRefreshDuration :: Word64
  }
  deriving (Show, Eq)
withCStructRefreshCycleDurationGOOGLE :: RefreshCycleDurationGOOGLE -> (VkRefreshCycleDurationGOOGLE -> IO a) -> IO a
withCStructRefreshCycleDurationGOOGLE from cont = cont (VkRefreshCycleDurationGOOGLE (vkRefreshDuration (from :: RefreshCycleDurationGOOGLE)))
fromCStructRefreshCycleDurationGOOGLE :: VkRefreshCycleDurationGOOGLE -> IO RefreshCycleDurationGOOGLE
fromCStructRefreshCycleDurationGOOGLE c = RefreshCycleDurationGOOGLE <$> pure (vkRefreshDuration (c :: VkRefreshCycleDurationGOOGLE))

-- | Wrapper for 'vkGetPastPresentationTimingGOOGLE'
getNumPastPresentationTimingGOOGLE :: Device ->  SwapchainKHR ->  IO (VkResult, Word32)
getNumPastPresentationTimingGOOGLE = \(Device device commandTable) -> \swapchain -> alloca (\pPresentationTimingCount -> Graphics.Vulkan.C.Dynamic.getPastPresentationTimingGOOGLE commandTable device swapchain pPresentationTimingCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pPresentationTimingCount)))

-- | Wrapper for 'vkGetPastPresentationTimingGOOGLE'
getPastPresentationTimingGOOGLE :: Device ->  SwapchainKHR ->  Word32 ->  IO ( VkResult
, Vector PastPresentationTimingGOOGLE )
getPastPresentationTimingGOOGLE = \(Device device commandTable) -> \swapchain -> \presentationTimingCount -> allocaArray (fromIntegral presentationTimingCount) (\pPresentationTimings -> with presentationTimingCount (\pPresentationTimingCount -> Graphics.Vulkan.C.Dynamic.getPastPresentationTimingGOOGLE commandTable device swapchain pPresentationTimingCount pPresentationTimings >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM ((\p -> fromCStructPastPresentationTimingGOOGLE <=< peekElemOff p) pPresentationTimings) =<< (fromIntegral <$> (peek pPresentationTimingCount)))))))
-- | Call 'getNumPastPresentationTimingGOOGLE' to get the number of return values, then use that
-- number to call 'getPastPresentationTimingGOOGLE' to get all the values.
getAllPastPresentationTimingGOOGLE :: Device ->  SwapchainKHR ->  IO (Vector PastPresentationTimingGOOGLE)
getAllPastPresentationTimingGOOGLE device swapchain =
  snd <$> getNumPastPresentationTimingGOOGLE device swapchain
    >>= \num -> snd <$> getPastPresentationTimingGOOGLE device swapchain num


-- | Wrapper for 'vkGetRefreshCycleDurationGOOGLE'
getRefreshCycleDurationGOOGLE :: Device ->  SwapchainKHR ->  IO (RefreshCycleDurationGOOGLE)
getRefreshCycleDurationGOOGLE = \(Device device commandTable) -> \swapchain -> alloca (\pDisplayTimingProperties -> Graphics.Vulkan.C.Dynamic.getRefreshCycleDurationGOOGLE commandTable device swapchain pDisplayTimingProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((fromCStructRefreshCycleDurationGOOGLE <=< peek) pDisplayTimingProperties)))
