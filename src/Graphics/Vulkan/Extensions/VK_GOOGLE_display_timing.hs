{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_GOOGLE_display_timing
  ( PastPresentationTimingGOOGLE(..)
  , PresentTimeGOOGLE(..)
#if defined(VK_USE_PLATFORM_GGP)
  , PresentTimesInfoGOOGLE(..)
#endif
  , RefreshCycleDurationGOOGLE(..)
#if defined(VK_USE_PLATFORM_GGP)
  , getNumPastPresentationTimingGOOGLE
  , getPastPresentationTimingGOOGLE
  , getAllPastPresentationTimingGOOGLE
  , getRefreshCycleDurationGOOGLE
#endif
  , pattern GOOGLE_DISPLAY_TIMING_EXTENSION_NAME
  , pattern GOOGLE_DISPLAY_TIMING_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif
import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import qualified Data.Vector
  ( generateM
  )
#endif
import Data.Word
  ( Word32
  , Word64
  )

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Alloc
  ( alloca
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Array
  ( allocaArray
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Utils
  ( with
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( nullPtr
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peekElemOff
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peek
  )
#endif


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing
  ( pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME
  , pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing
  ( vkGetRefreshCycleDurationGOOGLE
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing
  ( vkGetPastPresentationTimingGOOGLE
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( SwapchainKHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE
  )



-- No documentation found for TopLevel "VkPastPresentationTimingGOOGLE"
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

instance Zero PastPresentationTimingGOOGLE where
  zero = PastPresentationTimingGOOGLE zero
                                      zero
                                      zero
                                      zero
                                      zero



-- No documentation found for TopLevel "VkPresentTimeGOOGLE"
data PresentTimeGOOGLE = PresentTimeGOOGLE
  { -- No documentation found for Nested "PresentTimeGOOGLE" "presentID"
  presentID :: Word32
  , -- No documentation found for Nested "PresentTimeGOOGLE" "desiredPresentTime"
  desiredPresentTime :: Word64
  }
  deriving (Show, Eq)

instance Zero PresentTimeGOOGLE where
  zero = PresentTimeGOOGLE zero
                           zero



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPresentTimesInfoGOOGLE"
data PresentTimesInfoGOOGLE = PresentTimesInfoGOOGLE
  { -- No documentation found for Nested "PresentTimesInfoGOOGLE" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PresentTimesInfoGOOGLE" "pTimes"
  times :: Either Word32 (Vector PresentTimeGOOGLE)
  }
  deriving (Show, Eq)

instance Zero PresentTimesInfoGOOGLE where
  zero = PresentTimesInfoGOOGLE Nothing
                                (Left 0)

#endif


-- No documentation found for TopLevel "VkRefreshCycleDurationGOOGLE"
data RefreshCycleDurationGOOGLE = RefreshCycleDurationGOOGLE
  { -- No documentation found for Nested "RefreshCycleDurationGOOGLE" "refreshDuration"
  refreshDuration :: Word64
  }
  deriving (Show, Eq)

instance Zero RefreshCycleDurationGOOGLE where
  zero = RefreshCycleDurationGOOGLE zero



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPastPresentationTimingGOOGLE"
getNumPastPresentationTimingGOOGLE :: Device ->  SwapchainKHR ->  IO (VkResult, Word32)
getNumPastPresentationTimingGOOGLE = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetPastPresentationTimingGOOGLE"
getPastPresentationTimingGOOGLE :: Device ->  SwapchainKHR ->  Word32 ->  IO (VkResult, Vector PastPresentationTimingGOOGLE)
getPastPresentationTimingGOOGLE = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getPastPresentationTimingGOOGLE'.
getAllPastPresentationTimingGOOGLE :: Device ->  SwapchainKHR ->  IO (Vector PastPresentationTimingGOOGLE)
getAllPastPresentationTimingGOOGLE device' swapchain' =
  snd <$> getNumPastPresentationTimingGOOGLE device' swapchain'
    >>= \num -> snd <$> getPastPresentationTimingGOOGLE device' swapchain' num

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetRefreshCycleDurationGOOGLE"
getRefreshCycleDurationGOOGLE :: Device ->  SwapchainKHR ->  IO (RefreshCycleDurationGOOGLE)
getRefreshCycleDurationGOOGLE = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif

-- No documentation found for TopLevel "VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME"
pattern GOOGLE_DISPLAY_TIMING_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern GOOGLE_DISPLAY_TIMING_EXTENSION_NAME = VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME

-- No documentation found for TopLevel "VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION"
pattern GOOGLE_DISPLAY_TIMING_SPEC_VERSION :: Integral a => a
pattern GOOGLE_DISPLAY_TIMING_SPEC_VERSION = VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION
