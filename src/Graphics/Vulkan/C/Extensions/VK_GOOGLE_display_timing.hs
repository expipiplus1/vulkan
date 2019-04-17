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
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetPastPresentationTimingGOOGLE
#endif
  , FN_vkGetPastPresentationTimingGOOGLE
  , PFN_vkGetPastPresentationTimingGOOGLE
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetRefreshCycleDurationGOOGLE
#endif
  , FN_vkGetRefreshCycleDurationGOOGLE
  , PFN_vkGetRefreshCycleDurationGOOGLE
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
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkSwapchainKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkPastPresentationTimingGOOGLE"
data VkPastPresentationTimingGOOGLE = VkPastPresentationTimingGOOGLE
  { -- No documentation found for Nested "VkPastPresentationTimingGOOGLE" "presentID"
  vkPresentID :: Word32
  , -- No documentation found for Nested "VkPastPresentationTimingGOOGLE" "desiredPresentTime"
  vkDesiredPresentTime :: Word64
  , -- No documentation found for Nested "VkPastPresentationTimingGOOGLE" "actualPresentTime"
  vkActualPresentTime :: Word64
  , -- No documentation found for Nested "VkPastPresentationTimingGOOGLE" "earliestPresentTime"
  vkEarliestPresentTime :: Word64
  , -- No documentation found for Nested "VkPastPresentationTimingGOOGLE" "presentMargin"
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
-- No documentation found for TopLevel "VkPresentTimeGOOGLE"
data VkPresentTimeGOOGLE = VkPresentTimeGOOGLE
  { -- No documentation found for Nested "VkPresentTimeGOOGLE" "presentID"
  vkPresentID :: Word32
  , -- No documentation found for Nested "VkPresentTimeGOOGLE" "desiredPresentTime"
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
-- No documentation found for TopLevel "VkPresentTimesInfoGOOGLE"
data VkPresentTimesInfoGOOGLE = VkPresentTimesInfoGOOGLE
  { -- No documentation found for Nested "VkPresentTimesInfoGOOGLE" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPresentTimesInfoGOOGLE" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPresentTimesInfoGOOGLE" "swapchainCount"
  vkSwapchainCount :: Word32
  , -- No documentation found for Nested "VkPresentTimesInfoGOOGLE" "pTimes"
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
  zero = VkPresentTimesInfoGOOGLE zero
                                  zero
                                  zero
                                  zero
-- No documentation found for TopLevel "VkRefreshCycleDurationGOOGLE"
data VkRefreshCycleDurationGOOGLE = VkRefreshCycleDurationGOOGLE
  { -- No documentation found for Nested "VkRefreshCycleDurationGOOGLE" "refreshDuration"
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
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetPastPresentationTimingGOOGLE"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPastPresentationTimingGOOGLE" vkGetPastPresentationTimingGOOGLE :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pPresentationTimingCount" ::: Ptr Word32) -> ("pPresentationTimings" ::: Ptr VkPastPresentationTimingGOOGLE) -> IO VkResult

#endif
type FN_vkGetPastPresentationTimingGOOGLE = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pPresentationTimingCount" ::: Ptr Word32) -> ("pPresentationTimings" ::: Ptr VkPastPresentationTimingGOOGLE) -> IO VkResult
type PFN_vkGetPastPresentationTimingGOOGLE = FunPtr FN_vkGetPastPresentationTimingGOOGLE
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetRefreshCycleDurationGOOGLE"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetRefreshCycleDurationGOOGLE" vkGetRefreshCycleDurationGOOGLE :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pDisplayTimingProperties" ::: Ptr VkRefreshCycleDurationGOOGLE) -> IO VkResult

#endif
type FN_vkGetRefreshCycleDurationGOOGLE = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pDisplayTimingProperties" ::: Ptr VkRefreshCycleDurationGOOGLE) -> IO VkResult
type PFN_vkGetRefreshCycleDurationGOOGLE = FunPtr FN_vkGetRefreshCycleDurationGOOGLE
-- No documentation found for TopLevel "VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME"
pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME = "VK_GOOGLE_display_timing"
-- No documentation found for TopLevel "VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION"
pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION :: Integral a => a
pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE"
pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE :: VkStructureType
pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE = VkStructureType 1000092000
