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


-- | Nothing
pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE :: VkStructureType
pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE = VkStructureType 1000092000
pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION :: Integral a => a
pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION = 1
pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME = "VK_GOOGLE_display_timing"
-- | 
foreign import ccall "vkGetRefreshCycleDurationGOOGLE" vkGetRefreshCycleDurationGOOGLE :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pDisplayTimingProperties" ::: Ptr VkRefreshCycleDurationGOOGLE) -> IO VkResult
-- | 
foreign import ccall "vkGetPastPresentationTimingGOOGLE" vkGetPastPresentationTimingGOOGLE :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pPresentationTimingCount" ::: Ptr Word32) -> ("pPresentationTimings" ::: Ptr VkPastPresentationTimingGOOGLE) -> IO VkResult
-- | TODO: Struct comments
data VkRefreshCycleDurationGOOGLE = VkRefreshCycleDurationGOOGLE
  { vkRefreshDuration :: Word64
  }
  deriving (Eq, Show)

instance Storable VkRefreshCycleDurationGOOGLE where
  sizeOf ~_ = 8
  alignment ~_ = 8
  peek ptr = VkRefreshCycleDurationGOOGLE <$> peek (ptr `plusPtr` 0)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkRefreshDuration (poked :: VkRefreshCycleDurationGOOGLE))
-- | TODO: Struct comments
data VkPastPresentationTimingGOOGLE = VkPastPresentationTimingGOOGLE
  { vkPresentID :: Word32
  , vkDesiredPresentTime :: Word64
  , vkActualPresentTime :: Word64
  , vkEarliestPresentTime :: Word64
  , vkPresentMargin :: Word64
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
-- | TODO: Struct comments
data VkPresentTimesInfoGOOGLE = VkPresentTimesInfoGOOGLE
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkSwapchainCount :: Word32
  , vkPTimes :: Ptr VkPresentTimeGOOGLE
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
-- | TODO: Struct comments
data VkPresentTimeGOOGLE = VkPresentTimeGOOGLE
  { vkPresentID :: Word32
  , vkDesiredPresentTime :: Word64
  }
  deriving (Eq, Show)

instance Storable VkPresentTimeGOOGLE where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkPresentTimeGOOGLE <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkPresentID (poked :: VkPresentTimeGOOGLE))
                *> poke (ptr `plusPtr` 8) (vkDesiredPresentTime (poked :: VkPresentTimeGOOGLE))
