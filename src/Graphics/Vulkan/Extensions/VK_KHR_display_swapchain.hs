{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_display_swapchain
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  DisplayPresentInfoKHR(..)
  , 
#endif
  createSharedSwapchainsKHR
  , pattern KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME
  , pattern KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION
  , pattern STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR
  , pattern ERROR_INCOMPATIBLE_DISPLAY_KHR
  ) where

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
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Storable
  ( peekElemOff
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_display_swapchain
  ( vkCreateSharedSwapchainsKHR
  , pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME
  , pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pipeline
  ( Rect2D(..)
  )
#endif
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( SwapchainCreateInfoKHR(..)
  , SwapchainKHR
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern ERROR_INCOMPATIBLE_DISPLAY_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDisplayPresentInfoKHR"
data DisplayPresentInfoKHR = DisplayPresentInfoKHR
  { -- No documentation found for Nested "DisplayPresentInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayPresentInfoKHR" "srcRect"
  srcRect :: Rect2D
  , -- No documentation found for Nested "DisplayPresentInfoKHR" "dstRect"
  dstRect :: Rect2D
  , -- No documentation found for Nested "DisplayPresentInfoKHR" "persistent"
  persistent :: Bool
  }
  deriving (Show, Eq)

instance Zero DisplayPresentInfoKHR where
  zero = DisplayPresentInfoKHR Nothing
                               zero
                               zero
                               False

#endif


-- No documentation found for TopLevel "vkCreateSharedSwapchainsKHR"
createSharedSwapchainsKHR :: Device ->  Vector SwapchainCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (Vector SwapchainKHR)
createSharedSwapchainsKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME"
pattern KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME = VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION"
pattern KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION :: Integral a => a
pattern KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION = VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION
