{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_display_swapchain
  ( withCStructDisplayPresentInfoKHR
  , fromCStructDisplayPresentInfoKHR
  , DisplayPresentInfoKHR(..)
  , createSharedSwapchainsKHR
  , pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION
  , pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR
  , pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
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
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createSharedSwapchainsKHR
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_display_swapchain
  ( VkDisplayPresentInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Rect2D(..)
  , fromCStructRect2D
  , withCStructRect2D
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( SwapchainCreateInfoKHR(..)
  , SwapchainKHR
  , withCStructSwapchainCreateInfoKHR
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_display_swapchain
  ( pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR
  , pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME
  , pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION
  )


-- No documentation found for TopLevel "DisplayPresentInfoKHR"
data DisplayPresentInfoKHR = DisplayPresentInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "DisplayPresentInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayPresentInfoKHR" "srcRect"
  vkSrcRect :: Rect2D
  , -- No documentation found for Nested "DisplayPresentInfoKHR" "dstRect"
  vkDstRect :: Rect2D
  , -- No documentation found for Nested "DisplayPresentInfoKHR" "persistent"
  vkPersistent :: Bool
  }
  deriving (Show, Eq)
withCStructDisplayPresentInfoKHR :: DisplayPresentInfoKHR -> (VkDisplayPresentInfoKHR -> IO a) -> IO a
withCStructDisplayPresentInfoKHR from cont = withCStructRect2D (vkDstRect (from :: DisplayPresentInfoKHR)) (\dstRect -> withCStructRect2D (vkSrcRect (from :: DisplayPresentInfoKHR)) (\srcRect -> maybeWith withSomeVkStruct (vkPNext (from :: DisplayPresentInfoKHR)) (\pPNext -> cont (VkDisplayPresentInfoKHR VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR pPNext srcRect dstRect (boolToBool32 (vkPersistent (from :: DisplayPresentInfoKHR)))))))
fromCStructDisplayPresentInfoKHR :: VkDisplayPresentInfoKHR -> IO DisplayPresentInfoKHR
fromCStructDisplayPresentInfoKHR c = DisplayPresentInfoKHR <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDisplayPresentInfoKHR)))
                                                           <*> (fromCStructRect2D (vkSrcRect (c :: VkDisplayPresentInfoKHR)))
                                                           <*> (fromCStructRect2D (vkDstRect (c :: VkDisplayPresentInfoKHR)))
                                                           <*> pure (bool32ToBool (vkPersistent (c :: VkDisplayPresentInfoKHR)))

-- | Wrapper for 'vkCreateSharedSwapchainsKHR'
createSharedSwapchainsKHR :: Device ->  Vector SwapchainCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO ( Vector SwapchainKHR )
createSharedSwapchainsKHR = \(Device device commandTable) -> \createInfos -> \allocator -> allocaArray ((Data.Vector.length createInfos)) (\pSwapchains -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> withVec withCStructSwapchainCreateInfoKHR createInfos (\pCreateInfos -> Graphics.Vulkan.C.Dynamic.createSharedSwapchainsKHR commandTable device (fromIntegral $ Data.Vector.length createInfos) pCreateInfos pAllocator pSwapchains >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((Data.Vector.generateM ((Data.Vector.length createInfos)) (peekElemOff pSwapchains)))))))
