{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_display_swapchain
  ( VkDisplayPresentInfoKHR(..)
  , FN_vkCreateSharedSwapchainsKHR
  , PFN_vkCreateSharedSwapchainsKHR
  , vkCreateSharedSwapchainsKHR
  , pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR
  , pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME
  , pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
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
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkRect2D(..)
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkSwapchainCreateInfoKHR(..)
  , VkSwapchainKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkDisplayPresentInfoKHR"
data VkDisplayPresentInfoKHR = VkDisplayPresentInfoKHR
  { -- No documentation found for Nested "VkDisplayPresentInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDisplayPresentInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDisplayPresentInfoKHR" "srcRect"
  vkSrcRect :: VkRect2D
  , -- No documentation found for Nested "VkDisplayPresentInfoKHR" "dstRect"
  vkDstRect :: VkRect2D
  , -- No documentation found for Nested "VkDisplayPresentInfoKHR" "persistent"
  vkPersistent :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkDisplayPresentInfoKHR where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkDisplayPresentInfoKHR <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 32)
                                     <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSrcRect (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkDstRect (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 48) (vkPersistent (poked :: VkDisplayPresentInfoKHR))

instance Zero VkDisplayPresentInfoKHR where
  zero = VkDisplayPresentInfoKHR VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR
                                 zero
                                 zero
                                 zero
                                 zero

-- No documentation found for TopLevel "vkCreateSharedSwapchainsKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateSharedSwapchainsKHR" vkCreateSharedSwapchainsKHR :: ("device" ::: VkDevice) -> ("swapchainCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkSwapchainCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSwapchains" ::: Ptr VkSwapchainKHR) -> IO VkResult
#else
vkCreateSharedSwapchainsKHR :: DeviceCmds -> ("device" ::: VkDevice) -> ("swapchainCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkSwapchainCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSwapchains" ::: Ptr VkSwapchainKHR) -> IO VkResult
vkCreateSharedSwapchainsKHR deviceCmds = mkVkCreateSharedSwapchainsKHR (pVkCreateSharedSwapchainsKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateSharedSwapchainsKHR
  :: FunPtr (("device" ::: VkDevice) -> ("swapchainCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkSwapchainCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSwapchains" ::: Ptr VkSwapchainKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("swapchainCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkSwapchainCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSwapchains" ::: Ptr VkSwapchainKHR) -> IO VkResult)
#endif

type FN_vkCreateSharedSwapchainsKHR = ("device" ::: VkDevice) -> ("swapchainCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkSwapchainCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSwapchains" ::: Ptr VkSwapchainKHR) -> IO VkResult
type PFN_vkCreateSharedSwapchainsKHR = FunPtr FN_vkCreateSharedSwapchainsKHR

-- No documentation found for Nested "VkResult" "VK_ERROR_INCOMPATIBLE_DISPLAY_KHR"
pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR :: VkResult
pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR = VkResult (-1000003001)

-- No documentation found for TopLevel "VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME"
pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME = "VK_KHR_display_swapchain"

-- No documentation found for TopLevel "VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION"
pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION :: Integral a => a
pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION = 9

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR = VkStructureType 1000003000
