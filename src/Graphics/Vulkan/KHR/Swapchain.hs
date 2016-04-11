{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.KHR.Swapchain where

import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )


data VkSwapchainCreateInfoKHR =
  VkSwapchainCreateInfoKHR{ sType :: VkStructureType 
                          , pNext :: Ptr Void 
                          , flags :: VkSwapchainCreateFlagsKHR 
                          , surface :: SurfaceKHR 
                          , minImageCount :: Word32 
                          , imageFormat :: VkFormat 
                          , imageColorSpace :: VkColorSpaceKHR 
                          , imageExtent :: VkExtent2D 
                          , imageArrayLayers :: Word32 
                          , imageUsage :: VkImageUsageFlags 
                          , imageSharingMode :: VkSharingMode 
                          , queueFamilyIndexCount :: Word32 
                          , pQueueFamilyIndices :: Ptr Word32 
                          , preTransform :: VkSurfaceTransformFlagBitsKHR 
                          , compositeAlpha :: VkCompositeAlphaFlagBitsKHR 
                          , presentMode :: VkPresentModeKHR 
                          , clipped :: VkBool32 
                          , oldSwapchain :: SwapchainKHR 
                          }
  deriving (Eq)

instance Storable VkSwapchainCreateInfoKHR where
  sizeOf ~_ = 104
  alignment ~_ = 8
  peek ptr = VkSwapchainCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 32)
                                      <*> peek (ptr `plusPtr` 36)
                                      <*> peek (ptr `plusPtr` 40)
                                      <*> peek (ptr `plusPtr` 44)
                                      <*> peek (ptr `plusPtr` 52)
                                      <*> peek (ptr `plusPtr` 56)
                                      <*> peek (ptr `plusPtr` 60)
                                      <*> peek (ptr `plusPtr` 64)
                                      <*> peek (ptr `plusPtr` 72)
                                      <*> peek (ptr `plusPtr` 80)
                                      <*> peek (ptr `plusPtr` 84)
                                      <*> peek (ptr `plusPtr` 88)
                                      <*> peek (ptr `plusPtr` 92)
                                      <*> peek (ptr `plusPtr` 96)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (surface (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (minImageCount (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 36) (imageFormat (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 40) (imageColorSpace (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 44) (imageExtent (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 52) (imageArrayLayers (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 56) (imageUsage (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 60) (imageSharingMode (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 64) (queueFamilyIndexCount (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 72) (pQueueFamilyIndices (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 80) (preTransform (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 84) (compositeAlpha (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 88) (presentMode (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 92) (clipped (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 96) (oldSwapchain (poked :: VkSwapchainCreateInfoKHR))


-- ** vkGetSwapchainImagesKHR
foreign import ccall "vkGetSwapchainImagesKHR" vkGetSwapchainImagesKHR ::
  Device -> SwapchainKHR -> Ptr Word32 -> Ptr Image -> IO VkResult

-- ** vkDestroySwapchainKHR
foreign import ccall "vkDestroySwapchainKHR" vkDestroySwapchainKHR ::
  Device -> SwapchainKHR -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkQueuePresentKHR
foreign import ccall "vkQueuePresentKHR" vkQueuePresentKHR ::
  Queue -> Ptr VkPresentInfoKHR -> IO VkResult

-- ** VkSwapchainCreateFlagsKHR
-- | Opaque flag
newtype VkSwapchainCreateFlagsKHR = VkSwapchainCreateFlagsKHR VkFlags
  deriving (Eq, Storable)

-- ** vkCreateSwapchainKHR
foreign import ccall "vkCreateSwapchainKHR" vkCreateSwapchainKHR ::
  Device ->
  Ptr VkSwapchainCreateInfoKHR ->
    Ptr VkAllocationCallbacks -> Ptr SwapchainKHR -> IO VkResult

-- ** vkAcquireNextImageKHR
foreign import ccall "vkAcquireNextImageKHR" vkAcquireNextImageKHR ::
  Device ->
  SwapchainKHR ->
    Word64 -> Semaphore -> Fence -> Ptr Word32 -> IO VkResult


data VkPresentInfoKHR =
  VkPresentInfoKHR{ sType :: VkStructureType 
                  , pNext :: Ptr Void 
                  , waitSemaphoreCount :: Word32 
                  , pWaitSemaphores :: Ptr Semaphore 
                  , swapchainCount :: Word32 
                  , pSwapchains :: Ptr SwapchainKHR 
                  , pImageIndices :: Ptr Word32 
                  , pResults :: Ptr VkResult 
                  }
  deriving (Eq)

instance Storable VkPresentInfoKHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkPresentInfoKHR <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 8)
                              <*> peek (ptr `plusPtr` 16)
                              <*> peek (ptr `plusPtr` 24)
                              <*> peek (ptr `plusPtr` 32)
                              <*> peek (ptr `plusPtr` 40)
                              <*> peek (ptr `plusPtr` 48)
                              <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 16) (waitSemaphoreCount (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 24) (pWaitSemaphores (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 32) (swapchainCount (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 40) (pSwapchains (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 48) (pImageIndices (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 56) (pResults (poked :: VkPresentInfoKHR))


newtype SwapchainKHR = SwapchainKHR Word64
  deriving (Eq, Storable)

