{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.KHR.Swapchain where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Graphics.Vulkan.KHR.Surface( VkColorSpaceKHR(..)
                                  , VkSurfaceTransformFlagBitsKHR(..)
                                  , VkPresentModeKHR(..)
                                  , VkCompositeAlphaFlagBitsKHR(..)
                                  , VkSurfaceKHR(..)
                                  )
import Graphics.Vulkan.Queue( VkQueue(..)
                            )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Fence( VkFence(..)
                            )
import Data.Void( Void
                )
import Graphics.Vulkan.Memory( VkInternalAllocationType(..)
                             , PFN_vkAllocationFunction
                             , PFN_vkReallocationFunction
                             , PFN_vkInternalAllocationNotification
                             , VkAllocationCallbacks(..)
                             , VkSystemAllocationScope(..)
                             , PFN_vkFreeFunction
                             , PFN_vkInternalFreeNotification
                             )
import Graphics.Vulkan.Image( VkImageUsageFlags(..)
                            , VkImage(..)
                            , VkImageUsageFlagBits(..)
                            )
import Graphics.Vulkan.QueueSemaphore( VkSemaphore(..)
                                     )
import Graphics.Vulkan.Core( VkResult(..)
                           , VkBool32(..)
                           , VkExtent2D(..)
                           , VkFlags(..)
                           , VkFormat(..)
                           , VkStructureType(..)
                           , VkSharingMode(..)
                           )
import Foreign.C.Types( CSize(..)
                      )


data VkSwapchainCreateInfoKHR =
  VkSwapchainCreateInfoKHR{ vkSType :: VkStructureType 
                          , vkPNext :: Ptr Void 
                          , vkFlags :: VkSwapchainCreateFlagsKHR 
                          , vkSurface :: VkSurfaceKHR 
                          , vkMinImageCount :: Word32 
                          , vkImageFormat :: VkFormat 
                          , vkImageColorSpace :: VkColorSpaceKHR 
                          , vkImageExtent :: VkExtent2D 
                          , vkImageArrayLayers :: Word32 
                          , vkImageUsage :: VkImageUsageFlags 
                          , vkImageSharingMode :: VkSharingMode 
                          , vkQueueFamilyIndexCount :: Word32 
                          , vkPQueueFamilyIndices :: Ptr Word32 
                          , vkPreTransform :: VkSurfaceTransformFlagBitsKHR 
                          , vkCompositeAlpha :: VkCompositeAlphaFlagBitsKHR 
                          , vkPresentMode :: VkPresentModeKHR 
                          , vkClipped :: VkBool32 
                          , vkOldSwapchain :: VkSwapchainKHR 
                          }
  deriving (Eq, Ord)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkSurface (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkMinImageCount (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 36) (vkImageFormat (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkImageColorSpace (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 44) (vkImageExtent (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 52) (vkImageArrayLayers (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 56) (vkImageUsage (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 60) (vkImageSharingMode (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 64) (vkQueueFamilyIndexCount (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 72) (vkPQueueFamilyIndices (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 80) (vkPreTransform (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 84) (vkCompositeAlpha (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 88) (vkPresentMode (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 92) (vkClipped (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 96) (vkOldSwapchain (poked :: VkSwapchainCreateInfoKHR))


-- ** vkGetSwapchainImagesKHR
foreign import ccall "vkGetSwapchainImagesKHR" vkGetSwapchainImagesKHR ::
  VkDevice ->
  VkSwapchainKHR -> Ptr Word32 -> Ptr VkImage -> IO VkResult

-- ** vkDestroySwapchainKHR
foreign import ccall "vkDestroySwapchainKHR" vkDestroySwapchainKHR ::
  VkDevice -> VkSwapchainKHR -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkQueuePresentKHR
foreign import ccall "vkQueuePresentKHR" vkQueuePresentKHR ::
  VkQueue -> Ptr VkPresentInfoKHR -> IO VkResult

-- ** VkSwapchainCreateFlagsKHR
-- | Opaque flag
newtype VkSwapchainCreateFlagsKHR = VkSwapchainCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- ** vkCreateSwapchainKHR
foreign import ccall "vkCreateSwapchainKHR" vkCreateSwapchainKHR ::
  VkDevice ->
  Ptr VkSwapchainCreateInfoKHR ->
    Ptr VkAllocationCallbacks -> Ptr VkSwapchainKHR -> IO VkResult

-- ** vkAcquireNextImageKHR
foreign import ccall "vkAcquireNextImageKHR" vkAcquireNextImageKHR ::
  VkDevice ->
  VkSwapchainKHR ->
    Word64 -> VkSemaphore -> VkFence -> Ptr Word32 -> IO VkResult


data VkPresentInfoKHR =
  VkPresentInfoKHR{ vkSType :: VkStructureType 
                  , vkPNext :: Ptr Void 
                  , vkWaitSemaphoreCount :: Word32 
                  , vkPWaitSemaphores :: Ptr VkSemaphore 
                  , vkSwapchainCount :: Word32 
                  , vkPSwapchains :: Ptr VkSwapchainKHR 
                  , vkPImageIndices :: Ptr Word32 
                  , vkPResults :: Ptr VkResult 
                  }
  deriving (Eq, Ord)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkWaitSemaphoreCount (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkPWaitSemaphores (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkSwapchainCount (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkPSwapchains (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 48) (vkPImageIndices (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 56) (vkPResults (poked :: VkPresentInfoKHR))


newtype VkSwapchainKHR = VkSwapchainKHR Word64
  deriving (Eq, Ord, Storable)

