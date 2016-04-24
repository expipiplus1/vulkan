{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.KHR.Swapchain where

import Graphics.Vulkan.Device( Device(..)
                             )
import Data.Word( Word64(..)
                , Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , plusPtr
                  )
import Graphics.Vulkan.KHR.Surface( SurfaceTransformFlagsKHR(..)
                                  , PresentModeKHR(..)
                                  , ColorSpaceKHR(..)
                                  , SurfaceKHR(..)
                                  , CompositeAlphaFlagsKHR(..)
                                  )
import Graphics.Vulkan.Queue( Queue(..)
                            )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Fence( Fence(..)
                            )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.Memory( AllocationCallbacks(..)
                             )
import Graphics.Vulkan.Image( Image(..)
                            , ImageUsageFlags(..)
                            )
import Graphics.Vulkan.QueueSemaphore( Semaphore(..)
                                     )
import Graphics.Vulkan.Core( Bool32(..)
                           , SharingMode(..)
                           , StructureType(..)
                           , Format(..)
                           , Result(..)
                           , Flags(..)
                           , Extent2D(..)
                           )


data SwapchainCreateInfoKHR =
  SwapchainCreateInfoKHR{ sType :: StructureType 
                        , pNext :: Ptr Void 
                        , flags :: SwapchainCreateFlagsKHR 
                        , surface :: SurfaceKHR 
                        , minImageCount :: Word32 
                        , imageFormat :: Format 
                        , imageColorSpace :: ColorSpaceKHR 
                        , imageExtent :: Extent2D 
                        , imageArrayLayers :: Word32 
                        , imageUsage :: ImageUsageFlags 
                        , imageSharingMode :: SharingMode 
                        , queueFamilyIndexCount :: Word32 
                        , pQueueFamilyIndices :: Ptr Word32 
                        , preTransform :: SurfaceTransformFlagsKHR 
                        , compositeAlpha :: CompositeAlphaFlagsKHR 
                        , presentMode :: PresentModeKHR 
                        , clipped :: Bool32 
                        , oldSwapchain :: SwapchainKHR 
                        }
  deriving (Eq, Ord)

instance Storable SwapchainCreateInfoKHR where
  sizeOf ~_ = 104
  alignment ~_ = 8
  peek ptr = SwapchainCreateInfoKHR <$> peek (ptr `plusPtr` 0)
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
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: SwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: SwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (flags (poked :: SwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (surface (poked :: SwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (minImageCount (poked :: SwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 36) (imageFormat (poked :: SwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 40) (imageColorSpace (poked :: SwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 44) (imageExtent (poked :: SwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 52) (imageArrayLayers (poked :: SwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 56) (imageUsage (poked :: SwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 60) (imageSharingMode (poked :: SwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 64) (queueFamilyIndexCount (poked :: SwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 72) (pQueueFamilyIndices (poked :: SwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 80) (preTransform (poked :: SwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 84) (compositeAlpha (poked :: SwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 88) (presentMode (poked :: SwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 92) (clipped (poked :: SwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 96) (oldSwapchain (poked :: SwapchainCreateInfoKHR))


-- ** getSwapchainImagesKHR
foreign import ccall "vkGetSwapchainImagesKHR" getSwapchainImagesKHR ::
  Device -> SwapchainKHR -> Ptr Word32 -> Ptr Image -> IO Result

-- ** destroySwapchainKHR
foreign import ccall "vkDestroySwapchainKHR" destroySwapchainKHR ::
  Device -> SwapchainKHR -> Ptr AllocationCallbacks -> IO ()

-- ** queuePresentKHR
foreign import ccall "vkQueuePresentKHR" queuePresentKHR ::
  Queue -> Ptr PresentInfoKHR -> IO Result

-- ** SwapchainCreateFlagsKHR
-- | Opaque flag
newtype SwapchainCreateFlagsKHR = SwapchainCreateFlagsKHR Flags
  deriving (Eq, Ord, Storable)

-- ** createSwapchainKHR
foreign import ccall "vkCreateSwapchainKHR" createSwapchainKHR ::
  Device ->
  Ptr SwapchainCreateInfoKHR ->
    Ptr AllocationCallbacks -> Ptr SwapchainKHR -> IO Result

-- ** acquireNextImageKHR
foreign import ccall "vkAcquireNextImageKHR" acquireNextImageKHR ::
  Device ->
  SwapchainKHR ->
    Word64 -> Semaphore -> Fence -> Ptr Word32 -> IO Result


data PresentInfoKHR =
  PresentInfoKHR{ sType :: StructureType 
                , pNext :: Ptr Void 
                , waitSemaphoreCount :: Word32 
                , pWaitSemaphores :: Ptr Semaphore 
                , swapchainCount :: Word32 
                , pSwapchains :: Ptr SwapchainKHR 
                , pImageIndices :: Ptr Word32 
                , pResults :: Ptr Result 
                }
  deriving (Eq, Ord)

instance Storable PresentInfoKHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = PresentInfoKHR <$> peek (ptr `plusPtr` 0)
                            <*> peek (ptr `plusPtr` 8)
                            <*> peek (ptr `plusPtr` 16)
                            <*> peek (ptr `plusPtr` 24)
                            <*> peek (ptr `plusPtr` 32)
                            <*> peek (ptr `plusPtr` 40)
                            <*> peek (ptr `plusPtr` 48)
                            <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: PresentInfoKHR))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: PresentInfoKHR))
                *> poke (ptr `plusPtr` 16) (waitSemaphoreCount (poked :: PresentInfoKHR))
                *> poke (ptr `plusPtr` 24) (pWaitSemaphores (poked :: PresentInfoKHR))
                *> poke (ptr `plusPtr` 32) (swapchainCount (poked :: PresentInfoKHR))
                *> poke (ptr `plusPtr` 40) (pSwapchains (poked :: PresentInfoKHR))
                *> poke (ptr `plusPtr` 48) (pImageIndices (poked :: PresentInfoKHR))
                *> poke (ptr `plusPtr` 56) (pResults (poked :: PresentInfoKHR))


newtype SwapchainKHR = SwapchainKHR Word64
  deriving (Eq, Ord, Storable)

