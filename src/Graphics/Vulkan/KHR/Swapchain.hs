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
import Graphics.Vulkan.KHR.Surface( PresentMode(..)
                                  , Surface(..)
                                  , ColorSpace(..)
                                  , CompositeAlphaFlags(..)
                                  , SurfaceTransformFlags(..)
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


data SwapchainCreateInfo =
  SwapchainCreateInfo{ sType :: StructureType 
                     , pNext :: Ptr Void 
                     , flags :: SwapchainCreateFlags 
                     , surface :: Surface 
                     , minImageCount :: Word32 
                     , imageFormat :: Format 
                     , imageColorSpace :: ColorSpace 
                     , imageExtent :: Extent2D 
                     , imageArrayLayers :: Word32 
                     , imageUsage :: ImageUsageFlags 
                     , imageSharingMode :: SharingMode 
                     , queueFamilyIndexCount :: Word32 
                     , pQueueFamilyIndices :: Ptr Word32 
                     , preTransform :: SurfaceTransformFlags 
                     , compositeAlpha :: CompositeAlphaFlags 
                     , presentMode :: PresentMode 
                     , clipped :: Bool32 
                     , oldSwapchain :: Swapchain 
                     }
  deriving (Eq, Ord)

instance Storable SwapchainCreateInfo where
  sizeOf ~_ = 104
  alignment ~_ = 8
  peek ptr = SwapchainCreateInfo <$> peek (ptr `plusPtr` 0)
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
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: SwapchainCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: SwapchainCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: SwapchainCreateInfo))
                *> poke (ptr `plusPtr` 24) (surface (poked :: SwapchainCreateInfo))
                *> poke (ptr `plusPtr` 32) (minImageCount (poked :: SwapchainCreateInfo))
                *> poke (ptr `plusPtr` 36) (imageFormat (poked :: SwapchainCreateInfo))
                *> poke (ptr `plusPtr` 40) (imageColorSpace (poked :: SwapchainCreateInfo))
                *> poke (ptr `plusPtr` 44) (imageExtent (poked :: SwapchainCreateInfo))
                *> poke (ptr `plusPtr` 52) (imageArrayLayers (poked :: SwapchainCreateInfo))
                *> poke (ptr `plusPtr` 56) (imageUsage (poked :: SwapchainCreateInfo))
                *> poke (ptr `plusPtr` 60) (imageSharingMode (poked :: SwapchainCreateInfo))
                *> poke (ptr `plusPtr` 64) (queueFamilyIndexCount (poked :: SwapchainCreateInfo))
                *> poke (ptr `plusPtr` 72) (pQueueFamilyIndices (poked :: SwapchainCreateInfo))
                *> poke (ptr `plusPtr` 80) (preTransform (poked :: SwapchainCreateInfo))
                *> poke (ptr `plusPtr` 84) (compositeAlpha (poked :: SwapchainCreateInfo))
                *> poke (ptr `plusPtr` 88) (presentMode (poked :: SwapchainCreateInfo))
                *> poke (ptr `plusPtr` 92) (clipped (poked :: SwapchainCreateInfo))
                *> poke (ptr `plusPtr` 96) (oldSwapchain (poked :: SwapchainCreateInfo))


-- ** getSwapchainImages
foreign import ccall "vkGetSwapchainImagesKHR" getSwapchainImages ::
  Device -> Swapchain -> Ptr Word32 -> Ptr Image -> IO Result

-- ** destroySwapchain
foreign import ccall "vkDestroySwapchainKHR" destroySwapchain ::
  Device -> Swapchain -> Ptr AllocationCallbacks -> IO ()

-- ** queuePresent
foreign import ccall "vkQueuePresentKHR" queuePresent ::
  Queue -> Ptr PresentInfo -> IO Result

-- ** SwapchainCreateFlags
-- | Opaque flag
newtype SwapchainCreateFlags = SwapchainCreateFlags Flags
  deriving (Eq, Ord, Storable)

-- ** createSwapchain
foreign import ccall "vkCreateSwapchainKHR" createSwapchain ::
  Device ->
  Ptr SwapchainCreateInfo ->
    Ptr AllocationCallbacks -> Ptr Swapchain -> IO Result

-- ** acquireNextImage
foreign import ccall "vkAcquireNextImageKHR" acquireNextImage ::
  Device ->
  Swapchain ->
    Word64 -> Semaphore -> Fence -> Ptr Word32 -> IO Result


data PresentInfo =
  PresentInfo{ sType :: StructureType 
             , pNext :: Ptr Void 
             , waitSemaphoreCount :: Word32 
             , pWaitSemaphores :: Ptr Semaphore 
             , swapchainCount :: Word32 
             , pSwapchains :: Ptr Swapchain 
             , pImageIndices :: Ptr Word32 
             , pResults :: Ptr Result 
             }
  deriving (Eq, Ord)

instance Storable PresentInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = PresentInfo <$> peek (ptr `plusPtr` 0)
                         <*> peek (ptr `plusPtr` 8)
                         <*> peek (ptr `plusPtr` 16)
                         <*> peek (ptr `plusPtr` 24)
                         <*> peek (ptr `plusPtr` 32)
                         <*> peek (ptr `plusPtr` 40)
                         <*> peek (ptr `plusPtr` 48)
                         <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: PresentInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: PresentInfo))
                *> poke (ptr `plusPtr` 16) (waitSemaphoreCount (poked :: PresentInfo))
                *> poke (ptr `plusPtr` 24) (pWaitSemaphores (poked :: PresentInfo))
                *> poke (ptr `plusPtr` 32) (swapchainCount (poked :: PresentInfo))
                *> poke (ptr `plusPtr` 40) (pSwapchains (poked :: PresentInfo))
                *> poke (ptr `plusPtr` 48) (pImageIndices (poked :: PresentInfo))
                *> poke (ptr `plusPtr` 56) (pResults (poked :: PresentInfo))


newtype Swapchain = Swapchain Word64
  deriving (Eq, Ord, Storable)

