{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.KHR.DisplaySwapchain where

import Graphics.Vulkan.Device( Device(..)
                             )
import Graphics.Vulkan.KHR.Swapchain( VkSwapchainCreateInfoKHR(..)
                                    , SwapchainKHR(..)
                                    )
import Data.Word( Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , plusPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.Memory( VkAllocationCallbacks(..)
                             )
import Graphics.Vulkan.Core( VkStructureType(..)
                           , VkRect2D(..)
                           , VkBool32(..)
                           , VkResult(..)
                           )


data VkDisplayPresentInfoKHR =
  VkDisplayPresentInfoKHR{ sType :: VkStructureType 
                         , pNext :: Ptr Void 
                         , srcRect :: VkRect2D 
                         , dstRect :: VkRect2D 
                         , persistent :: VkBool32 
                         }
  deriving (Eq)

instance Storable VkDisplayPresentInfoKHR where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkDisplayPresentInfoKHR <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 32)
                                     <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 16) (srcRect (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 32) (dstRect (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 48) (persistent (poked :: VkDisplayPresentInfoKHR))


-- ** vkCreateSharedSwapchainsKHR
foreign import ccall "vkCreateSharedSwapchainsKHR" vkCreateSharedSwapchainsKHR ::
  Device ->
  Word32 ->
    Ptr VkSwapchainCreateInfoKHR ->
      Ptr VkAllocationCallbacks -> Ptr SwapchainKHR -> IO VkResult

