{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.KHR.DisplaySwapchain where

import Graphics.Vulkan.Device( Device(..)
                             )
import Graphics.Vulkan.KHR.Swapchain( SwapchainKHR(..)
                                    , VkSwapchainCreateInfoKHR(..)
                                    , VkSwapchainCreateFlagsKHR(..)
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
                                  , SurfaceKHR(..)
                                  )
import Data.Int( Int32
               )
import Foreign.Storable( Storable(..)
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
                            , VkImageUsageFlagBits(..)
                            )
import Graphics.Vulkan.Core( VkResult(..)
                           , VkBool32(..)
                           , VkExtent2D(..)
                           , VkFlags(..)
                           , VkFormat(..)
                           , VkOffset2D(..)
                           , VkRect2D(..)
                           , VkStructureType(..)
                           , VkSharingMode(..)
                           )
import Foreign.C.Types( CSize(..)
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

