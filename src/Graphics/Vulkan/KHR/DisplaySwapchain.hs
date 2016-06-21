{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.KHR.DisplaySwapchain where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Graphics.Vulkan.KHR.Swapchain( VkSwapchainKHR(..)
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
                                  , VkSurfaceKHR(..)
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
  VkDisplayPresentInfoKHR{ vkSType :: VkStructureType 
                         , vkPNext :: Ptr Void 
                         , vkSrcRect :: VkRect2D 
                         , vkDstRect :: VkRect2D 
                         , vkPersistent :: VkBool32 
                         }
  deriving (Eq, Ord, Show)

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


-- ** vkCreateSharedSwapchainsKHR
foreign import ccall "vkCreateSharedSwapchainsKHR" vkCreateSharedSwapchainsKHR ::
  VkDevice ->
  Word32 ->
    Ptr VkSwapchainCreateInfoKHR ->
      Ptr VkAllocationCallbacks -> Ptr VkSwapchainKHR -> IO VkResult

pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION =  0x9
pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME =  "VK_KHR_display_swapchain"
pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR = VkStructureType 1000003000
pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR = VkResult (-1000003001)
