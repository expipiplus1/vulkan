{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.KHR.DisplaySwapchain where

import Graphics.Vulkan.Device( Device(..)
                             )
import Graphics.Vulkan.KHR.Swapchain( Swapchain(..)
                                    , SwapchainCreateInfo(..)
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
import Graphics.Vulkan.Memory( AllocationCallbacks(..)
                             )
import Graphics.Vulkan.Core( Bool32(..)
                           , StructureType(..)
                           , Rect2D(..)
                           , Result(..)
                           )


data DisplayPresentInfo =
  DisplayPresentInfo{ sType :: StructureType 
                    , pNext :: Ptr Void 
                    , srcRect :: Rect2D 
                    , dstRect :: Rect2D 
                    , persistent :: Bool32 
                    }
  deriving (Eq, Ord)

instance Storable DisplayPresentInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = DisplayPresentInfo <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 32)
                                <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: DisplayPresentInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: DisplayPresentInfo))
                *> poke (ptr `plusPtr` 16) (srcRect (poked :: DisplayPresentInfo))
                *> poke (ptr `plusPtr` 32) (dstRect (poked :: DisplayPresentInfo))
                *> poke (ptr `plusPtr` 48) (persistent (poked :: DisplayPresentInfo))


-- ** createSharedSwapchains
foreign import ccall "vkCreateSharedSwapchainsKHR" createSharedSwapchains ::
  Device ->
  Word32 ->
    Ptr SwapchainCreateInfo ->
      Ptr AllocationCallbacks -> Ptr Swapchain -> IO Result

