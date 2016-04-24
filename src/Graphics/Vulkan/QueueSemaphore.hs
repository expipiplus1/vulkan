{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.QueueSemaphore where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Data.Bits( Bits
                , FiniteBits
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
import Graphics.Vulkan.Core( VkResult(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CSize(..)
                      )

-- ** VkSemaphoreCreateFlags
-- | Opaque flag
newtype VkSemaphoreCreateFlags = VkSemaphoreCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- ** vkDestroySemaphore
foreign import ccall "vkDestroySemaphore" vkDestroySemaphore ::
  VkDevice -> VkSemaphore -> Ptr VkAllocationCallbacks -> IO ()

newtype VkSemaphore = VkSemaphore Word64
  deriving (Eq, Ord, Storable)


data VkSemaphoreCreateInfo =
  VkSemaphoreCreateInfo{ vkSType :: VkStructureType 
                       , vkPNext :: Ptr Void 
                       , vkFlags :: VkSemaphoreCreateFlags 
                       }
  deriving (Eq, Ord)

instance Storable VkSemaphoreCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSemaphoreCreateInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSemaphoreCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSemaphoreCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkSemaphoreCreateInfo))


-- ** vkCreateSemaphore
foreign import ccall "vkCreateSemaphore" vkCreateSemaphore ::
  VkDevice ->
  Ptr VkSemaphoreCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkSemaphore -> IO VkResult

