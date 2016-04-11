{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.QueueSemaphore where

import Data.Word( Word64
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )

-- ** VkSemaphoreCreateFlags
-- | Opaque flag
newtype VkSemaphoreCreateFlags = VkSemaphoreCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** vkDestroySemaphore
foreign import ccall "vkDestroySemaphore" vkDestroySemaphore ::
  Device -> Semaphore -> Ptr VkAllocationCallbacks -> IO ()

newtype Semaphore = Semaphore Word64
  deriving (Eq, Storable)


data VkSemaphoreCreateInfo =
  VkSemaphoreCreateInfo{ sType :: VkStructureType 
                       , pNext :: Ptr Void 
                       , flags :: VkSemaphoreCreateFlags 
                       }
  deriving (Eq)

instance Storable VkSemaphoreCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSemaphoreCreateInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkSemaphoreCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkSemaphoreCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkSemaphoreCreateInfo))


-- ** vkCreateSemaphore
foreign import ccall "vkCreateSemaphore" vkCreateSemaphore ::
  Device ->
  Ptr VkSemaphoreCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr Semaphore -> IO VkResult

