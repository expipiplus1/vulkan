{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.QueueSemaphore where

import Graphics.Vulkan.Device( Device(..)
                             )
import Data.Word( Word64(..)
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
import Graphics.Vulkan.Core( StructureType(..)
                           , Result(..)
                           , Flags(..)
                           )

-- ** SemaphoreCreateFlags
-- | Opaque flag
newtype SemaphoreCreateFlags = SemaphoreCreateFlags Flags
  deriving (Eq, Ord, Storable)

-- ** destroySemaphore
foreign import ccall "vkDestroySemaphore" destroySemaphore ::
  Device -> Semaphore -> Ptr AllocationCallbacks -> IO ()

newtype Semaphore = Semaphore Word64
  deriving (Eq, Ord, Storable)


data SemaphoreCreateInfo =
  SemaphoreCreateInfo{ sType :: StructureType 
                     , pNext :: Ptr Void 
                     , flags :: SemaphoreCreateFlags 
                     }
  deriving (Eq, Ord)

instance Storable SemaphoreCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = SemaphoreCreateInfo <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: SemaphoreCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: SemaphoreCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: SemaphoreCreateInfo))


-- ** createSemaphore
foreign import ccall "vkCreateSemaphore" createSemaphore ::
  Device ->
  Ptr SemaphoreCreateInfo ->
    Ptr AllocationCallbacks -> Ptr Semaphore -> IO Result

