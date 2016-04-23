{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Event where

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
import Graphics.Vulkan.Core( VkStructureType(..)
                           , VkFlags(..)
                           , VkResult(..)
                           )

-- ** vkDestroyEvent
foreign import ccall "vkDestroyEvent" vkDestroyEvent ::
  Device -> Event -> Ptr AllocationCallbacks -> IO ()


data EventCreateInfo =
  EventCreateInfo{ sType :: VkStructureType 
                 , pNext :: Ptr Void 
                 , flags :: VkEventCreateFlags 
                 }
  deriving (Eq)

instance Storable EventCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = EventCreateInfo <$> peek (ptr `plusPtr` 0)
                             <*> peek (ptr `plusPtr` 8)
                             <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: EventCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: EventCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: EventCreateInfo))


-- ** vkSetEvent
foreign import ccall "vkSetEvent" vkSetEvent ::
  Device -> Event -> IO VkResult

-- ** vkGetEventStatus
foreign import ccall "vkGetEventStatus" vkGetEventStatus ::
  Device -> Event -> IO VkResult

-- ** vkResetEvent
foreign import ccall "vkResetEvent" vkResetEvent ::
  Device -> Event -> IO VkResult

-- ** vkCreateEvent
foreign import ccall "vkCreateEvent" vkCreateEvent ::
  Device ->
  Ptr EventCreateInfo ->
    Ptr AllocationCallbacks -> Ptr Event -> IO VkResult

newtype Event = Event Word64
  deriving (Eq, Storable)

-- ** VkEventCreateFlags
-- | Opaque flag
newtype VkEventCreateFlags = VkEventCreateFlags VkFlags
  deriving (Eq, Storable)

