{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Event where

import Graphics.Vulkan.Device( Device(..)
                             )
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
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

-- ** vkDestroyEvent
foreign import ccall "vkDestroyEvent" vkDestroyEvent ::
  Device -> Event -> Ptr VkAllocationCallbacks -> IO ()


data VkEventCreateInfo =
  VkEventCreateInfo{ sType :: VkStructureType 
                   , pNext :: Ptr Void 
                   , flags :: VkEventCreateFlags 
                   }
  deriving (Eq)

instance Storable VkEventCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkEventCreateInfo <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkEventCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkEventCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkEventCreateInfo))


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
  Ptr VkEventCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr Event -> IO VkResult

newtype Event = Event Word64
  deriving (Eq, Storable)

-- ** VkEventCreateFlags
-- | Opaque flag
newtype VkEventCreateFlags = VkEventCreateFlags VkFlags
  deriving (Eq, Storable)

