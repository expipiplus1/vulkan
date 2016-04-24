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
import Graphics.Vulkan.Core( StructureType(..)
                           , Result(..)
                           , Flags(..)
                           )

-- ** destroyEvent
foreign import ccall "vkDestroyEvent" destroyEvent ::
  Device -> Event -> Ptr AllocationCallbacks -> IO ()


data EventCreateInfo =
  EventCreateInfo{ sType :: StructureType 
                 , pNext :: Ptr Void 
                 , flags :: EventCreateFlags 
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


-- ** setEvent
foreign import ccall "vkSetEvent" setEvent ::
  Device -> Event -> IO Result

-- ** getEventStatus
foreign import ccall "vkGetEventStatus" getEventStatus ::
  Device -> Event -> IO Result

-- ** resetEvent
foreign import ccall "vkResetEvent" resetEvent ::
  Device -> Event -> IO Result

-- ** createEvent
foreign import ccall "vkCreateEvent" createEvent ::
  Device ->
  Ptr EventCreateInfo ->
    Ptr AllocationCallbacks -> Ptr Event -> IO Result

newtype Event = Event Word64
  deriving (Eq, Storable)

-- ** EventCreateFlags
-- | Opaque flag
newtype EventCreateFlags = EventCreateFlags Flags
  deriving (Eq, Storable)

