{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.BufferView where

import Graphics.Vulkan.Device( Device(..)
                             )
import Graphics.Vulkan.Buffer( Buffer(..)
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
                           , Format(..)
                           , Result(..)
                           , DeviceSize(..)
                           , Flags(..)
                           )

-- ** createBufferView
foreign import ccall "vkCreateBufferView" createBufferView ::
  Device ->
  Ptr BufferViewCreateInfo ->
    Ptr AllocationCallbacks -> Ptr BufferView -> IO Result

newtype BufferView = BufferView Word64
  deriving (Eq, Ord, Storable)


data BufferViewCreateInfo =
  BufferViewCreateInfo{ sType :: StructureType 
                      , pNext :: Ptr Void 
                      , flags :: BufferViewCreateFlags 
                      , buffer :: Buffer 
                      , format :: Format 
                      , offset :: DeviceSize 
                      , range :: DeviceSize 
                      }
  deriving (Eq, Ord)

instance Storable BufferViewCreateInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = BufferViewCreateInfo <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
                                  <*> peek (ptr `plusPtr` 32)
                                  <*> peek (ptr `plusPtr` 40)
                                  <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: BufferViewCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: BufferViewCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: BufferViewCreateInfo))
                *> poke (ptr `plusPtr` 24) (buffer (poked :: BufferViewCreateInfo))
                *> poke (ptr `plusPtr` 32) (format (poked :: BufferViewCreateInfo))
                *> poke (ptr `plusPtr` 40) (offset (poked :: BufferViewCreateInfo))
                *> poke (ptr `plusPtr` 48) (range (poked :: BufferViewCreateInfo))


-- ** BufferViewCreateFlags
-- | Opaque flag
newtype BufferViewCreateFlags = BufferViewCreateFlags Flags
  deriving (Eq, Ord, Storable)

-- ** destroyBufferView
foreign import ccall "vkDestroyBufferView" destroyBufferView ::
  Device -> BufferView -> Ptr AllocationCallbacks -> IO ()

