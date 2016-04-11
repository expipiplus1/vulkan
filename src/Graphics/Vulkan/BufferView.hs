{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.BufferView where

import Graphics.Vulkan.Device( Device(..)
                             )
import Graphics.Vulkan.Buffer( Buffer(..)
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
                           , VkDeviceSize(..)
                           , VkFlags(..)
                           , VkFormat(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CSize(..)
                      )

-- ** vkCreateBufferView
foreign import ccall "vkCreateBufferView" vkCreateBufferView ::
  Device ->
  Ptr VkBufferViewCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr BufferView -> IO VkResult

newtype BufferView = BufferView Word64
  deriving (Eq, Storable)


data VkBufferViewCreateInfo =
  VkBufferViewCreateInfo{ sType :: VkStructureType 
                        , pNext :: Ptr Void 
                        , flags :: VkBufferViewCreateFlags 
                        , buffer :: Buffer 
                        , format :: VkFormat 
                        , offset :: VkDeviceSize 
                        , range :: VkDeviceSize 
                        }
  deriving (Eq)

instance Storable VkBufferViewCreateInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkBufferViewCreateInfo <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 32)
                                    <*> peek (ptr `plusPtr` 40)
                                    <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 24) (buffer (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 32) (format (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 40) (offset (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 48) (range (poked :: VkBufferViewCreateInfo))


-- ** VkBufferViewCreateFlags
-- | Opaque flag
newtype VkBufferViewCreateFlags = VkBufferViewCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** vkDestroyBufferView
foreign import ccall "vkDestroyBufferView" vkDestroyBufferView ::
  Device -> BufferView -> Ptr VkAllocationCallbacks -> IO ()

