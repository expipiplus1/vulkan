{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.OtherTypes where

import Graphics.Vulkan.Buffer( Buffer(..)
                             )
import Graphics.Vulkan.Pass( VkAccessFlags(..)
                           )
import Data.Word( Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , plusPtr
                  )
import Data.Int( Int32(..)
               )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.Image( Image(..)
                            , ImageSubresourceRange(..)
                            , VkImageLayout(..)
                            )
import Graphics.Vulkan.Core( VkStructureType(..)
                           , VkDeviceSize(..)
                           )


data BufferMemoryBarrier =
  BufferMemoryBarrier{ sType :: VkStructureType 
                     , pNext :: Ptr Void 
                     , srcAccessMask :: VkAccessFlags 
                     , dstAccessMask :: VkAccessFlags 
                     , srcQueueFamilyIndex :: Word32 
                     , dstQueueFamilyIndex :: Word32 
                     , buffer :: Buffer 
                     , offset :: VkDeviceSize 
                     , size :: VkDeviceSize 
                     }
  deriving (Eq)

instance Storable BufferMemoryBarrier where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = BufferMemoryBarrier <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 20)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 28)
                                 <*> peek (ptr `plusPtr` 32)
                                 <*> peek (ptr `plusPtr` 40)
                                 <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: BufferMemoryBarrier))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: BufferMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (srcAccessMask (poked :: BufferMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (dstAccessMask (poked :: BufferMemoryBarrier))
                *> poke (ptr `plusPtr` 24) (srcQueueFamilyIndex (poked :: BufferMemoryBarrier))
                *> poke (ptr `plusPtr` 28) (dstQueueFamilyIndex (poked :: BufferMemoryBarrier))
                *> poke (ptr `plusPtr` 32) (buffer (poked :: BufferMemoryBarrier))
                *> poke (ptr `plusPtr` 40) (offset (poked :: BufferMemoryBarrier))
                *> poke (ptr `plusPtr` 48) (size (poked :: BufferMemoryBarrier))



data DrawIndexedIndirectCommand =
  DrawIndexedIndirectCommand{ indexCount :: Word32 
                            , instanceCount :: Word32 
                            , firstIndex :: Word32 
                            , vertexOffset :: Int32 
                            , firstInstance :: Word32 
                            }
  deriving (Eq)

instance Storable DrawIndexedIndirectCommand where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = DrawIndexedIndirectCommand <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 4)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 12)
                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (indexCount (poked :: DrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 4) (instanceCount (poked :: DrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 8) (firstIndex (poked :: DrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 12) (vertexOffset (poked :: DrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 16) (firstInstance (poked :: DrawIndexedIndirectCommand))



data ImageMemoryBarrier =
  ImageMemoryBarrier{ sType :: VkStructureType 
                    , pNext :: Ptr Void 
                    , srcAccessMask :: VkAccessFlags 
                    , dstAccessMask :: VkAccessFlags 
                    , oldLayout :: VkImageLayout 
                    , newLayout :: VkImageLayout 
                    , srcQueueFamilyIndex :: Word32 
                    , dstQueueFamilyIndex :: Word32 
                    , image :: Image 
                    , subresourceRange :: ImageSubresourceRange 
                    }
  deriving (Eq)

instance Storable ImageMemoryBarrier where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = ImageMemoryBarrier <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 20)
                                <*> peek (ptr `plusPtr` 24)
                                <*> peek (ptr `plusPtr` 28)
                                <*> peek (ptr `plusPtr` 32)
                                <*> peek (ptr `plusPtr` 36)
                                <*> peek (ptr `plusPtr` 40)
                                <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: ImageMemoryBarrier))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: ImageMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (srcAccessMask (poked :: ImageMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (dstAccessMask (poked :: ImageMemoryBarrier))
                *> poke (ptr `plusPtr` 24) (oldLayout (poked :: ImageMemoryBarrier))
                *> poke (ptr `plusPtr` 28) (newLayout (poked :: ImageMemoryBarrier))
                *> poke (ptr `plusPtr` 32) (srcQueueFamilyIndex (poked :: ImageMemoryBarrier))
                *> poke (ptr `plusPtr` 36) (dstQueueFamilyIndex (poked :: ImageMemoryBarrier))
                *> poke (ptr `plusPtr` 40) (image (poked :: ImageMemoryBarrier))
                *> poke (ptr `plusPtr` 48) (subresourceRange (poked :: ImageMemoryBarrier))



data MemoryBarrier =
  MemoryBarrier{ sType :: VkStructureType 
               , pNext :: Ptr Void 
               , srcAccessMask :: VkAccessFlags 
               , dstAccessMask :: VkAccessFlags 
               }
  deriving (Eq)

instance Storable MemoryBarrier where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = MemoryBarrier <$> peek (ptr `plusPtr` 0)
                           <*> peek (ptr `plusPtr` 8)
                           <*> peek (ptr `plusPtr` 16)
                           <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: MemoryBarrier))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: MemoryBarrier))
                *> poke (ptr `plusPtr` 16) (srcAccessMask (poked :: MemoryBarrier))
                *> poke (ptr `plusPtr` 20) (dstAccessMask (poked :: MemoryBarrier))



data DrawIndirectCommand =
  DrawIndirectCommand{ vertexCount :: Word32 
                     , instanceCount :: Word32 
                     , firstVertex :: Word32 
                     , firstInstance :: Word32 
                     }
  deriving (Eq)

instance Storable DrawIndirectCommand where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = DrawIndirectCommand <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 4)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vertexCount (poked :: DrawIndirectCommand))
                *> poke (ptr `plusPtr` 4) (instanceCount (poked :: DrawIndirectCommand))
                *> poke (ptr `plusPtr` 8) (firstVertex (poked :: DrawIndirectCommand))
                *> poke (ptr `plusPtr` 12) (firstInstance (poked :: DrawIndirectCommand))



data DispatchIndirectCommand =
  DispatchIndirectCommand{ x :: Word32 
                         , y :: Word32 
                         , z :: Word32 
                         }
  deriving (Eq)

instance Storable DispatchIndirectCommand where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = DispatchIndirectCommand <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 4)
                                     <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (x (poked :: DispatchIndirectCommand))
                *> poke (ptr `plusPtr` 4) (y (poked :: DispatchIndirectCommand))
                *> poke (ptr `plusPtr` 8) (z (poked :: DispatchIndirectCommand))


