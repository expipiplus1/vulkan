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
                            , VkImageSubresourceRange(..)
                            , VkImageLayout(..)
                            )
import Graphics.Vulkan.Core( VkStructureType(..)
                           , VkDeviceSize(..)
                           )


data VkBufferMemoryBarrier =
  VkBufferMemoryBarrier{ sType :: VkStructureType 
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

instance Storable VkBufferMemoryBarrier where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkBufferMemoryBarrier <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 20)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 28)
                                   <*> peek (ptr `plusPtr` 32)
                                   <*> peek (ptr `plusPtr` 40)
                                   <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (srcAccessMask (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (dstAccessMask (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 24) (srcQueueFamilyIndex (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 28) (dstQueueFamilyIndex (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 32) (buffer (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 40) (offset (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 48) (size (poked :: VkBufferMemoryBarrier))



data VkDrawIndexedIndirectCommand =
  VkDrawIndexedIndirectCommand{ indexCount :: Word32 
                              , instanceCount :: Word32 
                              , firstIndex :: Word32 
                              , vertexOffset :: Int32 
                              , firstInstance :: Word32 
                              }
  deriving (Eq)

instance Storable VkDrawIndexedIndirectCommand where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = VkDrawIndexedIndirectCommand <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 4)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 12)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (indexCount (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 4) (instanceCount (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 8) (firstIndex (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 12) (vertexOffset (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 16) (firstInstance (poked :: VkDrawIndexedIndirectCommand))



data VkImageMemoryBarrier =
  VkImageMemoryBarrier{ sType :: VkStructureType 
                      , pNext :: Ptr Void 
                      , srcAccessMask :: VkAccessFlags 
                      , dstAccessMask :: VkAccessFlags 
                      , oldLayout :: VkImageLayout 
                      , newLayout :: VkImageLayout 
                      , srcQueueFamilyIndex :: Word32 
                      , dstQueueFamilyIndex :: Word32 
                      , image :: Image 
                      , subresourceRange :: VkImageSubresourceRange 
                      }
  deriving (Eq)

instance Storable VkImageMemoryBarrier where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkImageMemoryBarrier <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 20)
                                  <*> peek (ptr `plusPtr` 24)
                                  <*> peek (ptr `plusPtr` 28)
                                  <*> peek (ptr `plusPtr` 32)
                                  <*> peek (ptr `plusPtr` 36)
                                  <*> peek (ptr `plusPtr` 40)
                                  <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (srcAccessMask (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (dstAccessMask (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 24) (oldLayout (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 28) (newLayout (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 32) (srcQueueFamilyIndex (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 36) (dstQueueFamilyIndex (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 40) (image (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 48) (subresourceRange (poked :: VkImageMemoryBarrier))



data VkMemoryBarrier =
  VkMemoryBarrier{ sType :: VkStructureType 
                 , pNext :: Ptr Void 
                 , srcAccessMask :: VkAccessFlags 
                 , dstAccessMask :: VkAccessFlags 
                 }
  deriving (Eq)

instance Storable VkMemoryBarrier where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryBarrier <$> peek (ptr `plusPtr` 0)
                             <*> peek (ptr `plusPtr` 8)
                             <*> peek (ptr `plusPtr` 16)
                             <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkMemoryBarrier))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (srcAccessMask (poked :: VkMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (dstAccessMask (poked :: VkMemoryBarrier))



data VkDrawIndirectCommand =
  VkDrawIndirectCommand{ vertexCount :: Word32 
                       , instanceCount :: Word32 
                       , firstVertex :: Word32 
                       , firstInstance :: Word32 
                       }
  deriving (Eq)

instance Storable VkDrawIndirectCommand where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VkDrawIndirectCommand <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 4)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vertexCount (poked :: VkDrawIndirectCommand))
                *> poke (ptr `plusPtr` 4) (instanceCount (poked :: VkDrawIndirectCommand))
                *> poke (ptr `plusPtr` 8) (firstVertex (poked :: VkDrawIndirectCommand))
                *> poke (ptr `plusPtr` 12) (firstInstance (poked :: VkDrawIndirectCommand))



data VkDispatchIndirectCommand =
  VkDispatchIndirectCommand{ x :: Word32 
                           , y :: Word32 
                           , z :: Word32 
                           }
  deriving (Eq)

instance Storable VkDispatchIndirectCommand where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkDispatchIndirectCommand <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 4)
                                       <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (x (poked :: VkDispatchIndirectCommand))
                *> poke (ptr `plusPtr` 4) (y (poked :: VkDispatchIndirectCommand))
                *> poke (ptr `plusPtr` 8) (z (poked :: VkDispatchIndirectCommand))


