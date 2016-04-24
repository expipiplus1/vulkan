{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.OtherTypes where

import Graphics.Vulkan.Buffer( VkBuffer(..)
                             )
import Graphics.Vulkan.Pass( VkAccessFlags(..)
                           , VkAccessFlagBits(..)
                           )
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Data.Int( Int32
               )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Image( VkImage(..)
                            , VkImageLayout(..)
                            , VkImageAspectFlagBits(..)
                            , VkImageSubresourceRange(..)
                            , VkImageAspectFlags(..)
                            )
import Graphics.Vulkan.Core( VkDeviceSize(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           )


data VkBufferMemoryBarrier =
  VkBufferMemoryBarrier{ vkSType :: VkStructureType 
                       , vkPNext :: Ptr Void 
                       , vkSrcAccessMask :: VkAccessFlags 
                       , vkDstAccessMask :: VkAccessFlags 
                       , vkSrcQueueFamilyIndex :: Word32 
                       , vkDstQueueFamilyIndex :: Word32 
                       , vkBuffer :: VkBuffer 
                       , vkOffset :: VkDeviceSize 
                       , vkSize :: VkDeviceSize 
                       }
  deriving (Eq, Ord)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 24) (vkSrcQueueFamilyIndex (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 28) (vkDstQueueFamilyIndex (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 32) (vkBuffer (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 40) (vkOffset (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 48) (vkSize (poked :: VkBufferMemoryBarrier))



data VkDrawIndexedIndirectCommand =
  VkDrawIndexedIndirectCommand{ vkIndexCount :: Word32 
                              , vkInstanceCount :: Word32 
                              , vkFirstIndex :: Word32 
                              , vkVertexOffset :: Int32 
                              , vkFirstInstance :: Word32 
                              }
  deriving (Eq, Ord)

instance Storable VkDrawIndexedIndirectCommand where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = VkDrawIndexedIndirectCommand <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 4)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 12)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkIndexCount (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 4) (vkInstanceCount (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 8) (vkFirstIndex (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 12) (vkVertexOffset (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 16) (vkFirstInstance (poked :: VkDrawIndexedIndirectCommand))



data VkImageMemoryBarrier =
  VkImageMemoryBarrier{ vkSType :: VkStructureType 
                      , vkPNext :: Ptr Void 
                      , vkSrcAccessMask :: VkAccessFlags 
                      , vkDstAccessMask :: VkAccessFlags 
                      , vkOldLayout :: VkImageLayout 
                      , vkNewLayout :: VkImageLayout 
                      , vkSrcQueueFamilyIndex :: Word32 
                      , vkDstQueueFamilyIndex :: Word32 
                      , vkImage :: VkImage 
                      , vkSubresourceRange :: VkImageSubresourceRange 
                      }
  deriving (Eq, Ord)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 24) (vkOldLayout (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 28) (vkNewLayout (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 32) (vkSrcQueueFamilyIndex (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 36) (vkDstQueueFamilyIndex (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 40) (vkImage (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 48) (vkSubresourceRange (poked :: VkImageMemoryBarrier))



data VkMemoryBarrier =
  VkMemoryBarrier{ vkSType :: VkStructureType 
                 , vkPNext :: Ptr Void 
                 , vkSrcAccessMask :: VkAccessFlags 
                 , vkDstAccessMask :: VkAccessFlags 
                 }
  deriving (Eq, Ord)

instance Storable VkMemoryBarrier where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryBarrier <$> peek (ptr `plusPtr` 0)
                             <*> peek (ptr `plusPtr` 8)
                             <*> peek (ptr `plusPtr` 16)
                             <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryBarrier))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkMemoryBarrier))



data VkDrawIndirectCommand =
  VkDrawIndirectCommand{ vkVertexCount :: Word32 
                       , vkInstanceCount :: Word32 
                       , vkFirstVertex :: Word32 
                       , vkFirstInstance :: Word32 
                       }
  deriving (Eq, Ord)

instance Storable VkDrawIndirectCommand where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VkDrawIndirectCommand <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 4)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkVertexCount (poked :: VkDrawIndirectCommand))
                *> poke (ptr `plusPtr` 4) (vkInstanceCount (poked :: VkDrawIndirectCommand))
                *> poke (ptr `plusPtr` 8) (vkFirstVertex (poked :: VkDrawIndirectCommand))
                *> poke (ptr `plusPtr` 12) (vkFirstInstance (poked :: VkDrawIndirectCommand))



data VkDispatchIndirectCommand =
  VkDispatchIndirectCommand{ vkX :: Word32 
                           , vkY :: Word32 
                           , vkZ :: Word32 
                           }
  deriving (Eq, Ord)

instance Storable VkDispatchIndirectCommand where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkDispatchIndirectCommand <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 4)
                                       <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkX (poked :: VkDispatchIndirectCommand))
                *> poke (ptr `plusPtr` 4) (vkY (poked :: VkDispatchIndirectCommand))
                *> poke (ptr `plusPtr` 8) (vkZ (poked :: VkDispatchIndirectCommand))


