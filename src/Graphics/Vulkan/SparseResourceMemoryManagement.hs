{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.SparseResourceMemoryManagement where

import Graphics.Vulkan.Device( Device(..)
                             , PhysicalDevice(..)
                             )
import Graphics.Vulkan.Buffer( Buffer(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , plusPtr
                  )
import Graphics.Vulkan.Queue( Queue(..)
                            )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Fence( Fence(..)
                            )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.Memory( DeviceMemory(..)
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Sampler( VkSampleCountFlags(..)
                              )
import Graphics.Vulkan.Image( Image(..)
                            , VkImageType(..)
                            , VkImageUsageFlags(..)
                            , VkImageSubresource(..)
                            , VkImageAspectFlags(..)
                            , VkImageTiling(..)
                            )
import Graphics.Vulkan.QueueSemaphore( Semaphore(..)
                                     )
import Graphics.Vulkan.Core( VkStructureType(..)
                           , VkFormat(..)
                           , VkFlags(..)
                           , VkOffset3D(..)
                           , VkResult(..)
                           , VkDeviceSize(..)
                           , VkExtent3D(..)
                           )


data VkSparseImageMemoryRequirements =
  VkSparseImageMemoryRequirements{ formatProperties :: VkSparseImageFormatProperties 
                                 , imageMipTailFirstLod :: Word32 
                                 , imageMipTailSize :: VkDeviceSize 
                                 , imageMipTailOffset :: VkDeviceSize 
                                 , imageMipTailStride :: VkDeviceSize 
                                 }
  deriving (Eq)

instance Storable VkSparseImageMemoryRequirements where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkSparseImageMemoryRequirements <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 20)
                                             <*> peek (ptr `plusPtr` 24)
                                             <*> peek (ptr `plusPtr` 32)
                                             <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (formatProperties (poked :: VkSparseImageMemoryRequirements))
                *> poke (ptr `plusPtr` 20) (imageMipTailFirstLod (poked :: VkSparseImageMemoryRequirements))
                *> poke (ptr `plusPtr` 24) (imageMipTailSize (poked :: VkSparseImageMemoryRequirements))
                *> poke (ptr `plusPtr` 32) (imageMipTailOffset (poked :: VkSparseImageMemoryRequirements))
                *> poke (ptr `plusPtr` 40) (imageMipTailStride (poked :: VkSparseImageMemoryRequirements))



data VkSparseMemoryBind =
  VkSparseMemoryBind{ resourceOffset :: VkDeviceSize 
                    , size :: VkDeviceSize 
                    , memory :: DeviceMemory 
                    , memoryOffset :: VkDeviceSize 
                    , flags :: VkSparseMemoryBindFlags 
                    }
  deriving (Eq)

instance Storable VkSparseMemoryBind where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkSparseMemoryBind <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 24)
                                <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (resourceOffset (poked :: VkSparseMemoryBind))
                *> poke (ptr `plusPtr` 8) (size (poked :: VkSparseMemoryBind))
                *> poke (ptr `plusPtr` 16) (memory (poked :: VkSparseMemoryBind))
                *> poke (ptr `plusPtr` 24) (memoryOffset (poked :: VkSparseMemoryBind))
                *> poke (ptr `plusPtr` 32) (flags (poked :: VkSparseMemoryBind))



data VkSparseImageMemoryBind =
  VkSparseImageMemoryBind{ subresource :: VkImageSubresource 
                         , offset :: VkOffset3D 
                         , extent :: VkExtent3D 
                         , memory :: DeviceMemory 
                         , memoryOffset :: VkDeviceSize 
                         , flags :: VkSparseMemoryBindFlags 
                         }
  deriving (Eq)

instance Storable VkSparseImageMemoryBind where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkSparseImageMemoryBind <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 12)
                                     <*> peek (ptr `plusPtr` 24)
                                     <*> peek (ptr `plusPtr` 40)
                                     <*> peek (ptr `plusPtr` 48)
                                     <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (subresource (poked :: VkSparseImageMemoryBind))
                *> poke (ptr `plusPtr` 12) (offset (poked :: VkSparseImageMemoryBind))
                *> poke (ptr `plusPtr` 24) (extent (poked :: VkSparseImageMemoryBind))
                *> poke (ptr `plusPtr` 40) (memory (poked :: VkSparseImageMemoryBind))
                *> poke (ptr `plusPtr` 48) (memoryOffset (poked :: VkSparseImageMemoryBind))
                *> poke (ptr `plusPtr` 56) (flags (poked :: VkSparseImageMemoryBind))



data VkSparseImageMemoryBindInfo =
  VkSparseImageMemoryBindInfo{ image :: Image 
                             , bindCount :: Word32 
                             , pBinds :: Ptr VkSparseImageMemoryBind 
                             }
  deriving (Eq)

instance Storable VkSparseImageMemoryBindInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSparseImageMemoryBindInfo <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (image (poked :: VkSparseImageMemoryBindInfo))
                *> poke (ptr `plusPtr` 8) (bindCount (poked :: VkSparseImageMemoryBindInfo))
                *> poke (ptr `plusPtr` 16) (pBinds (poked :: VkSparseImageMemoryBindInfo))


-- ** vkGetImageSparseMemoryRequirements
foreign import ccall "vkGetImageSparseMemoryRequirements" vkGetImageSparseMemoryRequirements ::
  Device ->
  Image -> Ptr Word32 -> Ptr VkSparseImageMemoryRequirements -> IO ()

-- ** vkQueueBindSparse
foreign import ccall "vkQueueBindSparse" vkQueueBindSparse ::
  Queue -> Word32 -> Ptr VkBindSparseInfo -> Fence -> IO VkResult


data VkBindSparseInfo =
  VkBindSparseInfo{ sType :: VkStructureType 
                  , pNext :: Ptr Void 
                  , waitSemaphoreCount :: Word32 
                  , pWaitSemaphores :: Ptr Semaphore 
                  , bufferBindCount :: Word32 
                  , pBufferBinds :: Ptr VkSparseBufferMemoryBindInfo 
                  , imageOpaqueBindCount :: Word32 
                  , pImageOpaqueBinds :: Ptr VkSparseImageOpaqueMemoryBindInfo 
                  , imageBindCount :: Word32 
                  , pImageBinds :: Ptr VkSparseImageMemoryBindInfo 
                  , signalSemaphoreCount :: Word32 
                  , pSignalSemaphores :: Ptr Semaphore 
                  }
  deriving (Eq)

instance Storable VkBindSparseInfo where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek ptr = VkBindSparseInfo <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 8)
                              <*> peek (ptr `plusPtr` 16)
                              <*> peek (ptr `plusPtr` 24)
                              <*> peek (ptr `plusPtr` 32)
                              <*> peek (ptr `plusPtr` 40)
                              <*> peek (ptr `plusPtr` 48)
                              <*> peek (ptr `plusPtr` 56)
                              <*> peek (ptr `plusPtr` 64)
                              <*> peek (ptr `plusPtr` 72)
                              <*> peek (ptr `plusPtr` 80)
                              <*> peek (ptr `plusPtr` 88)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 16) (waitSemaphoreCount (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 24) (pWaitSemaphores (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 32) (bufferBindCount (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 40) (pBufferBinds (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 48) (imageOpaqueBindCount (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 56) (pImageOpaqueBinds (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 64) (imageBindCount (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 72) (pImageBinds (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 80) (signalSemaphoreCount (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 88) (pSignalSemaphores (poked :: VkBindSparseInfo))



data VkSparseBufferMemoryBindInfo =
  VkSparseBufferMemoryBindInfo{ buffer :: Buffer 
                              , bindCount :: Word32 
                              , pBinds :: Ptr VkSparseMemoryBind 
                              }
  deriving (Eq)

instance Storable VkSparseBufferMemoryBindInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSparseBufferMemoryBindInfo <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (buffer (poked :: VkSparseBufferMemoryBindInfo))
                *> poke (ptr `plusPtr` 8) (bindCount (poked :: VkSparseBufferMemoryBindInfo))
                *> poke (ptr `plusPtr` 16) (pBinds (poked :: VkSparseBufferMemoryBindInfo))


-- ** VkSparseImageFormatFlags

newtype VkSparseImageFormatFlags = VkSparseImageFormatFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show VkSparseImageFormatFlags where
  showsPrec _ VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT = showString "VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT"
  showsPrec _ VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT = showString "VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT"
  showsPrec _ VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT = showString "VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT"
  
  showsPrec p (VkSparseImageFormatFlags x) = showParen (p >= 11) (showString "VkSparseImageFormatFlags " . showsPrec 11 x)

instance Read VkSparseImageFormatFlags where
  readPrec = parens ( choose [ ("VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT", pure VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT)
                             , ("VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT", pure VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT)
                             , ("VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT", pure VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSparseImageFormatFlags")
                        v <- step readPrec
                        pure (VkSparseImageFormatFlags v)
                        )
                    )

-- | Image uses a single miptail region for all array layers
pattern VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT = VkSparseImageFormatFlags 0x1
-- | Image requires mip levels to be an exact multiple of the sparse image block size for non-miptail levels.
pattern VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT = VkSparseImageFormatFlags 0x2
-- | Image uses a non-standard sparse block size
pattern VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT = VkSparseImageFormatFlags 0x4


-- ** vkGetPhysicalDeviceSparseImageFormatProperties
foreign import ccall "vkGetPhysicalDeviceSparseImageFormatProperties" vkGetPhysicalDeviceSparseImageFormatProperties ::
  PhysicalDevice ->
  VkFormat ->
    VkImageType ->
      VkSampleCountFlags ->
        VkImageUsageFlags ->
          VkImageTiling ->
            Ptr Word32 -> Ptr VkSparseImageFormatProperties -> IO ()

-- ** VkSparseMemoryBindFlags

newtype VkSparseMemoryBindFlags = VkSparseMemoryBindFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show VkSparseMemoryBindFlags where
  showsPrec _ VK_SPARSE_MEMORY_BIND_METADATA_BIT = showString "VK_SPARSE_MEMORY_BIND_METADATA_BIT"
  
  showsPrec p (VkSparseMemoryBindFlags x) = showParen (p >= 11) (showString "VkSparseMemoryBindFlags " . showsPrec 11 x)

instance Read VkSparseMemoryBindFlags where
  readPrec = parens ( choose [ ("VK_SPARSE_MEMORY_BIND_METADATA_BIT", pure VK_SPARSE_MEMORY_BIND_METADATA_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSparseMemoryBindFlags")
                        v <- step readPrec
                        pure (VkSparseMemoryBindFlags v)
                        )
                    )

-- | Operation binds resource metadata to memory
pattern VK_SPARSE_MEMORY_BIND_METADATA_BIT = VkSparseMemoryBindFlags 0x1



data VkSparseImageOpaqueMemoryBindInfo =
  VkSparseImageOpaqueMemoryBindInfo{ image :: Image 
                                   , bindCount :: Word32 
                                   , pBinds :: Ptr VkSparseMemoryBind 
                                   }
  deriving (Eq)

instance Storable VkSparseImageOpaqueMemoryBindInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSparseImageOpaqueMemoryBindInfo <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (image (poked :: VkSparseImageOpaqueMemoryBindInfo))
                *> poke (ptr `plusPtr` 8) (bindCount (poked :: VkSparseImageOpaqueMemoryBindInfo))
                *> poke (ptr `plusPtr` 16) (pBinds (poked :: VkSparseImageOpaqueMemoryBindInfo))



data VkSparseImageFormatProperties =
  VkSparseImageFormatProperties{ aspectMask :: VkImageAspectFlags 
                               , imageGranularity :: VkExtent3D 
                               , flags :: VkSparseImageFormatFlags 
                               }
  deriving (Eq)

instance Storable VkSparseImageFormatProperties where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = VkSparseImageFormatProperties <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 4)
                                           <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (aspectMask (poked :: VkSparseImageFormatProperties))
                *> poke (ptr `plusPtr` 4) (imageGranularity (poked :: VkSparseImageFormatProperties))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkSparseImageFormatProperties))


