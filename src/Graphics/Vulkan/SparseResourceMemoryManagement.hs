{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.SparseResourceMemoryManagement where

import Graphics.Vulkan.Device( VkPhysicalDevice(..)
                             , VkDevice(..)
                             )
import Graphics.Vulkan.Buffer( VkBuffer(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Graphics.Vulkan.Queue( VkQueue(..)
                            )
import Data.Int( Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Fence( VkFence(..)
                            )
import Data.Void( Void
                )
import Graphics.Vulkan.Memory( VkDeviceMemory(..)
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Sampler( VkSampleCountFlagBits(..)
                              )
import Graphics.Vulkan.Image( VkImageUsageFlags(..)
                            , VkImage(..)
                            , VkImageSubresource(..)
                            , VkImageType(..)
                            , VkImageAspectFlagBits(..)
                            , VkImageUsageFlagBits(..)
                            , VkImageTiling(..)
                            , VkImageAspectFlags(..)
                            )
import Graphics.Vulkan.QueueSemaphore( VkSemaphore(..)
                                     )
import Graphics.Vulkan.Core( VkExtent3D(..)
                           , VkResult(..)
                           , VkDeviceSize(..)
                           , VkFlags(..)
                           , VkFormat(..)
                           , VkOffset3D(..)
                           , VkStructureType(..)
                           )


data VkSparseImageMemoryRequirements =
  VkSparseImageMemoryRequirements{ vkFormatProperties :: VkSparseImageFormatProperties 
                                 , vkImageMipTailFirstLod :: Word32 
                                 , vkImageMipTailSize :: VkDeviceSize 
                                 , vkImageMipTailOffset :: VkDeviceSize 
                                 , vkImageMipTailStride :: VkDeviceSize 
                                 }
  deriving (Eq, Ord, Show)

instance Storable VkSparseImageMemoryRequirements where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkSparseImageMemoryRequirements <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 20)
                                             <*> peek (ptr `plusPtr` 24)
                                             <*> peek (ptr `plusPtr` 32)
                                             <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkFormatProperties (poked :: VkSparseImageMemoryRequirements))
                *> poke (ptr `plusPtr` 20) (vkImageMipTailFirstLod (poked :: VkSparseImageMemoryRequirements))
                *> poke (ptr `plusPtr` 24) (vkImageMipTailSize (poked :: VkSparseImageMemoryRequirements))
                *> poke (ptr `plusPtr` 32) (vkImageMipTailOffset (poked :: VkSparseImageMemoryRequirements))
                *> poke (ptr `plusPtr` 40) (vkImageMipTailStride (poked :: VkSparseImageMemoryRequirements))



data VkSparseMemoryBind =
  VkSparseMemoryBind{ vkResourceOffset :: VkDeviceSize 
                    , vkSize :: VkDeviceSize 
                    , vkMemory :: VkDeviceMemory 
                    , vkMemoryOffset :: VkDeviceSize 
                    , vkFlags :: VkSparseMemoryBindFlags 
                    }
  deriving (Eq, Ord, Show)

instance Storable VkSparseMemoryBind where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkSparseMemoryBind <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 24)
                                <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkResourceOffset (poked :: VkSparseMemoryBind))
                *> poke (ptr `plusPtr` 8) (vkSize (poked :: VkSparseMemoryBind))
                *> poke (ptr `plusPtr` 16) (vkMemory (poked :: VkSparseMemoryBind))
                *> poke (ptr `plusPtr` 24) (vkMemoryOffset (poked :: VkSparseMemoryBind))
                *> poke (ptr `plusPtr` 32) (vkFlags (poked :: VkSparseMemoryBind))



data VkSparseImageMemoryBind =
  VkSparseImageMemoryBind{ vkSubresource :: VkImageSubresource 
                         , vkOffset :: VkOffset3D 
                         , vkExtent :: VkExtent3D 
                         , vkMemory :: VkDeviceMemory 
                         , vkMemoryOffset :: VkDeviceSize 
                         , vkFlags :: VkSparseMemoryBindFlags 
                         }
  deriving (Eq, Ord, Show)

instance Storable VkSparseImageMemoryBind where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkSparseImageMemoryBind <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 12)
                                     <*> peek (ptr `plusPtr` 24)
                                     <*> peek (ptr `plusPtr` 40)
                                     <*> peek (ptr `plusPtr` 48)
                                     <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSubresource (poked :: VkSparseImageMemoryBind))
                *> poke (ptr `plusPtr` 12) (vkOffset (poked :: VkSparseImageMemoryBind))
                *> poke (ptr `plusPtr` 24) (vkExtent (poked :: VkSparseImageMemoryBind))
                *> poke (ptr `plusPtr` 40) (vkMemory (poked :: VkSparseImageMemoryBind))
                *> poke (ptr `plusPtr` 48) (vkMemoryOffset (poked :: VkSparseImageMemoryBind))
                *> poke (ptr `plusPtr` 56) (vkFlags (poked :: VkSparseImageMemoryBind))



data VkSparseImageMemoryBindInfo =
  VkSparseImageMemoryBindInfo{ vkImage :: VkImage 
                             , vkBindCount :: Word32 
                             , vkPBinds :: Ptr VkSparseImageMemoryBind 
                             }
  deriving (Eq, Ord, Show)

instance Storable VkSparseImageMemoryBindInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSparseImageMemoryBindInfo <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkImage (poked :: VkSparseImageMemoryBindInfo))
                *> poke (ptr `plusPtr` 8) (vkBindCount (poked :: VkSparseImageMemoryBindInfo))
                *> poke (ptr `plusPtr` 16) (vkPBinds (poked :: VkSparseImageMemoryBindInfo))


-- ** vkGetImageSparseMemoryRequirements
foreign import ccall "vkGetImageSparseMemoryRequirements" vkGetImageSparseMemoryRequirements ::
  VkDevice ->
  VkImage ->
    Ptr Word32 -> Ptr VkSparseImageMemoryRequirements -> IO ()

-- ** vkQueueBindSparse
foreign import ccall "vkQueueBindSparse" vkQueueBindSparse ::
  VkQueue -> Word32 -> Ptr VkBindSparseInfo -> VkFence -> IO VkResult


data VkBindSparseInfo =
  VkBindSparseInfo{ vkSType :: VkStructureType 
                  , vkPNext :: Ptr Void 
                  , vkWaitSemaphoreCount :: Word32 
                  , vkPWaitSemaphores :: Ptr VkSemaphore 
                  , vkBufferBindCount :: Word32 
                  , vkPBufferBinds :: Ptr VkSparseBufferMemoryBindInfo 
                  , vkImageOpaqueBindCount :: Word32 
                  , vkPImageOpaqueBinds :: Ptr VkSparseImageOpaqueMemoryBindInfo 
                  , vkImageBindCount :: Word32 
                  , vkPImageBinds :: Ptr VkSparseImageMemoryBindInfo 
                  , vkSignalSemaphoreCount :: Word32 
                  , vkPSignalSemaphores :: Ptr VkSemaphore 
                  }
  deriving (Eq, Ord, Show)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 16) (vkWaitSemaphoreCount (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 24) (vkPWaitSemaphores (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 32) (vkBufferBindCount (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 40) (vkPBufferBinds (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 48) (vkImageOpaqueBindCount (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 56) (vkPImageOpaqueBinds (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 64) (vkImageBindCount (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 72) (vkPImageBinds (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 80) (vkSignalSemaphoreCount (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 88) (vkPSignalSemaphores (poked :: VkBindSparseInfo))



data VkSparseBufferMemoryBindInfo =
  VkSparseBufferMemoryBindInfo{ vkBuffer :: VkBuffer 
                              , vkBindCount :: Word32 
                              , vkPBinds :: Ptr VkSparseMemoryBind 
                              }
  deriving (Eq, Ord, Show)

instance Storable VkSparseBufferMemoryBindInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSparseBufferMemoryBindInfo <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBuffer (poked :: VkSparseBufferMemoryBindInfo))
                *> poke (ptr `plusPtr` 8) (vkBindCount (poked :: VkSparseBufferMemoryBindInfo))
                *> poke (ptr `plusPtr` 16) (vkPBinds (poked :: VkSparseBufferMemoryBindInfo))


-- ** VkSparseImageFormatFlags

newtype VkSparseImageFormatFlagBits = VkSparseImageFormatFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkSparseImageFormatFlagBits
type VkSparseImageFormatFlags = VkSparseImageFormatFlagBits

instance Show VkSparseImageFormatFlagBits where
  showsPrec _ VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT = showString "VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT"
  showsPrec _ VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT = showString "VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT"
  showsPrec _ VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT = showString "VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT"
  
  showsPrec p (VkSparseImageFormatFlagBits x) = showParen (p >= 11) (showString "VkSparseImageFormatFlagBits " . showsPrec 11 x)

instance Read VkSparseImageFormatFlagBits where
  readPrec = parens ( choose [ ("VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT", pure VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT)
                             , ("VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT", pure VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT)
                             , ("VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT", pure VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSparseImageFormatFlagBits")
                        v <- step readPrec
                        pure (VkSparseImageFormatFlagBits v)
                        )
                    )

-- | Image uses a single miptail region for all array layers
pattern VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT = VkSparseImageFormatFlagBits 0x1
-- | Image requires mip level dimensions to be an integer multiple of the sparse image block dimensions for non-miptail levels.
pattern VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT = VkSparseImageFormatFlagBits 0x2
-- | Image uses a non-standard sparse image block dimensions
pattern VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT = VkSparseImageFormatFlagBits 0x4


-- ** vkGetPhysicalDeviceSparseImageFormatProperties
foreign import ccall "vkGetPhysicalDeviceSparseImageFormatProperties" vkGetPhysicalDeviceSparseImageFormatProperties ::
  VkPhysicalDevice ->
  VkFormat ->
    VkImageType ->
      VkSampleCountFlagBits ->
        VkImageUsageFlags ->
          VkImageTiling ->
            Ptr Word32 -> Ptr VkSparseImageFormatProperties -> IO ()

-- ** VkSparseMemoryBindFlags

newtype VkSparseMemoryBindFlagBits = VkSparseMemoryBindFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkSparseMemoryBindFlagBits
type VkSparseMemoryBindFlags = VkSparseMemoryBindFlagBits

instance Show VkSparseMemoryBindFlagBits where
  showsPrec _ VK_SPARSE_MEMORY_BIND_METADATA_BIT = showString "VK_SPARSE_MEMORY_BIND_METADATA_BIT"
  
  showsPrec p (VkSparseMemoryBindFlagBits x) = showParen (p >= 11) (showString "VkSparseMemoryBindFlagBits " . showsPrec 11 x)

instance Read VkSparseMemoryBindFlagBits where
  readPrec = parens ( choose [ ("VK_SPARSE_MEMORY_BIND_METADATA_BIT", pure VK_SPARSE_MEMORY_BIND_METADATA_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSparseMemoryBindFlagBits")
                        v <- step readPrec
                        pure (VkSparseMemoryBindFlagBits v)
                        )
                    )

-- | Operation binds resource metadata to memory
pattern VK_SPARSE_MEMORY_BIND_METADATA_BIT = VkSparseMemoryBindFlagBits 0x1



data VkSparseImageOpaqueMemoryBindInfo =
  VkSparseImageOpaqueMemoryBindInfo{ vkImage :: VkImage 
                                   , vkBindCount :: Word32 
                                   , vkPBinds :: Ptr VkSparseMemoryBind 
                                   }
  deriving (Eq, Ord, Show)

instance Storable VkSparseImageOpaqueMemoryBindInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSparseImageOpaqueMemoryBindInfo <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkImage (poked :: VkSparseImageOpaqueMemoryBindInfo))
                *> poke (ptr `plusPtr` 8) (vkBindCount (poked :: VkSparseImageOpaqueMemoryBindInfo))
                *> poke (ptr `plusPtr` 16) (vkPBinds (poked :: VkSparseImageOpaqueMemoryBindInfo))



data VkSparseImageFormatProperties =
  VkSparseImageFormatProperties{ vkAspectMask :: VkImageAspectFlags 
                               , vkImageGranularity :: VkExtent3D 
                               , vkFlags :: VkSparseImageFormatFlags 
                               }
  deriving (Eq, Ord, Show)

instance Storable VkSparseImageFormatProperties where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = VkSparseImageFormatProperties <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 4)
                                           <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAspectMask (poked :: VkSparseImageFormatProperties))
                *> poke (ptr `plusPtr` 4) (vkImageGranularity (poked :: VkSparseImageFormatProperties))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkSparseImageFormatProperties))


