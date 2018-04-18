{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( VkImageAspectFlagBits(..)
  , pattern VK_IMAGE_ASPECT_COLOR_BIT
  , pattern VK_IMAGE_ASPECT_DEPTH_BIT
  , pattern VK_IMAGE_ASPECT_STENCIL_BIT
  , pattern VK_IMAGE_ASPECT_METADATA_BIT
  , VkSparseMemoryBindFlagBits(..)
  , pattern VK_SPARSE_MEMORY_BIND_METADATA_BIT
  , VkSparseImageFormatFlagBits(..)
  , pattern VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT
  , pattern VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT
  , pattern VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT
  , vkGetImageSparseMemoryRequirements
  , vkGetPhysicalDeviceSparseImageFormatProperties
  , vkQueueBindSparse
  , VkOffset3D(..)
  , VkSparseImageFormatProperties(..)
  , VkSparseImageMemoryRequirements(..)
  , VkImageSubresource(..)
  , VkSparseMemoryBind(..)
  , VkSparseImageMemoryBind(..)
  , VkSparseBufferMemoryBindInfo(..)
  , VkSparseImageOpaqueMemoryBindInfo(..)
  , VkSparseImageMemoryBindInfo(..)
  , VkBindSparseInfo(..)
  , VkImageAspectFlags
  , VkSparseMemoryBindFlags
  , VkSparseImageFormatFlags
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  , VkResult(..)
  , VkFormat(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDeviceSize
  , VkExtent3D(..)
  , VkImageUsageFlagBits(..)
  , VkImageTiling(..)
  , VkImageUsageFlags
  , VkSampleCountFlagBits(..)
  , VkImageType(..)
  , VkPhysicalDevice
  , VkDevice
  )
import Graphics.Vulkan.Core10.Memory
  ( VkDeviceMemory
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  )
import Graphics.Vulkan.Core10.Queue
  ( VkSemaphore
  , VkFence
  , VkQueue
  )


-- ** VkImageAspectFlagBits

-- | 
newtype VkImageAspectFlagBits = VkImageAspectFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkImageAspectFlagBits where
  showsPrec _ VK_IMAGE_ASPECT_COLOR_BIT = showString "VK_IMAGE_ASPECT_COLOR_BIT"
  showsPrec _ VK_IMAGE_ASPECT_DEPTH_BIT = showString "VK_IMAGE_ASPECT_DEPTH_BIT"
  showsPrec _ VK_IMAGE_ASPECT_STENCIL_BIT = showString "VK_IMAGE_ASPECT_STENCIL_BIT"
  showsPrec _ VK_IMAGE_ASPECT_METADATA_BIT = showString "VK_IMAGE_ASPECT_METADATA_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkImageAspectFlagBits 0x00000010) = showString "VK_IMAGE_ASPECT_PLANE_0_BIT"
  showsPrec _ (VkImageAspectFlagBits 0x00000020) = showString "VK_IMAGE_ASPECT_PLANE_1_BIT"
  showsPrec _ (VkImageAspectFlagBits 0x00000040) = showString "VK_IMAGE_ASPECT_PLANE_2_BIT"
  showsPrec p (VkImageAspectFlagBits x) = showParen (p >= 11) (showString "VkImageAspectFlagBits " . showsPrec 11 x)

instance Read VkImageAspectFlagBits where
  readPrec = parens ( choose [ ("VK_IMAGE_ASPECT_COLOR_BIT",    pure VK_IMAGE_ASPECT_COLOR_BIT)
                             , ("VK_IMAGE_ASPECT_DEPTH_BIT",    pure VK_IMAGE_ASPECT_DEPTH_BIT)
                             , ("VK_IMAGE_ASPECT_STENCIL_BIT",  pure VK_IMAGE_ASPECT_STENCIL_BIT)
                             , ("VK_IMAGE_ASPECT_METADATA_BIT", pure VK_IMAGE_ASPECT_METADATA_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_IMAGE_ASPECT_PLANE_0_BIT", pure (VkImageAspectFlagBits 0x00000010))
                             , ("VK_IMAGE_ASPECT_PLANE_1_BIT", pure (VkImageAspectFlagBits 0x00000020))
                             , ("VK_IMAGE_ASPECT_PLANE_2_BIT", pure (VkImageAspectFlagBits 0x00000040))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageAspectFlagBits")
                        v <- step readPrec
                        pure (VkImageAspectFlagBits v)
                        )
                    )

-- | 
pattern VK_IMAGE_ASPECT_COLOR_BIT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_COLOR_BIT = VkImageAspectFlagBits 0x00000001

-- | 
pattern VK_IMAGE_ASPECT_DEPTH_BIT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_DEPTH_BIT = VkImageAspectFlagBits 0x00000002

-- | 
pattern VK_IMAGE_ASPECT_STENCIL_BIT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_STENCIL_BIT = VkImageAspectFlagBits 0x00000004

-- | 
pattern VK_IMAGE_ASPECT_METADATA_BIT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_METADATA_BIT = VkImageAspectFlagBits 0x00000008
-- ** VkSparseMemoryBindFlagBits

-- | 
newtype VkSparseMemoryBindFlagBits = VkSparseMemoryBindFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

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
pattern VK_SPARSE_MEMORY_BIND_METADATA_BIT :: VkSparseMemoryBindFlagBits
pattern VK_SPARSE_MEMORY_BIND_METADATA_BIT = VkSparseMemoryBindFlagBits 0x00000001
-- ** VkSparseImageFormatFlagBits

-- | 
newtype VkSparseImageFormatFlagBits = VkSparseImageFormatFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkSparseImageFormatFlagBits where
  showsPrec _ VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT = showString "VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT"
  showsPrec _ VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT = showString "VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT"
  showsPrec _ VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT = showString "VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT"
  showsPrec p (VkSparseImageFormatFlagBits x) = showParen (p >= 11) (showString "VkSparseImageFormatFlagBits " . showsPrec 11 x)

instance Read VkSparseImageFormatFlagBits where
  readPrec = parens ( choose [ ("VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT",         pure VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT)
                             , ("VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT",       pure VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT)
                             , ("VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT", pure VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSparseImageFormatFlagBits")
                        v <- step readPrec
                        pure (VkSparseImageFormatFlagBits v)
                        )
                    )

-- | Image uses a single mip tail region for all array layers
pattern VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT :: VkSparseImageFormatFlagBits
pattern VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT = VkSparseImageFormatFlagBits 0x00000001

-- | Image requires mip level dimensions to be an integer multiple of the sparse image block dimensions for non-tail mip levels.
pattern VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT :: VkSparseImageFormatFlagBits
pattern VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT = VkSparseImageFormatFlagBits 0x00000002

-- | Image uses a non-standard sparse image block dimensions
pattern VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT :: VkSparseImageFormatFlagBits
pattern VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT = VkSparseImageFormatFlagBits 0x00000004
-- | 
foreign import ccall "vkGetImageSparseMemoryRequirements" vkGetImageSparseMemoryRequirements :: ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements) -> IO ()
-- | 
foreign import ccall "vkGetPhysicalDeviceSparseImageFormatProperties" vkGetPhysicalDeviceSparseImageFormatProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("samples" ::: VkSampleCountFlagBits) -> ("usage" ::: VkImageUsageFlags) -> ("tiling" ::: VkImageTiling) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties) -> IO ()
-- | 
foreign import ccall "vkQueueBindSparse" vkQueueBindSparse :: ("queue" ::: VkQueue) -> ("bindInfoCount" ::: Word32) -> ("pBindInfo" ::: Ptr VkBindSparseInfo) -> ("fence" ::: VkFence) -> IO VkResult
-- | TODO: Struct comments
data VkOffset3D = VkOffset3D
  { vkX :: Int32
  , vkY :: Int32
  , vkZ :: Int32
  }
  deriving (Eq, Show)

instance Storable VkOffset3D where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkOffset3D <$> peek (ptr `plusPtr` 0)
                        <*> peek (ptr `plusPtr` 4)
                        <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkX (poked :: VkOffset3D))
                *> poke (ptr `plusPtr` 4) (vkY (poked :: VkOffset3D))
                *> poke (ptr `plusPtr` 8) (vkZ (poked :: VkOffset3D))
-- | TODO: Struct comments
data VkSparseImageFormatProperties = VkSparseImageFormatProperties
  { vkAspectMask :: VkImageAspectFlags
  , vkImageGranularity :: VkExtent3D
  , vkFlags :: VkSparseImageFormatFlags
  }
  deriving (Eq, Show)

instance Storable VkSparseImageFormatProperties where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = VkSparseImageFormatProperties <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 4)
                                           <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAspectMask (poked :: VkSparseImageFormatProperties))
                *> poke (ptr `plusPtr` 4) (vkImageGranularity (poked :: VkSparseImageFormatProperties))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkSparseImageFormatProperties))
-- | TODO: Struct comments
data VkSparseImageMemoryRequirements = VkSparseImageMemoryRequirements
  { vkFormatProperties :: VkSparseImageFormatProperties
  , vkImageMipTailFirstLod :: Word32
  , vkImageMipTailSize :: VkDeviceSize
  , vkImageMipTailOffset :: VkDeviceSize
  , vkImageMipTailStride :: VkDeviceSize
  }
  deriving (Eq, Show)

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
-- | TODO: Struct comments
data VkImageSubresource = VkImageSubresource
  { vkAspectMask :: VkImageAspectFlags
  , vkMipLevel :: Word32
  , vkArrayLayer :: Word32
  }
  deriving (Eq, Show)

instance Storable VkImageSubresource where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkImageSubresource <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 4)
                                <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAspectMask (poked :: VkImageSubresource))
                *> poke (ptr `plusPtr` 4) (vkMipLevel (poked :: VkImageSubresource))
                *> poke (ptr `plusPtr` 8) (vkArrayLayer (poked :: VkImageSubresource))
-- | TODO: Struct comments
data VkSparseMemoryBind = VkSparseMemoryBind
  { vkResourceOffset :: VkDeviceSize
  , vkSize :: VkDeviceSize
  , vkMemory :: VkDeviceMemory
  , vkMemoryOffset :: VkDeviceSize
  , vkFlags :: VkSparseMemoryBindFlags
  }
  deriving (Eq, Show)

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
-- | TODO: Struct comments
data VkSparseImageMemoryBind = VkSparseImageMemoryBind
  { vkSubresource :: VkImageSubresource
  , vkOffset :: VkOffset3D
  , vkExtent :: VkExtent3D
  , vkMemory :: VkDeviceMemory
  , vkMemoryOffset :: VkDeviceSize
  , vkFlags :: VkSparseMemoryBindFlags
  }
  deriving (Eq, Show)

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
-- | TODO: Struct comments
data VkSparseBufferMemoryBindInfo = VkSparseBufferMemoryBindInfo
  { vkBuffer :: VkBuffer
  , vkBindCount :: Word32
  , vkPBinds :: Ptr VkSparseMemoryBind
  }
  deriving (Eq, Show)

instance Storable VkSparseBufferMemoryBindInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSparseBufferMemoryBindInfo <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBuffer (poked :: VkSparseBufferMemoryBindInfo))
                *> poke (ptr `plusPtr` 8) (vkBindCount (poked :: VkSparseBufferMemoryBindInfo))
                *> poke (ptr `plusPtr` 16) (vkPBinds (poked :: VkSparseBufferMemoryBindInfo))
-- | TODO: Struct comments
data VkSparseImageOpaqueMemoryBindInfo = VkSparseImageOpaqueMemoryBindInfo
  { vkImage :: VkImage
  , vkBindCount :: Word32
  , vkPBinds :: Ptr VkSparseMemoryBind
  }
  deriving (Eq, Show)

instance Storable VkSparseImageOpaqueMemoryBindInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSparseImageOpaqueMemoryBindInfo <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkImage (poked :: VkSparseImageOpaqueMemoryBindInfo))
                *> poke (ptr `plusPtr` 8) (vkBindCount (poked :: VkSparseImageOpaqueMemoryBindInfo))
                *> poke (ptr `plusPtr` 16) (vkPBinds (poked :: VkSparseImageOpaqueMemoryBindInfo))
-- | TODO: Struct comments
data VkSparseImageMemoryBindInfo = VkSparseImageMemoryBindInfo
  { vkImage :: VkImage
  , vkBindCount :: Word32
  , vkPBinds :: Ptr VkSparseImageMemoryBind
  }
  deriving (Eq, Show)

instance Storable VkSparseImageMemoryBindInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSparseImageMemoryBindInfo <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkImage (poked :: VkSparseImageMemoryBindInfo))
                *> poke (ptr `plusPtr` 8) (vkBindCount (poked :: VkSparseImageMemoryBindInfo))
                *> poke (ptr `plusPtr` 16) (vkPBinds (poked :: VkSparseImageMemoryBindInfo))
-- | TODO: Struct comments
data VkBindSparseInfo = VkBindSparseInfo
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
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
  deriving (Eq, Show)

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
type VkImageAspectFlags = VkImageAspectFlagBits
type VkSparseMemoryBindFlags = VkSparseMemoryBindFlagBits
type VkSparseImageFormatFlags = VkSparseImageFormatFlagBits
