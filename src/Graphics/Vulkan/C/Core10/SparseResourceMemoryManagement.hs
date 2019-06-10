{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkBindSparseInfo(..)
  , VkImageAspectFlagBits(..)
  , pattern VK_IMAGE_ASPECT_COLOR_BIT
  , pattern VK_IMAGE_ASPECT_DEPTH_BIT
  , pattern VK_IMAGE_ASPECT_STENCIL_BIT
  , pattern VK_IMAGE_ASPECT_METADATA_BIT
  , VkImageAspectFlags
  , VkImageSubresource(..)
  , VkOffset3D(..)
  , VkSparseBufferMemoryBindInfo(..)
  , VkSparseImageFormatFlagBits(..)
  , pattern VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT
  , pattern VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT
  , pattern VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT
  , VkSparseImageFormatFlags
  , VkSparseImageFormatProperties(..)
  , VkSparseImageMemoryBind(..)
  , VkSparseImageMemoryBindInfo(..)
  , VkSparseImageMemoryRequirements(..)
  , VkSparseImageOpaqueMemoryBindInfo(..)
  , VkSparseMemoryBind(..)
  , VkSparseMemoryBindFlagBits(..)
  , pattern VK_SPARSE_MEMORY_BIND_METADATA_BIT
  , VkSparseMemoryBindFlags
  , FN_vkGetImageSparseMemoryRequirements
  , PFN_vkGetImageSparseMemoryRequirements
  , vkGetImageSparseMemoryRequirements
  , FN_vkGetPhysicalDeviceSparseImageFormatProperties
  , PFN_vkGetPhysicalDeviceSparseImageFormatProperties
  , vkGetPhysicalDeviceSparseImageFormatProperties
  , FN_vkQueueBindSparse
  , PFN_vkQueueBindSparse
  , vkQueueBindSparse
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
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
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


import Graphics.Vulkan.C.Core10.Core
  ( VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  , pattern VK_STRUCTURE_TYPE_BIND_SPARSE_INFO
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkExtent3D(..)
  , VkImageTiling(..)
  , VkImageType(..)
  , VkImageUsageFlagBits(..)
  , VkSampleCountFlagBits(..)
  , VkDevice
  , VkDeviceSize
  , VkImageUsageFlags
  , VkPhysicalDevice
  )
import Graphics.Vulkan.C.Core10.Memory
  ( VkDeviceMemory
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkFence
  , VkQueue
  , VkSemaphore
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  , InstanceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkBindSparseInfo"
data VkBindSparseInfo = VkBindSparseInfo
  { -- No documentation found for Nested "VkBindSparseInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBindSparseInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkBindSparseInfo" "waitSemaphoreCount"
  vkWaitSemaphoreCount :: Word32
  , -- No documentation found for Nested "VkBindSparseInfo" "pWaitSemaphores"
  vkPWaitSemaphores :: Ptr VkSemaphore
  , -- No documentation found for Nested "VkBindSparseInfo" "bufferBindCount"
  vkBufferBindCount :: Word32
  , -- No documentation found for Nested "VkBindSparseInfo" "pBufferBinds"
  vkPBufferBinds :: Ptr VkSparseBufferMemoryBindInfo
  , -- No documentation found for Nested "VkBindSparseInfo" "imageOpaqueBindCount"
  vkImageOpaqueBindCount :: Word32
  , -- No documentation found for Nested "VkBindSparseInfo" "pImageOpaqueBinds"
  vkPImageOpaqueBinds :: Ptr VkSparseImageOpaqueMemoryBindInfo
  , -- No documentation found for Nested "VkBindSparseInfo" "imageBindCount"
  vkImageBindCount :: Word32
  , -- No documentation found for Nested "VkBindSparseInfo" "pImageBinds"
  vkPImageBinds :: Ptr VkSparseImageMemoryBindInfo
  , -- No documentation found for Nested "VkBindSparseInfo" "signalSemaphoreCount"
  vkSignalSemaphoreCount :: Word32
  , -- No documentation found for Nested "VkBindSparseInfo" "pSignalSemaphores"
  vkPSignalSemaphores :: Ptr VkSemaphore
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

instance Zero VkBindSparseInfo where
  zero = VkBindSparseInfo VK_STRUCTURE_TYPE_BIND_SPARSE_INFO
                          zero
                          zero
                          zero
                          zero
                          zero
                          zero
                          zero
                          zero
                          zero
                          zero
                          zero

-- ** VkImageAspectFlagBits

-- No documentation found for TopLevel "VkImageAspectFlagBits"
newtype VkImageAspectFlagBits = VkImageAspectFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkImageAspectFlagBits where
  showsPrec _ VK_IMAGE_ASPECT_COLOR_BIT = showString "VK_IMAGE_ASPECT_COLOR_BIT"
  showsPrec _ VK_IMAGE_ASPECT_DEPTH_BIT = showString "VK_IMAGE_ASPECT_DEPTH_BIT"
  showsPrec _ VK_IMAGE_ASPECT_STENCIL_BIT = showString "VK_IMAGE_ASPECT_STENCIL_BIT"
  showsPrec _ VK_IMAGE_ASPECT_METADATA_BIT = showString "VK_IMAGE_ASPECT_METADATA_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkImageAspectFlagBits 0x00000010) = showString "VK_IMAGE_ASPECT_PLANE_0_BIT"
  showsPrec _ (VkImageAspectFlagBits 0x00000020) = showString "VK_IMAGE_ASPECT_PLANE_1_BIT"
  showsPrec _ (VkImageAspectFlagBits 0x00000040) = showString "VK_IMAGE_ASPECT_PLANE_2_BIT"
  showsPrec _ (VkImageAspectFlagBits 0x00000080) = showString "VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT"
  showsPrec _ (VkImageAspectFlagBits 0x00000100) = showString "VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT"
  showsPrec _ (VkImageAspectFlagBits 0x00000200) = showString "VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT"
  showsPrec _ (VkImageAspectFlagBits 0x00000400) = showString "VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT"
  showsPrec p (VkImageAspectFlagBits x) = showParen (p >= 11) (showString "VkImageAspectFlagBits " . showsPrec 11 x)

instance Read VkImageAspectFlagBits where
  readPrec = parens ( choose [ ("VK_IMAGE_ASPECT_COLOR_BIT",    pure VK_IMAGE_ASPECT_COLOR_BIT)
                             , ("VK_IMAGE_ASPECT_DEPTH_BIT",    pure VK_IMAGE_ASPECT_DEPTH_BIT)
                             , ("VK_IMAGE_ASPECT_STENCIL_BIT",  pure VK_IMAGE_ASPECT_STENCIL_BIT)
                             , ("VK_IMAGE_ASPECT_METADATA_BIT", pure VK_IMAGE_ASPECT_METADATA_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_IMAGE_ASPECT_PLANE_0_BIT",            pure (VkImageAspectFlagBits 0x00000010))
                             , ("VK_IMAGE_ASPECT_PLANE_1_BIT",            pure (VkImageAspectFlagBits 0x00000020))
                             , ("VK_IMAGE_ASPECT_PLANE_2_BIT",            pure (VkImageAspectFlagBits 0x00000040))
                             , ("VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT", pure (VkImageAspectFlagBits 0x00000080))
                             , ("VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT", pure (VkImageAspectFlagBits 0x00000100))
                             , ("VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT", pure (VkImageAspectFlagBits 0x00000200))
                             , ("VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT", pure (VkImageAspectFlagBits 0x00000400))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageAspectFlagBits")
                        v <- step readPrec
                        pure (VkImageAspectFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_COLOR_BIT"
pattern VK_IMAGE_ASPECT_COLOR_BIT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_COLOR_BIT = VkImageAspectFlagBits 0x00000001

-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_DEPTH_BIT"
pattern VK_IMAGE_ASPECT_DEPTH_BIT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_DEPTH_BIT = VkImageAspectFlagBits 0x00000002

-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_STENCIL_BIT"
pattern VK_IMAGE_ASPECT_STENCIL_BIT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_STENCIL_BIT = VkImageAspectFlagBits 0x00000004

-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_METADATA_BIT"
pattern VK_IMAGE_ASPECT_METADATA_BIT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_METADATA_BIT = VkImageAspectFlagBits 0x00000008

-- No documentation found for TopLevel "VkImageAspectFlags"
type VkImageAspectFlags = VkImageAspectFlagBits

-- No documentation found for TopLevel "VkImageSubresource"
data VkImageSubresource = VkImageSubresource
  { -- No documentation found for Nested "VkImageSubresource" "aspectMask"
  vkAspectMask :: VkImageAspectFlags
  , -- No documentation found for Nested "VkImageSubresource" "mipLevel"
  vkMipLevel :: Word32
  , -- No documentation found for Nested "VkImageSubresource" "arrayLayer"
  vkArrayLayer :: Word32
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

instance Zero VkImageSubresource where
  zero = VkImageSubresource zero
                            zero
                            zero

-- No documentation found for TopLevel "VkOffset3D"
data VkOffset3D = VkOffset3D
  { -- No documentation found for Nested "VkOffset3D" "x"
  vkX :: Int32
  , -- No documentation found for Nested "VkOffset3D" "y"
  vkY :: Int32
  , -- No documentation found for Nested "VkOffset3D" "z"
  vkZ :: Int32
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

instance Zero VkOffset3D where
  zero = VkOffset3D zero
                    zero
                    zero

-- No documentation found for TopLevel "VkSparseBufferMemoryBindInfo"
data VkSparseBufferMemoryBindInfo = VkSparseBufferMemoryBindInfo
  { -- No documentation found for Nested "VkSparseBufferMemoryBindInfo" "buffer"
  vkBuffer :: VkBuffer
  , -- No documentation found for Nested "VkSparseBufferMemoryBindInfo" "bindCount"
  vkBindCount :: Word32
  , -- No documentation found for Nested "VkSparseBufferMemoryBindInfo" "pBinds"
  vkPBinds :: Ptr VkSparseMemoryBind
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

instance Zero VkSparseBufferMemoryBindInfo where
  zero = VkSparseBufferMemoryBindInfo zero
                                      zero
                                      zero

-- ** VkSparseImageFormatFlagBits

-- No documentation found for TopLevel "VkSparseImageFormatFlagBits"
newtype VkSparseImageFormatFlagBits = VkSparseImageFormatFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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

-- No documentation found for Nested "VkSparseImageFormatFlagBits" "VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT"
pattern VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT :: VkSparseImageFormatFlagBits
pattern VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT = VkSparseImageFormatFlagBits 0x00000001

-- No documentation found for Nested "VkSparseImageFormatFlagBits" "VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT"
pattern VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT :: VkSparseImageFormatFlagBits
pattern VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT = VkSparseImageFormatFlagBits 0x00000002

-- No documentation found for Nested "VkSparseImageFormatFlagBits" "VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT"
pattern VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT :: VkSparseImageFormatFlagBits
pattern VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT = VkSparseImageFormatFlagBits 0x00000004

-- No documentation found for TopLevel "VkSparseImageFormatFlags"
type VkSparseImageFormatFlags = VkSparseImageFormatFlagBits

-- No documentation found for TopLevel "VkSparseImageFormatProperties"
data VkSparseImageFormatProperties = VkSparseImageFormatProperties
  { -- No documentation found for Nested "VkSparseImageFormatProperties" "aspectMask"
  vkAspectMask :: VkImageAspectFlags
  , -- No documentation found for Nested "VkSparseImageFormatProperties" "imageGranularity"
  vkImageGranularity :: VkExtent3D
  , -- No documentation found for Nested "VkSparseImageFormatProperties" "flags"
  vkFlags :: VkSparseImageFormatFlags
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

instance Zero VkSparseImageFormatProperties where
  zero = VkSparseImageFormatProperties zero
                                       zero
                                       zero

-- No documentation found for TopLevel "VkSparseImageMemoryBind"
data VkSparseImageMemoryBind = VkSparseImageMemoryBind
  { -- No documentation found for Nested "VkSparseImageMemoryBind" "subresource"
  vkSubresource :: VkImageSubresource
  , -- No documentation found for Nested "VkSparseImageMemoryBind" "offset"
  vkOffset :: VkOffset3D
  , -- No documentation found for Nested "VkSparseImageMemoryBind" "extent"
  vkExtent :: VkExtent3D
  , -- No documentation found for Nested "VkSparseImageMemoryBind" "memory"
  vkMemory :: VkDeviceMemory
  , -- No documentation found for Nested "VkSparseImageMemoryBind" "memoryOffset"
  vkMemoryOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkSparseImageMemoryBind" "flags"
  vkFlags :: VkSparseMemoryBindFlags
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

instance Zero VkSparseImageMemoryBind where
  zero = VkSparseImageMemoryBind zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero

-- No documentation found for TopLevel "VkSparseImageMemoryBindInfo"
data VkSparseImageMemoryBindInfo = VkSparseImageMemoryBindInfo
  { -- No documentation found for Nested "VkSparseImageMemoryBindInfo" "image"
  vkImage :: VkImage
  , -- No documentation found for Nested "VkSparseImageMemoryBindInfo" "bindCount"
  vkBindCount :: Word32
  , -- No documentation found for Nested "VkSparseImageMemoryBindInfo" "pBinds"
  vkPBinds :: Ptr VkSparseImageMemoryBind
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

instance Zero VkSparseImageMemoryBindInfo where
  zero = VkSparseImageMemoryBindInfo zero
                                     zero
                                     zero

-- No documentation found for TopLevel "VkSparseImageMemoryRequirements"
data VkSparseImageMemoryRequirements = VkSparseImageMemoryRequirements
  { -- No documentation found for Nested "VkSparseImageMemoryRequirements" "formatProperties"
  vkFormatProperties :: VkSparseImageFormatProperties
  , -- No documentation found for Nested "VkSparseImageMemoryRequirements" "imageMipTailFirstLod"
  vkImageMipTailFirstLod :: Word32
  , -- No documentation found for Nested "VkSparseImageMemoryRequirements" "imageMipTailSize"
  vkImageMipTailSize :: VkDeviceSize
  , -- No documentation found for Nested "VkSparseImageMemoryRequirements" "imageMipTailOffset"
  vkImageMipTailOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkSparseImageMemoryRequirements" "imageMipTailStride"
  vkImageMipTailStride :: VkDeviceSize
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

instance Zero VkSparseImageMemoryRequirements where
  zero = VkSparseImageMemoryRequirements zero
                                         zero
                                         zero
                                         zero
                                         zero

-- No documentation found for TopLevel "VkSparseImageOpaqueMemoryBindInfo"
data VkSparseImageOpaqueMemoryBindInfo = VkSparseImageOpaqueMemoryBindInfo
  { -- No documentation found for Nested "VkSparseImageOpaqueMemoryBindInfo" "image"
  vkImage :: VkImage
  , -- No documentation found for Nested "VkSparseImageOpaqueMemoryBindInfo" "bindCount"
  vkBindCount :: Word32
  , -- No documentation found for Nested "VkSparseImageOpaqueMemoryBindInfo" "pBinds"
  vkPBinds :: Ptr VkSparseMemoryBind
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

instance Zero VkSparseImageOpaqueMemoryBindInfo where
  zero = VkSparseImageOpaqueMemoryBindInfo zero
                                           zero
                                           zero

-- No documentation found for TopLevel "VkSparseMemoryBind"
data VkSparseMemoryBind = VkSparseMemoryBind
  { -- No documentation found for Nested "VkSparseMemoryBind" "resourceOffset"
  vkResourceOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkSparseMemoryBind" "size"
  vkSize :: VkDeviceSize
  , -- No documentation found for Nested "VkSparseMemoryBind" "memory"
  vkMemory :: VkDeviceMemory
  , -- No documentation found for Nested "VkSparseMemoryBind" "memoryOffset"
  vkMemoryOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkSparseMemoryBind" "flags"
  vkFlags :: VkSparseMemoryBindFlags
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

instance Zero VkSparseMemoryBind where
  zero = VkSparseMemoryBind zero
                            zero
                            zero
                            zero
                            zero

-- ** VkSparseMemoryBindFlagBits

-- No documentation found for TopLevel "VkSparseMemoryBindFlagBits"
newtype VkSparseMemoryBindFlagBits = VkSparseMemoryBindFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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

-- No documentation found for Nested "VkSparseMemoryBindFlagBits" "VK_SPARSE_MEMORY_BIND_METADATA_BIT"
pattern VK_SPARSE_MEMORY_BIND_METADATA_BIT :: VkSparseMemoryBindFlagBits
pattern VK_SPARSE_MEMORY_BIND_METADATA_BIT = VkSparseMemoryBindFlagBits 0x00000001

-- No documentation found for TopLevel "VkSparseMemoryBindFlags"
type VkSparseMemoryBindFlags = VkSparseMemoryBindFlagBits

-- No documentation found for TopLevel "vkGetImageSparseMemoryRequirements"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetImageSparseMemoryRequirements" vkGetImageSparseMemoryRequirements :: ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements) -> IO ()
#else
vkGetImageSparseMemoryRequirements :: DeviceCmds -> ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements) -> IO ()
vkGetImageSparseMemoryRequirements deviceCmds = mkVkGetImageSparseMemoryRequirements (pVkGetImageSparseMemoryRequirements deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageSparseMemoryRequirements
  :: FunPtr (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements) -> IO ()) -> (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements) -> IO ())
#endif

type FN_vkGetImageSparseMemoryRequirements = ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements) -> IO ()
type PFN_vkGetImageSparseMemoryRequirements = FunPtr FN_vkGetImageSparseMemoryRequirements

-- No documentation found for TopLevel "vkGetPhysicalDeviceSparseImageFormatProperties"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceSparseImageFormatProperties" vkGetPhysicalDeviceSparseImageFormatProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("samples" ::: VkSampleCountFlagBits) -> ("usage" ::: VkImageUsageFlags) -> ("tiling" ::: VkImageTiling) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties) -> IO ()
#else
vkGetPhysicalDeviceSparseImageFormatProperties :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("samples" ::: VkSampleCountFlagBits) -> ("usage" ::: VkImageUsageFlags) -> ("tiling" ::: VkImageTiling) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties) -> IO ()
vkGetPhysicalDeviceSparseImageFormatProperties deviceCmds = mkVkGetPhysicalDeviceSparseImageFormatProperties (pVkGetPhysicalDeviceSparseImageFormatProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSparseImageFormatProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("samples" ::: VkSampleCountFlagBits) -> ("usage" ::: VkImageUsageFlags) -> ("tiling" ::: VkImageTiling) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("samples" ::: VkSampleCountFlagBits) -> ("usage" ::: VkImageUsageFlags) -> ("tiling" ::: VkImageTiling) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties) -> IO ())
#endif

type FN_vkGetPhysicalDeviceSparseImageFormatProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("samples" ::: VkSampleCountFlagBits) -> ("usage" ::: VkImageUsageFlags) -> ("tiling" ::: VkImageTiling) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties) -> IO ()
type PFN_vkGetPhysicalDeviceSparseImageFormatProperties = FunPtr FN_vkGetPhysicalDeviceSparseImageFormatProperties

-- No documentation found for TopLevel "vkQueueBindSparse"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkQueueBindSparse" vkQueueBindSparse :: ("queue" ::: VkQueue) -> ("bindInfoCount" ::: Word32) -> ("pBindInfo" ::: Ptr VkBindSparseInfo) -> ("fence" ::: VkFence) -> IO VkResult
#else
vkQueueBindSparse :: DeviceCmds -> ("queue" ::: VkQueue) -> ("bindInfoCount" ::: Word32) -> ("pBindInfo" ::: Ptr VkBindSparseInfo) -> ("fence" ::: VkFence) -> IO VkResult
vkQueueBindSparse deviceCmds = mkVkQueueBindSparse (pVkQueueBindSparse deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueBindSparse
  :: FunPtr (("queue" ::: VkQueue) -> ("bindInfoCount" ::: Word32) -> ("pBindInfo" ::: Ptr VkBindSparseInfo) -> ("fence" ::: VkFence) -> IO VkResult) -> (("queue" ::: VkQueue) -> ("bindInfoCount" ::: Word32) -> ("pBindInfo" ::: Ptr VkBindSparseInfo) -> ("fence" ::: VkFence) -> IO VkResult)
#endif

type FN_vkQueueBindSparse = ("queue" ::: VkQueue) -> ("bindInfoCount" ::: Word32) -> ("pBindInfo" ::: Ptr VkBindSparseInfo) -> ("fence" ::: VkFence) -> IO VkResult
type PFN_vkQueueBindSparse = FunPtr FN_vkQueueBindSparse
