{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Buffer
  ( VkSharingMode(..)
  , pattern VK_SHARING_MODE_EXCLUSIVE
  , pattern VK_SHARING_MODE_CONCURRENT
  , VkBufferUsageFlagBits(..)
  , pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT
  , pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT
  , pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT
  , VkBufferCreateFlagBits(..)
  , pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT
  , pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT
  , pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT
  , vkCreateBuffer
  , vkDestroyBuffer
  , VkBufferCreateInfo(..)
  , VkBufferUsageFlags
  , VkBufferCreateFlags
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
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDeviceSize
  , VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkBuffer
  )


-- ** VkSharingMode

-- | 
newtype VkSharingMode = VkSharingMode Int32
  deriving (Eq, Ord, Storable)

instance Show VkSharingMode where
  showsPrec _ VK_SHARING_MODE_EXCLUSIVE = showString "VK_SHARING_MODE_EXCLUSIVE"
  showsPrec _ VK_SHARING_MODE_CONCURRENT = showString "VK_SHARING_MODE_CONCURRENT"
  showsPrec p (VkSharingMode x) = showParen (p >= 11) (showString "VkSharingMode " . showsPrec 11 x)

instance Read VkSharingMode where
  readPrec = parens ( choose [ ("VK_SHARING_MODE_EXCLUSIVE",  pure VK_SHARING_MODE_EXCLUSIVE)
                             , ("VK_SHARING_MODE_CONCURRENT", pure VK_SHARING_MODE_CONCURRENT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSharingMode")
                        v <- step readPrec
                        pure (VkSharingMode v)
                        )
                    )

-- | 
pattern VK_SHARING_MODE_EXCLUSIVE :: VkSharingMode
pattern VK_SHARING_MODE_EXCLUSIVE = VkSharingMode 0

-- | 
pattern VK_SHARING_MODE_CONCURRENT :: VkSharingMode
pattern VK_SHARING_MODE_CONCURRENT = VkSharingMode 1
-- ** VkBufferUsageFlagBits

-- | 
newtype VkBufferUsageFlagBits = VkBufferUsageFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkBufferUsageFlagBits where
  showsPrec _ VK_BUFFER_USAGE_TRANSFER_SRC_BIT = showString "VK_BUFFER_USAGE_TRANSFER_SRC_BIT"
  showsPrec _ VK_BUFFER_USAGE_TRANSFER_DST_BIT = showString "VK_BUFFER_USAGE_TRANSFER_DST_BIT"
  showsPrec _ VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT = showString "VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT = showString "VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT = showString "VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_STORAGE_BUFFER_BIT = showString "VK_BUFFER_USAGE_STORAGE_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_INDEX_BUFFER_BIT = showString "VK_BUFFER_USAGE_INDEX_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_VERTEX_BUFFER_BIT = showString "VK_BUFFER_USAGE_VERTEX_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT = showString "VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT"
  showsPrec p (VkBufferUsageFlagBits x) = showParen (p >= 11) (showString "VkBufferUsageFlagBits " . showsPrec 11 x)

instance Read VkBufferUsageFlagBits where
  readPrec = parens ( choose [ ("VK_BUFFER_USAGE_TRANSFER_SRC_BIT",         pure VK_BUFFER_USAGE_TRANSFER_SRC_BIT)
                             , ("VK_BUFFER_USAGE_TRANSFER_DST_BIT",         pure VK_BUFFER_USAGE_TRANSFER_DST_BIT)
                             , ("VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT", pure VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT", pure VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT",       pure VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_STORAGE_BUFFER_BIT",       pure VK_BUFFER_USAGE_STORAGE_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_INDEX_BUFFER_BIT",         pure VK_BUFFER_USAGE_INDEX_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_VERTEX_BUFFER_BIT",        pure VK_BUFFER_USAGE_VERTEX_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT",      pure VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBufferUsageFlagBits")
                        v <- step readPrec
                        pure (VkBufferUsageFlagBits v)
                        )
                    )

-- | Can be used as a source of transfer operations
pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT = VkBufferUsageFlagBits 0x00000001

-- | Can be used as a destination of transfer operations
pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT = VkBufferUsageFlagBits 0x00000002

-- | Can be used as TBO
pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT = VkBufferUsageFlagBits 0x00000004

-- | Can be used as IBO
pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT = VkBufferUsageFlagBits 0x00000008

-- | Can be used as UBO
pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT = VkBufferUsageFlagBits 0x00000010

-- | Can be used as SSBO
pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT = VkBufferUsageFlagBits 0x00000020

-- | Can be used as source of fixed-function index fetch (index buffer)
pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT = VkBufferUsageFlagBits 0x00000040

-- | Can be used as source of fixed-function vertex fetch (VBO)
pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT = VkBufferUsageFlagBits 0x00000080

-- | Can be the source of indirect parameters (e.g. indirect buffer, parameter buffer)
pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT = VkBufferUsageFlagBits 0x00000100
-- ** VkBufferCreateFlagBits

-- | 
newtype VkBufferCreateFlagBits = VkBufferCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkBufferCreateFlagBits where
  showsPrec _ VK_BUFFER_CREATE_SPARSE_BINDING_BIT = showString "VK_BUFFER_CREATE_SPARSE_BINDING_BIT"
  showsPrec _ VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT = showString "VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT"
  showsPrec _ VK_BUFFER_CREATE_SPARSE_ALIASED_BIT = showString "VK_BUFFER_CREATE_SPARSE_ALIASED_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkBufferCreateFlagBits 0x00000008) = showString "VK_BUFFER_CREATE_PROTECTED_BIT"
  showsPrec p (VkBufferCreateFlagBits x) = showParen (p >= 11) (showString "VkBufferCreateFlagBits " . showsPrec 11 x)

instance Read VkBufferCreateFlagBits where
  readPrec = parens ( choose [ ("VK_BUFFER_CREATE_SPARSE_BINDING_BIT",   pure VK_BUFFER_CREATE_SPARSE_BINDING_BIT)
                             , ("VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT", pure VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT)
                             , ("VK_BUFFER_CREATE_SPARSE_ALIASED_BIT",   pure VK_BUFFER_CREATE_SPARSE_ALIASED_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_BUFFER_CREATE_PROTECTED_BIT", pure (VkBufferCreateFlagBits 0x00000008))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBufferCreateFlagBits")
                        v <- step readPrec
                        pure (VkBufferCreateFlagBits v)
                        )
                    )

-- | Buffer should support sparse backing
pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT :: VkBufferCreateFlagBits
pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT = VkBufferCreateFlagBits 0x00000001

-- | Buffer should support sparse backing with partial residency
pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT :: VkBufferCreateFlagBits
pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT = VkBufferCreateFlagBits 0x00000002

-- | Buffer should support constent data access to physical memory ranges mapped into multiple locations of sparse buffers
pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT :: VkBufferCreateFlagBits
pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT = VkBufferCreateFlagBits 0x00000004
-- | 
foreign import ccall "vkCreateBuffer" vkCreateBuffer :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pBuffer" ::: Ptr VkBuffer) -> IO VkResult
-- | 
foreign import ccall "vkDestroyBuffer" vkDestroyBuffer :: ("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | TODO: Struct comments
data VkBufferCreateInfo = VkBufferCreateInfo
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkFlags :: VkBufferCreateFlags
  , vkSize :: VkDeviceSize
  , vkUsage :: VkBufferUsageFlags
  , vkSharingMode :: VkSharingMode
  , vkQueueFamilyIndexCount :: Word32
  , vkPQueueFamilyIndices :: Ptr Word32
  }
  deriving (Eq, Show)

instance Storable VkBufferCreateInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkBufferCreateInfo <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 24)
                                <*> peek (ptr `plusPtr` 32)
                                <*> peek (ptr `plusPtr` 36)
                                <*> peek (ptr `plusPtr` 40)
                                <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkSize (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkUsage (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 36) (vkSharingMode (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkQueueFamilyIndexCount (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkPQueueFamilyIndices (poked :: VkBufferCreateInfo))
type VkBufferUsageFlags = VkBufferUsageFlagBits
type VkBufferCreateFlags = VkBufferCreateFlagBits
