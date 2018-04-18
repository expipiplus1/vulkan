{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBits(..)
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT
  , VkExternalMemoryFeatureFlagBits(..)
  , pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT
  , pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT
  , pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES
  , VK_LUID_SIZE
  , pattern VK_LUID_SIZE
  , vkGetPhysicalDeviceExternalBufferProperties
  , VkExternalMemoryProperties(..)
  , VkPhysicalDeviceExternalImageFormatInfo(..)
  , VkExternalImageFormatProperties(..)
  , VkPhysicalDeviceExternalBufferInfo(..)
  , VkExternalBufferProperties(..)
  , VkPhysicalDeviceIDProperties(..)
  , VkExternalMemoryHandleTypeFlags
  , VkExternalMemoryFeatureFlags
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word32
  , Word8
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


import Graphics.Vulkan.Core10.Buffer
  ( VkBufferUsageFlags
  , VkBufferCreateFlags
  )
import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VK_UUID_SIZE
  , VkPhysicalDevice
  )


-- ** VkExternalMemoryHandleTypeFlagBits

-- | 
newtype VkExternalMemoryHandleTypeFlagBits = VkExternalMemoryHandleTypeFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkExternalMemoryHandleTypeFlagBits where
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkExternalMemoryHandleTypeFlagBits 0x00000200) = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT"
  showsPrec _ (VkExternalMemoryHandleTypeFlagBits 0x00000400) = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID"
  showsPrec _ (VkExternalMemoryHandleTypeFlagBits 0x00000080) = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT"
  showsPrec _ (VkExternalMemoryHandleTypeFlagBits 0x00000100) = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT"
  showsPrec p (VkExternalMemoryHandleTypeFlagBits x) = showParen (p >= 11) (showString "VkExternalMemoryHandleTypeFlagBits " . showsPrec 11 x)

instance Read VkExternalMemoryHandleTypeFlagBits where
  readPrec = parens ( choose [ ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT",         pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT",      pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT",  pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT",     pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT", pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT",        pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT",    pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT",                     pure (VkExternalMemoryHandleTypeFlagBits 0x00000200))
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID", pure (VkExternalMemoryHandleTypeFlagBits 0x00000400))
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT",             pure (VkExternalMemoryHandleTypeFlagBits 0x00000080))
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT",  pure (VkExternalMemoryHandleTypeFlagBits 0x00000100))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalMemoryHandleTypeFlagBits")
                        v <- step readPrec
                        pure (VkExternalMemoryHandleTypeFlagBits v)
                        )
                    )

-- | 
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000001

-- | 
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000002

-- | 
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000004

-- | 
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000008

-- | 
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000010

-- | 
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000020

-- | 
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000040
-- ** VkExternalMemoryFeatureFlagBits

-- | 
newtype VkExternalMemoryFeatureFlagBits = VkExternalMemoryFeatureFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkExternalMemoryFeatureFlagBits where
  showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT = showString "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT"
  showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT = showString "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT"
  showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT = showString "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT"
  showsPrec p (VkExternalMemoryFeatureFlagBits x) = showParen (p >= 11) (showString "VkExternalMemoryFeatureFlagBits " . showsPrec 11 x)

instance Read VkExternalMemoryFeatureFlagBits where
  readPrec = parens ( choose [ ("VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT", pure VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT)
                             , ("VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT",     pure VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT)
                             , ("VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT",     pure VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalMemoryFeatureFlagBits")
                        v <- step readPrec
                        pure (VkExternalMemoryFeatureFlagBits v)
                        )
                    )

-- | 
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT :: VkExternalMemoryFeatureFlagBits
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT = VkExternalMemoryFeatureFlagBits 0x00000001

-- | 
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT :: VkExternalMemoryFeatureFlagBits
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT = VkExternalMemoryFeatureFlagBits 0x00000002

-- | 
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT :: VkExternalMemoryFeatureFlagBits
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT = VkExternalMemoryFeatureFlagBits 0x00000004
-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO = VkStructureType 1000071000
-- | Nothing
pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES = VkStructureType 1000071001
-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO = VkStructureType 1000071002
-- | Nothing
pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES = VkStructureType 1000071003
-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES = VkStructureType 1000071004
type VK_LUID_SIZE = 8
pattern VK_LUID_SIZE :: Integral a => a
pattern VK_LUID_SIZE = 8
-- | 
foreign import ccall "vkGetPhysicalDeviceExternalBufferProperties" vkGetPhysicalDeviceExternalBufferProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalBufferInfo" ::: Ptr VkPhysicalDeviceExternalBufferInfo) -> ("pExternalBufferProperties" ::: Ptr VkExternalBufferProperties) -> IO ()
-- | TODO: Struct comments
data VkExternalMemoryProperties = VkExternalMemoryProperties
  { vkExternalMemoryFeatures :: VkExternalMemoryFeatureFlags
  , vkExportFromImportedHandleTypes :: VkExternalMemoryHandleTypeFlags
  , vkCompatibleHandleTypes :: VkExternalMemoryHandleTypeFlags
  }
  deriving (Eq, Show)

instance Storable VkExternalMemoryProperties where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkExternalMemoryProperties <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 4)
                                        <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkExternalMemoryFeatures (poked :: VkExternalMemoryProperties))
                *> poke (ptr `plusPtr` 4) (vkExportFromImportedHandleTypes (poked :: VkExternalMemoryProperties))
                *> poke (ptr `plusPtr` 8) (vkCompatibleHandleTypes (poked :: VkExternalMemoryProperties))
-- | TODO: Struct comments
data VkPhysicalDeviceExternalImageFormatInfo = VkPhysicalDeviceExternalImageFormatInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkHandleType :: VkExternalMemoryHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceExternalImageFormatInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceExternalImageFormatInfo <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceExternalImageFormatInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPhysicalDeviceExternalImageFormatInfo))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkPhysicalDeviceExternalImageFormatInfo))
-- | TODO: Struct comments
data VkExternalImageFormatProperties = VkExternalImageFormatProperties
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkExternalMemoryProperties :: VkExternalMemoryProperties
  }
  deriving (Eq, Show)

instance Storable VkExternalImageFormatProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkExternalImageFormatProperties <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalImageFormatProperties))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkExternalImageFormatProperties))
                *> poke (ptr `plusPtr` 16) (vkExternalMemoryProperties (poked :: VkExternalImageFormatProperties))
-- | TODO: Struct comments
data VkPhysicalDeviceExternalBufferInfo = VkPhysicalDeviceExternalBufferInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkFlags :: VkBufferCreateFlags
  , vkUsage :: VkBufferUsageFlags
  , vkHandleType :: VkExternalMemoryHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceExternalBufferInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceExternalBufferInfo <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
                                                <*> peek (ptr `plusPtr` 20)
                                                <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceExternalBufferInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPhysicalDeviceExternalBufferInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPhysicalDeviceExternalBufferInfo))
                *> poke (ptr `plusPtr` 20) (vkUsage (poked :: VkPhysicalDeviceExternalBufferInfo))
                *> poke (ptr `plusPtr` 24) (vkHandleType (poked :: VkPhysicalDeviceExternalBufferInfo))
-- | TODO: Struct comments
data VkExternalBufferProperties = VkExternalBufferProperties
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkExternalMemoryProperties :: VkExternalMemoryProperties
  }
  deriving (Eq, Show)

instance Storable VkExternalBufferProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkExternalBufferProperties <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalBufferProperties))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkExternalBufferProperties))
                *> poke (ptr `plusPtr` 16) (vkExternalMemoryProperties (poked :: VkExternalBufferProperties))
-- | TODO: Struct comments
data VkPhysicalDeviceIDProperties = VkPhysicalDeviceIDProperties
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkDeviceUUID :: Vector VK_UUID_SIZE Word8
  , vkDriverUUID :: Vector VK_UUID_SIZE Word8
  , vkDeviceLUID :: Vector VK_LUID_SIZE Word8
  , vkDeviceNodeMask :: Word32
  , vkDeviceLUIDValid :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceIDProperties where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceIDProperties <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
                                          <*> peek (ptr `plusPtr` 32)
                                          <*> peek (ptr `plusPtr` 48)
                                          <*> peek (ptr `plusPtr` 56)
                                          <*> peek (ptr `plusPtr` 60)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceIDProperties))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPhysicalDeviceIDProperties))
                *> poke (ptr `plusPtr` 16) (vkDeviceUUID (poked :: VkPhysicalDeviceIDProperties))
                *> poke (ptr `plusPtr` 32) (vkDriverUUID (poked :: VkPhysicalDeviceIDProperties))
                *> poke (ptr `plusPtr` 48) (vkDeviceLUID (poked :: VkPhysicalDeviceIDProperties))
                *> poke (ptr `plusPtr` 56) (vkDeviceNodeMask (poked :: VkPhysicalDeviceIDProperties))
                *> poke (ptr `plusPtr` 60) (vkDeviceLUIDValid (poked :: VkPhysicalDeviceIDProperties))
type VkExternalMemoryHandleTypeFlags = VkExternalMemoryHandleTypeFlagBits
type VkExternalMemoryFeatureFlags = VkExternalMemoryFeatureFlagBits
