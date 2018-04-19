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

-- | VkExternalMemoryHandleTypeFlagBits - Bit specifying external memory
-- handle types
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

-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000001

-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000002

-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000004

-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000008

-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000010

-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000020

-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000040
-- ** VkExternalMemoryFeatureFlagBits

-- | VkExternalMemoryFeatureFlagBits - Bitmask specifying features of an
-- external memory handle type
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

-- No documentation found for Nested "VkExternalMemoryFeatureFlagBits" "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT"
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT :: VkExternalMemoryFeatureFlagBits
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT = VkExternalMemoryFeatureFlagBits 0x00000001

-- No documentation found for Nested "VkExternalMemoryFeatureFlagBits" "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT"
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT :: VkExternalMemoryFeatureFlagBits
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT = VkExternalMemoryFeatureFlagBits 0x00000002

-- No documentation found for Nested "VkExternalMemoryFeatureFlagBits" "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT"
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT :: VkExternalMemoryFeatureFlagBits
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT = VkExternalMemoryFeatureFlagBits 0x00000004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO = VkStructureType 1000071000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES"
pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES = VkStructureType 1000071001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO = VkStructureType 1000071002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES"
pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES = VkStructureType 1000071003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES = VkStructureType 1000071004
-- No documentation found for TopLevel "VK_LUID_SIZE"
type VK_LUID_SIZE = 8
-- No documentation found for Nested "Integral a => a" "VK_LUID_SIZE"
pattern VK_LUID_SIZE :: Integral a => a
pattern VK_LUID_SIZE = 8
-- | vkGetPhysicalDeviceExternalBufferProperties - Query external handle
-- types supported by buffers
foreign import ccall "vkGetPhysicalDeviceExternalBufferProperties" vkGetPhysicalDeviceExternalBufferProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalBufferInfo" ::: Ptr VkPhysicalDeviceExternalBufferInfo) -> ("pExternalBufferProperties" ::: Ptr VkExternalBufferProperties) -> IO ()
-- | VkExternalMemoryProperties - Structure specifying external memory handle
-- type capabilities
data VkExternalMemoryProperties = VkExternalMemoryProperties
  { -- No documentation found for Nested "VkExternalMemoryProperties" "vkExternalMemoryFeatures"
  vkExternalMemoryFeatures :: VkExternalMemoryFeatureFlags
  , -- No documentation found for Nested "VkExternalMemoryProperties" "vkExportFromImportedHandleTypes"
  vkExportFromImportedHandleTypes :: VkExternalMemoryHandleTypeFlags
  , -- No documentation found for Nested "VkExternalMemoryProperties" "vkCompatibleHandleTypes"
  vkCompatibleHandleTypes :: VkExternalMemoryHandleTypeFlags
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
-- | VkPhysicalDeviceExternalImageFormatInfo - Structure specifying external
-- image creation parameters
data VkPhysicalDeviceExternalImageFormatInfo = VkPhysicalDeviceExternalImageFormatInfo
  { -- No documentation found for Nested "VkPhysicalDeviceExternalImageFormatInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceExternalImageFormatInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceExternalImageFormatInfo" "vkHandleType"
  vkHandleType :: VkExternalMemoryHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceExternalImageFormatInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceExternalImageFormatInfo <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceExternalImageFormatInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceExternalImageFormatInfo))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkPhysicalDeviceExternalImageFormatInfo))
-- | VkExternalImageFormatProperties - Structure specifying supported
-- external handle properties
data VkExternalImageFormatProperties = VkExternalImageFormatProperties
  { -- No documentation found for Nested "VkExternalImageFormatProperties" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExternalImageFormatProperties" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkExternalImageFormatProperties" "vkExternalMemoryProperties"
  vkExternalMemoryProperties :: VkExternalMemoryProperties
  }
  deriving (Eq, Show)

instance Storable VkExternalImageFormatProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkExternalImageFormatProperties <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalImageFormatProperties))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalImageFormatProperties))
                *> poke (ptr `plusPtr` 16) (vkExternalMemoryProperties (poked :: VkExternalImageFormatProperties))
-- | VkPhysicalDeviceExternalBufferInfo - Structure specifying buffer
-- creation parameters
data VkPhysicalDeviceExternalBufferInfo = VkPhysicalDeviceExternalBufferInfo
  { -- No documentation found for Nested "VkPhysicalDeviceExternalBufferInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceExternalBufferInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceExternalBufferInfo" "vkFlags"
  vkFlags :: VkBufferCreateFlags
  , -- No documentation found for Nested "VkPhysicalDeviceExternalBufferInfo" "vkUsage"
  vkUsage :: VkBufferUsageFlags
  , -- No documentation found for Nested "VkPhysicalDeviceExternalBufferInfo" "vkHandleType"
  vkHandleType :: VkExternalMemoryHandleTypeFlagBits
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceExternalBufferInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPhysicalDeviceExternalBufferInfo))
                *> poke (ptr `plusPtr` 20) (vkUsage (poked :: VkPhysicalDeviceExternalBufferInfo))
                *> poke (ptr `plusPtr` 24) (vkHandleType (poked :: VkPhysicalDeviceExternalBufferInfo))
-- | VkExternalBufferProperties - Structure specifying supported external
-- handle capabilities
data VkExternalBufferProperties = VkExternalBufferProperties
  { -- No documentation found for Nested "VkExternalBufferProperties" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExternalBufferProperties" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkExternalBufferProperties" "vkExternalMemoryProperties"
  vkExternalMemoryProperties :: VkExternalMemoryProperties
  }
  deriving (Eq, Show)

instance Storable VkExternalBufferProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkExternalBufferProperties <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalBufferProperties))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalBufferProperties))
                *> poke (ptr `plusPtr` 16) (vkExternalMemoryProperties (poked :: VkExternalBufferProperties))
-- | VkPhysicalDeviceIDProperties - Structure specifying IDs related to the
-- physical device
data VkPhysicalDeviceIDProperties = VkPhysicalDeviceIDProperties
  { -- No documentation found for Nested "VkPhysicalDeviceIDProperties" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceIDProperties" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceIDProperties" "vkDeviceUUID"
  vkDeviceUUID :: Vector VK_UUID_SIZE Word8
  , -- No documentation found for Nested "VkPhysicalDeviceIDProperties" "vkDriverUUID"
  vkDriverUUID :: Vector VK_UUID_SIZE Word8
  , -- No documentation found for Nested "VkPhysicalDeviceIDProperties" "vkDeviceLUID"
  vkDeviceLUID :: Vector VK_LUID_SIZE Word8
  , -- No documentation found for Nested "VkPhysicalDeviceIDProperties" "vkDeviceNodeMask"
  vkDeviceNodeMask :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceIDProperties" "vkDeviceLUIDValid"
  vkDeviceLUIDValid :: VkBool32
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceIDProperties))
                *> poke (ptr `plusPtr` 16) (vkDeviceUUID (poked :: VkPhysicalDeviceIDProperties))
                *> poke (ptr `plusPtr` 32) (vkDriverUUID (poked :: VkPhysicalDeviceIDProperties))
                *> poke (ptr `plusPtr` 48) (vkDeviceLUID (poked :: VkPhysicalDeviceIDProperties))
                *> poke (ptr `plusPtr` 56) (vkDeviceNodeMask (poked :: VkPhysicalDeviceIDProperties))
                *> poke (ptr `plusPtr` 60) (vkDeviceLUIDValid (poked :: VkPhysicalDeviceIDProperties))
-- | VkExternalMemoryHandleTypeFlags - Bitmask of
-- VkExternalMemoryHandleTypeFlagBits
type VkExternalMemoryHandleTypeFlags = VkExternalMemoryHandleTypeFlagBits
-- | VkExternalMemoryFeatureFlags - Bitmask of
-- VkExternalMemoryFeatureFlagBits
type VkExternalMemoryFeatureFlags = VkExternalMemoryFeatureFlagBits
