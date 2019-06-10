{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  ExternalBufferProperties(..)
  , 
  ExternalImageFormatProperties(..)
#endif
  , ExternalMemoryFeatureFlagBits
  , pattern EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT
  , pattern EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT
  , pattern EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT
  , ExternalMemoryFeatureFlagBitsKHR
  , ExternalMemoryFeatureFlags
  , ExternalMemoryFeatureFlagsKHR
  , ExternalMemoryHandleTypeFlagBits
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT
  , ExternalMemoryHandleTypeFlagBitsKHR
  , ExternalMemoryHandleTypeFlags
  , ExternalMemoryHandleTypeFlagsKHR
  , ExternalMemoryProperties(..)
#if defined(VK_USE_PLATFORM_GGP)
  , PhysicalDeviceExternalBufferInfo(..)
  , PhysicalDeviceExternalImageFormatInfo(..)
  , PhysicalDeviceIDProperties(..)
  , getPhysicalDeviceExternalBufferProperties
#endif
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO
  , pattern STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO
  , pattern STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.ByteString
  ( ByteString
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Alloc
  ( alloca
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Utils
  ( with
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peek
  )
#endif


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryFeatureFlagBits(..)
  , VkExternalMemoryHandleTypeFlagBits(..)
  , pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT
  , pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT
  , pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( vkGetPhysicalDeviceExternalBufferProperties
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_dma_buf
  ( pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host
  ( pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Buffer
  ( BufferCreateFlags
  , BufferUsageFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES
  , pattern STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkExternalBufferProperties"
data ExternalBufferProperties = ExternalBufferProperties
  { -- No documentation found for Nested "ExternalBufferProperties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalBufferProperties" "externalMemoryProperties"
  externalMemoryProperties :: ExternalMemoryProperties
  }
  deriving (Show, Eq)

instance Zero ExternalBufferProperties where
  zero = ExternalBufferProperties Nothing
                                  zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkExternalImageFormatProperties"
data ExternalImageFormatProperties = ExternalImageFormatProperties
  { -- No documentation found for Nested "ExternalImageFormatProperties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalImageFormatProperties" "externalMemoryProperties"
  externalMemoryProperties :: ExternalMemoryProperties
  }
  deriving (Show, Eq)

instance Zero ExternalImageFormatProperties where
  zero = ExternalImageFormatProperties Nothing
                                       zero

#endif

-- No documentation found for TopLevel "ExternalMemoryFeatureFlagBits"
type ExternalMemoryFeatureFlagBits = VkExternalMemoryFeatureFlagBits


{-# complete EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT, EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT, EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT :: ExternalMemoryFeatureFlagBits #-}


-- No documentation found for Nested "ExternalMemoryFeatureFlagBits" "EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT"
pattern EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT :: (a ~ ExternalMemoryFeatureFlagBits) => a
pattern EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT = VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT


-- No documentation found for Nested "ExternalMemoryFeatureFlagBits" "EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT"
pattern EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT :: (a ~ ExternalMemoryFeatureFlagBits) => a
pattern EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT = VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT


-- No documentation found for Nested "ExternalMemoryFeatureFlagBits" "EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT"
pattern EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT :: (a ~ ExternalMemoryFeatureFlagBits) => a
pattern EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT = VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT

-- No documentation found for TopLevel "ExternalMemoryFeatureFlagBitsKHR"
type ExternalMemoryFeatureFlagBitsKHR = ExternalMemoryFeatureFlagBits

-- No documentation found for TopLevel "ExternalMemoryFeatureFlags"
type ExternalMemoryFeatureFlags = ExternalMemoryFeatureFlagBits

-- No documentation found for TopLevel "ExternalMemoryFeatureFlagsKHR"
type ExternalMemoryFeatureFlagsKHR = ExternalMemoryFeatureFlags

-- No documentation found for TopLevel "ExternalMemoryHandleTypeFlagBits"
type ExternalMemoryHandleTypeFlagBits = VkExternalMemoryHandleTypeFlagBits


{-# complete EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT, EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT, EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT, EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT, EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT, EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT, EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT, EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT, EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID, EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT, EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT :: ExternalMemoryHandleTypeFlagBits #-}


-- No documentation found for Nested "ExternalMemoryHandleTypeFlagBits" "EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT :: (a ~ ExternalMemoryHandleTypeFlagBits) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT


-- No documentation found for Nested "ExternalMemoryHandleTypeFlagBits" "EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT :: (a ~ ExternalMemoryHandleTypeFlagBits) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT


-- No documentation found for Nested "ExternalMemoryHandleTypeFlagBits" "EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT :: (a ~ ExternalMemoryHandleTypeFlagBits) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT


-- No documentation found for Nested "ExternalMemoryHandleTypeFlagBits" "EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT :: (a ~ ExternalMemoryHandleTypeFlagBits) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT


-- No documentation found for Nested "ExternalMemoryHandleTypeFlagBits" "EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT :: (a ~ ExternalMemoryHandleTypeFlagBits) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT


-- No documentation found for Nested "ExternalMemoryHandleTypeFlagBits" "EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT :: (a ~ ExternalMemoryHandleTypeFlagBits) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT


-- No documentation found for Nested "ExternalMemoryHandleTypeFlagBits" "EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT :: (a ~ ExternalMemoryHandleTypeFlagBits) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT


-- No documentation found for Nested "ExternalMemoryHandleTypeFlagBits" "EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT :: (a ~ ExternalMemoryHandleTypeFlagBits) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT = VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT


-- No documentation found for Nested "ExternalMemoryHandleTypeFlagBits" "EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID :: (a ~ ExternalMemoryHandleTypeFlagBits) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID = VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID


-- No documentation found for Nested "ExternalMemoryHandleTypeFlagBits" "EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT :: (a ~ ExternalMemoryHandleTypeFlagBits) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT = VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT


-- No documentation found for Nested "ExternalMemoryHandleTypeFlagBits" "EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT :: (a ~ ExternalMemoryHandleTypeFlagBits) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT = VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT

-- No documentation found for TopLevel "ExternalMemoryHandleTypeFlagBitsKHR"
type ExternalMemoryHandleTypeFlagBitsKHR = ExternalMemoryHandleTypeFlagBits

-- No documentation found for TopLevel "ExternalMemoryHandleTypeFlags"
type ExternalMemoryHandleTypeFlags = ExternalMemoryHandleTypeFlagBits

-- No documentation found for TopLevel "ExternalMemoryHandleTypeFlagsKHR"
type ExternalMemoryHandleTypeFlagsKHR = ExternalMemoryHandleTypeFlags


-- No documentation found for TopLevel "VkExternalMemoryProperties"
data ExternalMemoryProperties = ExternalMemoryProperties
  { -- No documentation found for Nested "ExternalMemoryProperties" "externalMemoryFeatures"
  externalMemoryFeatures :: ExternalMemoryFeatureFlags
  , -- No documentation found for Nested "ExternalMemoryProperties" "exportFromImportedHandleTypes"
  exportFromImportedHandleTypes :: ExternalMemoryHandleTypeFlags
  , -- No documentation found for Nested "ExternalMemoryProperties" "compatibleHandleTypes"
  compatibleHandleTypes :: ExternalMemoryHandleTypeFlags
  }
  deriving (Show, Eq)

instance Zero ExternalMemoryProperties where
  zero = ExternalMemoryProperties zero
                                  zero
                                  zero



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceExternalBufferInfo"
data PhysicalDeviceExternalBufferInfo = PhysicalDeviceExternalBufferInfo
  { -- No documentation found for Nested "PhysicalDeviceExternalBufferInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceExternalBufferInfo" "flags"
  flags :: BufferCreateFlags
  , -- No documentation found for Nested "PhysicalDeviceExternalBufferInfo" "usage"
  usage :: BufferUsageFlags
  , -- No documentation found for Nested "PhysicalDeviceExternalBufferInfo" "handleType"
  handleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceExternalBufferInfo where
  zero = PhysicalDeviceExternalBufferInfo Nothing
                                          zero
                                          zero
                                          zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceExternalImageFormatInfo"
data PhysicalDeviceExternalImageFormatInfo = PhysicalDeviceExternalImageFormatInfo
  { -- No documentation found for Nested "PhysicalDeviceExternalImageFormatInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceExternalImageFormatInfo" "handleType"
  handleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceExternalImageFormatInfo where
  zero = PhysicalDeviceExternalImageFormatInfo Nothing
                                               zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceIDProperties"
data PhysicalDeviceIDProperties = PhysicalDeviceIDProperties
  { -- No documentation found for Nested "PhysicalDeviceIDProperties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceIDProperties" "deviceUUID"
  deviceUUID :: ByteString
  , -- No documentation found for Nested "PhysicalDeviceIDProperties" "driverUUID"
  driverUUID :: ByteString
  , -- No documentation found for Nested "PhysicalDeviceIDProperties" "deviceLUID"
  deviceLUID :: ByteString
  , -- No documentation found for Nested "PhysicalDeviceIDProperties" "deviceNodeMask"
  deviceNodeMask :: Word32
  , -- No documentation found for Nested "PhysicalDeviceIDProperties" "deviceLUIDValid"
  deviceLUIDValid :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceIDProperties where
  zero = PhysicalDeviceIDProperties Nothing
                                    mempty
                                    mempty
                                    mempty
                                    zero
                                    False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceExternalBufferProperties"
getPhysicalDeviceExternalBufferProperties :: PhysicalDevice ->  PhysicalDeviceExternalBufferInfo ->  IO (ExternalBufferProperties)
getPhysicalDeviceExternalBufferProperties = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif
