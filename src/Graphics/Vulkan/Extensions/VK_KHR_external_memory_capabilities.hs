{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_memory_capabilities
  ( pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
  , vkGetPhysicalDeviceExternalBufferPropertiesKHR
  , VkExternalMemoryHandleTypeFlagBitsKHR
  , VkExternalMemoryFeatureFlagBitsKHR
  , VkExternalMemoryHandleTypeFlagsKHR
  , VkExternalMemoryFeatureFlagsKHR
  , VkExternalMemoryPropertiesKHR
  , pattern VkExternalMemoryPropertiesKHR
  , VkPhysicalDeviceExternalImageFormatInfoKHR
  , pattern VkPhysicalDeviceExternalImageFormatInfoKHR
  , VkExternalImageFormatPropertiesKHR
  , pattern VkExternalImageFormatPropertiesKHR
  , VkPhysicalDeviceExternalBufferInfoKHR
  , pattern VkPhysicalDeviceExternalBufferInfoKHR
  , VkExternalBufferPropertiesKHR
  , pattern VkExternalBufferPropertiesKHR
  , VkPhysicalDeviceIDPropertiesKHR
  , pattern VkPhysicalDeviceIDPropertiesKHR
  , pattern VK_LUID_SIZE_KHR
  , VK_LUID_SIZE_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR
  , pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR
  , pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR
  , pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR
  ) where

import Data.String
  ( IsString
  )
import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word32
  , Word8
  )
import Foreign.Ptr
  ( Ptr
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Buffer
  ( VkBufferUsageFlags
  , VkBufferCreateFlags
  )
import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VK_UUID_SIZE
  , VkPhysicalDevice
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT
  , pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT
  , pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO
  , pattern VK_LUID_SIZE
  , VK_LUID_SIZE
  , VkPhysicalDeviceIDProperties(..)
  , VkExternalImageFormatProperties(..)
  , VkPhysicalDeviceExternalImageFormatInfo(..)
  , VkExternalMemoryProperties(..)
  , VkExternalMemoryFeatureFlags
  , VkExternalMemoryHandleTypeFlags
  , VkExternalMemoryFeatureFlagBits(..)
  , VkExternalMemoryHandleTypeFlagBits(..)
  , VkExternalBufferProperties(..)
  , VkPhysicalDeviceExternalBufferInfo(..)
  , vkGetPhysicalDeviceExternalBufferProperties
  )


pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = 1
pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME = "VK_KHR_external_memory_capabilities"
vkGetPhysicalDeviceExternalBufferPropertiesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalBufferInfo" ::: Ptr VkPhysicalDeviceExternalBufferInfo) -> ("pExternalBufferProperties" ::: Ptr VkExternalBufferProperties) -> IO ()
vkGetPhysicalDeviceExternalBufferPropertiesKHR = vkGetPhysicalDeviceExternalBufferProperties
type VkExternalMemoryHandleTypeFlagBitsKHR = VkExternalMemoryHandleTypeFlagBits
type VkExternalMemoryFeatureFlagBitsKHR = VkExternalMemoryFeatureFlagBits
type VkExternalMemoryHandleTypeFlagsKHR = VkExternalMemoryHandleTypeFlags
type VkExternalMemoryFeatureFlagsKHR = VkExternalMemoryFeatureFlags
type VkExternalMemoryPropertiesKHR = VkExternalMemoryProperties


pattern VkExternalMemoryPropertiesKHR :: ("externalMemoryFeatures" ::: VkExternalMemoryFeatureFlags) -> ("exportFromImportedHandleTypes" ::: VkExternalMemoryHandleTypeFlags) -> ("compatibleHandleTypes" ::: VkExternalMemoryHandleTypeFlags) -> VkExternalMemoryPropertiesKHR
pattern VkExternalMemoryPropertiesKHR vkExternalMemoryFeatures vkExportFromImportedHandleTypes vkCompatibleHandleTypes = VkExternalMemoryProperties vkExternalMemoryFeatures vkExportFromImportedHandleTypes vkCompatibleHandleTypes
type VkPhysicalDeviceExternalImageFormatInfoKHR = VkPhysicalDeviceExternalImageFormatInfo


pattern VkPhysicalDeviceExternalImageFormatInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> VkPhysicalDeviceExternalImageFormatInfoKHR
pattern VkPhysicalDeviceExternalImageFormatInfoKHR vkSType vkNext vkHandleType = VkPhysicalDeviceExternalImageFormatInfo vkSType vkNext vkHandleType
type VkExternalImageFormatPropertiesKHR = VkExternalImageFormatProperties


pattern VkExternalImageFormatPropertiesKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("externalMemoryProperties" ::: VkExternalMemoryProperties) -> VkExternalImageFormatPropertiesKHR
pattern VkExternalImageFormatPropertiesKHR vkSType vkNext vkExternalMemoryProperties = VkExternalImageFormatProperties vkSType vkNext vkExternalMemoryProperties
type VkPhysicalDeviceExternalBufferInfoKHR = VkPhysicalDeviceExternalBufferInfo


pattern VkPhysicalDeviceExternalBufferInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("flags" ::: VkBufferCreateFlags) -> ("usage" ::: VkBufferUsageFlags) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> VkPhysicalDeviceExternalBufferInfoKHR
pattern VkPhysicalDeviceExternalBufferInfoKHR vkSType vkNext vkFlags vkUsage vkHandleType = VkPhysicalDeviceExternalBufferInfo vkSType vkNext vkFlags vkUsage vkHandleType
type VkExternalBufferPropertiesKHR = VkExternalBufferProperties


pattern VkExternalBufferPropertiesKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("externalMemoryProperties" ::: VkExternalMemoryProperties) -> VkExternalBufferPropertiesKHR
pattern VkExternalBufferPropertiesKHR vkSType vkNext vkExternalMemoryProperties = VkExternalBufferProperties vkSType vkNext vkExternalMemoryProperties
type VkPhysicalDeviceIDPropertiesKHR = VkPhysicalDeviceIDProperties


pattern VkPhysicalDeviceIDPropertiesKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("deviceUUID" ::: Vector VK_UUID_SIZE Word8) -> ("driverUUID" ::: Vector VK_UUID_SIZE Word8) -> ("deviceLUID" ::: Vector VK_LUID_SIZE Word8) -> ("deviceNodeMask" ::: Word32) -> ("deviceLUIDValid" ::: VkBool32) -> VkPhysicalDeviceIDPropertiesKHR
pattern VkPhysicalDeviceIDPropertiesKHR vkSType vkNext vkDeviceUUID vkDriverUUID vkDeviceLUID vkDeviceNodeMask vkDeviceLUIDValid = VkPhysicalDeviceIDProperties vkSType vkNext vkDeviceUUID vkDriverUUID vkDeviceLUID vkDeviceNodeMask vkDeviceLUIDValid
pattern VK_LUID_SIZE_KHR :: Integral a => a
pattern VK_LUID_SIZE_KHR = VK_LUID_SIZE


type VK_LUID_SIZE_KHR = VK_LUID_SIZE
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO
pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR = VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO
pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR = VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR :: VkExternalMemoryFeatureFlagBits
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR = VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR :: VkExternalMemoryFeatureFlagBits
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR = VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR :: VkExternalMemoryFeatureFlagBits
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR = VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT
