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
  ( VkBufferCreateFlags
  , VkBufferUsageFlags
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
  ( VkExternalBufferProperties(..)
  , VkExternalImageFormatProperties(..)
  , VkExternalMemoryFeatureFlagBits(..)
  , VkExternalMemoryHandleTypeFlagBits(..)
  , VkExternalMemoryProperties(..)
  , VkPhysicalDeviceExternalBufferInfo(..)
  , VkPhysicalDeviceExternalImageFormatInfo(..)
  , VkPhysicalDeviceIDProperties(..)
  , VK_LUID_SIZE
  , VkExternalMemoryFeatureFlags
  , VkExternalMemoryHandleTypeFlags
  , vkGetPhysicalDeviceExternalBufferProperties
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
  , pattern VK_LUID_SIZE
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES
  )


-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME = "VK_KHR_external_memory_capabilities"
-- No documentation found for TopLevel "vkGetPhysicalDeviceExternalBufferPropertiesKHR"
vkGetPhysicalDeviceExternalBufferPropertiesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalBufferInfo" ::: Ptr VkPhysicalDeviceExternalBufferInfo) -> ("pExternalBufferProperties" ::: Ptr VkExternalBufferProperties) -> IO ()
vkGetPhysicalDeviceExternalBufferPropertiesKHR = vkGetPhysicalDeviceExternalBufferProperties
-- No documentation found for TopLevel "VkExternalMemoryHandleTypeFlagBitsKHR"
type VkExternalMemoryHandleTypeFlagBitsKHR = VkExternalMemoryHandleTypeFlagBits
-- No documentation found for TopLevel "VkExternalMemoryFeatureFlagBitsKHR"
type VkExternalMemoryFeatureFlagBitsKHR = VkExternalMemoryFeatureFlagBits
-- No documentation found for TopLevel "VkExternalMemoryHandleTypeFlagsKHR"
type VkExternalMemoryHandleTypeFlagsKHR = VkExternalMemoryHandleTypeFlags
-- No documentation found for TopLevel "VkExternalMemoryFeatureFlagsKHR"
type VkExternalMemoryFeatureFlagsKHR = VkExternalMemoryFeatureFlags
-- No documentation found for TopLevel "VkExternalMemoryPropertiesKHR"
type VkExternalMemoryPropertiesKHR = VkExternalMemoryProperties


-- No documentation found for TopLevel "VkExternalMemoryPropertiesKHR"
pattern VkExternalMemoryPropertiesKHR :: ("externalMemoryFeatures" ::: VkExternalMemoryFeatureFlags) -> ("exportFromImportedHandleTypes" ::: VkExternalMemoryHandleTypeFlags) -> ("compatibleHandleTypes" ::: VkExternalMemoryHandleTypeFlags) -> VkExternalMemoryPropertiesKHR
pattern VkExternalMemoryPropertiesKHR vkExternalMemoryFeatures vkExportFromImportedHandleTypes vkCompatibleHandleTypes = VkExternalMemoryProperties vkExternalMemoryFeatures vkExportFromImportedHandleTypes vkCompatibleHandleTypes
-- No documentation found for TopLevel "VkPhysicalDeviceExternalImageFormatInfoKHR"
type VkPhysicalDeviceExternalImageFormatInfoKHR = VkPhysicalDeviceExternalImageFormatInfo


-- No documentation found for TopLevel "VkPhysicalDeviceExternalImageFormatInfoKHR"
pattern VkPhysicalDeviceExternalImageFormatInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> VkPhysicalDeviceExternalImageFormatInfoKHR
pattern VkPhysicalDeviceExternalImageFormatInfoKHR vkSType vkPNext vkHandleType = VkPhysicalDeviceExternalImageFormatInfo vkSType vkPNext vkHandleType
-- No documentation found for TopLevel "VkExternalImageFormatPropertiesKHR"
type VkExternalImageFormatPropertiesKHR = VkExternalImageFormatProperties


-- No documentation found for TopLevel "VkExternalImageFormatPropertiesKHR"
pattern VkExternalImageFormatPropertiesKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("externalMemoryProperties" ::: VkExternalMemoryProperties) -> VkExternalImageFormatPropertiesKHR
pattern VkExternalImageFormatPropertiesKHR vkSType vkPNext vkExternalMemoryProperties = VkExternalImageFormatProperties vkSType vkPNext vkExternalMemoryProperties
-- No documentation found for TopLevel "VkPhysicalDeviceExternalBufferInfoKHR"
type VkPhysicalDeviceExternalBufferInfoKHR = VkPhysicalDeviceExternalBufferInfo


-- No documentation found for TopLevel "VkPhysicalDeviceExternalBufferInfoKHR"
pattern VkPhysicalDeviceExternalBufferInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("flags" ::: VkBufferCreateFlags) -> ("usage" ::: VkBufferUsageFlags) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> VkPhysicalDeviceExternalBufferInfoKHR
pattern VkPhysicalDeviceExternalBufferInfoKHR vkSType vkPNext vkFlags vkUsage vkHandleType = VkPhysicalDeviceExternalBufferInfo vkSType vkPNext vkFlags vkUsage vkHandleType
-- No documentation found for TopLevel "VkExternalBufferPropertiesKHR"
type VkExternalBufferPropertiesKHR = VkExternalBufferProperties


-- No documentation found for TopLevel "VkExternalBufferPropertiesKHR"
pattern VkExternalBufferPropertiesKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("externalMemoryProperties" ::: VkExternalMemoryProperties) -> VkExternalBufferPropertiesKHR
pattern VkExternalBufferPropertiesKHR vkSType vkPNext vkExternalMemoryProperties = VkExternalBufferProperties vkSType vkPNext vkExternalMemoryProperties
-- No documentation found for TopLevel "VkPhysicalDeviceIDPropertiesKHR"
type VkPhysicalDeviceIDPropertiesKHR = VkPhysicalDeviceIDProperties


-- No documentation found for TopLevel "VkPhysicalDeviceIDPropertiesKHR"
pattern VkPhysicalDeviceIDPropertiesKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("deviceUUID" ::: Vector VK_UUID_SIZE Word8) -> ("driverUUID" ::: Vector VK_UUID_SIZE Word8) -> ("deviceLUID" ::: Vector VK_LUID_SIZE Word8) -> ("deviceNodeMask" ::: Word32) -> ("deviceLUIDValid" ::: VkBool32) -> VkPhysicalDeviceIDPropertiesKHR
pattern VkPhysicalDeviceIDPropertiesKHR vkSType vkPNext vkDeviceUUID vkDriverUUID vkDeviceLUID vkDeviceNodeMask vkDeviceLUIDValid = VkPhysicalDeviceIDProperties vkSType vkPNext vkDeviceUUID vkDriverUUID vkDeviceLUID vkDeviceNodeMask vkDeviceLUIDValid
-- No documentation found for TopLevel "VK_LUID_SIZE_KHR"
pattern VK_LUID_SIZE_KHR :: Integral a => a
pattern VK_LUID_SIZE_KHR = VK_LUID_SIZE


-- No documentation found for TopLevel "VK_LUID_SIZE_KHR"
type VK_LUID_SIZE_KHR = VK_LUID_SIZE
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR"
pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR = VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR"
pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR = VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES
-- No documentation found for TopLevel "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR"
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR :: VkExternalMemoryFeatureFlagBits
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR = VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR"
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR :: VkExternalMemoryFeatureFlagBits
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR = VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR"
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR :: VkExternalMemoryFeatureFlagBits
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR = VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT
