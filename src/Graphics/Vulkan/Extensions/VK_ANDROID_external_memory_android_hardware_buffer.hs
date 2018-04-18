{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( AHardwareBuffer
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID
  , pattern VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID
  , pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION
  , pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
  , vkGetAndroidHardwareBufferPropertiesANDROID
  , vkGetMemoryAndroidHardwareBufferANDROID
  , VkImportAndroidHardwareBufferInfoANDROID(..)
  , VkAndroidHardwareBufferUsageANDROID(..)
  , VkAndroidHardwareBufferPropertiesANDROID(..)
  , VkMemoryGetAndroidHardwareBufferInfoANDROID(..)
  , VkAndroidHardwareBufferFormatPropertiesANDROID(..)
  , VkExternalFormatANDROID(..)
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word64
  , Word32
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkFormatFeatureFlags
  , VkDeviceSize
  , VkDevice
  )
import Graphics.Vulkan.Core10.ImageView
  ( VkComponentMapping(..)
  )
import Graphics.Vulkan.Core10.Memory
  ( VkDeviceMemory
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBits(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( VkChromaLocation(..)
  , VkSamplerYcbcrRange(..)
  , VkSamplerYcbcrModelConversion(..)
  )


data AHardwareBuffer
-- | Nothing
pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID :: VkStructureType
pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID = VkStructureType 1000129000
-- | Nothing
pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID :: VkStructureType
pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID = VkStructureType 1000129001
-- | Nothing
pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID :: VkStructureType
pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID = VkStructureType 1000129002
-- | Nothing
pattern VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID = VkStructureType 1000129003
-- | Nothing
pattern VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID = VkStructureType 1000129004
-- | Nothing
pattern VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID = VkStructureType 1000129005
-- | Nothing
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID = VkExternalMemoryHandleTypeFlagBits 0x00000400
pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION :: Integral a => a
pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION = 3
pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME = "VK_ANDROID_external_memory_android_hardware_buffer"
-- | 
foreign import ccall "vkGetAndroidHardwareBufferPropertiesANDROID" vkGetAndroidHardwareBufferPropertiesANDROID :: ("device" ::: VkDevice) -> ("buffer" ::: Ptr AHardwareBuffer) -> ("pProperties" ::: Ptr VkAndroidHardwareBufferPropertiesANDROID) -> IO VkResult
-- | 
foreign import ccall "vkGetMemoryAndroidHardwareBufferANDROID" vkGetMemoryAndroidHardwareBufferANDROID :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkMemoryGetAndroidHardwareBufferInfoANDROID) -> ("pBuffer" ::: Ptr (Ptr AHardwareBuffer)) -> IO VkResult
-- | TODO: Struct comments
data VkImportAndroidHardwareBufferInfoANDROID = VkImportAndroidHardwareBufferInfoANDROID
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkBuffer :: Ptr AHardwareBuffer
  }
  deriving (Eq, Show)

instance Storable VkImportAndroidHardwareBufferInfoANDROID where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImportAndroidHardwareBufferInfoANDROID <$> peek (ptr `plusPtr` 0)
                                                      <*> peek (ptr `plusPtr` 8)
                                                      <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportAndroidHardwareBufferInfoANDROID))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportAndroidHardwareBufferInfoANDROID))
                *> poke (ptr `plusPtr` 16) (vkBuffer (poked :: VkImportAndroidHardwareBufferInfoANDROID))
-- | TODO: Struct comments
data VkAndroidHardwareBufferUsageANDROID = VkAndroidHardwareBufferUsageANDROID
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkAndroidHardwareBufferUsage :: Word64
  }
  deriving (Eq, Show)

instance Storable VkAndroidHardwareBufferUsageANDROID where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkAndroidHardwareBufferUsageANDROID <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkAndroidHardwareBufferUsageANDROID))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkAndroidHardwareBufferUsageANDROID))
                *> poke (ptr `plusPtr` 16) (vkAndroidHardwareBufferUsage (poked :: VkAndroidHardwareBufferUsageANDROID))
-- | TODO: Struct comments
data VkAndroidHardwareBufferPropertiesANDROID = VkAndroidHardwareBufferPropertiesANDROID
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkAllocationSize :: VkDeviceSize
  , vkMemoryTypeBits :: Word32
  }
  deriving (Eq, Show)

instance Storable VkAndroidHardwareBufferPropertiesANDROID where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkAndroidHardwareBufferPropertiesANDROID <$> peek (ptr `plusPtr` 0)
                                                      <*> peek (ptr `plusPtr` 8)
                                                      <*> peek (ptr `plusPtr` 16)
                                                      <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkAndroidHardwareBufferPropertiesANDROID))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkAndroidHardwareBufferPropertiesANDROID))
                *> poke (ptr `plusPtr` 16) (vkAllocationSize (poked :: VkAndroidHardwareBufferPropertiesANDROID))
                *> poke (ptr `plusPtr` 24) (vkMemoryTypeBits (poked :: VkAndroidHardwareBufferPropertiesANDROID))
-- | TODO: Struct comments
data VkMemoryGetAndroidHardwareBufferInfoANDROID = VkMemoryGetAndroidHardwareBufferInfoANDROID
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkMemory :: VkDeviceMemory
  }
  deriving (Eq, Show)

instance Storable VkMemoryGetAndroidHardwareBufferInfoANDROID where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryGetAndroidHardwareBufferInfoANDROID <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryGetAndroidHardwareBufferInfoANDROID))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryGetAndroidHardwareBufferInfoANDROID))
                *> poke (ptr `plusPtr` 16) (vkMemory (poked :: VkMemoryGetAndroidHardwareBufferInfoANDROID))
-- | TODO: Struct comments
data VkAndroidHardwareBufferFormatPropertiesANDROID = VkAndroidHardwareBufferFormatPropertiesANDROID
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkFormat :: VkFormat
  , vkExternalFormat :: Word64
  , vkFormatFeatures :: VkFormatFeatureFlags
  , vkSamplerYcbcrConversionComponents :: VkComponentMapping
  , vkSuggestedYcbcrModel :: VkSamplerYcbcrModelConversion
  , vkSuggestedYcbcrRange :: VkSamplerYcbcrRange
  , vkSuggestedXChromaOffset :: VkChromaLocation
  , vkSuggestedYChromaOffset :: VkChromaLocation
  }
  deriving (Eq, Show)

instance Storable VkAndroidHardwareBufferFormatPropertiesANDROID where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkAndroidHardwareBufferFormatPropertiesANDROID <$> peek (ptr `plusPtr` 0)
                                                            <*> peek (ptr `plusPtr` 8)
                                                            <*> peek (ptr `plusPtr` 16)
                                                            <*> peek (ptr `plusPtr` 24)
                                                            <*> peek (ptr `plusPtr` 32)
                                                            <*> peek (ptr `plusPtr` 36)
                                                            <*> peek (ptr `plusPtr` 52)
                                                            <*> peek (ptr `plusPtr` 56)
                                                            <*> peek (ptr `plusPtr` 60)
                                                            <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                *> poke (ptr `plusPtr` 16) (vkFormat (poked :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                *> poke (ptr `plusPtr` 24) (vkExternalFormat (poked :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                *> poke (ptr `plusPtr` 32) (vkFormatFeatures (poked :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                *> poke (ptr `plusPtr` 36) (vkSamplerYcbcrConversionComponents (poked :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                *> poke (ptr `plusPtr` 52) (vkSuggestedYcbcrModel (poked :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                *> poke (ptr `plusPtr` 56) (vkSuggestedYcbcrRange (poked :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                *> poke (ptr `plusPtr` 60) (vkSuggestedXChromaOffset (poked :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                *> poke (ptr `plusPtr` 64) (vkSuggestedYChromaOffset (poked :: VkAndroidHardwareBufferFormatPropertiesANDROID))
-- | TODO: Struct comments
data VkExternalFormatANDROID = VkExternalFormatANDROID
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkExternalFormat :: Word64
  }
  deriving (Eq, Show)

instance Storable VkExternalFormatANDROID where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExternalFormatANDROID <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalFormatANDROID))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalFormatANDROID))
                *> poke (ptr `plusPtr` 16) (vkExternalFormat (poked :: VkExternalFormatANDROID))
