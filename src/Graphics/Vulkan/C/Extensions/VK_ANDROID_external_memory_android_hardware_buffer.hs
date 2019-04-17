{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( AHardwareBuffer
  , VkAndroidHardwareBufferFormatPropertiesANDROID(..)
  , VkAndroidHardwareBufferPropertiesANDROID(..)
  , VkAndroidHardwareBufferUsageANDROID(..)
  , VkExternalFormatANDROID(..)
  , VkImportAndroidHardwareBufferInfoANDROID(..)
  , VkMemoryGetAndroidHardwareBufferInfoANDROID(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetAndroidHardwareBufferPropertiesANDROID
#endif
  , FN_vkGetAndroidHardwareBufferPropertiesANDROID
  , PFN_vkGetAndroidHardwareBufferPropertiesANDROID
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetMemoryAndroidHardwareBufferANDROID
#endif
  , FN_vkGetMemoryAndroidHardwareBufferANDROID
  , PFN_vkGetMemoryAndroidHardwareBufferANDROID
  , pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
  , pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID
  , pattern VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  , Word64
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


import Graphics.Vulkan.C.Core10.Core
  ( VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  , VkDeviceSize
  , VkFormatFeatureFlags
  )
import Graphics.Vulkan.C.Core10.ImageView
  ( VkComponentMapping(..)
  )
import Graphics.Vulkan.C.Core10.Memory
  ( VkDeviceMemory
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBits(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( VkChromaLocation(..)
  , VkSamplerYcbcrModelConversion(..)
  , VkSamplerYcbcrRange(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | Opaque data
data AHardwareBuffer
-- No documentation found for TopLevel "VkAndroidHardwareBufferFormatPropertiesANDROID"
data VkAndroidHardwareBufferFormatPropertiesANDROID = VkAndroidHardwareBufferFormatPropertiesANDROID
  { -- No documentation found for Nested "VkAndroidHardwareBufferFormatPropertiesANDROID" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkAndroidHardwareBufferFormatPropertiesANDROID" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkAndroidHardwareBufferFormatPropertiesANDROID" "format"
  vkFormat :: VkFormat
  , -- No documentation found for Nested "VkAndroidHardwareBufferFormatPropertiesANDROID" "externalFormat"
  vkExternalFormat :: Word64
  , -- No documentation found for Nested "VkAndroidHardwareBufferFormatPropertiesANDROID" "formatFeatures"
  vkFormatFeatures :: VkFormatFeatureFlags
  , -- No documentation found for Nested "VkAndroidHardwareBufferFormatPropertiesANDROID" "samplerYcbcrConversionComponents"
  vkSamplerYcbcrConversionComponents :: VkComponentMapping
  , -- No documentation found for Nested "VkAndroidHardwareBufferFormatPropertiesANDROID" "suggestedYcbcrModel"
  vkSuggestedYcbcrModel :: VkSamplerYcbcrModelConversion
  , -- No documentation found for Nested "VkAndroidHardwareBufferFormatPropertiesANDROID" "suggestedYcbcrRange"
  vkSuggestedYcbcrRange :: VkSamplerYcbcrRange
  , -- No documentation found for Nested "VkAndroidHardwareBufferFormatPropertiesANDROID" "suggestedXChromaOffset"
  vkSuggestedXChromaOffset :: VkChromaLocation
  , -- No documentation found for Nested "VkAndroidHardwareBufferFormatPropertiesANDROID" "suggestedYChromaOffset"
  vkSuggestedYChromaOffset :: VkChromaLocation
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

instance Zero VkAndroidHardwareBufferFormatPropertiesANDROID where
  zero = VkAndroidHardwareBufferFormatPropertiesANDROID zero
                                                        zero
                                                        zero
                                                        zero
                                                        zero
                                                        zero
                                                        zero
                                                        zero
                                                        zero
                                                        zero
-- No documentation found for TopLevel "VkAndroidHardwareBufferPropertiesANDROID"
data VkAndroidHardwareBufferPropertiesANDROID = VkAndroidHardwareBufferPropertiesANDROID
  { -- No documentation found for Nested "VkAndroidHardwareBufferPropertiesANDROID" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkAndroidHardwareBufferPropertiesANDROID" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkAndroidHardwareBufferPropertiesANDROID" "allocationSize"
  vkAllocationSize :: VkDeviceSize
  , -- No documentation found for Nested "VkAndroidHardwareBufferPropertiesANDROID" "memoryTypeBits"
  vkMemoryTypeBits :: Word32
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

instance Zero VkAndroidHardwareBufferPropertiesANDROID where
  zero = VkAndroidHardwareBufferPropertiesANDROID zero
                                                  zero
                                                  zero
                                                  zero
-- No documentation found for TopLevel "VkAndroidHardwareBufferUsageANDROID"
data VkAndroidHardwareBufferUsageANDROID = VkAndroidHardwareBufferUsageANDROID
  { -- No documentation found for Nested "VkAndroidHardwareBufferUsageANDROID" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkAndroidHardwareBufferUsageANDROID" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkAndroidHardwareBufferUsageANDROID" "androidHardwareBufferUsage"
  vkAndroidHardwareBufferUsage :: Word64
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

instance Zero VkAndroidHardwareBufferUsageANDROID where
  zero = VkAndroidHardwareBufferUsageANDROID zero
                                             zero
                                             zero
-- No documentation found for TopLevel "VkExternalFormatANDROID"
data VkExternalFormatANDROID = VkExternalFormatANDROID
  { -- No documentation found for Nested "VkExternalFormatANDROID" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExternalFormatANDROID" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkExternalFormatANDROID" "externalFormat"
  vkExternalFormat :: Word64
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

instance Zero VkExternalFormatANDROID where
  zero = VkExternalFormatANDROID zero
                                 zero
                                 zero
-- No documentation found for TopLevel "VkImportAndroidHardwareBufferInfoANDROID"
data VkImportAndroidHardwareBufferInfoANDROID = VkImportAndroidHardwareBufferInfoANDROID
  { -- No documentation found for Nested "VkImportAndroidHardwareBufferInfoANDROID" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImportAndroidHardwareBufferInfoANDROID" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImportAndroidHardwareBufferInfoANDROID" "buffer"
  vkBuffer :: Ptr AHardwareBuffer
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

instance Zero VkImportAndroidHardwareBufferInfoANDROID where
  zero = VkImportAndroidHardwareBufferInfoANDROID zero
                                                  zero
                                                  zero
-- No documentation found for TopLevel "VkMemoryGetAndroidHardwareBufferInfoANDROID"
data VkMemoryGetAndroidHardwareBufferInfoANDROID = VkMemoryGetAndroidHardwareBufferInfoANDROID
  { -- No documentation found for Nested "VkMemoryGetAndroidHardwareBufferInfoANDROID" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMemoryGetAndroidHardwareBufferInfoANDROID" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMemoryGetAndroidHardwareBufferInfoANDROID" "memory"
  vkMemory :: VkDeviceMemory
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

instance Zero VkMemoryGetAndroidHardwareBufferInfoANDROID where
  zero = VkMemoryGetAndroidHardwareBufferInfoANDROID zero
                                                     zero
                                                     zero
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetAndroidHardwareBufferPropertiesANDROID"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetAndroidHardwareBufferPropertiesANDROID" vkGetAndroidHardwareBufferPropertiesANDROID :: ("device" ::: VkDevice) -> ("buffer" ::: Ptr AHardwareBuffer) -> ("pProperties" ::: Ptr VkAndroidHardwareBufferPropertiesANDROID) -> IO VkResult

#endif
type FN_vkGetAndroidHardwareBufferPropertiesANDROID = ("device" ::: VkDevice) -> ("buffer" ::: Ptr AHardwareBuffer) -> ("pProperties" ::: Ptr VkAndroidHardwareBufferPropertiesANDROID) -> IO VkResult
type PFN_vkGetAndroidHardwareBufferPropertiesANDROID = FunPtr FN_vkGetAndroidHardwareBufferPropertiesANDROID
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetMemoryAndroidHardwareBufferANDROID"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetMemoryAndroidHardwareBufferANDROID" vkGetMemoryAndroidHardwareBufferANDROID :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkMemoryGetAndroidHardwareBufferInfoANDROID) -> ("pBuffer" ::: Ptr (Ptr AHardwareBuffer)) -> IO VkResult

#endif
type FN_vkGetMemoryAndroidHardwareBufferANDROID = ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkMemoryGetAndroidHardwareBufferInfoANDROID) -> ("pBuffer" ::: Ptr (Ptr AHardwareBuffer)) -> IO VkResult
type PFN_vkGetMemoryAndroidHardwareBufferANDROID = FunPtr FN_vkGetMemoryAndroidHardwareBufferANDROID
-- No documentation found for TopLevel "VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME"
pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME = "VK_ANDROID_external_memory_android_hardware_buffer"
-- No documentation found for TopLevel "VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION"
pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION :: Integral a => a
pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION = 3
-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID = VkExternalMemoryHandleTypeFlagBits 0x00000400
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID"
pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID :: VkStructureType
pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID = VkStructureType 1000129002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID"
pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID :: VkStructureType
pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID = VkStructureType 1000129001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID"
pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID :: VkStructureType
pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID = VkStructureType 1000129000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID"
pattern VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID = VkStructureType 1000129005
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID"
pattern VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID = VkStructureType 1000129003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID"
pattern VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID = VkStructureType 1000129004
