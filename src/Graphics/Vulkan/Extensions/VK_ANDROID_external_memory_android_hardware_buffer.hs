{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  AndroidHardwareBufferFormatPropertiesANDROID(..)
  , 
  AndroidHardwareBufferPropertiesANDROID(..)
  , AndroidHardwareBufferUsageANDROID(..)
  , ExternalFormatANDROID(..)
  , ImportAndroidHardwareBufferInfoANDROID(..)
  , MemoryGetAndroidHardwareBufferInfoANDROID(..)
  , getAndroidHardwareBufferPropertiesANDROID
#endif
  , getMemoryAndroidHardwareBufferANDROID
  , pattern ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
  , pattern ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID
  , pattern STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID
  , pattern STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID
  , pattern STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID
  , pattern STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  , pattern STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  , pattern STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif
import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word64
  )
#endif
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( with
  )
import Foreign.Ptr
  ( Ptr
  )

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( nullPtr
  )
#endif
import Foreign.Storable
  ( peek
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( AHardwareBuffer
  , vkGetMemoryAndroidHardwareBufferANDROID
  , pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
  , pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( vkGetAndroidHardwareBufferPropertiesANDROID
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Core
  ( Format
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( DeviceSize
  , FormatFeatureFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.ImageView
  ( ComponentMapping(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( SamplerYcbcrModelConversion
  , SamplerYcbcrRange
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( ChromaLocation
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
  ( pattern STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID
  , pattern STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID
  , pattern STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID
  , pattern STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID
  , pattern STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  , pattern STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( pattern EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkAndroidHardwareBufferFormatPropertiesANDROID"
data AndroidHardwareBufferFormatPropertiesANDROID = AndroidHardwareBufferFormatPropertiesANDROID
  { -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "format"
  format :: Format
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "externalFormat"
  externalFormat :: Word64
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "formatFeatures"
  formatFeatures :: FormatFeatureFlags
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "samplerYcbcrConversionComponents"
  samplerYcbcrConversionComponents :: ComponentMapping
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "suggestedYcbcrModel"
  suggestedYcbcrModel :: SamplerYcbcrModelConversion
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "suggestedYcbcrRange"
  suggestedYcbcrRange :: SamplerYcbcrRange
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "suggestedXChromaOffset"
  suggestedXChromaOffset :: ChromaLocation
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "suggestedYChromaOffset"
  suggestedYChromaOffset :: ChromaLocation
  }
  deriving (Show, Eq)

instance Zero AndroidHardwareBufferFormatPropertiesANDROID where
  zero = AndroidHardwareBufferFormatPropertiesANDROID Nothing
                                                      zero
                                                      zero
                                                      zero
                                                      zero
                                                      zero
                                                      zero
                                                      zero
                                                      zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkAndroidHardwareBufferPropertiesANDROID"
data AndroidHardwareBufferPropertiesANDROID = AndroidHardwareBufferPropertiesANDROID
  { -- No documentation found for Nested "AndroidHardwareBufferPropertiesANDROID" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AndroidHardwareBufferPropertiesANDROID" "allocationSize"
  allocationSize :: DeviceSize
  , -- No documentation found for Nested "AndroidHardwareBufferPropertiesANDROID" "memoryTypeBits"
  memoryTypeBits :: Word32
  }
  deriving (Show, Eq)

instance Zero AndroidHardwareBufferPropertiesANDROID where
  zero = AndroidHardwareBufferPropertiesANDROID Nothing
                                                zero
                                                zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkAndroidHardwareBufferUsageANDROID"
data AndroidHardwareBufferUsageANDROID = AndroidHardwareBufferUsageANDROID
  { -- No documentation found for Nested "AndroidHardwareBufferUsageANDROID" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AndroidHardwareBufferUsageANDROID" "androidHardwareBufferUsage"
  androidHardwareBufferUsage :: Word64
  }
  deriving (Show, Eq)

instance Zero AndroidHardwareBufferUsageANDROID where
  zero = AndroidHardwareBufferUsageANDROID Nothing
                                           zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkExternalFormatANDROID"
data ExternalFormatANDROID = ExternalFormatANDROID
  { -- No documentation found for Nested "ExternalFormatANDROID" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalFormatANDROID" "externalFormat"
  externalFormat :: Word64
  }
  deriving (Show, Eq)

instance Zero ExternalFormatANDROID where
  zero = ExternalFormatANDROID Nothing
                               zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImportAndroidHardwareBufferInfoANDROID"
data ImportAndroidHardwareBufferInfoANDROID = ImportAndroidHardwareBufferInfoANDROID
  { -- No documentation found for Nested "ImportAndroidHardwareBufferInfoANDROID" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportAndroidHardwareBufferInfoANDROID" "buffer"
  buffer :: Ptr AHardwareBuffer
  }
  deriving (Show, Eq)

instance Zero ImportAndroidHardwareBufferInfoANDROID where
  zero = ImportAndroidHardwareBufferInfoANDROID Nothing
                                                nullPtr

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkMemoryGetAndroidHardwareBufferInfoANDROID"
data MemoryGetAndroidHardwareBufferInfoANDROID = MemoryGetAndroidHardwareBufferInfoANDROID
  { -- No documentation found for Nested "MemoryGetAndroidHardwareBufferInfoANDROID" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryGetAndroidHardwareBufferInfoANDROID" "memory"
  memory :: DeviceMemory
  }
  deriving (Show, Eq)

instance Zero MemoryGetAndroidHardwareBufferInfoANDROID where
  zero = MemoryGetAndroidHardwareBufferInfoANDROID Nothing
                                                   zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetAndroidHardwareBufferPropertiesANDROID"
getAndroidHardwareBufferPropertiesANDROID :: Device ->  Ptr AHardwareBuffer ->  IO (AndroidHardwareBufferPropertiesANDROID)
getAndroidHardwareBufferPropertiesANDROID = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif


-- No documentation found for TopLevel "vkGetMemoryAndroidHardwareBufferANDROID"
getMemoryAndroidHardwareBufferANDROID :: Device ->  MemoryGetAndroidHardwareBufferInfoANDROID ->  IO (Ptr AHardwareBuffer)
getMemoryAndroidHardwareBufferANDROID = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME"
pattern ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME = VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME

-- No documentation found for TopLevel "VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION"
pattern ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION :: Integral a => a
pattern ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION = VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION
