{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( withCStructAndroidHardwareBufferFormatPropertiesANDROID
  , fromCStructAndroidHardwareBufferFormatPropertiesANDROID
  , AndroidHardwareBufferFormatPropertiesANDROID(..)
  , withCStructAndroidHardwareBufferPropertiesANDROID
  , fromCStructAndroidHardwareBufferPropertiesANDROID
  , AndroidHardwareBufferPropertiesANDROID(..)
  , withCStructAndroidHardwareBufferUsageANDROID
  , fromCStructAndroidHardwareBufferUsageANDROID
  , AndroidHardwareBufferUsageANDROID(..)
  , withCStructExternalFormatANDROID
  , fromCStructExternalFormatANDROID
  , ExternalFormatANDROID(..)
  , withCStructImportAndroidHardwareBufferInfoANDROID
  , fromCStructImportAndroidHardwareBufferInfoANDROID
  , ImportAndroidHardwareBufferInfoANDROID(..)
  , withCStructMemoryGetAndroidHardwareBufferInfoANDROID
  , fromCStructMemoryGetAndroidHardwareBufferInfoANDROID
  , MemoryGetAndroidHardwareBufferInfoANDROID(..)
  , getAndroidHardwareBufferPropertiesANDROID
  , getMemoryAndroidHardwareBufferANDROID
  , pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION
  , pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID
  , pattern VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Word
  ( Word32
  , Word64
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( Ptr
  , castPtr
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( getAndroidHardwareBufferPropertiesANDROID
  , getMemoryAndroidHardwareBufferANDROID
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( VkAndroidHardwareBufferFormatPropertiesANDROID(..)
  , VkAndroidHardwareBufferPropertiesANDROID(..)
  , VkAndroidHardwareBufferUsageANDROID(..)
  , VkExternalFormatANDROID(..)
  , VkImportAndroidHardwareBufferInfoANDROID(..)
  , VkMemoryGetAndroidHardwareBufferInfoANDROID(..)
  , AHardwareBuffer
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID
  , pattern VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , DeviceSize
  , FormatFeatureFlags
  )
import Graphics.Vulkan.Core10.ImageView
  ( ComponentMapping(..)
  , fromCStructComponentMapping
  , withCStructComponentMapping
  )
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( ChromaLocation
  , SamplerYcbcrModelConversion
  , SamplerYcbcrRange
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
  , pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID
  )


-- No documentation found for TopLevel "AndroidHardwareBufferFormatPropertiesANDROID"
data AndroidHardwareBufferFormatPropertiesANDROID = AndroidHardwareBufferFormatPropertiesANDROID
  { -- Univalued Member elided
  -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "format"
  vkFormat :: Format
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "externalFormat"
  vkExternalFormat :: Word64
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "formatFeatures"
  vkFormatFeatures :: FormatFeatureFlags
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "samplerYcbcrConversionComponents"
  vkSamplerYcbcrConversionComponents :: ComponentMapping
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "suggestedYcbcrModel"
  vkSuggestedYcbcrModel :: SamplerYcbcrModelConversion
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "suggestedYcbcrRange"
  vkSuggestedYcbcrRange :: SamplerYcbcrRange
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "suggestedXChromaOffset"
  vkSuggestedXChromaOffset :: ChromaLocation
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "suggestedYChromaOffset"
  vkSuggestedYChromaOffset :: ChromaLocation
  }
  deriving (Show, Eq)
withCStructAndroidHardwareBufferFormatPropertiesANDROID :: AndroidHardwareBufferFormatPropertiesANDROID -> (VkAndroidHardwareBufferFormatPropertiesANDROID -> IO a) -> IO a
withCStructAndroidHardwareBufferFormatPropertiesANDROID from cont = withCStructComponentMapping (vkSamplerYcbcrConversionComponents (from :: AndroidHardwareBufferFormatPropertiesANDROID)) (\samplerYcbcrConversionComponents -> maybeWith withSomeVkStruct (vkPNext (from :: AndroidHardwareBufferFormatPropertiesANDROID)) (\pPNext -> cont (VkAndroidHardwareBufferFormatPropertiesANDROID VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID pPNext (vkFormat (from :: AndroidHardwareBufferFormatPropertiesANDROID)) (vkExternalFormat (from :: AndroidHardwareBufferFormatPropertiesANDROID)) (vkFormatFeatures (from :: AndroidHardwareBufferFormatPropertiesANDROID)) samplerYcbcrConversionComponents (vkSuggestedYcbcrModel (from :: AndroidHardwareBufferFormatPropertiesANDROID)) (vkSuggestedYcbcrRange (from :: AndroidHardwareBufferFormatPropertiesANDROID)) (vkSuggestedXChromaOffset (from :: AndroidHardwareBufferFormatPropertiesANDROID)) (vkSuggestedYChromaOffset (from :: AndroidHardwareBufferFormatPropertiesANDROID)))))
fromCStructAndroidHardwareBufferFormatPropertiesANDROID :: VkAndroidHardwareBufferFormatPropertiesANDROID -> IO AndroidHardwareBufferFormatPropertiesANDROID
fromCStructAndroidHardwareBufferFormatPropertiesANDROID c = AndroidHardwareBufferFormatPropertiesANDROID <$> -- Univalued Member elided
                                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkAndroidHardwareBufferFormatPropertiesANDROID)))
                                                                                                         <*> pure (vkFormat (c :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                                                                                                         <*> pure (vkExternalFormat (c :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                                                                                                         <*> pure (vkFormatFeatures (c :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                                                                                                         <*> (fromCStructComponentMapping (vkSamplerYcbcrConversionComponents (c :: VkAndroidHardwareBufferFormatPropertiesANDROID)))
                                                                                                         <*> pure (vkSuggestedYcbcrModel (c :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                                                                                                         <*> pure (vkSuggestedYcbcrRange (c :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                                                                                                         <*> pure (vkSuggestedXChromaOffset (c :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                                                                                                         <*> pure (vkSuggestedYChromaOffset (c :: VkAndroidHardwareBufferFormatPropertiesANDROID))
-- No documentation found for TopLevel "AndroidHardwareBufferPropertiesANDROID"
data AndroidHardwareBufferPropertiesANDROID = AndroidHardwareBufferPropertiesANDROID
  { -- Univalued Member elided
  -- No documentation found for Nested "AndroidHardwareBufferPropertiesANDROID" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AndroidHardwareBufferPropertiesANDROID" "allocationSize"
  vkAllocationSize :: DeviceSize
  , -- No documentation found for Nested "AndroidHardwareBufferPropertiesANDROID" "memoryTypeBits"
  vkMemoryTypeBits :: Word32
  }
  deriving (Show, Eq)
withCStructAndroidHardwareBufferPropertiesANDROID :: AndroidHardwareBufferPropertiesANDROID -> (VkAndroidHardwareBufferPropertiesANDROID -> IO a) -> IO a
withCStructAndroidHardwareBufferPropertiesANDROID from cont = maybeWith withSomeVkStruct (vkPNext (from :: AndroidHardwareBufferPropertiesANDROID)) (\pPNext -> cont (VkAndroidHardwareBufferPropertiesANDROID VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID pPNext (vkAllocationSize (from :: AndroidHardwareBufferPropertiesANDROID)) (vkMemoryTypeBits (from :: AndroidHardwareBufferPropertiesANDROID))))
fromCStructAndroidHardwareBufferPropertiesANDROID :: VkAndroidHardwareBufferPropertiesANDROID -> IO AndroidHardwareBufferPropertiesANDROID
fromCStructAndroidHardwareBufferPropertiesANDROID c = AndroidHardwareBufferPropertiesANDROID <$> -- Univalued Member elided
                                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkAndroidHardwareBufferPropertiesANDROID)))
                                                                                             <*> pure (vkAllocationSize (c :: VkAndroidHardwareBufferPropertiesANDROID))
                                                                                             <*> pure (vkMemoryTypeBits (c :: VkAndroidHardwareBufferPropertiesANDROID))
-- No documentation found for TopLevel "AndroidHardwareBufferUsageANDROID"
data AndroidHardwareBufferUsageANDROID = AndroidHardwareBufferUsageANDROID
  { -- Univalued Member elided
  -- No documentation found for Nested "AndroidHardwareBufferUsageANDROID" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AndroidHardwareBufferUsageANDROID" "androidHardwareBufferUsage"
  vkAndroidHardwareBufferUsage :: Word64
  }
  deriving (Show, Eq)
withCStructAndroidHardwareBufferUsageANDROID :: AndroidHardwareBufferUsageANDROID -> (VkAndroidHardwareBufferUsageANDROID -> IO a) -> IO a
withCStructAndroidHardwareBufferUsageANDROID from cont = maybeWith withSomeVkStruct (vkPNext (from :: AndroidHardwareBufferUsageANDROID)) (\pPNext -> cont (VkAndroidHardwareBufferUsageANDROID VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID pPNext (vkAndroidHardwareBufferUsage (from :: AndroidHardwareBufferUsageANDROID))))
fromCStructAndroidHardwareBufferUsageANDROID :: VkAndroidHardwareBufferUsageANDROID -> IO AndroidHardwareBufferUsageANDROID
fromCStructAndroidHardwareBufferUsageANDROID c = AndroidHardwareBufferUsageANDROID <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkAndroidHardwareBufferUsageANDROID)))
                                                                                   <*> pure (vkAndroidHardwareBufferUsage (c :: VkAndroidHardwareBufferUsageANDROID))
-- No documentation found for TopLevel "ExternalFormatANDROID"
data ExternalFormatANDROID = ExternalFormatANDROID
  { -- Univalued Member elided
  -- No documentation found for Nested "ExternalFormatANDROID" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalFormatANDROID" "externalFormat"
  vkExternalFormat :: Word64
  }
  deriving (Show, Eq)
withCStructExternalFormatANDROID :: ExternalFormatANDROID -> (VkExternalFormatANDROID -> IO a) -> IO a
withCStructExternalFormatANDROID from cont = maybeWith withSomeVkStruct (vkPNext (from :: ExternalFormatANDROID)) (\pPNext -> cont (VkExternalFormatANDROID VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID pPNext (vkExternalFormat (from :: ExternalFormatANDROID))))
fromCStructExternalFormatANDROID :: VkExternalFormatANDROID -> IO ExternalFormatANDROID
fromCStructExternalFormatANDROID c = ExternalFormatANDROID <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExternalFormatANDROID)))
                                                           <*> pure (vkExternalFormat (c :: VkExternalFormatANDROID))
-- No documentation found for TopLevel "ImportAndroidHardwareBufferInfoANDROID"
data ImportAndroidHardwareBufferInfoANDROID = ImportAndroidHardwareBufferInfoANDROID
  { -- Univalued Member elided
  -- No documentation found for Nested "ImportAndroidHardwareBufferInfoANDROID" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportAndroidHardwareBufferInfoANDROID" "buffer"
  vkBuffer :: Ptr AHardwareBuffer
  }
  deriving (Show, Eq)
withCStructImportAndroidHardwareBufferInfoANDROID :: ImportAndroidHardwareBufferInfoANDROID -> (VkImportAndroidHardwareBufferInfoANDROID -> IO a) -> IO a
withCStructImportAndroidHardwareBufferInfoANDROID from cont = maybeWith withSomeVkStruct (vkPNext (from :: ImportAndroidHardwareBufferInfoANDROID)) (\pPNext -> cont (VkImportAndroidHardwareBufferInfoANDROID VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID pPNext (vkBuffer (from :: ImportAndroidHardwareBufferInfoANDROID))))
fromCStructImportAndroidHardwareBufferInfoANDROID :: VkImportAndroidHardwareBufferInfoANDROID -> IO ImportAndroidHardwareBufferInfoANDROID
fromCStructImportAndroidHardwareBufferInfoANDROID c = ImportAndroidHardwareBufferInfoANDROID <$> -- Univalued Member elided
                                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImportAndroidHardwareBufferInfoANDROID)))
                                                                                             <*> pure (vkBuffer (c :: VkImportAndroidHardwareBufferInfoANDROID))
-- No documentation found for TopLevel "MemoryGetAndroidHardwareBufferInfoANDROID"
data MemoryGetAndroidHardwareBufferInfoANDROID = MemoryGetAndroidHardwareBufferInfoANDROID
  { -- Univalued Member elided
  -- No documentation found for Nested "MemoryGetAndroidHardwareBufferInfoANDROID" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryGetAndroidHardwareBufferInfoANDROID" "memory"
  vkMemory :: DeviceMemory
  }
  deriving (Show, Eq)
withCStructMemoryGetAndroidHardwareBufferInfoANDROID :: MemoryGetAndroidHardwareBufferInfoANDROID -> (VkMemoryGetAndroidHardwareBufferInfoANDROID -> IO a) -> IO a
withCStructMemoryGetAndroidHardwareBufferInfoANDROID from cont = maybeWith withSomeVkStruct (vkPNext (from :: MemoryGetAndroidHardwareBufferInfoANDROID)) (\pPNext -> cont (VkMemoryGetAndroidHardwareBufferInfoANDROID VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID pPNext (vkMemory (from :: MemoryGetAndroidHardwareBufferInfoANDROID))))
fromCStructMemoryGetAndroidHardwareBufferInfoANDROID :: VkMemoryGetAndroidHardwareBufferInfoANDROID -> IO MemoryGetAndroidHardwareBufferInfoANDROID
fromCStructMemoryGetAndroidHardwareBufferInfoANDROID c = MemoryGetAndroidHardwareBufferInfoANDROID <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryGetAndroidHardwareBufferInfoANDROID)))
                                                                                                   <*> pure (vkMemory (c :: VkMemoryGetAndroidHardwareBufferInfoANDROID))

-- | Wrapper for 'vkGetAndroidHardwareBufferPropertiesANDROID'
getAndroidHardwareBufferPropertiesANDROID :: Device ->  Ptr AHardwareBuffer ->  IO (AndroidHardwareBufferPropertiesANDROID)
getAndroidHardwareBufferPropertiesANDROID = \(Device device commandTable) -> \buffer -> alloca (\pProperties -> Graphics.Vulkan.C.Dynamic.getAndroidHardwareBufferPropertiesANDROID commandTable device buffer pProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((fromCStructAndroidHardwareBufferPropertiesANDROID <=< peek) pProperties)))

-- | Wrapper for 'vkGetMemoryAndroidHardwareBufferANDROID'
getMemoryAndroidHardwareBufferANDROID :: Device ->  MemoryGetAndroidHardwareBufferInfoANDROID ->  IO ( Ptr AHardwareBuffer )
getMemoryAndroidHardwareBufferANDROID = \(Device device commandTable) -> \info -> alloca (\pBuffer -> (\a -> withCStructMemoryGetAndroidHardwareBufferInfoANDROID a . flip with) info (\pInfo -> Graphics.Vulkan.C.Dynamic.getMemoryAndroidHardwareBufferANDROID commandTable device pInfo pBuffer >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pBuffer))))
