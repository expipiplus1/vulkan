{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities
  ( ExternalImageFormatPropertiesNV(..)
  , ExternalMemoryFeatureFlagBitsNV
  , pattern EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV
  , pattern EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV
  , pattern EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV
  , ExternalMemoryFeatureFlagsNV
  , ExternalMemoryHandleTypeFlagBitsNV
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV
  , ExternalMemoryHandleTypeFlagsNV
#if defined(VK_USE_PLATFORM_GGP)
  , getPhysicalDeviceExternalImageFormatPropertiesNV
#endif
  , pattern NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
  , pattern NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION
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
import Foreign.Marshal.Alloc
  ( alloca
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
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalMemoryFeatureFlagBitsNV(..)
  , VkExternalMemoryHandleTypeFlagBitsNV(..)
  , pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV
  , pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV
  , pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV
  , pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
  , pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities
  ( vkGetPhysicalDeviceExternalImageFormatPropertiesNV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Core
  ( Format
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( ImageFormatProperties(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  , ImageCreateFlags
  , ImageTiling
  , ImageType
  , ImageUsageFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  )
#endif



-- No documentation found for TopLevel "VkExternalImageFormatPropertiesNV"
data ExternalImageFormatPropertiesNV = ExternalImageFormatPropertiesNV
  { -- No documentation found for Nested "ExternalImageFormatPropertiesNV" "imageFormatProperties"
  imageFormatProperties :: ImageFormatProperties
  , -- No documentation found for Nested "ExternalImageFormatPropertiesNV" "externalMemoryFeatures"
  externalMemoryFeatures :: ExternalMemoryFeatureFlagsNV
  , -- No documentation found for Nested "ExternalImageFormatPropertiesNV" "exportFromImportedHandleTypes"
  exportFromImportedHandleTypes :: ExternalMemoryHandleTypeFlagsNV
  , -- No documentation found for Nested "ExternalImageFormatPropertiesNV" "compatibleHandleTypes"
  compatibleHandleTypes :: ExternalMemoryHandleTypeFlagsNV
  }
  deriving (Show, Eq)

instance Zero ExternalImageFormatPropertiesNV where
  zero = ExternalImageFormatPropertiesNV zero
                                         zero
                                         zero
                                         zero


-- No documentation found for TopLevel "ExternalMemoryFeatureFlagBitsNV"
type ExternalMemoryFeatureFlagBitsNV = VkExternalMemoryFeatureFlagBitsNV


{-# complete EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV, EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV, EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV :: ExternalMemoryFeatureFlagBitsNV #-}


-- No documentation found for Nested "ExternalMemoryFeatureFlagBitsNV" "EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV"
pattern EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV :: (a ~ ExternalMemoryFeatureFlagBitsNV) => a
pattern EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV = VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV


-- No documentation found for Nested "ExternalMemoryFeatureFlagBitsNV" "EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV"
pattern EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV :: (a ~ ExternalMemoryFeatureFlagBitsNV) => a
pattern EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV = VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV


-- No documentation found for Nested "ExternalMemoryFeatureFlagBitsNV" "EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV"
pattern EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV :: (a ~ ExternalMemoryFeatureFlagBitsNV) => a
pattern EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV = VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV

-- No documentation found for TopLevel "ExternalMemoryFeatureFlagsNV"
type ExternalMemoryFeatureFlagsNV = ExternalMemoryFeatureFlagBitsNV

-- No documentation found for TopLevel "ExternalMemoryHandleTypeFlagBitsNV"
type ExternalMemoryHandleTypeFlagBitsNV = VkExternalMemoryHandleTypeFlagBitsNV


{-# complete EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV, EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV, EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV, EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV :: ExternalMemoryHandleTypeFlagBitsNV #-}


-- No documentation found for Nested "ExternalMemoryHandleTypeFlagBitsNV" "EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV :: (a ~ ExternalMemoryHandleTypeFlagBitsNV) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV


-- No documentation found for Nested "ExternalMemoryHandleTypeFlagBitsNV" "EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV :: (a ~ ExternalMemoryHandleTypeFlagBitsNV) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV


-- No documentation found for Nested "ExternalMemoryHandleTypeFlagBitsNV" "EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV :: (a ~ ExternalMemoryHandleTypeFlagBitsNV) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV


-- No documentation found for Nested "ExternalMemoryHandleTypeFlagBitsNV" "EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV :: (a ~ ExternalMemoryHandleTypeFlagBitsNV) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV

-- No documentation found for TopLevel "ExternalMemoryHandleTypeFlagsNV"
type ExternalMemoryHandleTypeFlagsNV = ExternalMemoryHandleTypeFlagBitsNV


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceExternalImageFormatPropertiesNV"
getPhysicalDeviceExternalImageFormatPropertiesNV :: PhysicalDevice ->  Format ->  ImageType ->  ImageTiling ->  ImageUsageFlags ->  ImageCreateFlags ->  ExternalMemoryHandleTypeFlagsNV ->  IO (ExternalImageFormatPropertiesNV)
getPhysicalDeviceExternalImageFormatPropertiesNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME"
pattern NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME = VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION"
pattern NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION :: Integral a => a
pattern NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION
