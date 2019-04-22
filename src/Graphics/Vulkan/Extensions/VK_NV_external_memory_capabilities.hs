{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities
  ( withCStructExternalImageFormatPropertiesNV
  , fromCStructExternalImageFormatPropertiesNV
  , ExternalImageFormatPropertiesNV(..)
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
  , getPhysicalDeviceExternalImageFormatPropertiesNV
  , pattern NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
  , pattern NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.String
  ( IsString
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalImageFormatPropertiesNV(..)
  , VkExternalMemoryFeatureFlagBitsNV(..)
  , VkExternalMemoryHandleTypeFlagBitsNV(..)
  , vkGetPhysicalDeviceExternalImageFormatPropertiesNV
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
import Graphics.Vulkan.Core10.Core
  ( Format
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( ImageFormatProperties(..)
  , PhysicalDevice(..)
  , ImageCreateFlags
  , ImageTiling
  , ImageType
  , ImageUsageFlags
  , fromCStructImageFormatProperties
  , withCStructImageFormatProperties
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )



-- | VkExternalImageFormatPropertiesNV - Structure specifying external image
-- format properties
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryFeatureFlagsNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagsNV',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV'
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

-- | A function to temporarily allocate memory for a 'VkExternalImageFormatPropertiesNV' and
-- marshal a 'ExternalImageFormatPropertiesNV' into it. The 'VkExternalImageFormatPropertiesNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructExternalImageFormatPropertiesNV :: ExternalImageFormatPropertiesNV -> (VkExternalImageFormatPropertiesNV -> IO a) -> IO a
withCStructExternalImageFormatPropertiesNV marshalled cont = withCStructImageFormatProperties (imageFormatProperties (marshalled :: ExternalImageFormatPropertiesNV)) (\imageFormatProperties'' -> cont (VkExternalImageFormatPropertiesNV imageFormatProperties'' (externalMemoryFeatures (marshalled :: ExternalImageFormatPropertiesNV)) (exportFromImportedHandleTypes (marshalled :: ExternalImageFormatPropertiesNV)) (compatibleHandleTypes (marshalled :: ExternalImageFormatPropertiesNV))))

-- | A function to read a 'VkExternalImageFormatPropertiesNV' and all additional
-- structures in the pointer chain into a 'ExternalImageFormatPropertiesNV'.
fromCStructExternalImageFormatPropertiesNV :: VkExternalImageFormatPropertiesNV -> IO ExternalImageFormatPropertiesNV
fromCStructExternalImageFormatPropertiesNV c = ExternalImageFormatPropertiesNV <$> (fromCStructImageFormatProperties (vkImageFormatProperties (c :: VkExternalImageFormatPropertiesNV)))
                                                                               <*> pure (vkExternalMemoryFeatures (c :: VkExternalImageFormatPropertiesNV))
                                                                               <*> pure (vkExportFromImportedHandleTypes (c :: VkExternalImageFormatPropertiesNV))
                                                                               <*> pure (vkCompatibleHandleTypes (c :: VkExternalImageFormatPropertiesNV))

instance Zero ExternalImageFormatPropertiesNV where
  zero = ExternalImageFormatPropertiesNV zero
                                         zero
                                         zero
                                         zero


-- | VkExternalMemoryFeatureFlagBitsNV - Bitmask specifying external memory
-- features
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalImageFormatPropertiesNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryFeatureFlagsNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV'
type ExternalMemoryFeatureFlagBitsNV = VkExternalMemoryFeatureFlagBitsNV


{-# complete EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV, EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV, EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV :: ExternalMemoryFeatureFlagBitsNV #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV'
-- specifies that external memory of the specified type /must/ be created
-- as a dedicated allocation when used in the manner specified.
pattern EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV :: (a ~ ExternalMemoryFeatureFlagBitsNV) => a
pattern EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV = VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV'
-- specifies that the implementation supports exporting handles of the
-- specified type.
pattern EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV :: (a ~ ExternalMemoryFeatureFlagBitsNV) => a
pattern EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV = VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV'
-- specifies that the implementation supports importing handles of the
-- specified type.
pattern EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV :: (a ~ ExternalMemoryFeatureFlagBitsNV) => a
pattern EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV = VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV

-- | VkExternalMemoryFeatureFlagsNV - Bitmask of
-- VkExternalMemoryFeatureFlagBitsNV
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryFeatureFlagsNV'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryFeatureFlagBitsNV'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalImageFormatPropertiesNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryFeatureFlagBitsNV'
type ExternalMemoryFeatureFlagsNV = ExternalMemoryFeatureFlagBitsNV

-- | VkExternalMemoryHandleTypeFlagBitsNV - Bitmask specifying external
-- memory handle types
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagsNV'
type ExternalMemoryHandleTypeFlagBitsNV = VkExternalMemoryHandleTypeFlagBitsNV


{-# complete EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV, EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV, EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV, EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV :: ExternalMemoryHandleTypeFlagBitsNV #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV'
-- specifies a handle to memory returned by
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32.vkGetMemoryWin32HandleNV',
-- or one duplicated from such a handle using @DuplicateHandle()@.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV :: (a ~ ExternalMemoryHandleTypeFlagBitsNV) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV'
-- specifies a handle to memory returned by
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32.vkGetMemoryWin32HandleNV'.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV :: (a ~ ExternalMemoryHandleTypeFlagBitsNV) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV'
-- specifies a valid NT handle to memory returned by
-- @IDXGIResource1::CreateSharedHandle@, or a handle duplicated from such a
-- handle using @DuplicateHandle()@.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV :: (a ~ ExternalMemoryHandleTypeFlagBitsNV) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV'
-- specifies a handle to memory returned by
-- @IDXGIResource::GetSharedHandle()@.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV :: (a ~ ExternalMemoryHandleTypeFlagBitsNV) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV

-- | VkExternalMemoryHandleTypeFlagsNV - Bitmask of
-- VkExternalMemoryHandleTypeFlagBitsNV
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagsNV'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBitsNV'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory.VkExportMemoryAllocateInfoNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalImageFormatPropertiesNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBitsNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory.VkExternalMemoryImageCreateInfoNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32.VkImportMemoryWin32HandleInfoNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32.vkGetMemoryWin32HandleNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV'
type ExternalMemoryHandleTypeFlagsNV = ExternalMemoryHandleTypeFlagBitsNV


-- | vkGetPhysicalDeviceExternalImageFormatPropertiesNV - determine image
-- capabilities compatible with external memory handle types
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     image capabilities
--
-- -   @format@ is the image format, corresponding to
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@format@.
--
-- -   @type@ is the image type, corresponding to
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@imageType@.
--
-- -   @tiling@ is the image tiling, corresponding to
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@tiling@.
--
-- -   @usage@ is the intended usage of the image, corresponding to
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@usage@.
--
-- -   @flags@ is a bitmask describing additional parameters of the image,
--     corresponding to
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@flags@.
--
-- -   @externalHandleType@ is either one of the bits from
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBitsNV',
--     or 0.
--
-- -   @pExternalImageFormatProperties@ points to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalImageFormatPropertiesNV'
--     structure in which capabilities are returned.
--
-- = Description
--
-- If @externalHandleType@ is 0,
-- @pExternalImageFormatProperties@::imageFormatProperties will return the
-- same values as a call to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties',
-- and the other members of @pExternalImageFormatProperties@ will all be 0.
-- Otherwise, they are filled in as described for
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalImageFormatPropertiesNV'.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_FORMAT_NOT_SUPPORTED'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalImageFormatPropertiesNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagsNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageCreateFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageTiling',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
getPhysicalDeviceExternalImageFormatPropertiesNV :: PhysicalDevice ->  Format ->  ImageType ->  ImageTiling ->  ImageUsageFlags ->  ImageCreateFlags ->  ExternalMemoryHandleTypeFlagsNV ->  IO (ExternalImageFormatPropertiesNV)
getPhysicalDeviceExternalImageFormatPropertiesNV = \(PhysicalDevice physicalDevice' commandTable) -> \format' -> \type' -> \tiling' -> \usage' -> \flags' -> \externalHandleType' -> alloca (\pExternalImageFormatProperties' -> vkGetPhysicalDeviceExternalImageFormatPropertiesNV commandTable physicalDevice' format' type' tiling' usage' flags' externalHandleType' pExternalImageFormatProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((fromCStructExternalImageFormatPropertiesNV <=< peek) pExternalImageFormatProperties')))

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME"
pattern NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME = VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION"
pattern NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION :: Integral a => a
pattern NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION
