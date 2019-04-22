{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( withCStructExternalBufferProperties
  , fromCStructExternalBufferProperties
  , ExternalBufferProperties(..)
  , withCStructExternalImageFormatProperties
  , fromCStructExternalImageFormatProperties
  , ExternalImageFormatProperties(..)
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
  , withCStructExternalMemoryProperties
  , fromCStructExternalMemoryProperties
  , ExternalMemoryProperties(..)
  , withCStructPhysicalDeviceExternalBufferInfo
  , fromCStructPhysicalDeviceExternalBufferInfo
  , PhysicalDeviceExternalBufferInfo(..)
  , withCStructPhysicalDeviceExternalImageFormatInfo
  , fromCStructPhysicalDeviceExternalImageFormatInfo
  , PhysicalDeviceExternalImageFormatInfo(..)
  , withCStructPhysicalDeviceIDProperties
  , fromCStructPhysicalDeviceIDProperties
  , PhysicalDeviceIDProperties(..)
  , getPhysicalDeviceExternalBufferProperties
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO
  , pattern STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO
  , pattern STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES
  ) where

import Control.Monad
  ( (<=<)
  )
import Data.ByteString
  ( ByteString
  , packCStringLen
  )
import qualified Data.ByteString
  ( empty
  )
import qualified Data.Vector.Storable
  ( unsafeWith
  )
import qualified Data.Vector.Storable.Sized
  ( fromSized
  )
import Data.Word
  ( Word32
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
  ( castPtr
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( pattern VK_UUID_SIZE
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalBufferProperties(..)
  , VkExternalImageFormatProperties(..)
  , VkExternalMemoryFeatureFlagBits(..)
  , VkExternalMemoryHandleTypeFlagBits(..)
  , VkExternalMemoryProperties(..)
  , VkPhysicalDeviceExternalBufferInfo(..)
  , VkPhysicalDeviceExternalImageFormatInfo(..)
  , VkPhysicalDeviceIDProperties(..)
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
import Graphics.Vulkan.Core10.Buffer
  ( BufferCreateFlags
  , BufferUsageFlags
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( byteStringToSizedVector
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES
  , pattern STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES
  )



-- | VkExternalBufferProperties - Structure specifying supported external
-- handle capabilities
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryProperties',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.vkGetPhysicalDeviceExternalBufferProperties',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_capabilities.vkGetPhysicalDeviceExternalBufferPropertiesKHR'
data ExternalBufferProperties = ExternalBufferProperties
  { -- Univalued member elided
  -- No documentation found for Nested "ExternalBufferProperties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalBufferProperties" "externalMemoryProperties"
  externalMemoryProperties :: ExternalMemoryProperties
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkExternalBufferProperties' and
-- marshal a 'ExternalBufferProperties' into it. The 'VkExternalBufferProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructExternalBufferProperties :: ExternalBufferProperties -> (VkExternalBufferProperties -> IO a) -> IO a
withCStructExternalBufferProperties marshalled cont = withCStructExternalMemoryProperties (externalMemoryProperties (marshalled :: ExternalBufferProperties)) (\externalMemoryProperties'' -> maybeWith withSomeVkStruct (next (marshalled :: ExternalBufferProperties)) (\pPNext -> cont (VkExternalBufferProperties VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES pPNext externalMemoryProperties'')))

-- | A function to read a 'VkExternalBufferProperties' and all additional
-- structures in the pointer chain into a 'ExternalBufferProperties'.
fromCStructExternalBufferProperties :: VkExternalBufferProperties -> IO ExternalBufferProperties
fromCStructExternalBufferProperties c = ExternalBufferProperties <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExternalBufferProperties)))
                                                                 <*> (fromCStructExternalMemoryProperties (vkExternalMemoryProperties (c :: VkExternalBufferProperties)))

instance Zero ExternalBufferProperties where
  zero = ExternalBufferProperties Nothing
                                  zero



-- | VkExternalImageFormatProperties - Structure specifying supported
-- external handle properties
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryProperties',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data ExternalImageFormatProperties = ExternalImageFormatProperties
  { -- Univalued member elided
  -- No documentation found for Nested "ExternalImageFormatProperties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalImageFormatProperties" "externalMemoryProperties"
  externalMemoryProperties :: ExternalMemoryProperties
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkExternalImageFormatProperties' and
-- marshal a 'ExternalImageFormatProperties' into it. The 'VkExternalImageFormatProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructExternalImageFormatProperties :: ExternalImageFormatProperties -> (VkExternalImageFormatProperties -> IO a) -> IO a
withCStructExternalImageFormatProperties marshalled cont = withCStructExternalMemoryProperties (externalMemoryProperties (marshalled :: ExternalImageFormatProperties)) (\externalMemoryProperties'' -> maybeWith withSomeVkStruct (next (marshalled :: ExternalImageFormatProperties)) (\pPNext -> cont (VkExternalImageFormatProperties VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES pPNext externalMemoryProperties'')))

-- | A function to read a 'VkExternalImageFormatProperties' and all additional
-- structures in the pointer chain into a 'ExternalImageFormatProperties'.
fromCStructExternalImageFormatProperties :: VkExternalImageFormatProperties -> IO ExternalImageFormatProperties
fromCStructExternalImageFormatProperties c = ExternalImageFormatProperties <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExternalImageFormatProperties)))
                                                                           <*> (fromCStructExternalMemoryProperties (vkExternalMemoryProperties (c :: VkExternalImageFormatProperties)))

instance Zero ExternalImageFormatProperties where
  zero = ExternalImageFormatProperties Nothing
                                       zero


-- | VkExternalMemoryFeatureFlagBits - Bitmask specifying features of an
-- external memory handle type
--
-- = Description
--
-- Because their semantics in external APIs roughly align with that of an
-- image or buffer with a dedicated allocation in Vulkan, implementations
-- are /required/ to report
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT'
-- for the following external handle types:
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryFeatureFlags'
type ExternalMemoryFeatureFlagBits = VkExternalMemoryFeatureFlagBits


{-# complete EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT, EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT, EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT :: ExternalMemoryFeatureFlagBits #-}


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT'
-- specifies that images or buffers created with the specified parameters
-- and handle type /must/ use the mechanisms defined by
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedRequirements'
-- and
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
-- to create (or import) a dedicated allocation for the image or buffer.
pattern EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT :: (a ~ ExternalMemoryFeatureFlagBits) => a
pattern EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT = VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT'
-- specifies that handles of this type /can/ be exported from Vulkan memory
-- objects.
pattern EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT :: (a ~ ExternalMemoryFeatureFlagBits) => a
pattern EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT = VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT'
-- specifies that handles of this type /can/ be imported as Vulkan memory
-- objects.
pattern EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT :: (a ~ ExternalMemoryFeatureFlagBits) => a
pattern EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT = VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT

-- No documentation found for TopLevel "ExternalMemoryFeatureFlagBitsKHR"
type ExternalMemoryFeatureFlagBitsKHR = ExternalMemoryFeatureFlagBits

-- | VkExternalMemoryFeatureFlags - Bitmask of
-- VkExternalMemoryFeatureFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryFeatureFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryFeatureFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryFeatureFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryProperties'
type ExternalMemoryFeatureFlags = ExternalMemoryFeatureFlagBits

-- No documentation found for TopLevel "ExternalMemoryFeatureFlagsKHR"
type ExternalMemoryFeatureFlagsKHR = ExternalMemoryFeatureFlags

-- | VkExternalMemoryHandleTypeFlagBits - Bit specifying external memory
-- handle types
--
-- = Description
--
-- Some external memory handle types can only be shared within the same
-- underlying physical device and\/or the same driver version, as defined
-- in the following table:
--
-- > +----------------------+----------------------+-----------------------+
-- > | Handle type          | 'Graphics.Vulkan.C.C | 'Graphics.Vulkan.C.Co |
-- > |                      | ore11.Promoted_from_ | re11.Promoted_from_VK |
-- > |                      | VK_KHR_external_memo | _KHR_external_memory_ |
-- > |                      | ry_capabilities.VkPh | capabilities.VkPhysic |
-- > |                      | ysicalDeviceIDProper | alDeviceIDProperties' |
-- > |                      | ties'::@driverUUID@  | ::@deviceUUID@        |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_memo |                      |                       |
-- > | ry_capabilities.VK_E |                      |                       |
-- > | XTERNAL_MEMORY_HANDL |                      |                       |
-- > | E_TYPE_OPAQUE_FD_BIT |                      |                       |
-- > | '                    |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_memo |                      |                       |
-- > | ry_capabilities.VK_E |                      |                       |
-- > | XTERNAL_MEMORY_HANDL |                      |                       |
-- > | E_TYPE_OPAQUE_WIN32_ |                      |                       |
-- > | BIT'                 |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_memo |                      |                       |
-- > | ry_capabilities.VK_E |                      |                       |
-- > | XTERNAL_MEMORY_HANDL |                      |                       |
-- > | E_TYPE_OPAQUE_WIN32_ |                      |                       |
-- > | KMT_BIT'             |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_memo |                      |                       |
-- > | ry_capabilities.VK_E |                      |                       |
-- > | XTERNAL_MEMORY_HANDL |                      |                       |
-- > | E_TYPE_D3D11_TEXTURE |                      |                       |
-- > | _BIT'                |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_memo |                      |                       |
-- > | ry_capabilities.VK_E |                      |                       |
-- > | XTERNAL_MEMORY_HANDL |                      |                       |
-- > | E_TYPE_D3D11_TEXTURE |                      |                       |
-- > | _KMT_BIT'            |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_memo |                      |                       |
-- > | ry_capabilities.VK_E |                      |                       |
-- > | XTERNAL_MEMORY_HANDL |                      |                       |
-- > | E_TYPE_D3D12_HEAP_BI |                      |                       |
-- > | T'                   |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_memo |                      |                       |
-- > | ry_capabilities.VK_E |                      |                       |
-- > | XTERNAL_MEMORY_HANDL |                      |                       |
-- > | E_TYPE_D3D12_RESOURC |                      |                       |
-- > | E_BIT'               |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- >
-- > External memory handle types compatibility
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlags',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd.VkImportMemoryFdInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VkImportMemoryHostPointerInfoEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.VkImportMemoryWin32HandleInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd.VkMemoryGetFdInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.VkMemoryGetWin32HandleInfoKHR',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalBufferInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalImageFormatInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd.vkGetMemoryFdPropertiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.vkGetMemoryHostPointerPropertiesEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.vkGetMemoryWin32HandlePropertiesKHR'
type ExternalMemoryHandleTypeFlagBits = VkExternalMemoryHandleTypeFlagBits


{-# complete EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT, EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT, EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT, EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT, EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT, EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT, EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT, EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT, EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID, EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT, EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT :: ExternalMemoryHandleTypeFlagBits #-}


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT'
-- specifies a POSIX file descriptor handle that has only limited valid
-- usage outside of Vulkan and other compatible APIs. It /must/ be
-- compatible with the POSIX system calls @dup@, @dup2@, @close@, and the
-- non-standard system call @dup3@. Additionally, it /must/ be
-- transportable over a socket using an @SCM_RIGHTS@ control message. It
-- owns a reference to the underlying memory resource represented by its
-- Vulkan memory object.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT :: (a ~ ExternalMemoryHandleTypeFlagBits) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT'
-- specifies an NT handle that has only limited valid usage outside of
-- Vulkan and other compatible APIs. It /must/ be compatible with the
-- functions @DuplicateHandle@, @CloseHandle@, @CompareObjectHandles@,
-- @GetHandleInformation@, and @SetHandleInformation@. It owns a reference
-- to the underlying memory resource represented by its Vulkan memory
-- object.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT :: (a ~ ExternalMemoryHandleTypeFlagBits) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT'
-- specifies a global share handle that has only limited valid usage
-- outside of Vulkan and other compatible APIs. It is not compatible with
-- any native APIs. It does not own a reference to the underlying memory
-- resource represented its Vulkan memory object, and will therefore become
-- invalid when all Vulkan memory objects associated with it are destroyed.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT :: (a ~ ExternalMemoryHandleTypeFlagBits) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT'
-- specifies an NT handle returned by
-- @IDXGIResource1@::@CreateSharedHandle@ referring to a Direct3D 10 or 11
-- texture resource. It owns a reference to the memory used by the Direct3D
-- resource.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT :: (a ~ ExternalMemoryHandleTypeFlagBits) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT'
-- specifies a global share handle returned by
-- @IDXGIResource@::@GetSharedHandle@ referring to a Direct3D 10 or 11
-- texture resource. It does not own a reference to the underlying Direct3D
-- resource, and will therefore become invalid when all Vulkan memory
-- objects and Direct3D resources associated with it are destroyed.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT :: (a ~ ExternalMemoryHandleTypeFlagBits) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT'
-- specifies an NT handle returned by @ID3D12Device@::@CreateSharedHandle@
-- referring to a Direct3D 12 heap resource. It owns a reference to the
-- resources used by the Direct3D heap.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT :: (a ~ ExternalMemoryHandleTypeFlagBits) => a
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT'
-- specifies an NT handle returned by @ID3D12Device@::@CreateSharedHandle@
-- referring to a Direct3D 12 committed resource. It owns a reference to
-- the memory used by the Direct3D resource.
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

-- | VkExternalMemoryHandleTypeFlags - Bitmask of
-- VkExternalMemoryHandleTypeFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExportMemoryAllocateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryBufferCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryProperties'
type ExternalMemoryHandleTypeFlags = ExternalMemoryHandleTypeFlagBits

-- No documentation found for TopLevel "ExternalMemoryHandleTypeFlagsKHR"
type ExternalMemoryHandleTypeFlagsKHR = ExternalMemoryHandleTypeFlags


-- | VkExternalMemoryProperties - Structure specifying external memory handle
-- type capabilities
--
-- = Description
--
-- @compatibleHandleTypes@ /must/ include at least @handleType@. Inclusion
-- of a handle type in @compatibleHandleTypes@ does not imply the values
-- returned in
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkImageFormatProperties2'
-- will be the same when
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalImageFormatInfo'::@handleType@
-- is set to that type. The application is responsible for querying the
-- capabilities of all handle types intended for concurrent use in a single
-- image and intersecting them to obtain the compatible set of
-- capabilities.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalBufferProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalImageFormatProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryFeatureFlags',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlags'
data ExternalMemoryProperties = ExternalMemoryProperties
  { -- No documentation found for Nested "ExternalMemoryProperties" "externalMemoryFeatures"
  externalMemoryFeatures :: ExternalMemoryFeatureFlags
  , -- No documentation found for Nested "ExternalMemoryProperties" "exportFromImportedHandleTypes"
  exportFromImportedHandleTypes :: ExternalMemoryHandleTypeFlags
  , -- No documentation found for Nested "ExternalMemoryProperties" "compatibleHandleTypes"
  compatibleHandleTypes :: ExternalMemoryHandleTypeFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkExternalMemoryProperties' and
-- marshal a 'ExternalMemoryProperties' into it. The 'VkExternalMemoryProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructExternalMemoryProperties :: ExternalMemoryProperties -> (VkExternalMemoryProperties -> IO a) -> IO a
withCStructExternalMemoryProperties marshalled cont = cont (VkExternalMemoryProperties (externalMemoryFeatures (marshalled :: ExternalMemoryProperties)) (exportFromImportedHandleTypes (marshalled :: ExternalMemoryProperties)) (compatibleHandleTypes (marshalled :: ExternalMemoryProperties)))

-- | A function to read a 'VkExternalMemoryProperties' and all additional
-- structures in the pointer chain into a 'ExternalMemoryProperties'.
fromCStructExternalMemoryProperties :: VkExternalMemoryProperties -> IO ExternalMemoryProperties
fromCStructExternalMemoryProperties c = ExternalMemoryProperties <$> pure (vkExternalMemoryFeatures (c :: VkExternalMemoryProperties))
                                                                 <*> pure (vkExportFromImportedHandleTypes (c :: VkExternalMemoryProperties))
                                                                 <*> pure (vkCompatibleHandleTypes (c :: VkExternalMemoryProperties))

instance Zero ExternalMemoryProperties where
  zero = ExternalMemoryProperties zero
                                  zero
                                  zero



-- | VkPhysicalDeviceExternalBufferInfo - Structure specifying buffer
-- creation parameters
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferUsageFlags',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.vkGetPhysicalDeviceExternalBufferProperties',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_capabilities.vkGetPhysicalDeviceExternalBufferPropertiesKHR'
data PhysicalDeviceExternalBufferInfo = PhysicalDeviceExternalBufferInfo
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceExternalBufferInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceExternalBufferInfo" "flags"
  flags :: BufferCreateFlags
  , -- No documentation found for Nested "PhysicalDeviceExternalBufferInfo" "usage"
  usage :: BufferUsageFlags
  , -- No documentation found for Nested "PhysicalDeviceExternalBufferInfo" "handleType"
  handleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceExternalBufferInfo' and
-- marshal a 'PhysicalDeviceExternalBufferInfo' into it. The 'VkPhysicalDeviceExternalBufferInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceExternalBufferInfo :: PhysicalDeviceExternalBufferInfo -> (VkPhysicalDeviceExternalBufferInfo -> IO a) -> IO a
withCStructPhysicalDeviceExternalBufferInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceExternalBufferInfo)) (\pPNext -> cont (VkPhysicalDeviceExternalBufferInfo VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO pPNext (flags (marshalled :: PhysicalDeviceExternalBufferInfo)) (usage (marshalled :: PhysicalDeviceExternalBufferInfo)) (handleType (marshalled :: PhysicalDeviceExternalBufferInfo))))

-- | A function to read a 'VkPhysicalDeviceExternalBufferInfo' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceExternalBufferInfo'.
fromCStructPhysicalDeviceExternalBufferInfo :: VkPhysicalDeviceExternalBufferInfo -> IO PhysicalDeviceExternalBufferInfo
fromCStructPhysicalDeviceExternalBufferInfo c = PhysicalDeviceExternalBufferInfo <$> -- Univalued Member elided
                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceExternalBufferInfo)))
                                                                                 <*> pure (vkFlags (c :: VkPhysicalDeviceExternalBufferInfo))
                                                                                 <*> pure (vkUsage (c :: VkPhysicalDeviceExternalBufferInfo))
                                                                                 <*> pure (vkHandleType (c :: VkPhysicalDeviceExternalBufferInfo))

instance Zero PhysicalDeviceExternalBufferInfo where
  zero = PhysicalDeviceExternalBufferInfo Nothing
                                          zero
                                          zero
                                          zero



-- | VkPhysicalDeviceExternalImageFormatInfo - Structure specifying external
-- image creation parameters
--
-- = Description
--
-- If @handleType@ is 0,
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'
-- will behave as if
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalImageFormatInfo'
-- was not present, and
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalImageFormatProperties'
-- will be ignored.
--
-- If @handleType@ is not compatible with the @format@, @type@, @tiling@,
-- @usage@, and @flags@ specified in
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- then
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'
-- returns 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO'
--
-- -   If @handleType@ is not @0@, @handleType@ /must/ be a valid
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceExternalImageFormatInfo = PhysicalDeviceExternalImageFormatInfo
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceExternalImageFormatInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceExternalImageFormatInfo" "handleType"
  handleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceExternalImageFormatInfo' and
-- marshal a 'PhysicalDeviceExternalImageFormatInfo' into it. The 'VkPhysicalDeviceExternalImageFormatInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceExternalImageFormatInfo :: PhysicalDeviceExternalImageFormatInfo -> (VkPhysicalDeviceExternalImageFormatInfo -> IO a) -> IO a
withCStructPhysicalDeviceExternalImageFormatInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceExternalImageFormatInfo)) (\pPNext -> cont (VkPhysicalDeviceExternalImageFormatInfo VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO pPNext (handleType (marshalled :: PhysicalDeviceExternalImageFormatInfo))))

-- | A function to read a 'VkPhysicalDeviceExternalImageFormatInfo' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceExternalImageFormatInfo'.
fromCStructPhysicalDeviceExternalImageFormatInfo :: VkPhysicalDeviceExternalImageFormatInfo -> IO PhysicalDeviceExternalImageFormatInfo
fromCStructPhysicalDeviceExternalImageFormatInfo c = PhysicalDeviceExternalImageFormatInfo <$> -- Univalued Member elided
                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceExternalImageFormatInfo)))
                                                                                           <*> pure (vkHandleType (c :: VkPhysicalDeviceExternalImageFormatInfo))

instance Zero PhysicalDeviceExternalImageFormatInfo where
  zero = PhysicalDeviceExternalImageFormatInfo Nothing
                                               zero



-- | VkPhysicalDeviceIDProperties - Structure specifying IDs related to the
-- physical device
--
-- = Description
--
-- @deviceUUID@ /must/ be immutable for a given device across instances,
-- processes, driver APIs, driver versions, and system reboots.
--
-- Applications /can/ compare the @driverUUID@ value across instance and
-- process boundaries, and /can/ make similar queries in external APIs to
-- determine whether they are capable of sharing memory objects and
-- resources using them with the device.
--
-- @deviceUUID@ and\/or @driverUUID@ /must/ be used to determine whether a
-- particular external object can be shared between driver components,
-- where such a restriction exists as defined in the compatibility table
-- for the particular object type:
--
-- If @deviceLUIDValid@ is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', the
-- values of @deviceLUID@ and @deviceNodeMask@ are undefined. If
-- @deviceLUIDValid@ is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' and Vulkan
-- is running on the Windows operating system, the contents of @deviceLUID@
-- /can/ be cast to an @LUID@ object and /must/ be equal to the locally
-- unique identifier of a @IDXGIAdapter1@ object that corresponds to
-- @physicalDevice@. If @deviceLUIDValid@ is
-- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', @deviceNodeMask@ /must/ contain
-- exactly one bit. If Vulkan is running on an operating system that
-- supports the Direct3D 12 API and @physicalDevice@ corresponds to an
-- individual device in a linked device adapter, @deviceNodeMask@
-- identifies the Direct3D 12 node corresponding to @physicalDevice@.
-- Otherwise, @deviceNodeMask@ /must/ be @1@.
--
-- __Note__
--
-- Although they have identical descriptions,
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceIDProperties'::@deviceUUID@
-- may differ from
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2'::@pipelineCacheUUID@.
-- The former is intended to identify and correlate devices across API and
-- driver boundaries, while the latter is used to identify a compatible
-- device and driver combination to use when serializing and de-serializing
-- pipeline state.
--
-- __Note__
--
-- While
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceIDProperties'::@deviceUUID@
-- is specified to remain consistent across driver versions and system
-- reboots, it is not intended to be usable as a serializable persistent
-- identifier for a device. It may change when a device is physically added
-- to, removed from, or moved to a different connector in a system while
-- that system is powered down. Further, there is no reasonable way to
-- verify with conformance testing that a given device retains the same
-- UUID in a given system across all driver versions supported in that
-- system. While implementations should make every effort to report
-- consistent device UUIDs across driver versions, applications should
-- avoid relying on the persistence of this value for uses other than
-- identifying compatible devices for external object sharing purposes.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceIDProperties = PhysicalDeviceIDProperties
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceIDProperties" "pNext"
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

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceIDProperties' and
-- marshal a 'PhysicalDeviceIDProperties' into it. The 'VkPhysicalDeviceIDProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceIDProperties :: PhysicalDeviceIDProperties -> (VkPhysicalDeviceIDProperties -> IO a) -> IO a
withCStructPhysicalDeviceIDProperties marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceIDProperties)) (\pPNext -> cont (VkPhysicalDeviceIDProperties VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES pPNext (byteStringToSizedVector (deviceUUID (marshalled :: PhysicalDeviceIDProperties))) (byteStringToSizedVector (driverUUID (marshalled :: PhysicalDeviceIDProperties))) (byteStringToSizedVector (deviceLUID (marshalled :: PhysicalDeviceIDProperties))) (deviceNodeMask (marshalled :: PhysicalDeviceIDProperties)) (boolToBool32 (deviceLUIDValid (marshalled :: PhysicalDeviceIDProperties)))))

-- | A function to read a 'VkPhysicalDeviceIDProperties' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceIDProperties'.
fromCStructPhysicalDeviceIDProperties :: VkPhysicalDeviceIDProperties -> IO PhysicalDeviceIDProperties
fromCStructPhysicalDeviceIDProperties c = PhysicalDeviceIDProperties <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceIDProperties)))
                                                                     <*> Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized (vkDeviceUUID (c :: VkPhysicalDeviceIDProperties))) (\p -> packCStringLen (castPtr p, VK_UUID_SIZE))
                                                                     <*> Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized (vkDriverUUID (c :: VkPhysicalDeviceIDProperties))) (\p -> packCStringLen (castPtr p, VK_UUID_SIZE))
                                                                     <*> Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized (vkDeviceLUID (c :: VkPhysicalDeviceIDProperties))) (\p -> packCStringLen (castPtr p, VK_LUID_SIZE))
                                                                     <*> pure (vkDeviceNodeMask (c :: VkPhysicalDeviceIDProperties))
                                                                     <*> pure (bool32ToBool (vkDeviceLUIDValid (c :: VkPhysicalDeviceIDProperties)))

instance Zero PhysicalDeviceIDProperties where
  zero = PhysicalDeviceIDProperties Nothing
                                    Data.ByteString.empty
                                    Data.ByteString.empty
                                    Data.ByteString.empty
                                    zero
                                    False



-- | vkGetPhysicalDeviceExternalBufferProperties - Query external handle
-- types supported by buffers
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     buffer capabilities.
--
-- -   @pExternalBufferInfo@ points to an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalBufferInfo'
--     structure, describing the parameters that would be consumed by
--     'Graphics.Vulkan.C.Core10.Buffer.vkCreateBuffer'.
--
-- -   @pExternalBufferProperties@ points to an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalBufferProperties'
--     structure in which capabilities are returned.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalBufferProperties',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalBufferInfo'
getPhysicalDeviceExternalBufferProperties :: PhysicalDevice ->  PhysicalDeviceExternalBufferInfo ->  IO (ExternalBufferProperties)
getPhysicalDeviceExternalBufferProperties = \(PhysicalDevice physicalDevice' commandTable) -> \externalBufferInfo' -> alloca (\pExternalBufferProperties' -> (\marshalled -> withCStructPhysicalDeviceExternalBufferInfo marshalled . flip with) externalBufferInfo' (\pExternalBufferInfo' -> vkGetPhysicalDeviceExternalBufferProperties commandTable physicalDevice' pExternalBufferInfo' pExternalBufferProperties' *> ((fromCStructExternalBufferProperties <=< peek) pExternalBufferProperties')))
