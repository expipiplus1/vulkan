{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBits(..)
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT
  , VkExternalMemoryFeatureFlagBits(..)
  , pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT
  , pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT
  , pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES
  , VK_LUID_SIZE
  , pattern VK_LUID_SIZE
  , vkGetPhysicalDeviceExternalBufferProperties
  , VkExternalMemoryProperties(..)
  , VkPhysicalDeviceExternalImageFormatInfo(..)
  , VkExternalImageFormatProperties(..)
  , VkPhysicalDeviceExternalBufferInfo(..)
  , VkExternalBufferProperties(..)
  , VkPhysicalDeviceIDProperties(..)
  , VkExternalMemoryHandleTypeFlags
  , VkExternalMemoryFeatureFlags
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
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
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Buffer
  ( VkBufferCreateFlags
  , VkBufferUsageFlags
  )
import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VK_UUID_SIZE
  , VkPhysicalDevice
  )


-- ** VkExternalMemoryHandleTypeFlagBits

-- | VkExternalMemoryHandleTypeFlagBits - Bit specifying external memory
-- handle types
--
-- = Description
--
-- -   @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT@ specifies a POSIX
--     file descriptor handle that has only limited valid usage outside of
--     Vulkan and other compatible APIs. It /must/ be compatible with the
--     POSIX system calls @dup@, @dup2@, @close@, and the non-standard
--     system call @dup3@. Additionally, it /must/ be transportable over a
--     socket using an @SCM_RIGHTS@ control message. It owns a reference to
--     the underlying memory resource represented by its Vulkan memory
--     object.
--
-- -   @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT@ specifies an NT
--     handle that has only limited valid usage outside of Vulkan and other
--     compatible APIs. It /must/ be compatible with the functions
--     @DuplicateHandle@, @CloseHandle@, @CompareObjectHandles@,
--     @GetHandleInformation@, and @SetHandleInformation@. It owns a
--     reference to the underlying memory resource represented by its
--     Vulkan memory object.
--
-- -   @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT@ specifies a
--     global share handle that has only limited valid usage outside of
--     Vulkan and other compatible APIs. It is not compatible with any
--     native APIs. It does not own a reference to the underlying memory
--     resource represented its Vulkan memory object, and will therefore
--     become invalid when all Vulkan memory objects associated with it are
--     destroyed.
--
-- -   @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT@ specifies an NT
--     handle returned by @IDXGIResource1@::@CreateSharedHandle@ referring
--     to a Direct3D 10 or 11 texture resource. It owns a reference to the
--     memory used by the Direct3D resource.
--
-- -   @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT@ specifies a
--     global share handle returned by @IDXGIResource@::@GetSharedHandle@
--     referring to a Direct3D 10 or 11 texture resource. It does not own a
--     reference to the underlying Direct3D resource, and will therefore
--     become invalid when all Vulkan memory objects and Direct3D resources
--     associated with it are destroyed.
--
-- -   @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT@ specifies an NT
--     handle returned by @ID3D12Device@::@CreateSharedHandle@ referring to
--     a Direct3D 12 heap resource. It owns a reference to the resources
--     used by the Direct3D heap.
--
-- -   @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT@ specifies an NT
--     handle returned by @ID3D12Device@::@CreateSharedHandle@ referring to
--     a Direct3D 12 committed resource. It owns a reference to the memory
--     used by the Direct3D resource.
--
-- -   @VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT@ specifies a
--     host pointer returned by a host memory allocation command. It does
--     not own a reference to the underlying memory resource, and will
--     therefore become invalid if the host memory is freed.
--
-- -   @VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT@
--     specifies a host pointer to /host mapped foreign memory/. It does
--     not own a reference to the underlying memory resource, and will
--     therefore become invalid if the foreign memory is unmapped or
--     otherwise becomes no longer available.
--
-- -   @VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT@ is a file
--     descriptor for a Linux dma_buf. It owns a reference to the
--     underlying memory resource represented by its Vulkan memory object.
--
-- -   @VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID@
--     specifies an @AHardwareBuffer@ object defined by the Android NDK.
--     See [Android Hardware
--     Buffers](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-external-android-hardware-buffer)
--     for more details of this handle type.
--
-- Some external memory handle types can only be shared within the same
-- underlying physical device and\/or the same driver version, as defined
-- in the following table:
--
-- +----------------------------------------------------------------------+----------------------------------------------+----------------------------------------------+
-- | Handle type                                                          | @VkPhysicalDeviceIDProperties@::@driverUUID@ | @VkPhysicalDeviceIDProperties@::@deviceUUID@ |
-- +----------------------------------------------------------------------+----------------------------------------------+----------------------------------------------+
-- | @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT@                       | Must match                                   | Must match                                   |
-- +----------------------------------------------------------------------+----------------------------------------------+----------------------------------------------+
-- | @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT@                    | Must match                                   | Must match                                   |
-- +----------------------------------------------------------------------+----------------------------------------------+----------------------------------------------+
-- | @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT@                | Must match                                   | Must match                                   |
-- +----------------------------------------------------------------------+----------------------------------------------+----------------------------------------------+
-- | @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT@                   | Must match                                   | Must match                                   |
-- +----------------------------------------------------------------------+----------------------------------------------+----------------------------------------------+
-- | @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT@               | Must match                                   | Must match                                   |
-- +----------------------------------------------------------------------+----------------------------------------------+----------------------------------------------+
-- | @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT@                      | Must match                                   | Must match                                   |
-- +----------------------------------------------------------------------+----------------------------------------------+----------------------------------------------+
-- | @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT@                  | Must match                                   | Must match                                   |
-- +----------------------------------------------------------------------+----------------------------------------------+----------------------------------------------+
-- | @VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT@             | No restriction                               | No restriction                               |
-- +----------------------------------------------------------------------+----------------------------------------------+----------------------------------------------+
-- | @VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT@  | No restriction                               | No restriction                               |
-- +----------------------------------------------------------------------+----------------------------------------------+----------------------------------------------+
-- | @VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT@                     | No restriction                               | No restriction                               |
-- +----------------------------------------------------------------------+----------------------------------------------+----------------------------------------------+
-- | @VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID@ | No restriction                               | No restriction                               |
-- +----------------------------------------------------------------------+----------------------------------------------+----------------------------------------------+
--
-- External memory handle types compatibility
--
-- __Note__
--
-- The above table does not restrict the drivers and devices with which
-- @VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT@ and
-- @VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT@
-- /may/ be shared, as these handle types inherently mean memory that does
-- not come from the same device, as they import memory from the host or a
-- foreign device, respectively.
--
-- __Note__
--
-- Even though the above table does not restrict the drivers and devices
-- with which @VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT@ /may/ be
-- shared, query mechanisms exist in the Vulkan API that prevent the import
-- of incompatible dma-bufs (such as
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd.vkGetMemoryFdPropertiesKHR')
-- and that prevent incompatible usage of dma-bufs (such as
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalBufferInfoKHR'
-- and
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalImageFormatInfoKHR').
--
-- = See Also
--
-- 'VkExternalMemoryHandleTypeFlags',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd.VkImportMemoryFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_external_memory_host.VkImportMemoryHostPointerInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32.VkImportMemoryWin32HandleInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd.VkMemoryGetFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32.VkMemoryGetWin32HandleInfoKHR',
-- 'VkPhysicalDeviceExternalBufferInfo',
-- 'VkPhysicalDeviceExternalImageFormatInfo',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd.vkGetMemoryFdPropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_external_memory_host.vkGetMemoryHostPointerPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32.vkGetMemoryWin32HandlePropertiesKHR'
newtype VkExternalMemoryHandleTypeFlagBits = VkExternalMemoryHandleTypeFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkExternalMemoryHandleTypeFlagBits where
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkExternalMemoryHandleTypeFlagBits 0x00000200) = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT"
  showsPrec _ (VkExternalMemoryHandleTypeFlagBits 0x00000400) = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID"
  showsPrec _ (VkExternalMemoryHandleTypeFlagBits 0x00000080) = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT"
  showsPrec _ (VkExternalMemoryHandleTypeFlagBits 0x00000100) = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT"
  showsPrec p (VkExternalMemoryHandleTypeFlagBits x) = showParen (p >= 11) (showString "VkExternalMemoryHandleTypeFlagBits " . showsPrec 11 x)

instance Read VkExternalMemoryHandleTypeFlagBits where
  readPrec = parens ( choose [ ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT",         pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT",      pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT",  pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT",     pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT", pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT",        pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT",    pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT",                     pure (VkExternalMemoryHandleTypeFlagBits 0x00000200))
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID", pure (VkExternalMemoryHandleTypeFlagBits 0x00000400))
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT",             pure (VkExternalMemoryHandleTypeFlagBits 0x00000080))
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT",  pure (VkExternalMemoryHandleTypeFlagBits 0x00000100))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalMemoryHandleTypeFlagBits")
                        v <- step readPrec
                        pure (VkExternalMemoryHandleTypeFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000001

-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000002

-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000004

-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000008

-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000010

-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000020

-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT = VkExternalMemoryHandleTypeFlagBits 0x00000040
-- ** VkExternalMemoryFeatureFlagBits

-- | VkExternalMemoryFeatureFlagBits - Bitmask specifying features of an
-- external memory handle type
--
-- = Description
--
-- -   @VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT@ specifies that
--     images or buffers created with the specified parameters and handle
--     type /must/ use the mechanisms defined in the
--     @{html_spec_relative}#VK_NV_dedicated_allocation@ extension to
--     create (or import) a dedicated allocation for the image or buffer.
--
-- -   @VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT@ specifies that handles
--     of this type /can/ be exported from Vulkan memory objects.
--
-- -   @VK_INTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT@ specifies that handles
--     of this type /can/ be imported as Vulkan memory objects.
--
-- Because their semantics in external APIs roughly align with that of an
-- image or buffer with a dedicated allocation in Vulkan, implementations
-- are /required/ to report @VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT@
-- for the following external handle types:
--
-- -   @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT@
--
-- -   @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT@
--
-- -   @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT@
--
-- -   @VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID@
--     for images only
--
-- Implementations /must/ not report
-- @VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT@ for buffers with
-- external handle type
-- @VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID@.
--
-- = See Also
--
-- 'VkExternalMemoryFeatureFlags'
newtype VkExternalMemoryFeatureFlagBits = VkExternalMemoryFeatureFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkExternalMemoryFeatureFlagBits where
  showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT = showString "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT"
  showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT = showString "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT"
  showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT = showString "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT"
  showsPrec p (VkExternalMemoryFeatureFlagBits x) = showParen (p >= 11) (showString "VkExternalMemoryFeatureFlagBits " . showsPrec 11 x)

instance Read VkExternalMemoryFeatureFlagBits where
  readPrec = parens ( choose [ ("VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT", pure VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT)
                             , ("VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT",     pure VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT)
                             , ("VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT",     pure VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalMemoryFeatureFlagBits")
                        v <- step readPrec
                        pure (VkExternalMemoryFeatureFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkExternalMemoryFeatureFlagBits" "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT"
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT :: VkExternalMemoryFeatureFlagBits
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT = VkExternalMemoryFeatureFlagBits 0x00000001

-- No documentation found for Nested "VkExternalMemoryFeatureFlagBits" "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT"
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT :: VkExternalMemoryFeatureFlagBits
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT = VkExternalMemoryFeatureFlagBits 0x00000002

-- No documentation found for Nested "VkExternalMemoryFeatureFlagBits" "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT"
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT :: VkExternalMemoryFeatureFlagBits
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT = VkExternalMemoryFeatureFlagBits 0x00000004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO = VkStructureType 1000071000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES"
pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES = VkStructureType 1000071001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO = VkStructureType 1000071002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES"
pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES = VkStructureType 1000071003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES = VkStructureType 1000071004
-- No documentation found for TopLevel "VK_LUID_SIZE"
type VK_LUID_SIZE = 8
-- No documentation found for Nested "Integral a => a" "VK_LUID_SIZE"
pattern VK_LUID_SIZE :: Integral a => a
pattern VK_LUID_SIZE = 8
-- | vkGetPhysicalDeviceExternalBufferProperties - Query external handle
-- types supported by buffers
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     buffer capabilities.
--
-- -   @pExternalBufferInfo@ points to an instance of the
--     'VkPhysicalDeviceExternalBufferInfo' structure, describing the
--     parameters that would be consumed by
--     'Graphics.Vulkan.Core10.Buffer.vkCreateBuffer'.
--
-- -   @pExternalBufferProperties@ points to an instance of the
--     'VkExternalBufferProperties' structure in which capabilities are
--     returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @pExternalBufferInfo@ /must/ be a valid pointer to a valid
--     @VkPhysicalDeviceExternalBufferInfo@ structure
--
-- -   @pExternalBufferProperties@ /must/ be a valid pointer to a
--     @VkExternalBufferProperties@ structure
--
-- = See Also
--
-- 'VkExternalBufferProperties',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'VkPhysicalDeviceExternalBufferInfo'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceExternalBufferProperties" vkGetPhysicalDeviceExternalBufferProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalBufferInfo" ::: Ptr VkPhysicalDeviceExternalBufferInfo) -> ("pExternalBufferProperties" ::: Ptr VkExternalBufferProperties) -> IO ()
-- | VkExternalMemoryProperties - Structure specifying external memory handle
-- type capabilities
--
-- = Description
--
-- @compatibleHandleTypes@ /must/ include at least @handleType@. Inclusion
-- of a handle type in @compatibleHandleTypes@ does not imply the values
-- returned in
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkImageFormatProperties2'
-- will be the same when
-- 'VkPhysicalDeviceExternalImageFormatInfo'::@handleType@ is set to that
-- type. The application is responsible for querying the capabilities of
-- all handle types intended for concurrent use in a single image and
-- intersecting them to obtain the compatible set of capabilities.
--
-- = See Also
--
-- 'VkExternalBufferProperties', 'VkExternalImageFormatProperties',
-- 'VkExternalMemoryFeatureFlags', 'VkExternalMemoryHandleTypeFlags'
data VkExternalMemoryProperties = VkExternalMemoryProperties
  { -- | @externalMemoryFeatures@ is a bitmask of
  -- 'VkExternalMemoryFeatureFlagBits' specifying the features of
  -- @handleType@.
  vkExternalMemoryFeatures :: VkExternalMemoryFeatureFlags
  , -- | @exportFromImportedHandleTypes@ is a bitmask of
  -- 'VkExternalMemoryHandleTypeFlagBits' specifying which types of imported
  -- handle @handleType@ /can/ be exported from.
  vkExportFromImportedHandleTypes :: VkExternalMemoryHandleTypeFlags
  , -- | @compatibleHandleTypes@ is a bitmask of
  -- 'VkExternalMemoryHandleTypeFlagBits' specifying handle types which /can/
  -- be specified at the same time as @handleType@ when creating an image
  -- compatible with external memory.
  vkCompatibleHandleTypes :: VkExternalMemoryHandleTypeFlags
  }
  deriving (Eq, Show)

instance Storable VkExternalMemoryProperties where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkExternalMemoryProperties <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 4)
                                        <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkExternalMemoryFeatures (poked :: VkExternalMemoryProperties))
                *> poke (ptr `plusPtr` 4) (vkExportFromImportedHandleTypes (poked :: VkExternalMemoryProperties))
                *> poke (ptr `plusPtr` 8) (vkCompatibleHandleTypes (poked :: VkExternalMemoryProperties))
-- | VkPhysicalDeviceExternalImageFormatInfo - Structure specifying external
-- image creation parameters
--
-- = Description
--
-- If @handleType@ is 0,
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'
-- will behave as if 'VkPhysicalDeviceExternalImageFormatInfo' was not
-- present, and 'VkExternalImageFormatProperties' will be ignored.
--
-- If @handleType@ is not compatible with the @format@, @type@, @tiling@,
-- @usage@, and @flags@ specified in
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- then
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'
-- returns @VK_ERROR_FORMAT_NOT_SUPPORTED@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO@
--
-- -   If @handleType@ is not @0@, @handleType@ /must/ be a valid
--     'VkExternalMemoryHandleTypeFlagBits' value
--
-- = See Also
--
-- 'VkExternalMemoryHandleTypeFlagBits',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDeviceExternalImageFormatInfo = VkPhysicalDeviceExternalImageFormatInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @handleType@ is a 'VkExternalMemoryHandleTypeFlagBits' value specifying
  -- the memory handle type that will be used with the memory associated with
  -- the image.
  vkHandleType :: VkExternalMemoryHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceExternalImageFormatInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceExternalImageFormatInfo <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceExternalImageFormatInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceExternalImageFormatInfo))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkPhysicalDeviceExternalImageFormatInfo))
-- | VkExternalImageFormatProperties - Structure specifying supported
-- external handle properties
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES@
--
-- = See Also
--
-- 'VkExternalMemoryProperties',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkExternalImageFormatProperties = VkExternalImageFormatProperties
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @externalMemoryProperties@ is an instance of the
  -- 'VkExternalMemoryProperties' structure specifying various capabilities
  -- of the external handle type when used with the specified image creation
  -- parameters.
  vkExternalMemoryProperties :: VkExternalMemoryProperties
  }
  deriving (Eq, Show)

instance Storable VkExternalImageFormatProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkExternalImageFormatProperties <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalImageFormatProperties))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalImageFormatProperties))
                *> poke (ptr `plusPtr` 16) (vkExternalMemoryProperties (poked :: VkExternalImageFormatProperties))
-- | VkPhysicalDeviceExternalBufferInfo - Structure specifying buffer
-- creation parameters
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Buffer.VkBufferCreateFlagBits' values
--
-- -   @usage@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Buffer.VkBufferUsageFlagBits' values
--
-- -   @usage@ /must/ not be @0@
--
-- -   @handleType@ /must/ be a valid 'VkExternalMemoryHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Buffer.VkBufferCreateFlags',
-- 'Graphics.Vulkan.Core10.Buffer.VkBufferUsageFlags',
-- 'VkExternalMemoryHandleTypeFlagBits',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkGetPhysicalDeviceExternalBufferProperties',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_capabilities.vkGetPhysicalDeviceExternalBufferPropertiesKHR'
data VkPhysicalDeviceExternalBufferInfo = VkPhysicalDeviceExternalBufferInfo
  { -- | @sType@ is the type of this structure
  vkSType :: VkStructureType
  , -- | @pNext@ is NULL or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is a bitmask of
  -- 'Graphics.Vulkan.Core10.Buffer.VkBufferCreateFlagBits' describing
  -- additional parameters of the buffer, corresponding to
  -- 'Graphics.Vulkan.Core10.Buffer.VkBufferCreateInfo'::@flags@.
  vkFlags :: VkBufferCreateFlags
  , -- | @usage@ is a bitmask of
  -- 'Graphics.Vulkan.Core10.Buffer.VkBufferUsageFlagBits' describing the
  -- intended usage of the buffer, corresponding to
  -- 'Graphics.Vulkan.Core10.Buffer.VkBufferCreateInfo'::@usage@.
  vkUsage :: VkBufferUsageFlags
  , -- | @handleType@ is a 'VkExternalMemoryHandleTypeFlagBits' value specifying
  -- the memory handle type that will be used with the memory associated with
  -- the buffer.
  vkHandleType :: VkExternalMemoryHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceExternalBufferInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceExternalBufferInfo <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
                                                <*> peek (ptr `plusPtr` 20)
                                                <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceExternalBufferInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceExternalBufferInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPhysicalDeviceExternalBufferInfo))
                *> poke (ptr `plusPtr` 20) (vkUsage (poked :: VkPhysicalDeviceExternalBufferInfo))
                *> poke (ptr `plusPtr` 24) (vkHandleType (poked :: VkPhysicalDeviceExternalBufferInfo))
-- | VkExternalBufferProperties - Structure specifying supported external
-- handle capabilities
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES@
--
-- -   @pNext@ /must/ be @NULL@
--
-- = See Also
--
-- 'VkExternalMemoryProperties',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkGetPhysicalDeviceExternalBufferProperties',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_capabilities.vkGetPhysicalDeviceExternalBufferPropertiesKHR'
data VkExternalBufferProperties = VkExternalBufferProperties
  { -- | @sType@ is the type of this structure
  vkSType :: VkStructureType
  , -- | @pNext@ is NULL or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @externalMemoryProperties@ is an instance of the
  -- 'VkExternalMemoryProperties' structure specifying various capabilities
  -- of the external handle type when used with the specified buffer creation
  -- parameters.
  vkExternalMemoryProperties :: VkExternalMemoryProperties
  }
  deriving (Eq, Show)

instance Storable VkExternalBufferProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkExternalBufferProperties <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalBufferProperties))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalBufferProperties))
                *> poke (ptr `plusPtr` 16) (vkExternalMemoryProperties (poked :: VkExternalBufferProperties))
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
-- -   [External memory handle types
--     compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#external-memory-handle-types-compatibility)
--
-- -   [External semaphore handle types
--     compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#external-semaphore-handle-types-compatibility)
--
-- -   [External fence handle types
--     compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#external-fence-handle-types-compatibility)
--
-- If @deviceLUIDValid@ is @VK_FALSE@, the contents of @deviceLUID@ and
-- @deviceNodeMask@ are undefined. If @deviceLUIDValid@ is @VK_TRUE@ and
-- Vulkan is running on the Windows operating system, the contents of
-- @deviceLUID@ /can/ be cast to an @LUID@ object and /must/ be equal to
-- the locally unique identifier of a @IDXGIAdapter1@ object that
-- corresponds to @physicalDevice@. If @deviceLUIDValid@ is @VK_TRUE@,
-- @deviceNodeMask@ /must/ contain exactly one bit. If Vulkan is running on
-- an operating system that supports the Direct3D 12 API and
-- @physicalDevice@ corresponds to an individual device in a linked device
-- adapter, @deviceNodeMask@ identifies the Direct3D 12 node corresponding
-- to @physicalDevice@. Otherwise, @deviceNodeMask@ /must/ be @1@.
--
-- __Note__
--
-- Although they have identical descriptions,
-- 'VkPhysicalDeviceIDProperties'::@deviceUUID@ may differ from
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2'::@pipelineCacheUUID@.
-- The former is intended to identify and correlate devices across API and
-- driver boundaries, while the latter is used to identify a compatible
-- device and driver combination to use when serializing and de-serializing
-- pipeline state.
--
-- __Note__
--
-- While 'VkPhysicalDeviceIDProperties'::@deviceUUID@ is specified to
-- remain consistent across driver versions and system reboots, it is not
-- intended to be usable as a serializable persistent identifier for a
-- device. It may change when a device is physically added to, removed
-- from, or moved to a different connector in a system while that system is
-- powered down. Further, there is no reasonable way to verify with
-- conformance testing that a given device retains the same UUID in a given
-- system across all driver versions supported in that system. While
-- implementations should make every effort to report consistent device
-- UUIDs across driver versions, applications should avoid relying on the
-- persistence of this value for uses other than identifying compatible
-- devices for external object sharing purposes.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES@
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDeviceIDProperties = VkPhysicalDeviceIDProperties
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @deviceUUID@ is an array of size @VK_UUID_SIZE@, containing 8-bit values
  -- that represent a universally unique identifier for the device.
  vkDeviceUUID :: Vector VK_UUID_SIZE Word8
  , -- | @driverUUID@ is an array of size @VK_UUID_SIZE@, containing 8-bit values
  -- that represent a universally unique identifier for the driver build in
  -- use by the device.
  vkDriverUUID :: Vector VK_UUID_SIZE Word8
  , -- | @deviceLUID@ is an array of size @VK_LUID_SIZE@, containing 8-bit values
  -- that represent a locally unique identifier for the device.
  vkDeviceLUID :: Vector VK_LUID_SIZE Word8
  , -- | @deviceNodeMask@ is a bitfield identifying the node within a linked
  -- device adapter corresponding to the device.
  vkDeviceNodeMask :: Word32
  , -- | @deviceLUIDValid@ is a boolean value that will be @VK_TRUE@ if
  -- @deviceLUID@ contains a valid LUID and @deviceNodeMask@ contains a valid
  -- node mask, and @VK_FALSE@ if they do not.
  vkDeviceLUIDValid :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceIDProperties where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceIDProperties <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
                                          <*> peek (ptr `plusPtr` 32)
                                          <*> peek (ptr `plusPtr` 48)
                                          <*> peek (ptr `plusPtr` 56)
                                          <*> peek (ptr `plusPtr` 60)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceIDProperties))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceIDProperties))
                *> poke (ptr `plusPtr` 16) (vkDeviceUUID (poked :: VkPhysicalDeviceIDProperties))
                *> poke (ptr `plusPtr` 32) (vkDriverUUID (poked :: VkPhysicalDeviceIDProperties))
                *> poke (ptr `plusPtr` 48) (vkDeviceLUID (poked :: VkPhysicalDeviceIDProperties))
                *> poke (ptr `plusPtr` 56) (vkDeviceNodeMask (poked :: VkPhysicalDeviceIDProperties))
                *> poke (ptr `plusPtr` 60) (vkDeviceLUIDValid (poked :: VkPhysicalDeviceIDProperties))
-- | VkExternalMemoryHandleTypeFlags - Bitmask of
-- VkExternalMemoryHandleTypeFlagBits
--
-- = Description
--
-- @VkExternalMemoryHandleTypeFlags@ is a bitmask type for setting a mask
-- of zero or more 'VkExternalMemoryHandleTypeFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory.VkExportMemoryAllocateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryBufferCreateInfo',
-- 'VkExternalMemoryHandleTypeFlagBits',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo',
-- 'VkExternalMemoryProperties'
type VkExternalMemoryHandleTypeFlags = VkExternalMemoryHandleTypeFlagBits
-- | VkExternalMemoryFeatureFlags - Bitmask of
-- VkExternalMemoryFeatureFlagBits
--
-- = Description
--
-- @VkExternalMemoryFeatureFlags@ is a bitmask type for setting a mask of
-- zero or more 'VkExternalMemoryFeatureFlagBits'.
--
-- = See Also
--
-- 'VkExternalMemoryFeatureFlagBits', 'VkExternalMemoryProperties'
type VkExternalMemoryFeatureFlags = VkExternalMemoryFeatureFlagBits
