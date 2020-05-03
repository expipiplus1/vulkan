{-# language CPP #-}
module Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits  ( ExternalMemoryHandleTypeFlagBits( EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT
                                                                                               , EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT
                                                                                               , EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
                                                                                               , EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT
                                                                                               , EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT
                                                                                               , EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT
                                                                                               , EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT
                                                                                               , EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT
                                                                                               , EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT
                                                                                               , EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID
                                                                                               , EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT
                                                                                               , ..
                                                                                               )
                                                             , ExternalMemoryHandleTypeFlags
                                                             ) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.BaseType (Flags)
import Vulkan.Zero (Zero)
-- | VkExternalMemoryHandleTypeFlagBits - Bit specifying external memory
-- handle types
--
-- = Description
--
-- Some external memory handle types can only be shared within the same
-- underlying physical device and\/or the same driver version, as defined
-- in the following table:
--
-- +-------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | Handle type                                                       | 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceIDProperties'::@driverUUID@ | 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceIDProperties'::@deviceUUID@ |
-- +-------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT'                       | Must match                                                                                                 | Must match                                                                                                 |
-- +-------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT'                    | Must match                                                                                                 | Must match                                                                                                 |
-- +-------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT'                | Must match                                                                                                 | Must match                                                                                                 |
-- +-------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT'                   | Must match                                                                                                 | Must match                                                                                                 |
-- +-------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT'               | Must match                                                                                                 | Must match                                                                                                 |
-- +-------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT'                      | Must match                                                                                                 | Must match                                                                                                 |
-- +-------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT'                  | Must match                                                                                                 | Must match                                                                                                 |
-- +-------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT'             | No restriction                                                                                             | No restriction                                                                                             |
-- +-------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT'  | No restriction                                                                                             | No restriction                                                                                             |
-- +-------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT'                     | No restriction                                                                                             | No restriction                                                                                             |
-- +-------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID' | No restriction                                                                                             | No restriction                                                                                             |
-- +-------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
--
-- External memory handle types compatibility
--
-- Note
--
-- The above table does not restrict the drivers and devices with which
-- 'EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT' and
-- 'EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT' /may/
-- be shared, as these handle types inherently mean memory that does not
-- come from the same device, as they import memory from the host or a
-- foreign device, respectively.
--
-- Note
--
-- Even though the above table does not restrict the drivers and devices
-- with which 'EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT' /may/ be
-- shared, query mechanisms exist in the Vulkan API that prevent the import
-- of incompatible dma-bufs (such as
-- 'Vulkan.Extensions.VK_KHR_external_memory_fd.getMemoryFdPropertiesKHR')
-- and that prevent incompatible usage of dma-bufs (such as
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceExternalBufferInfo'
-- and
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceExternalImageFormatInfo').
--
-- = See Also
--
-- 'ExternalMemoryHandleTypeFlags',
-- 'Vulkan.Extensions.VK_KHR_external_memory_fd.ImportMemoryFdInfoKHR',
-- 'Vulkan.Extensions.VK_EXT_external_memory_host.ImportMemoryHostPointerInfoEXT',
-- 'Vulkan.Extensions.VK_KHR_external_memory_win32.ImportMemoryWin32HandleInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_memory_fd.MemoryGetFdInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_memory_win32.MemoryGetWin32HandleInfoKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceExternalBufferInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceExternalImageFormatInfo',
-- 'Vulkan.Extensions.VK_KHR_external_memory_fd.getMemoryFdPropertiesKHR',
-- 'Vulkan.Extensions.VK_EXT_external_memory_host.getMemoryHostPointerPropertiesEXT',
-- 'Vulkan.Extensions.VK_KHR_external_memory_win32.getMemoryWin32HandlePropertiesKHR'
newtype ExternalMemoryHandleTypeFlagBits = ExternalMemoryHandleTypeFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT' specifies a POSIX file
-- descriptor handle that has only limited valid usage outside of Vulkan
-- and other compatible APIs. It /must/ be compatible with the POSIX system
-- calls @dup@, @dup2@, @close@, and the non-standard system call @dup3@.
-- Additionally, it /must/ be transportable over a socket using an
-- @SCM_RIGHTS@ control message. It owns a reference to the underlying
-- memory resource represented by its Vulkan memory object.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT = ExternalMemoryHandleTypeFlagBits 0x00000001
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT' specifies an NT handle
-- that has only limited valid usage outside of Vulkan and other compatible
-- APIs. It /must/ be compatible with the functions @DuplicateHandle@,
-- @CloseHandle@, @CompareObjectHandles@, @GetHandleInformation@, and
-- @SetHandleInformation@. It owns a reference to the underlying memory
-- resource represented by its Vulkan memory object.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT = ExternalMemoryHandleTypeFlagBits 0x00000002
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT' specifies a global
-- share handle that has only limited valid usage outside of Vulkan and
-- other compatible APIs. It is not compatible with any native APIs. It
-- does not own a reference to the underlying memory resource represented
-- its Vulkan memory object, and will therefore become invalid when all
-- Vulkan memory objects associated with it are destroyed.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = ExternalMemoryHandleTypeFlagBits 0x00000004
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT' specifies an NT handle
-- returned by @IDXGIResource1@::@CreateSharedHandle@ referring to a
-- Direct3D 10 or 11 texture resource. It owns a reference to the memory
-- used by the Direct3D resource.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT = ExternalMemoryHandleTypeFlagBits 0x00000008
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT' specifies a global
-- share handle returned by @IDXGIResource@::@GetSharedHandle@ referring to
-- a Direct3D 10 or 11 texture resource. It does not own a reference to the
-- underlying Direct3D resource, and will therefore become invalid when all
-- Vulkan memory objects and Direct3D resources associated with it are
-- destroyed.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT = ExternalMemoryHandleTypeFlagBits 0x00000010
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT' specifies an NT handle
-- returned by @ID3D12Device@::@CreateSharedHandle@ referring to a Direct3D
-- 12 heap resource. It owns a reference to the resources used by the
-- Direct3D heap.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT = ExternalMemoryHandleTypeFlagBits 0x00000020
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT' specifies an NT handle
-- returned by @ID3D12Device@::@CreateSharedHandle@ referring to a Direct3D
-- 12 committed resource. It owns a reference to the memory used by the
-- Direct3D resource.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT = ExternalMemoryHandleTypeFlagBits 0x00000040
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT'
-- specifies a host pointer to /host mapped foreign memory/. It does not
-- own a reference to the underlying memory resource, and will therefore
-- become invalid if the foreign memory is unmapped or otherwise becomes no
-- longer available.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT = ExternalMemoryHandleTypeFlagBits 0x00000100
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT' specifies a host
-- pointer returned by a host memory allocation command. It does not own a
-- reference to the underlying memory resource, and will therefore become
-- invalid if the host memory is freed.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT = ExternalMemoryHandleTypeFlagBits 0x00000080
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID'
-- specifies an 'Vulkan.Extensions.WSITypes.AHardwareBuffer' object defined
-- by the Android NDK. See
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-external-android-hardware-buffer Android Hardware Buffers>
-- for more details of this handle type.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID = ExternalMemoryHandleTypeFlagBits 0x00000400
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT' is a file descriptor for a
-- Linux dma_buf. It owns a reference to the underlying memory resource
-- represented by its Vulkan memory object.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT = ExternalMemoryHandleTypeFlagBits 0x00000200

type ExternalMemoryHandleTypeFlags = ExternalMemoryHandleTypeFlagBits

instance Show ExternalMemoryHandleTypeFlagBits where
  showsPrec p = \case
    EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT -> showString "EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT"
    EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT -> showString "EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT"
    EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT -> showString "EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT"
    EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT -> showString "EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT"
    EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT -> showString "EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT"
    EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT -> showString "EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT"
    EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT -> showString "EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT"
    EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT -> showString "EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT"
    EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT -> showString "EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT"
    EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID -> showString "EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID"
    EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT -> showString "EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT"
    ExternalMemoryHandleTypeFlagBits x -> showParen (p >= 11) (showString "ExternalMemoryHandleTypeFlagBits 0x" . showHex x)

instance Read ExternalMemoryHandleTypeFlagBits where
  readPrec = parens (choose [("EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT", pure EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT)
                            , ("EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT", pure EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT)
                            , ("EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT", pure EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT)
                            , ("EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT", pure EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT)
                            , ("EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT", pure EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT)
                            , ("EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT", pure EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT)
                            , ("EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT", pure EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT)
                            , ("EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT", pure EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT)
                            , ("EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT", pure EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT)
                            , ("EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID", pure EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID)
                            , ("EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT", pure EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "ExternalMemoryHandleTypeFlagBits")
                       v <- step readPrec
                       pure (ExternalMemoryHandleTypeFlagBits v)))

