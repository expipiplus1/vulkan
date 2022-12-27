{-# language CPP #-}
-- No documentation found for Chapter "ExternalSemaphoreHandleTypeFlagBits"
module Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits  ( pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D11_FENCE_BIT
                                                                , ExternalSemaphoreHandleTypeFlags
                                                                , ExternalSemaphoreHandleTypeFlagBits( EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT
                                                                                                     , EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT
                                                                                                     , EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
                                                                                                     , EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT
                                                                                                     , EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT
                                                                                                     , EXTERNAL_SEMAPHORE_HANDLE_TYPE_ZIRCON_EVENT_BIT_FUCHSIA
                                                                                                     , ..
                                                                                                     )
                                                                ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
-- No documentation found for TopLevel "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D11_FENCE_BIT"
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D11_FENCE_BIT = EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT


type ExternalSemaphoreHandleTypeFlags = ExternalSemaphoreHandleTypeFlagBits

-- | VkExternalSemaphoreHandleTypeFlagBits - Bitmask of valid external
-- semaphore handle types
--
-- = Description
--
-- Note
--
-- Handles of type 'EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT' generated
-- by the implementation may represent either Linux Sync Files or Android
-- Fences at the implementation’s discretion. Applications /should/ only
-- use operations defined for both types of file descriptors, unless they
-- know via means external to Vulkan the type of the file descriptor, or
-- are prepared to deal with the system-defined operation failures
-- resulting from using the wrong type.
--
-- Some external semaphore handle types can only be shared within the same
-- underlying physical device and\/or the same driver version, as defined
-- in the following table:
--
-- +-----------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | Handle type                                               | 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceIDProperties'::@driverUUID@ | 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceIDProperties'::@deviceUUID@ |
-- +-----------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT'            | Must match                                                                                                 | Must match                                                                                                 |
-- +-----------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT'         | Must match                                                                                                 | Must match                                                                                                 |
-- +-----------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT'     | Must match                                                                                                 | Must match                                                                                                 |
-- +-----------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT'          | Must match                                                                                                 | Must match                                                                                                 |
-- +-----------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT'              | No restriction                                                                                             | No restriction                                                                                             |
-- +-----------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_SEMAPHORE_HANDLE_TYPE_ZIRCON_EVENT_BIT_FUCHSIA' | No restriction                                                                                             | No restriction                                                                                             |
-- +-----------------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
--
-- External semaphore handle types compatibility
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'ExternalSemaphoreHandleTypeFlags',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_fd.ImportSemaphoreFdInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_win32.ImportSemaphoreWin32HandleInfoKHR',
-- 'Vulkan.Extensions.VK_FUCHSIA_external_semaphore.ImportSemaphoreZirconHandleInfoFUCHSIA',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities.PhysicalDeviceExternalSemaphoreInfo',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_fd.SemaphoreGetFdInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_win32.SemaphoreGetWin32HandleInfoKHR',
-- 'Vulkan.Extensions.VK_FUCHSIA_external_semaphore.SemaphoreGetZirconHandleInfoFUCHSIA'
newtype ExternalSemaphoreHandleTypeFlagBits = ExternalSemaphoreHandleTypeFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT' specifies a POSIX file
-- descriptor handle that has only limited valid usage outside of Vulkan
-- and other compatible APIs. It /must/ be compatible with the POSIX system
-- calls @dup@, @dup2@, @close@, and the non-standard system call @dup3@.
-- Additionally, it /must/ be transportable over a socket using an
-- @SCM_RIGHTS@ control message. It owns a reference to the underlying
-- synchronization primitive represented by its Vulkan semaphore object.
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT = ExternalSemaphoreHandleTypeFlagBits 0x00000001

-- | 'EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT' specifies an NT handle
-- that has only limited valid usage outside of Vulkan and other compatible
-- APIs. It /must/ be compatible with the functions @DuplicateHandle@,
-- @CloseHandle@, @CompareObjectHandles@, @GetHandleInformation@, and
-- @SetHandleInformation@. It owns a reference to the underlying
-- synchronization primitive represented by its Vulkan semaphore object.
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT = ExternalSemaphoreHandleTypeFlagBits 0x00000002

-- | 'EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT' specifies a global
-- share handle that has only limited valid usage outside of Vulkan and
-- other compatible APIs. It is not compatible with any native APIs. It
-- does not own a reference to the underlying synchronization primitive
-- represented by its Vulkan semaphore object, and will therefore become
-- invalid when all Vulkan semaphore objects associated with it are
-- destroyed.
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = ExternalSemaphoreHandleTypeFlagBits 0x00000004

-- | 'EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT' specifies an NT handle
-- returned by @ID3D12Device@::@CreateSharedHandle@ referring to a Direct3D
-- 12 fence, or @ID3D11Device5@::'Vulkan.Core10.Fence.createFence'
-- referring to a Direct3D 11 fence. It owns a reference to the underlying
-- synchronization primitive associated with the Direct3D fence.
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT = ExternalSemaphoreHandleTypeFlagBits 0x00000008

-- | 'EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT' specifies a POSIX file
-- descriptor handle to a Linux Sync File or Android Fence object. It can
-- be used with any native API accepting a valid sync file or fence as
-- input. It owns a reference to the underlying synchronization primitive
-- associated with the file descriptor. Implementations which support
-- importing this handle type /must/ accept any type of sync or fence FD
-- supported by the native system they are running on.
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT = ExternalSemaphoreHandleTypeFlagBits 0x00000010

-- | 'EXTERNAL_SEMAPHORE_HANDLE_TYPE_ZIRCON_EVENT_BIT_FUCHSIA' specifies a
-- handle to a Zircon event object. It can be used with any native API that
-- accepts a Zircon event handle. Zircon event handles are created with
-- @ZX_RIGHTS_BASIC@ and @ZX_RIGHTS_SIGNAL@ rights. Vulkan on Fuchsia uses
-- only the ZX_EVENT_SIGNALED bit when signaling or waiting.
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_ZIRCON_EVENT_BIT_FUCHSIA = ExternalSemaphoreHandleTypeFlagBits 0x00000080

conNameExternalSemaphoreHandleTypeFlagBits :: String
conNameExternalSemaphoreHandleTypeFlagBits = "ExternalSemaphoreHandleTypeFlagBits"

enumPrefixExternalSemaphoreHandleTypeFlagBits :: String
enumPrefixExternalSemaphoreHandleTypeFlagBits = "EXTERNAL_SEMAPHORE_HANDLE_TYPE_"

showTableExternalSemaphoreHandleTypeFlagBits :: [(ExternalSemaphoreHandleTypeFlagBits, String)]
showTableExternalSemaphoreHandleTypeFlagBits =
  [
    ( EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT
    , "OPAQUE_FD_BIT"
    )
  ,
    ( EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT
    , "OPAQUE_WIN32_BIT"
    )
  ,
    ( EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
    , "OPAQUE_WIN32_KMT_BIT"
    )
  ,
    ( EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT
    , "D3D12_FENCE_BIT"
    )
  ,
    ( EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT
    , "SYNC_FD_BIT"
    )
  ,
    ( EXTERNAL_SEMAPHORE_HANDLE_TYPE_ZIRCON_EVENT_BIT_FUCHSIA
    , "ZIRCON_EVENT_BIT_FUCHSIA"
    )
  ]

instance Show ExternalSemaphoreHandleTypeFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixExternalSemaphoreHandleTypeFlagBits
      showTableExternalSemaphoreHandleTypeFlagBits
      conNameExternalSemaphoreHandleTypeFlagBits
      (\(ExternalSemaphoreHandleTypeFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ExternalSemaphoreHandleTypeFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixExternalSemaphoreHandleTypeFlagBits
      showTableExternalSemaphoreHandleTypeFlagBits
      conNameExternalSemaphoreHandleTypeFlagBits
      ExternalSemaphoreHandleTypeFlagBits
