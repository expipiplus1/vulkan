{-# language CPP #-}
-- No documentation found for Chapter "ExternalFenceHandleTypeFlagBits"
module Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits  ( ExternalFenceHandleTypeFlags
                                                            , ExternalFenceHandleTypeFlagBits( EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT
                                                                                             , EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT
                                                                                             , EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
                                                                                             , EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT
                                                                                             , ..
                                                                                             )
                                                            ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type ExternalFenceHandleTypeFlags = ExternalFenceHandleTypeFlagBits

-- | VkExternalFenceHandleTypeFlagBits - Bitmask of valid external fence
-- handle types
--
-- = Description
--
-- Some external fence handle types can only be shared within the same
-- underlying physical device and\/or the same driver version, as defined
-- in the following table:
--
-- +---------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | Handle type                                       | 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceIDProperties'::@driverUUID@ | 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceIDProperties'::@deviceUUID@ |
-- +---------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT'        | Must match                                                                                                 | Must match                                                                                                 |
-- +---------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT'     | Must match                                                                                                 | Must match                                                                                                 |
-- +---------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT' | Must match                                                                                                 | Must match                                                                                                 |
-- +---------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
-- | 'EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT'          | No restriction                                                                                             | No restriction                                                                                             |
-- +---------------------------------------------------+------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+
--
-- External fence handle types compatibility
--
-- = See Also
--
-- 'ExternalFenceHandleTypeFlags',
-- 'Vulkan.Extensions.VK_KHR_external_fence_fd.FenceGetFdInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_fence_win32.FenceGetWin32HandleInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_fence_fd.ImportFenceFdInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_fence_win32.ImportFenceWin32HandleInfoKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities.PhysicalDeviceExternalFenceInfo'
newtype ExternalFenceHandleTypeFlagBits = ExternalFenceHandleTypeFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT' specifies a POSIX file
-- descriptor handle that has only limited valid usage outside of Vulkan
-- and other compatible APIs. It /must/ be compatible with the POSIX system
-- calls @dup@, @dup2@, @close@, and the non-standard system call @dup3@.
-- Additionally, it /must/ be transportable over a socket using an
-- @SCM_RIGHTS@ control message. It owns a reference to the underlying
-- synchronization primitive represented by its Vulkan fence object.
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT        = ExternalFenceHandleTypeFlagBits 0x00000001
-- | 'EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT' specifies an NT handle
-- that has only limited valid usage outside of Vulkan and other compatible
-- APIs. It /must/ be compatible with the functions @DuplicateHandle@,
-- @CloseHandle@, @CompareObjectHandles@, @GetHandleInformation@, and
-- @SetHandleInformation@. It owns a reference to the underlying
-- synchronization primitive represented by its Vulkan fence object.
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT     = ExternalFenceHandleTypeFlagBits 0x00000002
-- | 'EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT' specifies a global
-- share handle that has only limited valid usage outside of Vulkan and
-- other compatible APIs. It is not compatible with any native APIs. It
-- does not own a reference to the underlying synchronization primitive
-- represented by its Vulkan fence object, and will therefore become
-- invalid when all Vulkan fence objects associated with it are destroyed.
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = ExternalFenceHandleTypeFlagBits 0x00000004
-- | 'EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT' specifies a POSIX file
-- descriptor handle to a Linux Sync File or Android Fence. It can be used
-- with any native API accepting a valid sync file or fence as input. It
-- owns a reference to the underlying synchronization primitive associated
-- with the file descriptor. Implementations which support importing this
-- handle type /must/ accept any type of sync or fence FD supported by the
-- native system they are running on.
pattern EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT          = ExternalFenceHandleTypeFlagBits 0x00000008

conNameExternalFenceHandleTypeFlagBits :: String
conNameExternalFenceHandleTypeFlagBits = "ExternalFenceHandleTypeFlagBits"

enumPrefixExternalFenceHandleTypeFlagBits :: String
enumPrefixExternalFenceHandleTypeFlagBits = "EXTERNAL_FENCE_HANDLE_TYPE_"

showTableExternalFenceHandleTypeFlagBits :: [(ExternalFenceHandleTypeFlagBits, String)]
showTableExternalFenceHandleTypeFlagBits =
  [ (EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT       , "OPAQUE_FD_BIT")
  , (EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT    , "OPAQUE_WIN32_BIT")
  , (EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT, "OPAQUE_WIN32_KMT_BIT")
  , (EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT         , "SYNC_FD_BIT")
  ]

instance Show ExternalFenceHandleTypeFlagBits where
  showsPrec = enumShowsPrec enumPrefixExternalFenceHandleTypeFlagBits
                            showTableExternalFenceHandleTypeFlagBits
                            conNameExternalFenceHandleTypeFlagBits
                            (\(ExternalFenceHandleTypeFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read ExternalFenceHandleTypeFlagBits where
  readPrec = enumReadPrec enumPrefixExternalFenceHandleTypeFlagBits
                          showTableExternalFenceHandleTypeFlagBits
                          conNameExternalFenceHandleTypeFlagBits
                          ExternalFenceHandleTypeFlagBits

