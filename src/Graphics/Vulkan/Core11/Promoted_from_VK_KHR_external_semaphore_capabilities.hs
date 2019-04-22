{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( ExternalSemaphoreFeatureFlagBits
  , pattern EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT
  , pattern EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT
  , ExternalSemaphoreFeatureFlagBitsKHR
  , ExternalSemaphoreFeatureFlags
  , ExternalSemaphoreFeatureFlagsKHR
  , ExternalSemaphoreHandleTypeFlagBits
  , pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT
  , pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT
  , ExternalSemaphoreHandleTypeFlagBitsKHR
  , ExternalSemaphoreHandleTypeFlags
  , ExternalSemaphoreHandleTypeFlagsKHR
  , withCStructExternalSemaphoreProperties
  , fromCStructExternalSemaphoreProperties
  , ExternalSemaphoreProperties(..)
  , withCStructPhysicalDeviceExternalSemaphoreInfo
  , fromCStructPhysicalDeviceExternalSemaphoreInfo
  , PhysicalDeviceExternalSemaphoreInfo(..)
  , getPhysicalDeviceExternalSemaphoreProperties
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO
  , pattern STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES
  ) where

import Control.Monad
  ( (<=<)
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
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreFeatureFlagBits(..)
  , VkExternalSemaphoreHandleTypeFlagBits(..)
  , VkExternalSemaphoreProperties(..)
  , VkPhysicalDeviceExternalSemaphoreInfo(..)
  , vkGetPhysicalDeviceExternalSemaphoreProperties
  , pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO
  )


-- | VkExternalSemaphoreFeatureFlagBits - Bitfield describing features of an
-- external semaphore handle type
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreFeatureFlags'
type ExternalSemaphoreFeatureFlagBits = VkExternalSemaphoreFeatureFlagBits


{-# complete EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT, EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT :: ExternalSemaphoreFeatureFlagBits #-}


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT'
-- specifies that handles of this type /can/ be exported from Vulkan
-- semaphore objects.
pattern EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT :: (a ~ ExternalSemaphoreFeatureFlagBits) => a
pattern EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT = VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT'
-- specifies that handles of this type /can/ be imported as Vulkan
-- semaphore objects.
pattern EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT :: (a ~ ExternalSemaphoreFeatureFlagBits) => a
pattern EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT = VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT

-- No documentation found for TopLevel "ExternalSemaphoreFeatureFlagBitsKHR"
type ExternalSemaphoreFeatureFlagBitsKHR = ExternalSemaphoreFeatureFlagBits

-- | VkExternalSemaphoreFeatureFlags - Bitmask of
-- VkExternalSemaphoreFeatureFlagBitsKHR
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreFeatureFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreFeatureFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreFeatureFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreProperties'
type ExternalSemaphoreFeatureFlags = ExternalSemaphoreFeatureFlagBits

-- No documentation found for TopLevel "ExternalSemaphoreFeatureFlagsKHR"
type ExternalSemaphoreFeatureFlagsKHR = ExternalSemaphoreFeatureFlags

-- | VkExternalSemaphoreHandleTypeFlagBits - Bitmask of valid external
-- semaphore handle types
--
-- = Description
--
-- __Note__
--
-- Handles of type
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT'
-- generated by the implementation may represent either Linux Sync Files or
-- Android Fences at the implementation’s discretion. Applications /should/
-- only use operations defined for both types of file descriptors, unless
-- they know via means external to Vulkan the type of the file descriptor,
-- or are prepared to deal with the system-defined operation failures
-- resulting from using the wrong type.
--
-- Some external semaphore handle types can only be shared within the same
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
-- > | VK_KHR_external_sema |                      |                       |
-- > | phore_capabilities.V |                      |                       |
-- > | K_EXTERNAL_SEMAPHORE |                      |                       |
-- > | _HANDLE_TYPE_OPAQUE_ |                      |                       |
-- > | FD_BIT'              |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_sema |                      |                       |
-- > | phore_capabilities.V |                      |                       |
-- > | K_EXTERNAL_SEMAPHORE |                      |                       |
-- > | _HANDLE_TYPE_OPAQUE_ |                      |                       |
-- > | WIN32_BIT'           |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_sema |                      |                       |
-- > | phore_capabilities.V |                      |                       |
-- > | K_EXTERNAL_SEMAPHORE |                      |                       |
-- > | _HANDLE_TYPE_OPAQUE_ |                      |                       |
-- > | WIN32_KMT_BIT'       |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_sema |                      |                       |
-- > | phore_capabilities.V |                      |                       |
-- > | K_EXTERNAL_SEMAPHORE |                      |                       |
-- > | _HANDLE_TYPE_D3D12_F |                      |                       |
-- > | ENCE_BIT'            |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | No restriction       | No restriction        |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_sema |                      |                       |
-- > | phore_capabilities.V |                      |                       |
-- > | K_EXTERNAL_SEMAPHORE |                      |                       |
-- > | _HANDLE_TYPE_SYNC_FD |                      |                       |
-- > | _BIT'                |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- >
-- > External semaphore handle types compatibility
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlags',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd.VkImportSemaphoreFdInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32.VkImportSemaphoreWin32HandleInfoKHR',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkPhysicalDeviceExternalSemaphoreInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd.VkSemaphoreGetFdInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32.VkSemaphoreGetWin32HandleInfoKHR'
type ExternalSemaphoreHandleTypeFlagBits = VkExternalSemaphoreHandleTypeFlagBits


{-# complete EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT, EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT, EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT, EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT, EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT :: ExternalSemaphoreHandleTypeFlagBits #-}


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT'
-- specifies a POSIX file descriptor handle that has only limited valid
-- usage outside of Vulkan and other compatible APIs. It /must/ be
-- compatible with the POSIX system calls @dup@, @dup2@, @close@, and the
-- non-standard system call @dup3@. Additionally, it /must/ be
-- transportable over a socket using an @SCM_RIGHTS@ control message. It
-- owns a reference to the underlying synchronization primitive represented
-- by its Vulkan semaphore object.
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT :: (a ~ ExternalSemaphoreHandleTypeFlagBits) => a
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT'
-- specifies an NT handle that has only limited valid usage outside of
-- Vulkan and other compatible APIs. It /must/ be compatible with the
-- functions @DuplicateHandle@, @CloseHandle@, @CompareObjectHandles@,
-- @GetHandleInformation@, and @SetHandleInformation@. It owns a reference
-- to the underlying synchronization primitive represented by its Vulkan
-- semaphore object.
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT :: (a ~ ExternalSemaphoreHandleTypeFlagBits) => a
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT'
-- specifies a global share handle that has only limited valid usage
-- outside of Vulkan and other compatible APIs. It is not compatible with
-- any native APIs. It does not own a reference to the underlying
-- synchronization primitive represented its Vulkan semaphore object, and
-- will therefore become invalid when all Vulkan semaphore objects
-- associated with it are destroyed.
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT :: (a ~ ExternalSemaphoreHandleTypeFlagBits) => a
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT'
-- specifies an NT handle returned by @ID3D12Device@::@CreateSharedHandle@
-- referring to a Direct3D 12 fence. It owns a reference to the underlying
-- synchronization primitive associated with the Direct3D fence.
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT :: (a ~ ExternalSemaphoreHandleTypeFlagBits) => a
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT'
-- specifies a POSIX file descriptor handle to a Linux Sync File or Android
-- Fence object. It can be used with any native API accepting a valid sync
-- file or fence as input. It owns a reference to the underlying
-- synchronization primitive associated with the file descriptor.
-- Implementations which support importing this handle type /must/ accept
-- any type of sync or fence FD supported by the native system they are
-- running on.
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT :: (a ~ ExternalSemaphoreHandleTypeFlagBits) => a
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT

-- No documentation found for TopLevel "ExternalSemaphoreHandleTypeFlagBitsKHR"
type ExternalSemaphoreHandleTypeFlagBitsKHR = ExternalSemaphoreHandleTypeFlagBits

-- | VkExternalSemaphoreHandleTypeFlags - Bitmask of
-- VkExternalSemaphoreHandleTypeFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore.VkExportSemaphoreCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreProperties'
type ExternalSemaphoreHandleTypeFlags = ExternalSemaphoreHandleTypeFlagBits

-- No documentation found for TopLevel "ExternalSemaphoreHandleTypeFlagsKHR"
type ExternalSemaphoreHandleTypeFlagsKHR = ExternalSemaphoreHandleTypeFlags


-- | VkExternalSemaphoreProperties - Structure describing supported external
-- semaphore handle features
--
-- = Description
--
-- If @handleType@ is not supported by the implementation, then
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreProperties'::@externalSemaphoreFeatures@
-- will be set to zero.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreFeatureFlags',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.vkGetPhysicalDeviceExternalSemaphoreProperties',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_capabilities.vkGetPhysicalDeviceExternalSemaphorePropertiesKHR'
data ExternalSemaphoreProperties = ExternalSemaphoreProperties
  { -- Univalued member elided
  -- No documentation found for Nested "ExternalSemaphoreProperties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalSemaphoreProperties" "exportFromImportedHandleTypes"
  exportFromImportedHandleTypes :: ExternalSemaphoreHandleTypeFlags
  , -- No documentation found for Nested "ExternalSemaphoreProperties" "compatibleHandleTypes"
  compatibleHandleTypes :: ExternalSemaphoreHandleTypeFlags
  , -- No documentation found for Nested "ExternalSemaphoreProperties" "externalSemaphoreFeatures"
  externalSemaphoreFeatures :: ExternalSemaphoreFeatureFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkExternalSemaphoreProperties' and
-- marshal a 'ExternalSemaphoreProperties' into it. The 'VkExternalSemaphoreProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructExternalSemaphoreProperties :: ExternalSemaphoreProperties -> (VkExternalSemaphoreProperties -> IO a) -> IO a
withCStructExternalSemaphoreProperties marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ExternalSemaphoreProperties)) (\pPNext -> cont (VkExternalSemaphoreProperties VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES pPNext (exportFromImportedHandleTypes (marshalled :: ExternalSemaphoreProperties)) (compatibleHandleTypes (marshalled :: ExternalSemaphoreProperties)) (externalSemaphoreFeatures (marshalled :: ExternalSemaphoreProperties))))

-- | A function to read a 'VkExternalSemaphoreProperties' and all additional
-- structures in the pointer chain into a 'ExternalSemaphoreProperties'.
fromCStructExternalSemaphoreProperties :: VkExternalSemaphoreProperties -> IO ExternalSemaphoreProperties
fromCStructExternalSemaphoreProperties c = ExternalSemaphoreProperties <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExternalSemaphoreProperties)))
                                                                       <*> pure (vkExportFromImportedHandleTypes (c :: VkExternalSemaphoreProperties))
                                                                       <*> pure (vkCompatibleHandleTypes (c :: VkExternalSemaphoreProperties))
                                                                       <*> pure (vkExternalSemaphoreFeatures (c :: VkExternalSemaphoreProperties))

instance Zero ExternalSemaphoreProperties where
  zero = ExternalSemaphoreProperties Nothing
                                     zero
                                     zero
                                     zero



-- | VkPhysicalDeviceExternalSemaphoreInfo - Structure specifying semaphore
-- creation parameters.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlagBits',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.vkGetPhysicalDeviceExternalSemaphoreProperties',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_capabilities.vkGetPhysicalDeviceExternalSemaphorePropertiesKHR'
data PhysicalDeviceExternalSemaphoreInfo = PhysicalDeviceExternalSemaphoreInfo
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceExternalSemaphoreInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceExternalSemaphoreInfo" "handleType"
  handleType :: ExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceExternalSemaphoreInfo' and
-- marshal a 'PhysicalDeviceExternalSemaphoreInfo' into it. The 'VkPhysicalDeviceExternalSemaphoreInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceExternalSemaphoreInfo :: PhysicalDeviceExternalSemaphoreInfo -> (VkPhysicalDeviceExternalSemaphoreInfo -> IO a) -> IO a
withCStructPhysicalDeviceExternalSemaphoreInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceExternalSemaphoreInfo)) (\pPNext -> cont (VkPhysicalDeviceExternalSemaphoreInfo VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO pPNext (handleType (marshalled :: PhysicalDeviceExternalSemaphoreInfo))))

-- | A function to read a 'VkPhysicalDeviceExternalSemaphoreInfo' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceExternalSemaphoreInfo'.
fromCStructPhysicalDeviceExternalSemaphoreInfo :: VkPhysicalDeviceExternalSemaphoreInfo -> IO PhysicalDeviceExternalSemaphoreInfo
fromCStructPhysicalDeviceExternalSemaphoreInfo c = PhysicalDeviceExternalSemaphoreInfo <$> -- Univalued Member elided
                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceExternalSemaphoreInfo)))
                                                                                       <*> pure (vkHandleType (c :: VkPhysicalDeviceExternalSemaphoreInfo))

instance Zero PhysicalDeviceExternalSemaphoreInfo where
  zero = PhysicalDeviceExternalSemaphoreInfo Nothing
                                             zero



-- | vkGetPhysicalDeviceExternalSemaphoreProperties - Function for querying
-- external semaphore handle capabilities.
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     semaphore capabilities.
--
-- -   @pExternalSemaphoreInfo@ points to an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkPhysicalDeviceExternalSemaphoreInfo'
--     structure, describing the parameters that would be consumed by
--     'Graphics.Vulkan.C.Core10.QueueSemaphore.vkCreateSemaphore'.
--
-- -   @pExternalSemaphoreProperties@ points to an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreProperties'
--     structure in which capabilities are returned.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreProperties',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkPhysicalDeviceExternalSemaphoreInfo'
getPhysicalDeviceExternalSemaphoreProperties :: PhysicalDevice ->  PhysicalDeviceExternalSemaphoreInfo ->  IO (ExternalSemaphoreProperties)
getPhysicalDeviceExternalSemaphoreProperties = \(PhysicalDevice physicalDevice' commandTable) -> \externalSemaphoreInfo' -> alloca (\pExternalSemaphoreProperties' -> (\marshalled -> withCStructPhysicalDeviceExternalSemaphoreInfo marshalled . flip with) externalSemaphoreInfo' (\pExternalSemaphoreInfo' -> vkGetPhysicalDeviceExternalSemaphoreProperties commandTable physicalDevice' pExternalSemaphoreInfo' pExternalSemaphoreProperties' *> ((fromCStructExternalSemaphoreProperties <=< peek) pExternalSemaphoreProperties')))
