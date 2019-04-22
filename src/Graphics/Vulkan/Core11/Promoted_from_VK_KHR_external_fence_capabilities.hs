{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( ExternalFenceFeatureFlagBits
  , pattern EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT
  , pattern EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
  , ExternalFenceFeatureFlagBitsKHR
  , ExternalFenceFeatureFlags
  , ExternalFenceFeatureFlagsKHR
  , ExternalFenceHandleTypeFlagBits
  , pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT
  , ExternalFenceHandleTypeFlagBitsKHR
  , ExternalFenceHandleTypeFlags
  , ExternalFenceHandleTypeFlagsKHR
  , withCStructExternalFenceProperties
  , fromCStructExternalFenceProperties
  , ExternalFenceProperties(..)
  , withCStructPhysicalDeviceExternalFenceInfo
  , fromCStructPhysicalDeviceExternalFenceInfo
  , PhysicalDeviceExternalFenceInfo(..)
  , getPhysicalDeviceExternalFenceProperties
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
  , pattern STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
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
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceFeatureFlagBits(..)
  , VkExternalFenceHandleTypeFlagBits(..)
  , VkExternalFenceProperties(..)
  , VkPhysicalDeviceExternalFenceInfo(..)
  , vkGetPhysicalDeviceExternalFenceProperties
  , pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT
  , pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
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
  ( pattern STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
  )


-- | VkExternalFenceFeatureFlagBits - Bitfield describing features of an
-- external fence handle type
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceFeatureFlags'
type ExternalFenceFeatureFlagBits = VkExternalFenceFeatureFlagBits


{-# complete EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT, EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT :: ExternalFenceFeatureFlagBits #-}


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT'
-- specifies handles of this type /can/ be exported from Vulkan fence
-- objects.
pattern EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT :: (a ~ ExternalFenceFeatureFlagBits) => a
pattern EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT = VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT'
-- specifies handles of this type /can/ be imported to Vulkan fence
-- objects.
pattern EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT :: (a ~ ExternalFenceFeatureFlagBits) => a
pattern EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT = VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT

-- No documentation found for TopLevel "ExternalFenceFeatureFlagBitsKHR"
type ExternalFenceFeatureFlagBitsKHR = ExternalFenceFeatureFlagBits

-- | VkExternalFenceFeatureFlags - Bitmask of VkExternalFenceFeatureFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceFeatureFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceFeatureFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceFeatureFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceProperties'
type ExternalFenceFeatureFlags = ExternalFenceFeatureFlagBits

-- No documentation found for TopLevel "ExternalFenceFeatureFlagsKHR"
type ExternalFenceFeatureFlagsKHR = ExternalFenceFeatureFlags

-- | VkExternalFenceHandleTypeFlagBits - Bitmask of valid external fence
-- handle types
--
-- = Description
--
-- Some external fence handle types can only be shared within the same
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
-- > | VK_KHR_external_fenc |                      |                       |
-- > | e_capabilities.VK_EX |                      |                       |
-- > | TERNAL_FENCE_HANDLE_ |                      |                       |
-- > | TYPE_OPAQUE_FD_BIT'  |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_fenc |                      |                       |
-- > | e_capabilities.VK_EX |                      |                       |
-- > | TERNAL_FENCE_HANDLE_ |                      |                       |
-- > | TYPE_OPAQUE_WIN32_BI |                      |                       |
-- > | T'                   |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_fenc |                      |                       |
-- > | e_capabilities.VK_EX |                      |                       |
-- > | TERNAL_FENCE_HANDLE_ |                      |                       |
-- > | TYPE_OPAQUE_WIN32_KM |                      |                       |
-- > | T_BIT'               |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | No restriction       | No restriction        |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_fenc |                      |                       |
-- > | e_capabilities.VK_EX |                      |                       |
-- > | TERNAL_FENCE_HANDLE_ |                      |                       |
-- > | TYPE_SYNC_FD_BIT'    |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- >
-- > External fence handle types compatibility
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlags',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd.VkFenceGetFdInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.VkFenceGetWin32HandleInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd.VkImportFenceFdInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.VkImportFenceWin32HandleInfoKHR',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkPhysicalDeviceExternalFenceInfo'
type ExternalFenceHandleTypeFlagBits = VkExternalFenceHandleTypeFlagBits


{-# complete EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT, EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT, EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT, EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT :: ExternalFenceHandleTypeFlagBits #-}


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT'
-- specifies a POSIX file descriptor handle that has only limited valid
-- usage outside of Vulkan and other compatible APIs. It /must/ be
-- compatible with the POSIX system calls @dup@, @dup2@, @close@, and the
-- non-standard system call @dup3@. Additionally, it /must/ be
-- transportable over a socket using an @SCM_RIGHTS@ control message. It
-- owns a reference to the underlying synchronization primitive represented
-- by its Vulkan fence object.
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT :: (a ~ ExternalFenceHandleTypeFlagBits) => a
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT = VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT'
-- specifies an NT handle that has only limited valid usage outside of
-- Vulkan and other compatible APIs. It /must/ be compatible with the
-- functions @DuplicateHandle@, @CloseHandle@, @CompareObjectHandles@,
-- @GetHandleInformation@, and @SetHandleInformation@. It owns a reference
-- to the underlying synchronization primitive represented by its Vulkan
-- fence object.
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT :: (a ~ ExternalFenceHandleTypeFlagBits) => a
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT = VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT'
-- specifies a global share handle that has only limited valid usage
-- outside of Vulkan and other compatible APIs. It is not compatible with
-- any native APIs. It does not own a reference to the underlying
-- synchronization primitive represented by its Vulkan fence object, and
-- will therefore become invalid when all Vulkan fence objects associated
-- with it are destroyed.
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT :: (a ~ ExternalFenceHandleTypeFlagBits) => a
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT'
-- specifies a POSIX file descriptor handle to a Linux Sync File or Android
-- Fence. It can be used with any native API accepting a valid sync file or
-- fence as input. It owns a reference to the underlying synchronization
-- primitive associated with the file descriptor. Implementations which
-- support importing this handle type /must/ accept any type of sync or
-- fence FD supported by the native system they are running on.
pattern EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT :: (a ~ ExternalFenceHandleTypeFlagBits) => a
pattern EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT = VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT

-- No documentation found for TopLevel "ExternalFenceHandleTypeFlagBitsKHR"
type ExternalFenceHandleTypeFlagBitsKHR = ExternalFenceHandleTypeFlagBits

-- | VkExternalFenceHandleTypeFlags - Bitmask of
-- VkExternalFenceHandleTypeFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VkExportFenceCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceProperties'
type ExternalFenceHandleTypeFlags = ExternalFenceHandleTypeFlagBits

-- No documentation found for TopLevel "ExternalFenceHandleTypeFlagsKHR"
type ExternalFenceHandleTypeFlagsKHR = ExternalFenceHandleTypeFlags


-- | VkExternalFenceProperties - Structure describing supported external
-- fence handle features
--
-- = Description
--
-- If @handleType@ is not supported by the implementation, then
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceProperties'::@externalFenceFeatures@
-- will be set to zero.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceFeatureFlags',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.vkGetPhysicalDeviceExternalFenceProperties',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_capabilities.vkGetPhysicalDeviceExternalFencePropertiesKHR'
data ExternalFenceProperties = ExternalFenceProperties
  { -- Univalued member elided
  -- No documentation found for Nested "ExternalFenceProperties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalFenceProperties" "exportFromImportedHandleTypes"
  exportFromImportedHandleTypes :: ExternalFenceHandleTypeFlags
  , -- No documentation found for Nested "ExternalFenceProperties" "compatibleHandleTypes"
  compatibleHandleTypes :: ExternalFenceHandleTypeFlags
  , -- No documentation found for Nested "ExternalFenceProperties" "externalFenceFeatures"
  externalFenceFeatures :: ExternalFenceFeatureFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkExternalFenceProperties' and
-- marshal a 'ExternalFenceProperties' into it. The 'VkExternalFenceProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructExternalFenceProperties :: ExternalFenceProperties -> (VkExternalFenceProperties -> IO a) -> IO a
withCStructExternalFenceProperties marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ExternalFenceProperties)) (\pPNext -> cont (VkExternalFenceProperties VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES pPNext (exportFromImportedHandleTypes (marshalled :: ExternalFenceProperties)) (compatibleHandleTypes (marshalled :: ExternalFenceProperties)) (externalFenceFeatures (marshalled :: ExternalFenceProperties))))

-- | A function to read a 'VkExternalFenceProperties' and all additional
-- structures in the pointer chain into a 'ExternalFenceProperties'.
fromCStructExternalFenceProperties :: VkExternalFenceProperties -> IO ExternalFenceProperties
fromCStructExternalFenceProperties c = ExternalFenceProperties <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExternalFenceProperties)))
                                                               <*> pure (vkExportFromImportedHandleTypes (c :: VkExternalFenceProperties))
                                                               <*> pure (vkCompatibleHandleTypes (c :: VkExternalFenceProperties))
                                                               <*> pure (vkExternalFenceFeatures (c :: VkExternalFenceProperties))

instance Zero ExternalFenceProperties where
  zero = ExternalFenceProperties Nothing
                                 zero
                                 zero
                                 zero



-- | VkPhysicalDeviceExternalFenceInfo - Structure specifying fence creation
-- parameters.
--
-- = Description
--
-- __Note__
--
-- Handles of type
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT'
-- generated by the implementation may represent either Linux Sync Files or
-- Android Fences at the implementation’s discretion. Applications /should/
-- only use operations defined for both types of file descriptors, unless
-- they know via means external to Vulkan the type of the file descriptor,
-- or are prepared to deal with the system-defined operation failures
-- resulting from using the wrong type.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlagBits',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.vkGetPhysicalDeviceExternalFenceProperties',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_capabilities.vkGetPhysicalDeviceExternalFencePropertiesKHR'
data PhysicalDeviceExternalFenceInfo = PhysicalDeviceExternalFenceInfo
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceExternalFenceInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceExternalFenceInfo" "handleType"
  handleType :: ExternalFenceHandleTypeFlagBits
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceExternalFenceInfo' and
-- marshal a 'PhysicalDeviceExternalFenceInfo' into it. The 'VkPhysicalDeviceExternalFenceInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceExternalFenceInfo :: PhysicalDeviceExternalFenceInfo -> (VkPhysicalDeviceExternalFenceInfo -> IO a) -> IO a
withCStructPhysicalDeviceExternalFenceInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceExternalFenceInfo)) (\pPNext -> cont (VkPhysicalDeviceExternalFenceInfo VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO pPNext (handleType (marshalled :: PhysicalDeviceExternalFenceInfo))))

-- | A function to read a 'VkPhysicalDeviceExternalFenceInfo' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceExternalFenceInfo'.
fromCStructPhysicalDeviceExternalFenceInfo :: VkPhysicalDeviceExternalFenceInfo -> IO PhysicalDeviceExternalFenceInfo
fromCStructPhysicalDeviceExternalFenceInfo c = PhysicalDeviceExternalFenceInfo <$> -- Univalued Member elided
                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceExternalFenceInfo)))
                                                                               <*> pure (vkHandleType (c :: VkPhysicalDeviceExternalFenceInfo))

instance Zero PhysicalDeviceExternalFenceInfo where
  zero = PhysicalDeviceExternalFenceInfo Nothing
                                         zero



-- | vkGetPhysicalDeviceExternalFenceProperties - Function for querying
-- external fence handle capabilities.
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     fence capabilities.
--
-- -   @pExternalFenceInfo@ points to an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkPhysicalDeviceExternalFenceInfo'
--     structure, describing the parameters that would be consumed by
--     'Graphics.Vulkan.C.Core10.Fence.vkCreateFence'.
--
-- -   @pExternalFenceProperties@ points to an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceProperties'
--     structure in which capabilities are returned.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceProperties',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkPhysicalDeviceExternalFenceInfo'
getPhysicalDeviceExternalFenceProperties :: PhysicalDevice ->  PhysicalDeviceExternalFenceInfo ->  IO (ExternalFenceProperties)
getPhysicalDeviceExternalFenceProperties = \(PhysicalDevice physicalDevice' commandTable) -> \externalFenceInfo' -> alloca (\pExternalFenceProperties' -> (\marshalled -> withCStructPhysicalDeviceExternalFenceInfo marshalled . flip with) externalFenceInfo' (\pExternalFenceInfo' -> vkGetPhysicalDeviceExternalFenceProperties commandTable physicalDevice' pExternalFenceInfo' pExternalFenceProperties' *> ((fromCStructExternalFenceProperties <=< peek) pExternalFenceProperties')))
