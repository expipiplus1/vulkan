{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceHandleTypeFlagBits(..)
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT
  , VkExternalFenceFeatureFlagBits(..)
  , pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT
  , pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
  , vkGetPhysicalDeviceExternalFenceProperties
  , VkPhysicalDeviceExternalFenceInfo(..)
  , VkExternalFenceProperties(..)
  , VkExternalFenceHandleTypeFlags
  , VkExternalFenceFeatureFlags
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
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


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkPhysicalDevice
  )


-- ** VkExternalFenceHandleTypeFlagBits

-- | VkExternalFenceHandleTypeFlagBits - Bitmask of valid external fence
-- handle types
--
-- = Description
--
-- -   @VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT@ specifies a POSIX file
--     descriptor handle that has only limited valid usage outside of
--     Vulkan and other compatible APIs. It /must/ be compatible with the
--     POSIX system calls @dup@, @dup2@, @close@, and the non-standard
--     system call @dup3@. Additionally, it /must/ be transportable over a
--     socket using an @SCM_RIGHTS@ control message. It owns a reference to
--     the underlying synchronization primitive represented by its Vulkan
--     fence object.
--
-- -   @VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT@ specifies an NT
--     handle that has only limited valid usage outside of Vulkan and other
--     compatible APIs. It /must/ be compatible with the functions
--     @DuplicateHandle@, @CloseHandle@, @CompareObjectHandles@,
--     @GetHandleInformation@, and @SetHandleInformation@. It owns a
--     reference to the underlying synchronization primitive represented by
--     its Vulkan fence object.
--
-- -   @VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT@ specifies a
--     global share handle that has only limited valid usage outside of
--     Vulkan and other compatible APIs. It is not compatible with any
--     native APIs. It does not own a reference to the underlying
--     synchronization primitive represented by its Vulkan fence object,
--     and will therefore become invalid when all Vulkan fence objects
--     associated with it are destroyed.
--
-- -   @VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT@ specifies a POSIX file
--     descriptor handle to a Linux Sync File or Android Fence. It can be
--     used with any native API accepting a valid sync file or fence as
--     input. It owns a reference to the underlying synchronization
--     primitive associated with the file descriptor. Implementations which
--     support importing this handle type /must/ accept any type of sync or
--     fence FD supported by the native system they are running on.
--
-- Some external fence handle types can only be shared within the same
-- underlying physical device and\/or the same driver version, as defined
-- in the following table:
--
-- +------------------------------------------------------+----------------------------------------------+----------------------------------------------+
-- | Handle type                                          | @VkPhysicalDeviceIDProperties@::@driverUUID@ | @VkPhysicalDeviceIDProperties@::@deviceUUID@ |
-- +------------------------------------------------------+----------------------------------------------+----------------------------------------------+
-- | @VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT@        | Must match                                   | Must match                                   |
-- +------------------------------------------------------+----------------------------------------------+----------------------------------------------+
-- | @VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT@     | Must match                                   | Must match                                   |
-- +------------------------------------------------------+----------------------------------------------+----------------------------------------------+
-- | @VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT@ | Must match                                   | Must match                                   |
-- +------------------------------------------------------+----------------------------------------------+----------------------------------------------+
-- | @VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT@          | No restriction                               | No restriction                               |
-- +------------------------------------------------------+----------------------------------------------+----------------------------------------------+
--
-- External fence handle types compatibility
--
-- = See Also
--
-- 'VkExternalFenceHandleTypeFlags',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd.VkFenceGetFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32.VkFenceGetWin32HandleInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd.VkImportFenceFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32.VkImportFenceWin32HandleInfoKHR',
-- 'VkPhysicalDeviceExternalFenceInfo'
newtype VkExternalFenceHandleTypeFlagBits = VkExternalFenceHandleTypeFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkExternalFenceHandleTypeFlagBits where
  showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT"
  showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT"
  showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT"
  showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT"
  showsPrec p (VkExternalFenceHandleTypeFlagBits x) = showParen (p >= 11) (showString "VkExternalFenceHandleTypeFlagBits " . showsPrec 11 x)

instance Read VkExternalFenceHandleTypeFlagBits where
  readPrec = parens ( choose [ ("VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT",        pure VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT)
                             , ("VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT",     pure VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT)
                             , ("VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT", pure VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT)
                             , ("VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT",          pure VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalFenceHandleTypeFlagBits")
                        v <- step readPrec
                        pure (VkExternalFenceHandleTypeFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkExternalFenceHandleTypeFlagBits" "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT"
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT :: VkExternalFenceHandleTypeFlagBits
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT = VkExternalFenceHandleTypeFlagBits 0x00000001

-- No documentation found for Nested "VkExternalFenceHandleTypeFlagBits" "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT"
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT :: VkExternalFenceHandleTypeFlagBits
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT = VkExternalFenceHandleTypeFlagBits 0x00000002

-- No documentation found for Nested "VkExternalFenceHandleTypeFlagBits" "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT"
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT :: VkExternalFenceHandleTypeFlagBits
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = VkExternalFenceHandleTypeFlagBits 0x00000004

-- No documentation found for Nested "VkExternalFenceHandleTypeFlagBits" "VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT"
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT :: VkExternalFenceHandleTypeFlagBits
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT = VkExternalFenceHandleTypeFlagBits 0x00000008
-- ** VkExternalFenceFeatureFlagBits

-- | VkExternalFenceFeatureFlagBits - Bitfield describing features of an
-- external fence handle type
--
-- = See Also
--
-- 'VkExternalFenceFeatureFlags'
newtype VkExternalFenceFeatureFlagBits = VkExternalFenceFeatureFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkExternalFenceFeatureFlagBits where
  showsPrec _ VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT = showString "VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT"
  showsPrec _ VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT = showString "VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT"
  showsPrec p (VkExternalFenceFeatureFlagBits x) = showParen (p >= 11) (showString "VkExternalFenceFeatureFlagBits " . showsPrec 11 x)

instance Read VkExternalFenceFeatureFlagBits where
  readPrec = parens ( choose [ ("VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT", pure VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT)
                             , ("VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT", pure VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalFenceFeatureFlagBits")
                        v <- step readPrec
                        pure (VkExternalFenceFeatureFlagBits v)
                        )
                    )

-- | @VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT@ specifies handles of this
-- type /can/ be exported from Vulkan fence objects.
pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT :: VkExternalFenceFeatureFlagBits
pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT = VkExternalFenceFeatureFlagBits 0x00000001

-- | @VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT@ specifies handles of this
-- type /can/ be imported to Vulkan fence objects.
pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT :: VkExternalFenceFeatureFlagBits
pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT = VkExternalFenceFeatureFlagBits 0x00000002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO = VkStructureType 1000112000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES"
pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES = VkStructureType 1000112001
-- | vkGetPhysicalDeviceExternalFenceProperties - Function for querying
-- external fence handle capabilities.
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     fence capabilities.
--
-- -   @pExternalFenceInfo@ points to an instance of the
--     'VkPhysicalDeviceExternalFenceInfo' structure, describing the
--     parameters that would be consumed by
--     'Graphics.Vulkan.Core10.Fence.vkCreateFence'.
--
-- -   @pExternalFenceProperties@ points to an instance of the
--     'VkExternalFenceProperties' structure in which capabilities are
--     returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @pExternalFenceInfo@ /must/ be a valid pointer to a valid
--     @VkPhysicalDeviceExternalFenceInfo@ structure
--
-- -   @pExternalFenceProperties@ /must/ be a valid pointer to a
--     @VkExternalFenceProperties@ structure
--
-- = See Also
--
-- 'VkExternalFenceProperties',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'VkPhysicalDeviceExternalFenceInfo'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceExternalFenceProperties" vkGetPhysicalDeviceExternalFenceProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalFenceInfo" ::: Ptr VkPhysicalDeviceExternalFenceInfo) -> ("pExternalFenceProperties" ::: Ptr VkExternalFenceProperties) -> IO ()
-- | VkPhysicalDeviceExternalFenceInfo - Structure specifying fence creation
-- parameters.
--
-- = Description
--
-- __Note__
--
-- Handles of type @VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT@ generated by
-- the implementation may represent either Linux Sync Files or Android
-- Fences at the implementation’s discretion. Applications /should/ only
-- use operations defined for both types of file descriptors, unless they
-- know via means external to Vulkan the type of the file descriptor, or
-- are prepared to deal with the system-defined operation failures
-- resulting from using the wrong type.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @handleType@ /must/ be a valid 'VkExternalFenceHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- 'VkExternalFenceHandleTypeFlagBits',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkGetPhysicalDeviceExternalFenceProperties',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_capabilities.vkGetPhysicalDeviceExternalFencePropertiesKHR'
data VkPhysicalDeviceExternalFenceInfo = VkPhysicalDeviceExternalFenceInfo
  { -- | @sType@ is the type of this structure
  vkSType :: VkStructureType
  , -- | @pNext@ is NULL or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @handleType@ is a 'VkExternalFenceHandleTypeFlagBits' value indicating
  -- an external fence handle type for which capabilities will be returned.
  vkHandleType :: VkExternalFenceHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceExternalFenceInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceExternalFenceInfo <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceExternalFenceInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceExternalFenceInfo))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkPhysicalDeviceExternalFenceInfo))
-- | VkExternalFenceProperties - Structure describing supported external
-- fence handle features
--
-- = Description
--
-- If @handleType@ is not supported by the implementation, then
-- 'VkExternalFenceProperties'::@externalFenceFeatures@ will be set to
-- zero.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES@
--
-- -   @pNext@ /must/ be @NULL@
--
-- = See Also
--
-- 'VkExternalFenceFeatureFlags', 'VkExternalFenceHandleTypeFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkGetPhysicalDeviceExternalFenceProperties',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_capabilities.vkGetPhysicalDeviceExternalFencePropertiesKHR'
data VkExternalFenceProperties = VkExternalFenceProperties
  { -- No documentation found for Nested "VkExternalFenceProperties" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExternalFenceProperties" "pNext"
  vkPNext :: Ptr ()
  , -- | @exportFromImportedHandleTypes@ is a bitmask of
  -- 'VkExternalFenceHandleTypeFlagBits' indicating which types of imported
  -- handle @handleType@ /can/ be exported from.
  vkExportFromImportedHandleTypes :: VkExternalFenceHandleTypeFlags
  , -- | @compatibleHandleTypes@ is a bitmask of
  -- 'VkExternalFenceHandleTypeFlagBits' specifying handle types which /can/
  -- be specified at the same time as @handleType@ when creating a fence.
  vkCompatibleHandleTypes :: VkExternalFenceHandleTypeFlags
  , -- | @externalFenceFeatures@ is a bitmask of 'VkExternalFenceFeatureFlagBits'
  -- indicating the features of @handleType@.
  vkExternalFenceFeatures :: VkExternalFenceFeatureFlags
  }
  deriving (Eq, Show)

instance Storable VkExternalFenceProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkExternalFenceProperties <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 20)
                                       <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalFenceProperties))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalFenceProperties))
                *> poke (ptr `plusPtr` 16) (vkExportFromImportedHandleTypes (poked :: VkExternalFenceProperties))
                *> poke (ptr `plusPtr` 20) (vkCompatibleHandleTypes (poked :: VkExternalFenceProperties))
                *> poke (ptr `plusPtr` 24) (vkExternalFenceFeatures (poked :: VkExternalFenceProperties))
-- | VkExternalFenceHandleTypeFlags - Bitmask of
-- VkExternalFenceHandleTypeFlagBits
--
-- = Description
--
-- @VkExternalFenceHandleTypeFlags@ is a bitmask type for setting a mask of
-- zero or more 'VkExternalFenceHandleTypeFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence.VkExportFenceCreateInfo',
-- 'VkExternalFenceHandleTypeFlagBits', 'VkExternalFenceProperties'
type VkExternalFenceHandleTypeFlags = VkExternalFenceHandleTypeFlagBits
-- | VkExternalFenceFeatureFlags - Bitmask of VkExternalFenceFeatureFlagBits
--
-- = Description
--
-- @VkExternalFenceFeatureFlags@ is a bitmask type for setting a mask of
-- zero or more 'VkExternalFenceFeatureFlagBits'.
--
-- = See Also
--
-- 'VkExternalFenceFeatureFlagBits', 'VkExternalFenceProperties'
type VkExternalFenceFeatureFlags = VkExternalFenceFeatureFlagBits
