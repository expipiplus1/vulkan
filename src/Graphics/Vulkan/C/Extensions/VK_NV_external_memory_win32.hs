{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( DWORD
  , HANDLE
  , SECURITY_ATTRIBUTES
  , VkExportMemoryWin32HandleInfoNV(..)
  , VkImportMemoryWin32HandleInfoNV(..)
  , FN_vkGetMemoryWin32HandleNV
  , PFN_vkGetMemoryWin32HandleNV
  , vkGetMemoryWin32HandleNV
  , pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  , pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.C.Core10.Memory
  ( VkDeviceMemory
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBitsNV(..)
  , VkExternalMemoryHandleTypeFlagsNV
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "DWORD"
type DWORD = Word32
  

-- No documentation found for TopLevel "HANDLE"
type HANDLE = Ptr ()
  

-- | Opaque data
data SECURITY_ATTRIBUTES

-- | VkExportMemoryWin32HandleInfoNV - specify security attributes and access
-- rights for Win32 memory handles
--
-- = Description
--
-- If this structure is not present, or if @pAttributes@ is set to @NULL@,
-- default security descriptor values will be used, and child processes
-- created by the application will not inherit the handle, as described in
-- the MSDN documentation for “Synchronization Object Security and Access
-- Rights”[1]. Further, if the structure is not present, the access rights
-- will be
--
-- > DXGI_SHARED_RESOURCE_READ | DXGI_SHARED_RESOURCE_WRITE
--
-- [1]
-- <https://msdn.microsoft.com/en-us/library/windows/desktop/ms686670.aspx>
--
-- Unresolved directive in VkExportMemoryWin32HandleInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkExportMemoryWin32HandleInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkExportMemoryWin32HandleInfoNV = VkExportMemoryWin32HandleInfoNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @pAttributes@ is a pointer to a Windows 'SECURITY_ATTRIBUTES' structure
  -- specifying security attributes of the handle.
  vkPAttributes :: Ptr SECURITY_ATTRIBUTES
  , -- | @dwAccess@ is a 'DWORD' specifying access rights of the handle.
  vkDwAccess :: DWORD
  }
  deriving (Eq, Show)

instance Storable VkExportMemoryWin32HandleInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkExportMemoryWin32HandleInfoNV <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportMemoryWin32HandleInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExportMemoryWin32HandleInfoNV))
                *> poke (ptr `plusPtr` 16) (vkPAttributes (poked :: VkExportMemoryWin32HandleInfoNV))
                *> poke (ptr `plusPtr` 24) (vkDwAccess (poked :: VkExportMemoryWin32HandleInfoNV))

instance Zero VkExportMemoryWin32HandleInfoNV where
  zero = VkExportMemoryWin32HandleInfoNV VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV
                                         zero
                                         zero
                                         zero

-- | VkImportMemoryWin32HandleInfoNV - import Win32 memory created on the
-- same physical device
--
-- = Description
--
-- If @handleType@ is @0@, this structure is ignored by consumers of the
-- 'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo' structure it is
-- chained from.
--
-- == Valid Usage
--
-- Unresolved directive in VkImportMemoryWin32HandleInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkImportMemoryWin32HandleInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkImportMemoryWin32HandleInfoNV = VkImportMemoryWin32HandleInfoNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @handleType@ /must/ not have more than one bit set.
  vkHandleType :: VkExternalMemoryHandleTypeFlagsNV
  , -- | @handle@ /must/ be a valid handle to memory, obtained as specified by
  -- @handleType@.
  vkHandle :: HANDLE
  }
  deriving (Eq, Show)

instance Storable VkImportMemoryWin32HandleInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkImportMemoryWin32HandleInfoNV <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportMemoryWin32HandleInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportMemoryWin32HandleInfoNV))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkImportMemoryWin32HandleInfoNV))
                *> poke (ptr `plusPtr` 24) (vkHandle (poked :: VkImportMemoryWin32HandleInfoNV))

instance Zero VkImportMemoryWin32HandleInfoNV where
  zero = VkImportMemoryWin32HandleInfoNV VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV
                                         zero
                                         zero
                                         zero

-- | vkGetMemoryWin32HandleNV - retrieve Win32 handle to a device memory
-- object
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the memory.
--
-- -   @memory@ is the 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory'
--     object.
--
-- -   @handleType@ is a bitmask of
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBitsNV'
--     containing a single bit specifying the type of handle requested.
--
-- -   @handle@ points to a Windows 'HANDLE' in which the handle is
--     returned.
--
-- == Valid Usage
--
-- Unresolved directive in vkGetMemoryWin32HandleNV.txt -
-- include::{generated}\/validity\/protos\/vkGetMemoryWin32HandleNV.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetMemoryWin32HandleNV" vkGetMemoryWin32HandleNV :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
#else
vkGetMemoryWin32HandleNV :: DeviceCmds -> ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
vkGetMemoryWin32HandleNV deviceCmds = mkVkGetMemoryWin32HandleNV (pVkGetMemoryWin32HandleNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryWin32HandleNV
  :: FunPtr (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult) -> (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult)
#endif

type FN_vkGetMemoryWin32HandleNV = ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
type PFN_vkGetMemoryWin32HandleNV = FunPtr FN_vkGetMemoryWin32HandleNV

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME"
pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = "VK_NV_external_memory_win32"

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION"
pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION :: Integral a => a
pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV"
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV = VkStructureType 1000057001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV"
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV = VkStructureType 1000057000
