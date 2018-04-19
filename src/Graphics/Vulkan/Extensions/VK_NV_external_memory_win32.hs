{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_external_memory_win32
  ( HANDLE
  , SECURITY_ATTRIBUTES
  , DWORD
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV
  , pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
  , pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  , vkGetMemoryWin32HandleNV
  , VkImportMemoryWin32HandleInfoNV(..)
  , VkExportMemoryWin32HandleInfoNV(..)
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.Core10.Memory
  ( VkDeviceMemory
  )
import Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBitsNV(..)
  , VkExternalMemoryHandleTypeFlagsNV
  )


-- No documentation found for TopLevel "HANDLE"
type HANDLE = Ptr ()
-- | Opaque data
data SECURITY_ATTRIBUTES
-- No documentation found for TopLevel "DWORD"
type DWORD = Word32
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV"
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV = VkStructureType 1000057000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV"
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV = VkStructureType 1000057001
-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION"
pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION :: Integral a => a
pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME"
pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = "VK_NV_external_memory_win32"
-- | vkGetMemoryWin32HandleNV - retrieve Win32 handle to a device memory
-- object
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that owns the memory.
--
-- -   @memory@ is the @VkDeviceMemory@ object.
--
-- -   @handleType@ is a bitmask of
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBitsNV'
--     containing a single bit specifying the type of handle requested.
--
-- -   @handle@ points to a Windows @HANDLE@ in which the handle is
--     returned.
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   @handleType@ /must/ be a flag specified in
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory.VkExportMemoryAllocateInfoNV'::@handleTypes@
--     when allocating @memory@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @memory@ /must/ be a valid @VkDeviceMemory@ handle
--
-- -   @handleType@ /must/ be a valid combination of
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBitsNV'
--     values
--
-- -   @handleType@ /must/ not be @0@
--
-- -   @pHandle@ /must/ be a valid pointer to a @HANDLE@ value
--
-- -   @memory@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_TOO_MANY_OBJECTS@
--
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagsNV'
foreign import ccall "vkGetMemoryWin32HandleNV" vkGetMemoryWin32HandleNV :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
-- | VkImportMemoryWin32HandleInfoNV - import Win32 memory created on the
-- same physical device
--
-- = Description
-- #_description#
--
-- If @handleType@ is @0@, this structure is ignored by consumers of the
-- 'Graphics.Vulkan.Core10.Memory.VkMemoryAllocateInfo' structure it is
-- chained from.
--
-- == Valid Usage
--
-- -   @handleType@ /must/ not have more than one bit set.
--
-- -   @handle@ /must/ be a valid handle to memory, obtained as specified
--     by @handleType@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV@
--
-- -   @handleType@ /must/ be a valid combination of
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBitsNV'
--     values
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagsNV',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkImportMemoryWin32HandleInfoNV = VkImportMemoryWin32HandleInfoNV
  { -- No documentation found for Nested "VkImportMemoryWin32HandleInfoNV" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImportMemoryWin32HandleInfoNV" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImportMemoryWin32HandleInfoNV" "vkHandleType"
  vkHandleType :: VkExternalMemoryHandleTypeFlagsNV
  , -- No documentation found for Nested "VkImportMemoryWin32HandleInfoNV" "vkHandle"
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
-- | VkExportMemoryWin32HandleInfoNV - specify security attributes and access
-- rights for Win32 memory handles
--
-- = Description
-- #_description#
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
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV@
--
-- -   If @pAttributes@ is not @NULL@, @pAttributes@ /must/ be a valid
--     pointer to a valid @SECURITY_ATTRIBUTES@ value
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkExportMemoryWin32HandleInfoNV = VkExportMemoryWin32HandleInfoNV
  { -- No documentation found for Nested "VkExportMemoryWin32HandleInfoNV" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExportMemoryWin32HandleInfoNV" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkExportMemoryWin32HandleInfoNV" "vkPAttributes"
  vkPAttributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "VkExportMemoryWin32HandleInfoNV" "vkDwAccess"
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
