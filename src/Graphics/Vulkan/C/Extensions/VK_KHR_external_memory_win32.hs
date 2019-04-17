{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( LPCWSTR
  , VkExportMemoryWin32HandleInfoKHR(..)
  , VkImportMemoryWin32HandleInfoKHR(..)
  , VkMemoryGetWin32HandleInfoKHR(..)
  , VkMemoryWin32HandlePropertiesKHR(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetMemoryWin32HandleKHR
#endif
  , FN_vkGetMemoryWin32HandleKHR
  , PFN_vkGetMemoryWin32HandleKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetMemoryWin32HandlePropertiesKHR
#endif
  , FN_vkGetMemoryWin32HandlePropertiesKHR
  , PFN_vkGetMemoryWin32HandlePropertiesKHR
  , pattern VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CWchar
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
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBits(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( DWORD
  , HANDLE
  , SECURITY_ATTRIBUTES
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "LPCWSTR"
type LPCWSTR = Ptr CWchar
  
-- | VkExportMemoryWin32HandleInfoKHR - Structure specifying additional
-- attributes of Windows handles exported from a memory
--
-- = Description
--
-- If this structure is not present, or if @pAttributes@ is set to @NULL@,
-- default security descriptor values will be used, and child processes
-- created by the application will not inherit the handle, as described in
-- the MSDN documentation for “Synchronization Object Security and Access
-- Rights”1. Further, if the structure is not present, the access rights
-- will be
--
-- @DXGI_SHARED_RESOURCE_READ@ | @DXGI_SHARED_RESOURCE_WRITE@
--
-- for handles of the following types:
--
-- @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT@
-- @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT@
--
-- And
--
-- @GENERIC_ALL@
--
-- for handles of the following types:
--
-- @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT@
-- @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT@
--
-- [1]
--     <https://msdn.microsoft.com/en-us/library/windows/desktop/ms686670.aspx>
--
-- == Valid Usage
--
-- -   If
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExportMemoryAllocateInfo'::@handleTypes@
--     does not include @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT@,
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT@,
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT@, or
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT@,
--     @VkExportMemoryWin32HandleInfoKHR@ /must/ not be in the @pNext@
--     chain of 'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR@
--
-- -   If @pAttributes@ is not @NULL@, @pAttributes@ /must/ be a valid
--     pointer to a valid @SECURITY_ATTRIBUTES@ value
--
-- = See Also
--
-- No cross-references are available
data VkExportMemoryWin32HandleInfoKHR = VkExportMemoryWin32HandleInfoKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @pAttributes@ is a pointer to a Windows @SECURITY_ATTRIBUTES@ structure
  -- specifying security attributes of the handle.
  vkPAttributes :: Ptr SECURITY_ATTRIBUTES
  , -- | @dwAccess@ is a @DWORD@ specifying access rights of the handle.
  vkDwAccess :: DWORD
  , -- | @name@ is a NULL-terminated UTF-16 string to associate with the
  -- underlying resource referenced by NT handles exported from the created
  -- memory.
  vkName :: LPCWSTR
  }
  deriving (Eq, Show)

instance Storable VkExportMemoryWin32HandleInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkExportMemoryWin32HandleInfoKHR <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 24)
                                              <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkPAttributes (poked :: VkExportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkDwAccess (poked :: VkExportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkName (poked :: VkExportMemoryWin32HandleInfoKHR))

instance Zero VkExportMemoryWin32HandleInfoKHR where
  zero = VkExportMemoryWin32HandleInfoKHR zero
                                          zero
                                          zero
                                          zero
                                          zero
-- | VkImportMemoryWin32HandleInfoKHR - import Win32 memory created on the
-- same physical device
--
-- = Description
--
-- Importing memory objects from Windows handles does not transfer
-- ownership of the handle to the Vulkan implementation. For handle types
-- defined as NT handles, the application /must/ release ownership using
-- the @CloseHandle@ system call when the handle is no longer needed.
--
-- Applications /can/ import the same underlying memory into multiple
-- instances of Vulkan, into the same instance from which it was exported,
-- and multiple times into a given Vulkan instance. In all cases, each
-- import operation /must/ create a distinct @VkDeviceMemory@ object.
--
-- == Valid Usage
--
-- -   If @handleType@ is not @0@, it /must/ be supported for import, as
--     reported by
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalImageFormatProperties'
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalBufferProperties'.
--
-- -   The memory from which @handle@ was exported, or the memory named by
--     @name@ /must/ have been created on the same underlying physical
--     device as @device@.
--
-- -   If @handleType@ is not @0@, it /must/ be defined as an NT handle or
--     a global share handle.
--
-- -   If @handleType@ is not
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT@,
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT@,
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT@, or
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT@, @name@ /must/
--     be @NULL@.
--
-- -   If @handleType@ is not @0@ and @handle@ is @NULL@, @name@ /must/
--     name a valid memory resource of the type specified by @handleType@.
--
-- -   If @handleType@ is not @0@ and @name@ is @NULL@, @handle@ /must/ be
--     a valid handle of the type specified by @handleType@.
--
-- -   if @handle@ is not @NULL@, @name@ must be @NULL@.
--
-- -   If @handle@ is not @NULL@, it /must/ obey any requirements listed
--     for @handleType@ in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#external-memory-handle-types-compatibility external memory handle types compatibility>.
--
-- -   If @name@ is not @NULL@, it /must/ obey any requirements listed for
--     @handleType@ in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#external-memory-handle-types-compatibility external memory handle types compatibility>.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR@
--
-- -   If @handleType@ is not @0@, @handleType@ /must/ be a valid
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- No cross-references are available
data VkImportMemoryWin32HandleInfoKHR = VkImportMemoryWin32HandleInfoKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @handleType@ specifies the type of @handle@ or @name@.
  vkHandleType :: VkExternalMemoryHandleTypeFlagBits
  , -- | @handle@ is the external handle to import, or @NULL@.
  vkHandle :: HANDLE
  , -- | @name@ is a NULL-terminated UTF-16 string naming the underlying memory
  -- resource to import, or @NULL@.
  vkName :: LPCWSTR
  }
  deriving (Eq, Show)

instance Storable VkImportMemoryWin32HandleInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkImportMemoryWin32HandleInfoKHR <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 24)
                                              <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkImportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHandle (poked :: VkImportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkName (poked :: VkImportMemoryWin32HandleInfoKHR))

instance Zero VkImportMemoryWin32HandleInfoKHR where
  zero = VkImportMemoryWin32HandleInfoKHR zero
                                          zero
                                          zero
                                          zero
                                          zero
-- | VkMemoryGetWin32HandleInfoKHR - Structure describing a Win32 handle
-- semaphore export operation
--
-- = Description
--
-- The properties of the handle returned depend on the value of
-- @handleType@. See
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits'
-- for a description of the properties of the defined external memory
-- handle types.
--
-- == Valid Usage
--
-- -   @handleType@ /must/ have been included in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExportMemoryAllocateInfo'::@handleTypes@
--     when @memory@ was created.
--
-- -   If @handleType@ is defined as an NT handle,
--     'vkGetMemoryWin32HandleKHR' /must/ be called no more than once for
--     each valid unique combination of @memory@ and @handleType@.
--
-- -   @handleType@ /must/ be defined as an NT handle or a global share
--     handle.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @memory@ /must/ be a valid @VkDeviceMemory@ handle
--
-- -   @handleType@ /must/ be a valid
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- No cross-references are available
data VkMemoryGetWin32HandleInfoKHR = VkMemoryGetWin32HandleInfoKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @memory@ is the memory object from which the handle will be exported.
  vkMemory :: VkDeviceMemory
  , -- | @handleType@ is the type of handle requested.
  vkHandleType :: VkExternalMemoryHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkMemoryGetWin32HandleInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkMemoryGetWin32HandleInfoKHR <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryGetWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryGetWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkMemory (poked :: VkMemoryGetWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHandleType (poked :: VkMemoryGetWin32HandleInfoKHR))

instance Zero VkMemoryGetWin32HandleInfoKHR where
  zero = VkMemoryGetWin32HandleInfoKHR zero
                                       zero
                                       zero
                                       zero
-- | VkMemoryWin32HandlePropertiesKHR - Properties of External Memory Windows
-- Handles
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- No cross-references are available
data VkMemoryWin32HandlePropertiesKHR = VkMemoryWin32HandlePropertiesKHR
  { -- | @sType@ /must/ be @VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR@
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @memoryTypeBits@ is a bitmask containing one bit set for every memory
  -- type which the specified windows handle /can/ be imported as.
  vkMemoryTypeBits :: Word32
  }
  deriving (Eq, Show)

instance Storable VkMemoryWin32HandlePropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryWin32HandlePropertiesKHR <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryWin32HandlePropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryWin32HandlePropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkMemoryTypeBits (poked :: VkMemoryWin32HandlePropertiesKHR))

instance Zero VkMemoryWin32HandlePropertiesKHR where
  zero = VkMemoryWin32HandlePropertiesKHR zero
                                          zero
                                          zero
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkGetMemoryWin32HandleKHR - Get a Windows HANDLE for a memory object
--
-- = Parameters
--
-- -   @device@ is the logical device that created the device memory being
--     exported.
--
-- -   @pGetWin32HandleInfo@ is a pointer to an instance of the
--     'VkMemoryGetWin32HandleInfoKHR' structure containing parameters of
--     the export operation.
--
-- -   @pHandle@ will return the Windows handle representing the underlying
--     resources of the device memory object.
--
-- = Description
--
-- For handle types defined as NT handles, the handles returned by
-- @vkGetMemoryWin32HandleKHR@ are owned by the application. To avoid
-- leaking resources, the application /must/ release ownership of them
-- using the @CloseHandle@ system call when they are no longer needed.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_TOO_MANY_OBJECTS@
--
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetMemoryWin32HandleKHR" vkGetMemoryWin32HandleKHR :: ("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkMemoryGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult

#endif
type FN_vkGetMemoryWin32HandleKHR = ("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkMemoryGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
type PFN_vkGetMemoryWin32HandleKHR = FunPtr FN_vkGetMemoryWin32HandleKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkGetMemoryWin32HandlePropertiesKHR - Get Properties of External Memory
-- Win32 Handles
--
-- = Parameters
--
-- -   @device@ is the logical device that will be importing @handle@.
--
-- -   @handleType@ is the type of the handle @handle@.
--
-- -   @handle@ is the handle which will be imported.
--
-- -   @pMemoryWin32HandleProperties@ will return properties of @handle@.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_INVALID_EXTERNAL_HANDLE@
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetMemoryWin32HandlePropertiesKHR" vkGetMemoryWin32HandlePropertiesKHR :: ("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("handle" ::: HANDLE) -> ("pMemoryWin32HandleProperties" ::: Ptr VkMemoryWin32HandlePropertiesKHR) -> IO VkResult

#endif
type FN_vkGetMemoryWin32HandlePropertiesKHR = ("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("handle" ::: HANDLE) -> ("pMemoryWin32HandleProperties" ::: Ptr VkMemoryWin32HandlePropertiesKHR) -> IO VkResult
type PFN_vkGetMemoryWin32HandlePropertiesKHR = FunPtr FN_vkGetMemoryWin32HandlePropertiesKHR
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = "VK_KHR_external_memory_win32"
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR = VkStructureType 1000073001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR = VkStructureType 1000073000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR = VkStructureType 1000073003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR"
pattern VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR = VkStructureType 1000073002
