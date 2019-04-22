{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32
  ( withCStructExportMemoryWin32HandleInfoKHR
  , fromCStructExportMemoryWin32HandleInfoKHR
  , ExportMemoryWin32HandleInfoKHR(..)
  , withCStructImportMemoryWin32HandleInfoKHR
  , fromCStructImportMemoryWin32HandleInfoKHR
  , ImportMemoryWin32HandleInfoKHR(..)
  , withCStructMemoryGetWin32HandleInfoKHR
  , fromCStructMemoryGetWin32HandleInfoKHR
  , MemoryGetWin32HandleInfoKHR(..)
  , withCStructMemoryWin32HandlePropertiesKHR
  , fromCStructMemoryWin32HandlePropertiesKHR
  , MemoryWin32HandlePropertiesKHR(..)
  , getMemoryWin32HandleKHR
  , getMemoryWin32HandlePropertiesKHR
  , pattern KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  , pattern KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
  , pattern STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR
  , pattern STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
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
  ( Ptr
  , castPtr
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( VkExportMemoryWin32HandleInfoKHR(..)
  , VkImportMemoryWin32HandleInfoKHR(..)
  , VkMemoryGetWin32HandleInfoKHR(..)
  , VkMemoryWin32HandlePropertiesKHR(..)
  , LPCWSTR
  , vkGetMemoryWin32HandleKHR
  , vkGetMemoryWin32HandlePropertiesKHR
  , pattern VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( DWORD
  , HANDLE
  , SECURITY_ATTRIBUTES
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( ExternalMemoryHandleTypeFlagBits
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR
  )



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
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT'
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT'
--
-- And
--
-- @GENERIC_ALL@
--
-- for handles of the following types:
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT'
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT'
--
-- [1]
--     <https://msdn.microsoft.com/en-us/library/windows/desktop/ms686670.aspx>
--
-- == Valid Usage
--
-- -   If
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExportMemoryAllocateInfo'::@handleTypes@
--     does not include
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT',
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT',
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.VkExportMemoryWin32HandleInfoKHR'
--     /must/ not be in the @pNext@ chain of
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR'
--
-- -   If @pAttributes@ is not @NULL@, @pAttributes@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32.SECURITY_ATTRIBUTES'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data ExportMemoryWin32HandleInfoKHR = ExportMemoryWin32HandleInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "ExportMemoryWin32HandleInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportMemoryWin32HandleInfoKHR" "pAttributes"
  attributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "ExportMemoryWin32HandleInfoKHR" "dwAccess"
  dwAccess :: DWORD
  , -- No documentation found for Nested "ExportMemoryWin32HandleInfoKHR" "name"
  name :: LPCWSTR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkExportMemoryWin32HandleInfoKHR' and
-- marshal a 'ExportMemoryWin32HandleInfoKHR' into it. The 'VkExportMemoryWin32HandleInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructExportMemoryWin32HandleInfoKHR :: ExportMemoryWin32HandleInfoKHR -> (VkExportMemoryWin32HandleInfoKHR -> IO a) -> IO a
withCStructExportMemoryWin32HandleInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ExportMemoryWin32HandleInfoKHR)) (\pPNext -> cont (VkExportMemoryWin32HandleInfoKHR VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR pPNext (attributes (marshalled :: ExportMemoryWin32HandleInfoKHR)) (dwAccess (marshalled :: ExportMemoryWin32HandleInfoKHR)) (name (marshalled :: ExportMemoryWin32HandleInfoKHR))))

-- | A function to read a 'VkExportMemoryWin32HandleInfoKHR' and all additional
-- structures in the pointer chain into a 'ExportMemoryWin32HandleInfoKHR'.
fromCStructExportMemoryWin32HandleInfoKHR :: VkExportMemoryWin32HandleInfoKHR -> IO ExportMemoryWin32HandleInfoKHR
fromCStructExportMemoryWin32HandleInfoKHR c = ExportMemoryWin32HandleInfoKHR <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExportMemoryWin32HandleInfoKHR)))
                                                                             <*> pure (vkPAttributes (c :: VkExportMemoryWin32HandleInfoKHR))
                                                                             <*> pure (vkDwAccess (c :: VkExportMemoryWin32HandleInfoKHR))
                                                                             <*> pure (vkName (c :: VkExportMemoryWin32HandleInfoKHR))

instance Zero ExportMemoryWin32HandleInfoKHR where
  zero = ExportMemoryWin32HandleInfoKHR Nothing
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
-- import operation /must/ create a distinct
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object.
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
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT',
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT',
--     @name@ /must/ be @NULL@.
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
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#external-memory-handle-types-compatibility external memory handle types compatibility>.
--
-- -   If @name@ is not @NULL@, it /must/ obey any requirements listed for
--     @handleType@ in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#external-memory-handle-types-compatibility external memory handle types compatibility>.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR'
--
-- -   If @handleType@ is not @0@, @handleType@ /must/ be a valid
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data ImportMemoryWin32HandleInfoKHR = ImportMemoryWin32HandleInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "ImportMemoryWin32HandleInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportMemoryWin32HandleInfoKHR" "handleType"
  handleType :: ExternalMemoryHandleTypeFlagBits
  , -- No documentation found for Nested "ImportMemoryWin32HandleInfoKHR" "handle"
  handle :: HANDLE
  , -- No documentation found for Nested "ImportMemoryWin32HandleInfoKHR" "name"
  name :: LPCWSTR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImportMemoryWin32HandleInfoKHR' and
-- marshal a 'ImportMemoryWin32HandleInfoKHR' into it. The 'VkImportMemoryWin32HandleInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImportMemoryWin32HandleInfoKHR :: ImportMemoryWin32HandleInfoKHR -> (VkImportMemoryWin32HandleInfoKHR -> IO a) -> IO a
withCStructImportMemoryWin32HandleInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ImportMemoryWin32HandleInfoKHR)) (\pPNext -> cont (VkImportMemoryWin32HandleInfoKHR VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR pPNext (handleType (marshalled :: ImportMemoryWin32HandleInfoKHR)) (handle (marshalled :: ImportMemoryWin32HandleInfoKHR)) (name (marshalled :: ImportMemoryWin32HandleInfoKHR))))

-- | A function to read a 'VkImportMemoryWin32HandleInfoKHR' and all additional
-- structures in the pointer chain into a 'ImportMemoryWin32HandleInfoKHR'.
fromCStructImportMemoryWin32HandleInfoKHR :: VkImportMemoryWin32HandleInfoKHR -> IO ImportMemoryWin32HandleInfoKHR
fromCStructImportMemoryWin32HandleInfoKHR c = ImportMemoryWin32HandleInfoKHR <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImportMemoryWin32HandleInfoKHR)))
                                                                             <*> pure (vkHandleType (c :: VkImportMemoryWin32HandleInfoKHR))
                                                                             <*> pure (vkHandle (c :: VkImportMemoryWin32HandleInfoKHR))
                                                                             <*> pure (vkName (c :: VkImportMemoryWin32HandleInfoKHR))

instance Zero ImportMemoryWin32HandleInfoKHR where
  zero = ImportMemoryWin32HandleInfoKHR Nothing
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
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.vkGetMemoryWin32HandleKHR'
--     /must/ be called no more than once for each valid unique combination
--     of @memory@ and @handleType@.
--
-- -   @handleType@ /must/ be defined as an NT handle or a global share
--     handle.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @memory@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' handle
--
-- -   @handleType@ /must/ be a valid
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.vkGetMemoryWin32HandleKHR'
data MemoryGetWin32HandleInfoKHR = MemoryGetWin32HandleInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "MemoryGetWin32HandleInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryGetWin32HandleInfoKHR" "memory"
  memory :: DeviceMemory
  , -- No documentation found for Nested "MemoryGetWin32HandleInfoKHR" "handleType"
  handleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkMemoryGetWin32HandleInfoKHR' and
-- marshal a 'MemoryGetWin32HandleInfoKHR' into it. The 'VkMemoryGetWin32HandleInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructMemoryGetWin32HandleInfoKHR :: MemoryGetWin32HandleInfoKHR -> (VkMemoryGetWin32HandleInfoKHR -> IO a) -> IO a
withCStructMemoryGetWin32HandleInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: MemoryGetWin32HandleInfoKHR)) (\pPNext -> cont (VkMemoryGetWin32HandleInfoKHR VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR pPNext (memory (marshalled :: MemoryGetWin32HandleInfoKHR)) (handleType (marshalled :: MemoryGetWin32HandleInfoKHR))))

-- | A function to read a 'VkMemoryGetWin32HandleInfoKHR' and all additional
-- structures in the pointer chain into a 'MemoryGetWin32HandleInfoKHR'.
fromCStructMemoryGetWin32HandleInfoKHR :: VkMemoryGetWin32HandleInfoKHR -> IO MemoryGetWin32HandleInfoKHR
fromCStructMemoryGetWin32HandleInfoKHR c = MemoryGetWin32HandleInfoKHR <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryGetWin32HandleInfoKHR)))
                                                                       <*> pure (vkMemory (c :: VkMemoryGetWin32HandleInfoKHR))
                                                                       <*> pure (vkHandleType (c :: VkMemoryGetWin32HandleInfoKHR))

instance Zero MemoryGetWin32HandleInfoKHR where
  zero = MemoryGetWin32HandleInfoKHR Nothing
                                     zero
                                     zero



-- | VkMemoryWin32HandlePropertiesKHR - Properties of External Memory Windows
-- Handles
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.vkGetMemoryWin32HandlePropertiesKHR'
data MemoryWin32HandlePropertiesKHR = MemoryWin32HandlePropertiesKHR
  { -- Univalued member elided
  -- No documentation found for Nested "MemoryWin32HandlePropertiesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryWin32HandlePropertiesKHR" "memoryTypeBits"
  memoryTypeBits :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkMemoryWin32HandlePropertiesKHR' and
-- marshal a 'MemoryWin32HandlePropertiesKHR' into it. The 'VkMemoryWin32HandlePropertiesKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructMemoryWin32HandlePropertiesKHR :: MemoryWin32HandlePropertiesKHR -> (VkMemoryWin32HandlePropertiesKHR -> IO a) -> IO a
withCStructMemoryWin32HandlePropertiesKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: MemoryWin32HandlePropertiesKHR)) (\pPNext -> cont (VkMemoryWin32HandlePropertiesKHR VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR pPNext (memoryTypeBits (marshalled :: MemoryWin32HandlePropertiesKHR))))

-- | A function to read a 'VkMemoryWin32HandlePropertiesKHR' and all additional
-- structures in the pointer chain into a 'MemoryWin32HandlePropertiesKHR'.
fromCStructMemoryWin32HandlePropertiesKHR :: VkMemoryWin32HandlePropertiesKHR -> IO MemoryWin32HandlePropertiesKHR
fromCStructMemoryWin32HandlePropertiesKHR c = MemoryWin32HandlePropertiesKHR <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryWin32HandlePropertiesKHR)))
                                                                             <*> pure (vkMemoryTypeBits (c :: VkMemoryWin32HandlePropertiesKHR))

instance Zero MemoryWin32HandlePropertiesKHR where
  zero = MemoryWin32HandlePropertiesKHR Nothing
                                        zero



-- | vkGetMemoryWin32HandleKHR - Get a Windows HANDLE for a memory object
--
-- = Parameters
--
-- -   @device@ is the logical device that created the device memory being
--     exported.
--
-- -   @pGetWin32HandleInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.VkMemoryGetWin32HandleInfoKHR'
--     structure containing parameters of the export operation.
--
-- -   @pHandle@ will return the Windows handle representing the underlying
--     resources of the device memory object.
--
-- = Description
--
-- For handle types defined as NT handles, the handles returned by
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.vkGetMemoryWin32HandleKHR'
-- are owned by the application. To avoid leaking resources, the
-- application /must/ release ownership of them using the @CloseHandle@
-- system call when they are no longer needed.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_TOO_MANY_OBJECTS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.VkMemoryGetWin32HandleInfoKHR'
getMemoryWin32HandleKHR :: Device ->  MemoryGetWin32HandleInfoKHR ->  IO (HANDLE)
getMemoryWin32HandleKHR = \(Device device' commandTable) -> \getWin32HandleInfo' -> alloca (\pHandle' -> (\marshalled -> withCStructMemoryGetWin32HandleInfoKHR marshalled . flip with) getWin32HandleInfo' (\pGetWin32HandleInfo' -> vkGetMemoryWin32HandleKHR commandTable device' pGetWin32HandleInfo' pHandle' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pHandle'))))


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
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VK_ERROR_INVALID_EXTERNAL_HANDLE'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.VkMemoryWin32HandlePropertiesKHR'
getMemoryWin32HandlePropertiesKHR :: Device ->  ExternalMemoryHandleTypeFlagBits ->  HANDLE ->  IO (MemoryWin32HandlePropertiesKHR)
getMemoryWin32HandlePropertiesKHR = \(Device device' commandTable) -> \handleType' -> \handle' -> alloca (\pMemoryWin32HandleProperties' -> vkGetMemoryWin32HandlePropertiesKHR commandTable device' handleType' handle' pMemoryWin32HandleProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((fromCStructMemoryWin32HandlePropertiesKHR <=< peek) pMemoryWin32HandleProperties')))

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME"
pattern KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION"
pattern KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION :: Integral a => a
pattern KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
