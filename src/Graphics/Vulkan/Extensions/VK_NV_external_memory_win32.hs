{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_external_memory_win32
  ( withCStructExportMemoryWin32HandleInfoNV
  , fromCStructExportMemoryWin32HandleInfoNV
  , ExportMemoryWin32HandleInfoNV(..)
  , withCStructImportMemoryWin32HandleInfoNV
  , fromCStructImportMemoryWin32HandleInfoNV
  , ImportMemoryWin32HandleInfoNV(..)
  , getMemoryWin32HandleNV
  , pattern NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  , pattern NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
  , pattern STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV
  , pattern STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.String
  ( IsString
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
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
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( VkExportMemoryWin32HandleInfoNV(..)
  , VkImportMemoryWin32HandleInfoNV(..)
  , DWORD
  , HANDLE
  , SECURITY_ATTRIBUTES
  , vkGetMemoryWin32HandleNV
  , pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  , pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities
  ( ExternalMemoryHandleTypeFlagsNV
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV
  , pattern STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV
  )



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
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32.VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV'
--
-- -   If @pAttributes@ is not @NULL@, @pAttributes@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32.SECURITY_ATTRIBUTES'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data ExportMemoryWin32HandleInfoNV = ExportMemoryWin32HandleInfoNV
  { -- Univalued member elided
  -- No documentation found for Nested "ExportMemoryWin32HandleInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportMemoryWin32HandleInfoNV" "pAttributes"
  attributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "ExportMemoryWin32HandleInfoNV" "dwAccess"
  dwAccess :: DWORD
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkExportMemoryWin32HandleInfoNV' and
-- marshal a 'ExportMemoryWin32HandleInfoNV' into it. The 'VkExportMemoryWin32HandleInfoNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructExportMemoryWin32HandleInfoNV :: ExportMemoryWin32HandleInfoNV -> (VkExportMemoryWin32HandleInfoNV -> IO a) -> IO a
withCStructExportMemoryWin32HandleInfoNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ExportMemoryWin32HandleInfoNV)) (\pPNext -> cont (VkExportMemoryWin32HandleInfoNV VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV pPNext (attributes (marshalled :: ExportMemoryWin32HandleInfoNV)) (dwAccess (marshalled :: ExportMemoryWin32HandleInfoNV))))

-- | A function to read a 'VkExportMemoryWin32HandleInfoNV' and all additional
-- structures in the pointer chain into a 'ExportMemoryWin32HandleInfoNV'.
fromCStructExportMemoryWin32HandleInfoNV :: VkExportMemoryWin32HandleInfoNV -> IO ExportMemoryWin32HandleInfoNV
fromCStructExportMemoryWin32HandleInfoNV c = ExportMemoryWin32HandleInfoNV <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExportMemoryWin32HandleInfoNV)))
                                                                           <*> pure (vkPAttributes (c :: VkExportMemoryWin32HandleInfoNV))
                                                                           <*> pure (vkDwAccess (c :: VkExportMemoryWin32HandleInfoNV))

instance Zero ExportMemoryWin32HandleInfoNV where
  zero = ExportMemoryWin32HandleInfoNV Nothing
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
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagsNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data ImportMemoryWin32HandleInfoNV = ImportMemoryWin32HandleInfoNV
  { -- Univalued member elided
  -- No documentation found for Nested "ImportMemoryWin32HandleInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportMemoryWin32HandleInfoNV" "handleType"
  handleType :: ExternalMemoryHandleTypeFlagsNV
  , -- No documentation found for Nested "ImportMemoryWin32HandleInfoNV" "handle"
  handle :: HANDLE
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImportMemoryWin32HandleInfoNV' and
-- marshal a 'ImportMemoryWin32HandleInfoNV' into it. The 'VkImportMemoryWin32HandleInfoNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImportMemoryWin32HandleInfoNV :: ImportMemoryWin32HandleInfoNV -> (VkImportMemoryWin32HandleInfoNV -> IO a) -> IO a
withCStructImportMemoryWin32HandleInfoNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ImportMemoryWin32HandleInfoNV)) (\pPNext -> cont (VkImportMemoryWin32HandleInfoNV VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV pPNext (handleType (marshalled :: ImportMemoryWin32HandleInfoNV)) (handle (marshalled :: ImportMemoryWin32HandleInfoNV))))

-- | A function to read a 'VkImportMemoryWin32HandleInfoNV' and all additional
-- structures in the pointer chain into a 'ImportMemoryWin32HandleInfoNV'.
fromCStructImportMemoryWin32HandleInfoNV :: VkImportMemoryWin32HandleInfoNV -> IO ImportMemoryWin32HandleInfoNV
fromCStructImportMemoryWin32HandleInfoNV c = ImportMemoryWin32HandleInfoNV <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImportMemoryWin32HandleInfoNV)))
                                                                           <*> pure (vkHandleType (c :: VkImportMemoryWin32HandleInfoNV))
                                                                           <*> pure (vkHandle (c :: VkImportMemoryWin32HandleInfoNV))

instance Zero ImportMemoryWin32HandleInfoNV where
  zero = ImportMemoryWin32HandleInfoNV Nothing
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
-- -   @handle@ points to a Windows
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32.HANDLE' in
--     which the handle is returned.
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
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagsNV'
getMemoryWin32HandleNV :: Device ->  DeviceMemory ->  ExternalMemoryHandleTypeFlagsNV ->  IO (HANDLE)
getMemoryWin32HandleNV = \(Device device' commandTable) -> \memory' -> \handleType' -> alloca (\pHandle' -> vkGetMemoryWin32HandleNV commandTable device' memory' handleType' pHandle' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pHandle')))

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME"
pattern NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION"
pattern NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION :: Integral a => a
pattern NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
