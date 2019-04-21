{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( DWORD
  , HANDLE
  , SECURITY_ATTRIBUTES
  , VkExportMemoryWin32HandleInfoNV
  , VkImportMemoryWin32HandleInfoNV
  , FN_vkGetMemoryWin32HandleNV
  , PFN_vkGetMemoryWin32HandleNV
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Memory
  ( VkDeviceMemory
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagsNV
  )


-- No documentation found for TopLevel "DWORD"
type DWORD = Word32
  

-- No documentation found for TopLevel "HANDLE"
type HANDLE = Ptr ()
  

-- | Opaque data
data SECURITY_ATTRIBUTES

data VkExportMemoryWin32HandleInfoNV

data VkImportMemoryWin32HandleInfoNV

type FN_vkGetMemoryWin32HandleNV = ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
type PFN_vkGetMemoryWin32HandleNV = FunPtr FN_vkGetMemoryWin32HandleNV
