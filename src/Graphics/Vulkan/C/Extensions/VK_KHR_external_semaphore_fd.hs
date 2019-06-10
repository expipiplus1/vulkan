{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd
  ( VkImportSemaphoreFdInfoKHR(..)
  , VkSemaphoreGetFdInfoKHR(..)
  , FN_vkGetSemaphoreFdKHR
  , PFN_vkGetSemaphoreFdKHR
  , vkGetSemaphoreFdKHR
  , FN_vkImportSemaphoreFdKHR
  , PFN_vkImportSemaphoreFdKHR
  , vkImportSemaphoreFdKHR
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR
  ) where

import Data.String
  ( IsString
  )
import Foreign.C.Types
  ( CInt(..)
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
import Graphics.Vulkan.C.Core10.Queue
  ( VkSemaphore
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore
  ( VkSemaphoreImportFlags
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreHandleTypeFlagBits(..)
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkImportSemaphoreFdInfoKHR"
data VkImportSemaphoreFdInfoKHR = VkImportSemaphoreFdInfoKHR
  { -- No documentation found for Nested "VkImportSemaphoreFdInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImportSemaphoreFdInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImportSemaphoreFdInfoKHR" "semaphore"
  vkSemaphore :: VkSemaphore
  , -- No documentation found for Nested "VkImportSemaphoreFdInfoKHR" "flags"
  vkFlags :: VkSemaphoreImportFlags
  , -- No documentation found for Nested "VkImportSemaphoreFdInfoKHR" "handleType"
  vkHandleType :: VkExternalSemaphoreHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportSemaphoreFdInfoKHR" "fd"
  vkFd :: CInt
  }
  deriving (Eq, Show)

instance Storable VkImportSemaphoreFdInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkImportSemaphoreFdInfoKHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 24)
                                        <*> peek (ptr `plusPtr` 28)
                                        <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportSemaphoreFdInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportSemaphoreFdInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSemaphore (poked :: VkImportSemaphoreFdInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkFlags (poked :: VkImportSemaphoreFdInfoKHR))
                *> poke (ptr `plusPtr` 28) (vkHandleType (poked :: VkImportSemaphoreFdInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkFd (poked :: VkImportSemaphoreFdInfoKHR))

instance Zero VkImportSemaphoreFdInfoKHR where
  zero = VkImportSemaphoreFdInfoKHR VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero

-- No documentation found for TopLevel "VkSemaphoreGetFdInfoKHR"
data VkSemaphoreGetFdInfoKHR = VkSemaphoreGetFdInfoKHR
  { -- No documentation found for Nested "VkSemaphoreGetFdInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSemaphoreGetFdInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSemaphoreGetFdInfoKHR" "semaphore"
  vkSemaphore :: VkSemaphore
  , -- No documentation found for Nested "VkSemaphoreGetFdInfoKHR" "handleType"
  vkHandleType :: VkExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkSemaphoreGetFdInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkSemaphoreGetFdInfoKHR <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSemaphoreGetFdInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSemaphoreGetFdInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSemaphore (poked :: VkSemaphoreGetFdInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHandleType (poked :: VkSemaphoreGetFdInfoKHR))

instance Zero VkSemaphoreGetFdInfoKHR where
  zero = VkSemaphoreGetFdInfoKHR VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR
                                 zero
                                 zero
                                 zero

-- No documentation found for TopLevel "vkGetSemaphoreFdKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetSemaphoreFdKHR" vkGetSemaphoreFdKHR :: ("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkSemaphoreGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult
#else
vkGetSemaphoreFdKHR :: DeviceCmds -> ("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkSemaphoreGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult
vkGetSemaphoreFdKHR deviceCmds = mkVkGetSemaphoreFdKHR (pVkGetSemaphoreFdKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSemaphoreFdKHR
  :: FunPtr (("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkSemaphoreGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkSemaphoreGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult)
#endif

type FN_vkGetSemaphoreFdKHR = ("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkSemaphoreGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult
type PFN_vkGetSemaphoreFdKHR = FunPtr FN_vkGetSemaphoreFdKHR

-- No documentation found for TopLevel "vkImportSemaphoreFdKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkImportSemaphoreFdKHR" vkImportSemaphoreFdKHR :: ("device" ::: VkDevice) -> ("pImportSemaphoreFdInfo" ::: Ptr VkImportSemaphoreFdInfoKHR) -> IO VkResult
#else
vkImportSemaphoreFdKHR :: DeviceCmds -> ("device" ::: VkDevice) -> ("pImportSemaphoreFdInfo" ::: Ptr VkImportSemaphoreFdInfoKHR) -> IO VkResult
vkImportSemaphoreFdKHR deviceCmds = mkVkImportSemaphoreFdKHR (pVkImportSemaphoreFdKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkImportSemaphoreFdKHR
  :: FunPtr (("device" ::: VkDevice) -> ("pImportSemaphoreFdInfo" ::: Ptr VkImportSemaphoreFdInfoKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pImportSemaphoreFdInfo" ::: Ptr VkImportSemaphoreFdInfoKHR) -> IO VkResult)
#endif

type FN_vkImportSemaphoreFdKHR = ("device" ::: VkDevice) -> ("pImportSemaphoreFdInfo" ::: Ptr VkImportSemaphoreFdInfoKHR) -> IO VkResult
type PFN_vkImportSemaphoreFdKHR = FunPtr FN_vkImportSemaphoreFdKHR

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME = "VK_KHR_external_semaphore_fd"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR"
pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR = VkStructureType 1000079000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR"
pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR = VkStructureType 1000079001
