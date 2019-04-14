{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd
  ( VkFenceGetFdInfoKHR(..)
  , VkImportFenceFdInfoKHR(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetFenceFdKHR
#endif
  , FN_vkGetFenceFdKHR
  , PFN_vkGetFenceFdKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkImportFenceFdKHR
#endif
  , FN_vkImportFenceFdKHR
  , PFN_vkImportFenceFdKHR
  , pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR
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
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkFence
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence
  ( VkFenceImportFlags
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceHandleTypeFlagBits(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkFenceGetFdInfoKHR"
data VkFenceGetFdInfoKHR = VkFenceGetFdInfoKHR
  { -- No documentation found for Nested "VkFenceGetFdInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkFenceGetFdInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkFenceGetFdInfoKHR" "fence"
  vkFence :: VkFence
  , -- No documentation found for Nested "VkFenceGetFdInfoKHR" "handleType"
  vkHandleType :: VkExternalFenceHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkFenceGetFdInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkFenceGetFdInfoKHR <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkFenceGetFdInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkFenceGetFdInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFence (poked :: VkFenceGetFdInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHandleType (poked :: VkFenceGetFdInfoKHR))
-- No documentation found for TopLevel "VkImportFenceFdInfoKHR"
data VkImportFenceFdInfoKHR = VkImportFenceFdInfoKHR
  { -- No documentation found for Nested "VkImportFenceFdInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImportFenceFdInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImportFenceFdInfoKHR" "fence"
  vkFence :: VkFence
  , -- No documentation found for Nested "VkImportFenceFdInfoKHR" "flags"
  vkFlags :: VkFenceImportFlags
  , -- No documentation found for Nested "VkImportFenceFdInfoKHR" "handleType"
  vkHandleType :: VkExternalFenceHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportFenceFdInfoKHR" "fd"
  vkFd :: CInt
  }
  deriving (Eq, Show)

instance Storable VkImportFenceFdInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkImportFenceFdInfoKHR <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 28)
                                    <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportFenceFdInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportFenceFdInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFence (poked :: VkImportFenceFdInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkFlags (poked :: VkImportFenceFdInfoKHR))
                *> poke (ptr `plusPtr` 28) (vkHandleType (poked :: VkImportFenceFdInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkFd (poked :: VkImportFenceFdInfoKHR))
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetFenceFdKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetFenceFdKHR" vkGetFenceFdKHR :: ("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkFenceGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult

#endif
type FN_vkGetFenceFdKHR = ("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkFenceGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult
type PFN_vkGetFenceFdKHR = FunPtr FN_vkGetFenceFdKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkImportFenceFdKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkImportFenceFdKHR" vkImportFenceFdKHR :: ("device" ::: VkDevice) -> ("pImportFenceFdInfo" ::: Ptr VkImportFenceFdInfoKHR) -> IO VkResult

#endif
type FN_vkImportFenceFdKHR = ("device" ::: VkDevice) -> ("pImportFenceFdInfo" ::: Ptr VkImportFenceFdInfoKHR) -> IO VkResult
type PFN_vkImportFenceFdKHR = FunPtr FN_vkImportFenceFdKHR
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME = "VK_KHR_external_fence_fd"
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR"
pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR = VkStructureType 1000115001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR"
pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR = VkStructureType 1000115000
