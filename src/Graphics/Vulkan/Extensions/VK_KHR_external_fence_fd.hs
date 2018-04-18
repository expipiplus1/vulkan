{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd
  ( pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR
  , pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
  , vkGetFenceFdKHR
  , vkImportFenceFdKHR
  , VkImportFenceFdInfoKHR(..)
  , VkFenceGetFdInfoKHR(..)
  ) where

import Data.String
  ( IsString
  )
import Foreign.C.Types
  ( CInt(..)
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
import Graphics.Vulkan.Core10.Queue
  ( VkFence
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence
  ( VkFenceImportFlags
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceHandleTypeFlagBits(..)
  )


-- | Nothing
pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR = VkStructureType 1000115000
-- | Nothing
pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR = VkStructureType 1000115001
pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION = 1
pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME = "VK_KHR_external_fence_fd"
-- | 
foreign import ccall "vkGetFenceFdKHR" vkGetFenceFdKHR :: ("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkFenceGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult
-- | 
foreign import ccall "vkImportFenceFdKHR" vkImportFenceFdKHR :: ("device" ::: VkDevice) -> ("pImportFenceFdInfo" ::: Ptr VkImportFenceFdInfoKHR) -> IO VkResult
-- | TODO: Struct comments
data VkImportFenceFdInfoKHR = VkImportFenceFdInfoKHR
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkFence :: VkFence
  , vkFlags :: VkFenceImportFlags
  , vkHandleType :: VkExternalFenceHandleTypeFlagBits
  , vkFd :: CInt
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
-- | TODO: Struct comments
data VkFenceGetFdInfoKHR = VkFenceGetFdInfoKHR
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkFence :: VkFence
  , vkHandleType :: VkExternalFenceHandleTypeFlagBits
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
