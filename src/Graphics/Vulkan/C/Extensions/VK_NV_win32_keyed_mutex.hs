{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_win32_keyed_mutex
  ( VkWin32KeyedMutexAcquireReleaseInfoNV(..)
  , pattern VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME
  , pattern VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  , Word64
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.C.Core10.Memory
  ( VkDeviceMemory
  )


-- No documentation found for TopLevel "VkWin32KeyedMutexAcquireReleaseInfoNV"
data VkWin32KeyedMutexAcquireReleaseInfoNV = VkWin32KeyedMutexAcquireReleaseInfoNV
  { -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoNV" "acquireCount"
  vkAcquireCount :: Word32
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoNV" "pAcquireSyncs"
  vkPAcquireSyncs :: Ptr VkDeviceMemory
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoNV" "pAcquireKeys"
  vkPAcquireKeys :: Ptr Word64
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoNV" "pAcquireTimeoutMilliseconds"
  vkPAcquireTimeoutMilliseconds :: Ptr Word32
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoNV" "releaseCount"
  vkReleaseCount :: Word32
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoNV" "pReleaseSyncs"
  vkPReleaseSyncs :: Ptr VkDeviceMemory
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoNV" "pReleaseKeys"
  vkPReleaseKeys :: Ptr Word64
  }
  deriving (Eq, Show)

instance Storable VkWin32KeyedMutexAcquireReleaseInfoNV where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkWin32KeyedMutexAcquireReleaseInfoNV <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
                                                   <*> peek (ptr `plusPtr` 24)
                                                   <*> peek (ptr `plusPtr` 32)
                                                   <*> peek (ptr `plusPtr` 40)
                                                   <*> peek (ptr `plusPtr` 48)
                                                   <*> peek (ptr `plusPtr` 56)
                                                   <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkWin32KeyedMutexAcquireReleaseInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkWin32KeyedMutexAcquireReleaseInfoNV))
                *> poke (ptr `plusPtr` 16) (vkAcquireCount (poked :: VkWin32KeyedMutexAcquireReleaseInfoNV))
                *> poke (ptr `plusPtr` 24) (vkPAcquireSyncs (poked :: VkWin32KeyedMutexAcquireReleaseInfoNV))
                *> poke (ptr `plusPtr` 32) (vkPAcquireKeys (poked :: VkWin32KeyedMutexAcquireReleaseInfoNV))
                *> poke (ptr `plusPtr` 40) (vkPAcquireTimeoutMilliseconds (poked :: VkWin32KeyedMutexAcquireReleaseInfoNV))
                *> poke (ptr `plusPtr` 48) (vkReleaseCount (poked :: VkWin32KeyedMutexAcquireReleaseInfoNV))
                *> poke (ptr `plusPtr` 56) (vkPReleaseSyncs (poked :: VkWin32KeyedMutexAcquireReleaseInfoNV))
                *> poke (ptr `plusPtr` 64) (vkPReleaseKeys (poked :: VkWin32KeyedMutexAcquireReleaseInfoNV))
-- No documentation found for TopLevel "VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME"
pattern VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME = "VK_NV_win32_keyed_mutex"
-- No documentation found for TopLevel "VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION"
pattern VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION :: Integral a => a
pattern VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV"
pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV = VkStructureType 1000058000
