{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_win32_keyed_mutex
  ( VkWin32KeyedMutexAcquireReleaseInfoKHR(..)
  , pattern VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME
  , pattern VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR
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


-- No documentation found for TopLevel "VkWin32KeyedMutexAcquireReleaseInfoKHR"
data VkWin32KeyedMutexAcquireReleaseInfoKHR = VkWin32KeyedMutexAcquireReleaseInfoKHR
  { -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoKHR" "acquireCount"
  vkAcquireCount :: Word32
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoKHR" "pAcquireSyncs"
  vkPAcquireSyncs :: Ptr VkDeviceMemory
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoKHR" "pAcquireKeys"
  vkPAcquireKeys :: Ptr Word64
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoKHR" "pAcquireTimeouts"
  vkPAcquireTimeouts :: Ptr Word32
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoKHR" "releaseCount"
  vkReleaseCount :: Word32
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoKHR" "pReleaseSyncs"
  vkPReleaseSyncs :: Ptr VkDeviceMemory
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoKHR" "pReleaseKeys"
  vkPReleaseKeys :: Ptr Word64
  }
  deriving (Eq, Show)

instance Storable VkWin32KeyedMutexAcquireReleaseInfoKHR where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkWin32KeyedMutexAcquireReleaseInfoKHR <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
                                                    <*> peek (ptr `plusPtr` 24)
                                                    <*> peek (ptr `plusPtr` 32)
                                                    <*> peek (ptr `plusPtr` 40)
                                                    <*> peek (ptr `plusPtr` 48)
                                                    <*> peek (ptr `plusPtr` 56)
                                                    <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkWin32KeyedMutexAcquireReleaseInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkWin32KeyedMutexAcquireReleaseInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkAcquireCount (poked :: VkWin32KeyedMutexAcquireReleaseInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkPAcquireSyncs (poked :: VkWin32KeyedMutexAcquireReleaseInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkPAcquireKeys (poked :: VkWin32KeyedMutexAcquireReleaseInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkPAcquireTimeouts (poked :: VkWin32KeyedMutexAcquireReleaseInfoKHR))
                *> poke (ptr `plusPtr` 48) (vkReleaseCount (poked :: VkWin32KeyedMutexAcquireReleaseInfoKHR))
                *> poke (ptr `plusPtr` 56) (vkPReleaseSyncs (poked :: VkWin32KeyedMutexAcquireReleaseInfoKHR))
                *> poke (ptr `plusPtr` 64) (vkPReleaseKeys (poked :: VkWin32KeyedMutexAcquireReleaseInfoKHR))
-- No documentation found for TopLevel "VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME"
pattern VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME = "VK_KHR_win32_keyed_mutex"
-- No documentation found for TopLevel "VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION"
pattern VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION :: Integral a => a
pattern VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR = VkStructureType 1000075000
