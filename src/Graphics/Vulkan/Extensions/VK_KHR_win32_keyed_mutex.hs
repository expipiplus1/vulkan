{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_win32_keyed_mutex
  ( pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR
  , pattern VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION
  , pattern VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME
  , VkWin32KeyedMutexAcquireReleaseInfoKHR(..)
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word64
  , Word32
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.Core10.Memory
  ( VkDeviceMemory
  )


-- | Nothing
pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR = VkStructureType 1000075000
pattern VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION :: Integral a => a
pattern VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION = 1
pattern VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME = "VK_KHR_win32_keyed_mutex"
-- | TODO: Struct comments
data VkWin32KeyedMutexAcquireReleaseInfoKHR = VkWin32KeyedMutexAcquireReleaseInfoKHR
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkAcquireCount :: Word32
  , vkPAcquireSyncs :: Ptr VkDeviceMemory
  , vkPAcquireKeys :: Ptr Word64
  , vkPAcquireTimeouts :: Ptr Word32
  , vkReleaseCount :: Word32
  , vkPReleaseSyncs :: Ptr VkDeviceMemory
  , vkPReleaseKeys :: Ptr Word64
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
