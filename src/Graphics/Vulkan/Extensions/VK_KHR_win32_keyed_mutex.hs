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


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.Core10.Memory
  ( VkDeviceMemory
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR = VkStructureType 1000075000
-- No documentation found for TopLevel "VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION"
pattern VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION :: Integral a => a
pattern VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME"
pattern VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME = "VK_KHR_win32_keyed_mutex"
-- | VkWin32KeyedMutexAcquireReleaseInfoKHR - Use the Windows keyed mutex
-- mechanism to synchronize work
--
-- == Valid Usage
--
-- -   Each member of @pAcquireSyncs@ and @pReleaseSyncs@ /must/ be a
--     device memory object imported by setting
--     'Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32.VkImportMemoryWin32HandleInfoKHR'::@handleType@
--     to @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT@ or
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR@
--
-- -   If @acquireCount@ is not @0@, @pAcquireSyncs@ /must/ be a valid
--     pointer to an array of @acquireCount@ valid @VkDeviceMemory@ handles
--
-- -   If @acquireCount@ is not @0@, @pAcquireKeys@ /must/ be a valid
--     pointer to an array of @acquireCount@ @uint64_t@ values
--
-- -   If @acquireCount@ is not @0@, @pAcquireTimeouts@ /must/ be a valid
--     pointer to an array of @acquireCount@ @uint32_t@ values
--
-- -   If @releaseCount@ is not @0@, @pReleaseSyncs@ /must/ be a valid
--     pointer to an array of @releaseCount@ valid @VkDeviceMemory@ handles
--
-- -   If @releaseCount@ is not @0@, @pReleaseKeys@ /must/ be a valid
--     pointer to an array of @releaseCount@ @uint64_t@ values
--
-- -   Both of the elements of @pAcquireSyncs@, and the elements of
--     @pReleaseSyncs@ that are valid handles /must/ have been created,
--     allocated, or retrieved from the same @VkDevice@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkWin32KeyedMutexAcquireReleaseInfoKHR = VkWin32KeyedMutexAcquireReleaseInfoKHR
  { -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- | @acquireCount@ is the number of entries in the @pAcquireSyncs@,
  -- @pAcquireKeys@, and @pAcquireTimeoutMilliseconds@ arrays.
  vkAcquireCount :: Word32
  , -- | @pAcquireSyncs@ is a pointer to an array of
  -- 'Graphics.Vulkan.Core10.Memory.VkDeviceMemory' objects which were
  -- imported from Direct3D 11 resources.
  vkPAcquireSyncs :: Ptr VkDeviceMemory
  , -- | @pAcquireKeys@ is a pointer to an array of mutex key values to wait for
  -- prior to beginning the submitted work. Entries refer to the keyed mutex
  -- associated with the corresponding entries in @pAcquireSyncs@.
  vkPAcquireKeys :: Ptr Word64
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoKHR" "pAcquireTimeouts"
  vkPAcquireTimeouts :: Ptr Word32
  , -- | @releaseCount@ is the number of entries in the @pReleaseSyncs@ and
  -- @pReleaseKeys@ arrays.
  vkReleaseCount :: Word32
  , -- | @pReleaseSyncs@ is a pointer to an array of
  -- 'Graphics.Vulkan.Core10.Memory.VkDeviceMemory' objects which were
  -- imported from Direct3D 11 resources.
  vkPReleaseSyncs :: Ptr VkDeviceMemory
  , -- | @pReleaseKeys@ is a pointer to an array of mutex key values to set when
  -- the submitted work has completed. Entries refer to the keyed mutex
  -- associated with the corresponding entries in @pReleaseSyncs@.
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
