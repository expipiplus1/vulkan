{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_win32_keyed_mutex
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  Win32KeyedMutexAcquireReleaseInfoNV(..)
  , 
#endif
  pattern NV_WIN32_KEYED_MUTEX_EXTENSION_NAME
  , pattern NV_WIN32_KEYED_MUTEX_SPEC_VERSION
  , pattern STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
  ) where

import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word64
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_NV_win32_keyed_mutex
  ( pattern VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME
  , pattern VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkWin32KeyedMutexAcquireReleaseInfoNV"
data Win32KeyedMutexAcquireReleaseInfoNV = Win32KeyedMutexAcquireReleaseInfoNV
  { -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoNV" "pAcquireSyncs"
  acquireSyncs :: Vector DeviceMemory
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoNV" "pAcquireKeys"
  acquireKeys :: Vector Word64
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoNV" "pAcquireTimeoutMilliseconds"
  acquireTimeoutMilliseconds :: Vector Word32
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoNV" "pReleaseSyncs"
  releaseSyncs :: Vector DeviceMemory
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoNV" "pReleaseKeys"
  releaseKeys :: Vector Word64
  }
  deriving (Show, Eq)

instance Zero Win32KeyedMutexAcquireReleaseInfoNV where
  zero = Win32KeyedMutexAcquireReleaseInfoNV Nothing
                                             mempty
                                             mempty
                                             mempty
                                             mempty
                                             mempty

#endif

-- No documentation found for TopLevel "VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME"
pattern NV_WIN32_KEYED_MUTEX_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_WIN32_KEYED_MUTEX_EXTENSION_NAME = VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION"
pattern NV_WIN32_KEYED_MUTEX_SPEC_VERSION :: Integral a => a
pattern NV_WIN32_KEYED_MUTEX_SPEC_VERSION = VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION
