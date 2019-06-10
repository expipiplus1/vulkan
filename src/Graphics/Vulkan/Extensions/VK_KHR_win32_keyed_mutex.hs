{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_win32_keyed_mutex
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  Win32KeyedMutexAcquireReleaseInfoKHR(..)
  , 
#endif
  pattern KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME
  , pattern KHR_WIN32_KEYED_MUTEX_SPEC_VERSION
  , pattern STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_win32_keyed_mutex
  ( pattern VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME
  , pattern VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION
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
  ( pattern STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkWin32KeyedMutexAcquireReleaseInfoKHR"
data Win32KeyedMutexAcquireReleaseInfoKHR = Win32KeyedMutexAcquireReleaseInfoKHR
  { -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoKHR" "pAcquireSyncs"
  acquireSyncs :: Vector DeviceMemory
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoKHR" "pAcquireKeys"
  acquireKeys :: Vector Word64
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoKHR" "pAcquireTimeouts"
  acquireTimeouts :: Vector Word32
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoKHR" "pReleaseSyncs"
  releaseSyncs :: Vector DeviceMemory
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoKHR" "pReleaseKeys"
  releaseKeys :: Vector Word64
  }
  deriving (Show, Eq)

instance Zero Win32KeyedMutexAcquireReleaseInfoKHR where
  zero = Win32KeyedMutexAcquireReleaseInfoKHR Nothing
                                              mempty
                                              mempty
                                              mempty
                                              mempty
                                              mempty

#endif

-- No documentation found for TopLevel "VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME"
pattern KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME = VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION"
pattern KHR_WIN32_KEYED_MUTEX_SPEC_VERSION :: Integral a => a
pattern KHR_WIN32_KEYED_MUTEX_SPEC_VERSION = VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION
