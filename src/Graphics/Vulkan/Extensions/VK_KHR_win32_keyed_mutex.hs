{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_win32_keyed_mutex
  ( withCStructWin32KeyedMutexAcquireReleaseInfoKHR
  , fromCStructWin32KeyedMutexAcquireReleaseInfoKHR
  , Win32KeyedMutexAcquireReleaseInfoKHR(..)
  , pattern VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION
  , pattern VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR
  ) where

import Data.Function
  ( (&)
  )
import Data.List
  ( minimum
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Word
  ( Word32
  , Word64
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_win32_keyed_mutex
  ( VkWin32KeyedMutexAcquireReleaseInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR
  )
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_win32_keyed_mutex
  ( pattern VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME
  , pattern VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION
  )


-- No documentation found for TopLevel "Win32KeyedMutexAcquireReleaseInfoKHR"
data Win32KeyedMutexAcquireReleaseInfoKHR = Win32KeyedMutexAcquireReleaseInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoKHR" "pAcquireSyncs"
  vkPAcquireSyncs :: Vector DeviceMemory
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoKHR" "pAcquireKeys"
  vkPAcquireKeys :: Vector Word64
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoKHR" "pAcquireTimeouts"
  vkPAcquireTimeouts :: Vector Word32
  -- Length valued member elided
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoKHR" "pReleaseSyncs"
  vkPReleaseSyncs :: Vector DeviceMemory
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoKHR" "pReleaseKeys"
  vkPReleaseKeys :: Vector Word64
  }
  deriving (Show, Eq)
withCStructWin32KeyedMutexAcquireReleaseInfoKHR :: Win32KeyedMutexAcquireReleaseInfoKHR -> (VkWin32KeyedMutexAcquireReleaseInfoKHR -> IO a) -> IO a
withCStructWin32KeyedMutexAcquireReleaseInfoKHR from cont = withVec (&) (vkPReleaseKeys (from :: Win32KeyedMutexAcquireReleaseInfoKHR)) (\pReleaseKeys -> withVec (&) (vkPReleaseSyncs (from :: Win32KeyedMutexAcquireReleaseInfoKHR)) (\pReleaseSyncs -> withVec (&) (vkPAcquireTimeouts (from :: Win32KeyedMutexAcquireReleaseInfoKHR)) (\pAcquireTimeouts -> withVec (&) (vkPAcquireKeys (from :: Win32KeyedMutexAcquireReleaseInfoKHR)) (\pAcquireKeys -> withVec (&) (vkPAcquireSyncs (from :: Win32KeyedMutexAcquireReleaseInfoKHR)) (\pAcquireSyncs -> maybeWith withSomeVkStruct (vkPNext (from :: Win32KeyedMutexAcquireReleaseInfoKHR)) (\pPNext -> cont (VkWin32KeyedMutexAcquireReleaseInfoKHR VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR pPNext (fromIntegral (minimum ([Data.Vector.length (vkPAcquireSyncs (from :: Win32KeyedMutexAcquireReleaseInfoKHR)), Data.Vector.length (vkPAcquireKeys (from :: Win32KeyedMutexAcquireReleaseInfoKHR)), Data.Vector.length (vkPAcquireTimeouts (from :: Win32KeyedMutexAcquireReleaseInfoKHR))]))) pAcquireSyncs pAcquireKeys pAcquireTimeouts (fromIntegral (minimum ([Data.Vector.length (vkPReleaseSyncs (from :: Win32KeyedMutexAcquireReleaseInfoKHR)), Data.Vector.length (vkPReleaseKeys (from :: Win32KeyedMutexAcquireReleaseInfoKHR))]))) pReleaseSyncs pReleaseKeys)))))))
fromCStructWin32KeyedMutexAcquireReleaseInfoKHR :: VkWin32KeyedMutexAcquireReleaseInfoKHR -> IO Win32KeyedMutexAcquireReleaseInfoKHR
fromCStructWin32KeyedMutexAcquireReleaseInfoKHR c = Win32KeyedMutexAcquireReleaseInfoKHR <$> -- Univalued Member elided
                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkWin32KeyedMutexAcquireReleaseInfoKHR)))
                                                                                         -- Length valued member elided
                                                                                         <*> (Data.Vector.generateM (fromIntegral (vkAcquireCount (c :: VkWin32KeyedMutexAcquireReleaseInfoKHR))) (peekElemOff (vkPAcquireSyncs (c :: VkWin32KeyedMutexAcquireReleaseInfoKHR))))
                                                                                         <*> (Data.Vector.generateM (fromIntegral (vkAcquireCount (c :: VkWin32KeyedMutexAcquireReleaseInfoKHR))) (peekElemOff (vkPAcquireKeys (c :: VkWin32KeyedMutexAcquireReleaseInfoKHR))))
                                                                                         <*> (Data.Vector.generateM (fromIntegral (vkAcquireCount (c :: VkWin32KeyedMutexAcquireReleaseInfoKHR))) (peekElemOff (vkPAcquireTimeouts (c :: VkWin32KeyedMutexAcquireReleaseInfoKHR))))
                                                                                         -- Length valued member elided
                                                                                         <*> (Data.Vector.generateM (fromIntegral (vkReleaseCount (c :: VkWin32KeyedMutexAcquireReleaseInfoKHR))) (peekElemOff (vkPReleaseSyncs (c :: VkWin32KeyedMutexAcquireReleaseInfoKHR))))
                                                                                         <*> (Data.Vector.generateM (fromIntegral (vkReleaseCount (c :: VkWin32KeyedMutexAcquireReleaseInfoKHR))) (peekElemOff (vkPReleaseKeys (c :: VkWin32KeyedMutexAcquireReleaseInfoKHR))))
instance Zero Win32KeyedMutexAcquireReleaseInfoKHR where
  zero = Win32KeyedMutexAcquireReleaseInfoKHR Nothing
                                              Data.Vector.empty
                                              Data.Vector.empty
                                              Data.Vector.empty
                                              Data.Vector.empty
                                              Data.Vector.empty
