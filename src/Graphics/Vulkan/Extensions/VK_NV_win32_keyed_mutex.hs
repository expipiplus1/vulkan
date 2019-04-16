{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_win32_keyed_mutex
  ( withCStructWin32KeyedMutexAcquireReleaseInfoNV
  , fromCStructWin32KeyedMutexAcquireReleaseInfoNV
  , Win32KeyedMutexAcquireReleaseInfoNV(..)
  , pattern VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION
  , pattern VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
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
  ( generateM
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


import Graphics.Vulkan.C.Extensions.VK_NV_win32_keyed_mutex
  ( VkWin32KeyedMutexAcquireReleaseInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
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
import Graphics.Vulkan.C.Extensions.VK_NV_win32_keyed_mutex
  ( pattern VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME
  , pattern VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION
  )


-- No documentation found for TopLevel "Win32KeyedMutexAcquireReleaseInfoNV"
data Win32KeyedMutexAcquireReleaseInfoNV = Win32KeyedMutexAcquireReleaseInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoNV" "pAcquireSyncs"
  vkPAcquireSyncs :: Vector DeviceMemory
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoNV" "pAcquireKeys"
  vkPAcquireKeys :: Vector Word64
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoNV" "pAcquireTimeoutMilliseconds"
  vkPAcquireTimeoutMilliseconds :: Vector Word32
  -- Length valued member elided
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoNV" "pReleaseSyncs"
  vkPReleaseSyncs :: Vector DeviceMemory
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoNV" "pReleaseKeys"
  vkPReleaseKeys :: Vector Word64
  }
  deriving (Show, Eq)
withCStructWin32KeyedMutexAcquireReleaseInfoNV :: Win32KeyedMutexAcquireReleaseInfoNV -> (VkWin32KeyedMutexAcquireReleaseInfoNV -> IO a) -> IO a
withCStructWin32KeyedMutexAcquireReleaseInfoNV from cont = withVec (&) (vkPReleaseKeys (from :: Win32KeyedMutexAcquireReleaseInfoNV)) (\pReleaseKeys -> withVec (&) (vkPReleaseSyncs (from :: Win32KeyedMutexAcquireReleaseInfoNV)) (\pReleaseSyncs -> withVec (&) (vkPAcquireTimeoutMilliseconds (from :: Win32KeyedMutexAcquireReleaseInfoNV)) (\pAcquireTimeoutMilliseconds -> withVec (&) (vkPAcquireKeys (from :: Win32KeyedMutexAcquireReleaseInfoNV)) (\pAcquireKeys -> withVec (&) (vkPAcquireSyncs (from :: Win32KeyedMutexAcquireReleaseInfoNV)) (\pAcquireSyncs -> maybeWith withSomeVkStruct (vkPNext (from :: Win32KeyedMutexAcquireReleaseInfoNV)) (\pPNext -> cont (VkWin32KeyedMutexAcquireReleaseInfoNV VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV pPNext (fromIntegral (minimum ([Data.Vector.length (vkPAcquireSyncs (from :: Win32KeyedMutexAcquireReleaseInfoNV)), Data.Vector.length (vkPAcquireKeys (from :: Win32KeyedMutexAcquireReleaseInfoNV)), Data.Vector.length (vkPAcquireTimeoutMilliseconds (from :: Win32KeyedMutexAcquireReleaseInfoNV))]))) pAcquireSyncs pAcquireKeys pAcquireTimeoutMilliseconds (fromIntegral (minimum ([Data.Vector.length (vkPReleaseSyncs (from :: Win32KeyedMutexAcquireReleaseInfoNV)), Data.Vector.length (vkPReleaseKeys (from :: Win32KeyedMutexAcquireReleaseInfoNV))]))) pReleaseSyncs pReleaseKeys)))))))
fromCStructWin32KeyedMutexAcquireReleaseInfoNV :: VkWin32KeyedMutexAcquireReleaseInfoNV -> IO Win32KeyedMutexAcquireReleaseInfoNV
fromCStructWin32KeyedMutexAcquireReleaseInfoNV c = Win32KeyedMutexAcquireReleaseInfoNV <$> -- Univalued Member elided
                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkWin32KeyedMutexAcquireReleaseInfoNV)))
                                                                                       -- Length valued member elided
                                                                                       <*> (Data.Vector.generateM (fromIntegral (vkAcquireCount (c :: VkWin32KeyedMutexAcquireReleaseInfoNV))) (peekElemOff (vkPAcquireSyncs (c :: VkWin32KeyedMutexAcquireReleaseInfoNV))))
                                                                                       <*> (Data.Vector.generateM (fromIntegral (vkAcquireCount (c :: VkWin32KeyedMutexAcquireReleaseInfoNV))) (peekElemOff (vkPAcquireKeys (c :: VkWin32KeyedMutexAcquireReleaseInfoNV))))
                                                                                       <*> (Data.Vector.generateM (fromIntegral (vkAcquireCount (c :: VkWin32KeyedMutexAcquireReleaseInfoNV))) (peekElemOff (vkPAcquireTimeoutMilliseconds (c :: VkWin32KeyedMutexAcquireReleaseInfoNV))))
                                                                                       -- Length valued member elided
                                                                                       <*> (Data.Vector.generateM (fromIntegral (vkReleaseCount (c :: VkWin32KeyedMutexAcquireReleaseInfoNV))) (peekElemOff (vkPReleaseSyncs (c :: VkWin32KeyedMutexAcquireReleaseInfoNV))))
                                                                                       <*> (Data.Vector.generateM (fromIntegral (vkReleaseCount (c :: VkWin32KeyedMutexAcquireReleaseInfoNV))) (peekElemOff (vkPReleaseKeys (c :: VkWin32KeyedMutexAcquireReleaseInfoNV))))
