{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core11.DeviceInitialization
  ( enumerateInstanceVersion
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( enumerateInstanceVersion
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )



-- | Wrapper for 'vkEnumerateInstanceVersion'
enumerateInstanceVersion :: IO (Word32)
enumerateInstanceVersion = alloca (\pApiVersion -> Graphics.Vulkan.C.Dynamic.enumerateInstanceVersion pApiVersion >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pApiVersion)))
