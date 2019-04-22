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


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core11.DeviceInitialization
  ( vkEnumerateInstanceVersion
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )



-- | vkEnumerateInstanceVersion - Query instance-level version before
-- instance creation
--
-- = Parameters
--
-- -   @pApiVersion@ points to a @uint32_t@, which is the version of Vulkan
--     supported by instance-level functionality, encoded as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#extendingvulkan-coreversions-versionnumbers>.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- = See Also
--
-- No cross-references are available
enumerateInstanceVersion :: IO (Word32)
enumerateInstanceVersion = alloca (\pApiVersion' -> vkEnumerateInstanceVersion pApiVersion' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pApiVersion')))
