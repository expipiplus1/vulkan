{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Core11.DeviceInitialization
  ( vkEnumerateInstanceVersion
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  )


-- | vkEnumerateInstanceVersion - Query instance-level version before
-- instance creation
--
-- = Parameters
--
-- -   @pApiVersion@ points to a @uint32_t@, which is the version of Vulkan
--     supported by instance-level functionality, encoded as described in
--     the [API Version Numbers and
--     Semantics](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-versionnum)
--     section.
--
-- == Valid Usage (Implicit)
--
-- -   @pApiVersion@ /must/ be a valid pointer to a @uint32_t@ value
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEnumerateInstanceVersion" vkEnumerateInstanceVersion :: ("pApiVersion" ::: Ptr Word32) -> IO VkResult
