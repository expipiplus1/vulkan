{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.DeviceInitialization
  ( 
#if defined(EXPOSE_CORE11_COMMANDS)
  vkEnumerateInstanceVersion
  , 
#endif
  FN_vkEnumerateInstanceVersion
  , PFN_vkEnumerateInstanceVersion
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


#if defined(EXPOSE_CORE11_COMMANDS)
-- | vkEnumerateInstanceVersion - Query instance-level version before
-- instance creation
--
-- = Parameters
--
-- -   @pApiVersion@ points to a @uint32_t@, which is the version of Vulkan
--     supported by instance-level functionality, encoded as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#extendingvulkan-coreversions-versionnumbers {html_spec_relative}#extendingvulkan-coreversions-versionnumbers>.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
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

#endif
type FN_vkEnumerateInstanceVersion = ("pApiVersion" ::: Ptr Word32) -> IO VkResult
type PFN_vkEnumerateInstanceVersion = FunPtr FN_vkEnumerateInstanceVersion
