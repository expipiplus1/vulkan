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
-- #_parameters#
--
-- -   @pApiVersion@ points to a @uint32_t@, which is the version of Vulkan
--     supported by instance-level functionality, encoded as described in
--     the
--     <{html_spec_relative}#fundamentals-versionnum API Version Numbers and Semantics>
--     section.
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @pApiVersion@ /must/ be a valid pointer to a @uint32_t@ value
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- = See Also
-- #_see_also#
--
-- No cross-references are available
foreign import ccall "vkEnumerateInstanceVersion" vkEnumerateInstanceVersion :: ("pApiVersion" ::: Ptr Word32) -> IO VkResult
