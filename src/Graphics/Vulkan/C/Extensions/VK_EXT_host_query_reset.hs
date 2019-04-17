{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_host_query_reset
  ( VkPhysicalDeviceHostQueryResetFeaturesEXT(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkResetQueryPoolEXT
#endif
  , FN_vkResetQueryPoolEXT
  , PFN_vkResetQueryPoolEXT
  , pattern VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME
  , pattern VK_EXT_HOST_QUERY_RESET_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.C.Core10.Query
  ( VkQueryPool
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkPhysicalDeviceHostQueryResetFeaturesEXT - Structure describing whether
-- queries can be reset from the host
--
-- = Members
--
-- The members of the @VkPhysicalDeviceHostQueryResetFeaturesEXT@ structure
-- describe the following features:
--
-- = Description
--
-- If the @VkPhysicalDeviceHostQueryResetFeaturesEXT@ structure is included
-- in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- @VkPhysicalDeviceHostQueryResetFeaturesEXT@ /can/ also be used in the
-- @pNext@ chain of 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to
-- enable features.
--
-- Unresolved directive in VkPhysicalDeviceHostQueryResetFeaturesEXT.txt -
-- include::..\/validity\/structs\/VkPhysicalDeviceHostQueryResetFeaturesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceHostQueryResetFeaturesEXT = VkPhysicalDeviceHostQueryResetFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceHostQueryResetFeaturesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceHostQueryResetFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- | @hostQueryReset@ indicates that the implementation supports resetting
  -- queries from the host with 'vkResetQueryPoolEXT'.
  vkHostQueryReset :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceHostQueryResetFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceHostQueryResetFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                       <*> peek (ptr `plusPtr` 8)
                                                       <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceHostQueryResetFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceHostQueryResetFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkHostQueryReset (poked :: VkPhysicalDeviceHostQueryResetFeaturesEXT))

instance Zero VkPhysicalDeviceHostQueryResetFeaturesEXT where
  zero = VkPhysicalDeviceHostQueryResetFeaturesEXT zero
                                                   zero
                                                   zero
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkResetQueryPoolEXT - Reset queries in a query pool
--
-- = Parameters
--
-- -   @queryPool@ is the handle of the query pool managing the queries
--     being reset.
--
-- -   @firstQuery@ is the initial query index to reset.
--
-- -   @queryCount@ is the number of queries to reset.
--
-- = Description
--
-- This command sets the status of query indices [@firstQuery@,
-- @firstQuery@ + @queryCount@ - 1] to unavailable.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-hostQueryReset hostQueryReset>
--     feature /must/ be enabled
--
-- -   @firstQuery@ /must/ be less than the number of queries in
--     @queryPool@
--
-- -   The sum of @firstQuery@ and @queryCount@ /must/ be less than or
--     equal to the number of queries in @queryPool@
--
-- Unresolved directive in vkResetQueryPoolEXT.txt -
-- include::..\/validity\/protos\/vkResetQueryPoolEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkResetQueryPoolEXT" vkResetQueryPoolEXT :: ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ()

#endif
type FN_vkResetQueryPoolEXT = ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ()
type PFN_vkResetQueryPoolEXT = FunPtr FN_vkResetQueryPoolEXT
-- No documentation found for TopLevel "VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME"
pattern VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME = "VK_EXT_host_query_reset"
-- No documentation found for TopLevel "VK_EXT_HOST_QUERY_RESET_SPEC_VERSION"
pattern VK_EXT_HOST_QUERY_RESET_SPEC_VERSION :: Integral a => a
pattern VK_EXT_HOST_QUERY_RESET_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT = VkStructureType 1000261000
