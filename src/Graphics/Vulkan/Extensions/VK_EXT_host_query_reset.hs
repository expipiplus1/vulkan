{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_host_query_reset
  ( withCStructPhysicalDeviceHostQueryResetFeaturesEXT
  , fromCStructPhysicalDeviceHostQueryResetFeaturesEXT
  , PhysicalDeviceHostQueryResetFeaturesEXT(..)
  , resetQueryPoolEXT
  , pattern VK_EXT_HOST_QUERY_RESET_SPEC_VERSION
  , pattern VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_host_query_reset
  ( VkPhysicalDeviceHostQueryResetFeaturesEXT(..)
  , vkResetQueryPoolEXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.Query
  ( QueryPool
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_host_query_reset
  ( pattern VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME
  , pattern VK_EXT_HOST_QUERY_RESET_SPEC_VERSION
  )



-- | VkPhysicalDeviceHostQueryResetFeaturesEXT - Structure describing whether
-- queries can be reset from the host
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_host_query_reset.VkPhysicalDeviceHostQueryResetFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_host_query_reset.VkPhysicalDeviceHostQueryResetFeaturesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_host_query_reset.VkPhysicalDeviceHostQueryResetFeaturesEXT'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable features.
--
-- Unresolved directive in VkPhysicalDeviceHostQueryResetFeaturesEXT.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceHostQueryResetFeaturesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceHostQueryResetFeaturesEXT = PhysicalDeviceHostQueryResetFeaturesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceHostQueryResetFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceHostQueryResetFeaturesEXT" "hostQueryReset"
  hostQueryReset :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceHostQueryResetFeaturesEXT' and
-- marshal a 'PhysicalDeviceHostQueryResetFeaturesEXT' into it. The 'VkPhysicalDeviceHostQueryResetFeaturesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceHostQueryResetFeaturesEXT :: PhysicalDeviceHostQueryResetFeaturesEXT -> (VkPhysicalDeviceHostQueryResetFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceHostQueryResetFeaturesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceHostQueryResetFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceHostQueryResetFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT pPNext (boolToBool32 (hostQueryReset (marshalled :: PhysicalDeviceHostQueryResetFeaturesEXT)))))

-- | A function to read a 'VkPhysicalDeviceHostQueryResetFeaturesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceHostQueryResetFeaturesEXT'.
fromCStructPhysicalDeviceHostQueryResetFeaturesEXT :: VkPhysicalDeviceHostQueryResetFeaturesEXT -> IO PhysicalDeviceHostQueryResetFeaturesEXT
fromCStructPhysicalDeviceHostQueryResetFeaturesEXT c = PhysicalDeviceHostQueryResetFeaturesEXT <$> -- Univalued Member elided
                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceHostQueryResetFeaturesEXT)))
                                                                                               <*> pure (bool32ToBool (vkHostQueryReset (c :: VkPhysicalDeviceHostQueryResetFeaturesEXT)))

instance Zero PhysicalDeviceHostQueryResetFeaturesEXT where
  zero = PhysicalDeviceHostQueryResetFeaturesEXT Nothing
                                                 False



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
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-hostQueryReset hostQueryReset>
--     feature /must/ be enabled
--
-- -   @firstQuery@ /must/ be less than the number of queries in
--     @queryPool@
--
-- -   The sum of @firstQuery@ and @queryCount@ /must/ be less than or
--     equal to the number of queries in @queryPool@
--
-- Unresolved directive in vkResetQueryPoolEXT.txt -
-- include::{generated}\/validity\/protos\/vkResetQueryPoolEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
resetQueryPoolEXT :: Device ->  QueryPool ->  Word32 ->  Word32 ->  IO ()
resetQueryPoolEXT = \(Device device' commandTable) -> \queryPool' -> \firstQuery' -> \queryCount' -> vkResetQueryPoolEXT commandTable device' queryPool' firstQuery' queryCount' *> (pure ())
