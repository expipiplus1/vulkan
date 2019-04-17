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
import qualified Graphics.Vulkan.C.Dynamic
  ( resetQueryPoolEXT
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_host_query_reset
  ( VkPhysicalDeviceHostQueryResetFeaturesEXT(..)
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


-- No documentation found for TopLevel "PhysicalDeviceHostQueryResetFeaturesEXT"
data PhysicalDeviceHostQueryResetFeaturesEXT = PhysicalDeviceHostQueryResetFeaturesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceHostQueryResetFeaturesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceHostQueryResetFeaturesEXT" "hostQueryReset"
  vkHostQueryReset :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceHostQueryResetFeaturesEXT :: PhysicalDeviceHostQueryResetFeaturesEXT -> (VkPhysicalDeviceHostQueryResetFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceHostQueryResetFeaturesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceHostQueryResetFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceHostQueryResetFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT pPNext (boolToBool32 (vkHostQueryReset (from :: PhysicalDeviceHostQueryResetFeaturesEXT)))))
fromCStructPhysicalDeviceHostQueryResetFeaturesEXT :: VkPhysicalDeviceHostQueryResetFeaturesEXT -> IO PhysicalDeviceHostQueryResetFeaturesEXT
fromCStructPhysicalDeviceHostQueryResetFeaturesEXT c = PhysicalDeviceHostQueryResetFeaturesEXT <$> -- Univalued Member elided
                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceHostQueryResetFeaturesEXT)))
                                                                                               <*> pure (bool32ToBool (vkHostQueryReset (c :: VkPhysicalDeviceHostQueryResetFeaturesEXT)))
instance Zero PhysicalDeviceHostQueryResetFeaturesEXT where
  zero = PhysicalDeviceHostQueryResetFeaturesEXT Nothing
                                                 False

-- | Wrapper for 'vkResetQueryPoolEXT'
resetQueryPoolEXT :: Device ->  QueryPool ->  Word32 ->  Word32 ->  IO ()
resetQueryPoolEXT = \(Device device commandTable) -> \queryPool -> \firstQuery -> \queryCount -> Graphics.Vulkan.C.Dynamic.resetQueryPoolEXT commandTable device queryPool firstQuery queryCount *> (pure ())
