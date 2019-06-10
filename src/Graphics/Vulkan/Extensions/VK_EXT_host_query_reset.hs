{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_host_query_reset
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceHostQueryResetFeaturesEXT(..)
  , 
#endif
  resetQueryPoolEXT
  , pattern EXT_HOST_QUERY_RESET_EXTENSION_NAME
  , pattern EXT_HOST_QUERY_RESET_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_host_query_reset
  ( vkResetQueryPoolEXT
  , pattern VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME
  , pattern VK_EXT_HOST_QUERY_RESET_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.Query
  ( QueryPool
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceHostQueryResetFeaturesEXT"
data PhysicalDeviceHostQueryResetFeaturesEXT = PhysicalDeviceHostQueryResetFeaturesEXT
  { -- No documentation found for Nested "PhysicalDeviceHostQueryResetFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceHostQueryResetFeaturesEXT" "hostQueryReset"
  hostQueryReset :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceHostQueryResetFeaturesEXT where
  zero = PhysicalDeviceHostQueryResetFeaturesEXT Nothing
                                                 False

#endif


-- No documentation found for TopLevel "vkResetQueryPoolEXT"
resetQueryPoolEXT :: Device ->  QueryPool ->  Word32 ->  Word32 ->  IO ()
resetQueryPoolEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME"
pattern EXT_HOST_QUERY_RESET_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_HOST_QUERY_RESET_EXTENSION_NAME = VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_HOST_QUERY_RESET_SPEC_VERSION"
pattern EXT_HOST_QUERY_RESET_SPEC_VERSION :: Integral a => a
pattern EXT_HOST_QUERY_RESET_SPEC_VERSION = VK_EXT_HOST_QUERY_RESET_SPEC_VERSION
