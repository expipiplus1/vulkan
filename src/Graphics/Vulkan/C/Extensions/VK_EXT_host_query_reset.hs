{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_host_query_reset
  ( VkPhysicalDeviceHostQueryResetFeaturesEXT(..)
  , FN_vkResetQueryPoolEXT
  , PFN_vkResetQueryPoolEXT
  , vkResetQueryPoolEXT
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
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkPhysicalDeviceHostQueryResetFeaturesEXT"
data VkPhysicalDeviceHostQueryResetFeaturesEXT = VkPhysicalDeviceHostQueryResetFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceHostQueryResetFeaturesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceHostQueryResetFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceHostQueryResetFeaturesEXT" "hostQueryReset"
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
  zero = VkPhysicalDeviceHostQueryResetFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT
                                                   zero
                                                   zero

-- No documentation found for TopLevel "vkResetQueryPoolEXT"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkResetQueryPoolEXT" vkResetQueryPoolEXT :: ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ()
#else
vkResetQueryPoolEXT :: DeviceCmds -> ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ()
vkResetQueryPoolEXT deviceCmds = mkVkResetQueryPoolEXT (pVkResetQueryPoolEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetQueryPoolEXT
  :: FunPtr (("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ()) -> (("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ())
#endif

type FN_vkResetQueryPoolEXT = ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ()
type PFN_vkResetQueryPoolEXT = FunPtr FN_vkResetQueryPoolEXT

-- No documentation found for TopLevel "VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME"
pattern VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME = "VK_EXT_host_query_reset"

-- No documentation found for TopLevel "VK_EXT_HOST_QUERY_RESET_SPEC_VERSION"
pattern VK_EXT_HOST_QUERY_RESET_SPEC_VERSION :: Integral a => a
pattern VK_EXT_HOST_QUERY_RESET_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT = VkStructureType 1000261000
