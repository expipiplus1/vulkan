{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core10.Device
  ( DeviceCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , DeviceCreateInfo(..)
#endif
  , DeviceQueueCreateFlagBits
  , pattern DEVICE_QUEUE_CREATE_PROTECTED_BIT
  , DeviceQueueCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , DeviceQueueCreateInfo(..)
#endif
  , createDevice
  , destroyDevice
  , withDevice
  ) where

import Control.Exception
  ( bracket
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.ByteString
  ( ByteString
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Storable
  ( peek
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core10.Device
  ( VkDeviceCreateFlags(..)
  , VkDeviceQueueCreateFlagBits(..)
  , vkCreateDevice
  , vkDestroyDevice
  )
import Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory
  ( pattern VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT
  )
import Graphics.Vulkan.C.Dynamic
  ( initDeviceCmds
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , PhysicalDevice(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDeviceFeatures(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif


-- No documentation found for TopLevel "DeviceCreateFlags"
type DeviceCreateFlags = VkDeviceCreateFlags


-- No complete pragma for DeviceCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDeviceCreateInfo"
data DeviceCreateInfo = DeviceCreateInfo
  { -- No documentation found for Nested "DeviceCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceCreateInfo" "flags"
  flags :: DeviceCreateFlags
  , -- No documentation found for Nested "DeviceCreateInfo" "pQueueCreateInfos"
  queueCreateInfos :: Vector DeviceQueueCreateInfo
  , -- No documentation found for Nested "DeviceCreateInfo" "ppEnabledLayerNames"
  enabledLayerNames :: Vector ByteString
  , -- No documentation found for Nested "DeviceCreateInfo" "ppEnabledExtensionNames"
  enabledExtensionNames :: Vector ByteString
  , -- No documentation found for Nested "DeviceCreateInfo" "pEnabledFeatures"
  enabledFeatures :: Maybe PhysicalDeviceFeatures
  }
  deriving (Show, Eq)

instance Zero DeviceCreateInfo where
  zero = DeviceCreateInfo Nothing
                          zero
                          mempty
                          mempty
                          mempty
                          Nothing

#endif

-- No documentation found for TopLevel "DeviceQueueCreateFlagBits"
type DeviceQueueCreateFlagBits = VkDeviceQueueCreateFlagBits


{-# complete DEVICE_QUEUE_CREATE_PROTECTED_BIT :: DeviceQueueCreateFlagBits #-}


-- No documentation found for Nested "DeviceQueueCreateFlagBits" "DEVICE_QUEUE_CREATE_PROTECTED_BIT"
pattern DEVICE_QUEUE_CREATE_PROTECTED_BIT :: (a ~ DeviceQueueCreateFlagBits) => a
pattern DEVICE_QUEUE_CREATE_PROTECTED_BIT = VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT

-- No documentation found for TopLevel "DeviceQueueCreateFlags"
type DeviceQueueCreateFlags = DeviceQueueCreateFlagBits


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDeviceQueueCreateInfo"
data DeviceQueueCreateInfo = DeviceQueueCreateInfo
  { -- No documentation found for Nested "DeviceQueueCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceQueueCreateInfo" "flags"
  flags :: DeviceQueueCreateFlags
  , -- No documentation found for Nested "DeviceQueueCreateInfo" "queueFamilyIndex"
  queueFamilyIndex :: Word32
  , -- No documentation found for Nested "DeviceQueueCreateInfo" "pQueuePriorities"
  queuePriorities :: Vector Float
  }
  deriving (Show, Eq)

instance Zero DeviceQueueCreateInfo where
  zero = DeviceQueueCreateInfo Nothing
                               zero
                               zero
                               mempty

#endif


-- No documentation found for TopLevel "vkCreateDevice"
createDevice :: PhysicalDevice ->  DeviceCreateInfo ->  Maybe AllocationCallbacks ->  IO (Device)
createDevice = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyDevice"
destroyDevice :: Device ->  Maybe AllocationCallbacks ->  IO ()
destroyDevice = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createDevice' and 'destroyDevice' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withDevice
  :: PhysicalDevice -> DeviceCreateInfo -> Maybe AllocationCallbacks -> (Device -> IO a) -> IO a
withDevice physicalDevice deviceCreateInfo allocationCallbacks = bracket
  (createDevice physicalDevice deviceCreateInfo allocationCallbacks)
  (\o -> destroyDevice o allocationCallbacks)
