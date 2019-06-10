{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_EXT_display_control
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  DeviceEventInfoEXT(..)
  , 
#endif
  DeviceEventTypeEXT
  , pattern DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT
#if defined(VK_USE_PLATFORM_GGP)
  , DisplayEventInfoEXT(..)
#endif
  , DisplayEventTypeEXT
  , pattern DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT
#if defined(VK_USE_PLATFORM_GGP)
  , DisplayPowerInfoEXT(..)
#endif
  , DisplayPowerStateEXT
  , pattern DISPLAY_POWER_STATE_OFF_EXT
  , pattern DISPLAY_POWER_STATE_SUSPEND_EXT
  , pattern DISPLAY_POWER_STATE_ON_EXT
#if defined(VK_USE_PLATFORM_GGP)
  , SwapchainCounterCreateInfoEXT(..)
#endif
  , displayPowerControlEXT
  , getSwapchainCounterEXT
  , registerDeviceEventEXT
  , registerDisplayEventEXT
  , pattern EXT_DISPLAY_CONTROL_EXTENSION_NAME
  , pattern EXT_DISPLAY_CONTROL_SPEC_VERSION
  , pattern STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT
  , pattern STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT
  , pattern STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT
  , pattern STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word64
  )
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
import Graphics.Vulkan.C.Extensions.VK_EXT_display_control
  ( VkDeviceEventTypeEXT(..)
  , VkDisplayEventTypeEXT(..)
  , VkDisplayPowerStateEXT(..)
  , vkDisplayPowerControlEXT
  , vkGetSwapchainCounterEXT
  , vkRegisterDeviceEventEXT
  , vkRegisterDisplayEventEXT
  , pattern VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT
  , pattern VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT
  , pattern VK_DISPLAY_POWER_STATE_OFF_EXT
  , pattern VK_DISPLAY_POWER_STATE_ON_EXT
  , pattern VK_DISPLAY_POWER_STATE_SUSPEND_EXT
  , pattern VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME
  , pattern VK_EXT_DISPLAY_CONTROL_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( Fence
  )
import Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter
  ( SurfaceCounterFlagBitsEXT
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter
  ( SurfaceCounterFlagsEXT
  )
#endif
import Graphics.Vulkan.Extensions.VK_KHR_display
  ( DisplayKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( SwapchainKHR
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT
  , pattern STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT
  , pattern STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT
  , pattern STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDeviceEventInfoEXT"
data DeviceEventInfoEXT = DeviceEventInfoEXT
  { -- No documentation found for Nested "DeviceEventInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceEventInfoEXT" "deviceEvent"
  deviceEvent :: DeviceEventTypeEXT
  }
  deriving (Show, Eq)

instance Zero DeviceEventInfoEXT where
  zero = DeviceEventInfoEXT Nothing
                            zero

#endif

-- No documentation found for TopLevel "DeviceEventTypeEXT"
type DeviceEventTypeEXT = VkDeviceEventTypeEXT


{-# complete DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT :: DeviceEventTypeEXT #-}


-- No documentation found for Nested "DeviceEventTypeEXT" "DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT"
pattern DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT :: (a ~ DeviceEventTypeEXT) => a
pattern DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT = VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDisplayEventInfoEXT"
data DisplayEventInfoEXT = DisplayEventInfoEXT
  { -- No documentation found for Nested "DisplayEventInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayEventInfoEXT" "displayEvent"
  displayEvent :: DisplayEventTypeEXT
  }
  deriving (Show, Eq)

instance Zero DisplayEventInfoEXT where
  zero = DisplayEventInfoEXT Nothing
                             zero

#endif

-- No documentation found for TopLevel "DisplayEventTypeEXT"
type DisplayEventTypeEXT = VkDisplayEventTypeEXT


{-# complete DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT :: DisplayEventTypeEXT #-}


-- No documentation found for Nested "DisplayEventTypeEXT" "DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT"
pattern DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT :: (a ~ DisplayEventTypeEXT) => a
pattern DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT = VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDisplayPowerInfoEXT"
data DisplayPowerInfoEXT = DisplayPowerInfoEXT
  { -- No documentation found for Nested "DisplayPowerInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayPowerInfoEXT" "powerState"
  powerState :: DisplayPowerStateEXT
  }
  deriving (Show, Eq)

instance Zero DisplayPowerInfoEXT where
  zero = DisplayPowerInfoEXT Nothing
                             zero

#endif

-- No documentation found for TopLevel "DisplayPowerStateEXT"
type DisplayPowerStateEXT = VkDisplayPowerStateEXT


{-# complete DISPLAY_POWER_STATE_OFF_EXT, DISPLAY_POWER_STATE_SUSPEND_EXT, DISPLAY_POWER_STATE_ON_EXT :: DisplayPowerStateEXT #-}


-- No documentation found for Nested "DisplayPowerStateEXT" "DISPLAY_POWER_STATE_OFF_EXT"
pattern DISPLAY_POWER_STATE_OFF_EXT :: (a ~ DisplayPowerStateEXT) => a
pattern DISPLAY_POWER_STATE_OFF_EXT = VK_DISPLAY_POWER_STATE_OFF_EXT


-- No documentation found for Nested "DisplayPowerStateEXT" "DISPLAY_POWER_STATE_SUSPEND_EXT"
pattern DISPLAY_POWER_STATE_SUSPEND_EXT :: (a ~ DisplayPowerStateEXT) => a
pattern DISPLAY_POWER_STATE_SUSPEND_EXT = VK_DISPLAY_POWER_STATE_SUSPEND_EXT


-- No documentation found for Nested "DisplayPowerStateEXT" "DISPLAY_POWER_STATE_ON_EXT"
pattern DISPLAY_POWER_STATE_ON_EXT :: (a ~ DisplayPowerStateEXT) => a
pattern DISPLAY_POWER_STATE_ON_EXT = VK_DISPLAY_POWER_STATE_ON_EXT


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSwapchainCounterCreateInfoEXT"
data SwapchainCounterCreateInfoEXT = SwapchainCounterCreateInfoEXT
  { -- No documentation found for Nested "SwapchainCounterCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SwapchainCounterCreateInfoEXT" "surfaceCounters"
  surfaceCounters :: SurfaceCounterFlagsEXT
  }
  deriving (Show, Eq)

instance Zero SwapchainCounterCreateInfoEXT where
  zero = SwapchainCounterCreateInfoEXT Nothing
                                       zero

#endif


-- No documentation found for TopLevel "vkDisplayPowerControlEXT"
displayPowerControlEXT :: Device ->  DisplayKHR ->  DisplayPowerInfoEXT ->  IO ()
displayPowerControlEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetSwapchainCounterEXT"
getSwapchainCounterEXT :: Device ->  SwapchainKHR ->  SurfaceCounterFlagBitsEXT ->  IO (Word64)
getSwapchainCounterEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkRegisterDeviceEventEXT"
registerDeviceEventEXT :: Device ->  DeviceEventInfoEXT ->  Maybe AllocationCallbacks ->  IO (Fence)
registerDeviceEventEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkRegisterDisplayEventEXT"
registerDisplayEventEXT :: Device ->  DisplayKHR ->  DisplayEventInfoEXT ->  Maybe AllocationCallbacks ->  IO (Fence)
registerDisplayEventEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME"
pattern EXT_DISPLAY_CONTROL_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_DISPLAY_CONTROL_EXTENSION_NAME = VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_DISPLAY_CONTROL_SPEC_VERSION"
pattern EXT_DISPLAY_CONTROL_SPEC_VERSION :: Integral a => a
pattern EXT_DISPLAY_CONTROL_SPEC_VERSION = VK_EXT_DISPLAY_CONTROL_SPEC_VERSION
