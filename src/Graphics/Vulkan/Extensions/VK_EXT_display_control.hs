{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_display_control
  ( withCStructDeviceEventInfoEXT
  , fromCStructDeviceEventInfoEXT
  , DeviceEventInfoEXT(..)
  , DeviceEventTypeEXT
  , withCStructDisplayEventInfoEXT
  , fromCStructDisplayEventInfoEXT
  , DisplayEventInfoEXT(..)
  , DisplayEventTypeEXT
  , withCStructDisplayPowerInfoEXT
  , fromCStructDisplayPowerInfoEXT
  , DisplayPowerInfoEXT(..)
  , DisplayPowerStateEXT
  , withCStructSwapchainCounterCreateInfoEXT
  , fromCStructSwapchainCounterCreateInfoEXT
  , SwapchainCounterCreateInfoEXT(..)
  , displayPowerControlEXT
  , getSwapchainCounterEXT
  , registerDeviceEventEXT
  , registerDisplayEventEXT
  , pattern VK_EXT_DISPLAY_CONTROL_SPEC_VERSION
  , pattern VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.Word
  ( Word64
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( displayPowerControlEXT
  , getSwapchainCounterEXT
  , registerDeviceEventEXT
  , registerDisplayEventEXT
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_display_control
  ( VkDeviceEventInfoEXT(..)
  , VkDeviceEventTypeEXT(..)
  , VkDisplayEventInfoEXT(..)
  , VkDisplayEventTypeEXT(..)
  , VkDisplayPowerInfoEXT(..)
  , VkDisplayPowerStateEXT(..)
  , VkSwapchainCounterCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Queue
  ( Fence
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter
  ( SurfaceCounterFlagBitsEXT
  , SurfaceCounterFlagsEXT
  )
import Graphics.Vulkan.Extensions.VK_KHR_display
  ( DisplayKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( SwapchainKHR
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_display_control
  ( pattern VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME
  , pattern VK_EXT_DISPLAY_CONTROL_SPEC_VERSION
  )


-- No documentation found for TopLevel "DeviceEventInfoEXT"
data DeviceEventInfoEXT = DeviceEventInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "DeviceEventInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceEventInfoEXT" "deviceEvent"
  vkDeviceEvent :: DeviceEventTypeEXT
  }
  deriving (Show, Eq)
withCStructDeviceEventInfoEXT :: DeviceEventInfoEXT -> (VkDeviceEventInfoEXT -> IO a) -> IO a
withCStructDeviceEventInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: DeviceEventInfoEXT)) (\pPNext -> cont (VkDeviceEventInfoEXT VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT pPNext (vkDeviceEvent (from :: DeviceEventInfoEXT))))
fromCStructDeviceEventInfoEXT :: VkDeviceEventInfoEXT -> IO DeviceEventInfoEXT
fromCStructDeviceEventInfoEXT c = DeviceEventInfoEXT <$> -- Univalued Member elided
                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceEventInfoEXT)))
                                                     <*> pure (vkDeviceEvent (c :: VkDeviceEventInfoEXT))
-- No documentation found for TopLevel "DeviceEventTypeEXT"
type DeviceEventTypeEXT = VkDeviceEventTypeEXT
-- No documentation found for TopLevel "DisplayEventInfoEXT"
data DisplayEventInfoEXT = DisplayEventInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "DisplayEventInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayEventInfoEXT" "displayEvent"
  vkDisplayEvent :: DisplayEventTypeEXT
  }
  deriving (Show, Eq)
withCStructDisplayEventInfoEXT :: DisplayEventInfoEXT -> (VkDisplayEventInfoEXT -> IO a) -> IO a
withCStructDisplayEventInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: DisplayEventInfoEXT)) (\pPNext -> cont (VkDisplayEventInfoEXT VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT pPNext (vkDisplayEvent (from :: DisplayEventInfoEXT))))
fromCStructDisplayEventInfoEXT :: VkDisplayEventInfoEXT -> IO DisplayEventInfoEXT
fromCStructDisplayEventInfoEXT c = DisplayEventInfoEXT <$> -- Univalued Member elided
                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDisplayEventInfoEXT)))
                                                       <*> pure (vkDisplayEvent (c :: VkDisplayEventInfoEXT))
-- No documentation found for TopLevel "DisplayEventTypeEXT"
type DisplayEventTypeEXT = VkDisplayEventTypeEXT
-- No documentation found for TopLevel "DisplayPowerInfoEXT"
data DisplayPowerInfoEXT = DisplayPowerInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "DisplayPowerInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayPowerInfoEXT" "powerState"
  vkPowerState :: DisplayPowerStateEXT
  }
  deriving (Show, Eq)
withCStructDisplayPowerInfoEXT :: DisplayPowerInfoEXT -> (VkDisplayPowerInfoEXT -> IO a) -> IO a
withCStructDisplayPowerInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: DisplayPowerInfoEXT)) (\pPNext -> cont (VkDisplayPowerInfoEXT VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT pPNext (vkPowerState (from :: DisplayPowerInfoEXT))))
fromCStructDisplayPowerInfoEXT :: VkDisplayPowerInfoEXT -> IO DisplayPowerInfoEXT
fromCStructDisplayPowerInfoEXT c = DisplayPowerInfoEXT <$> -- Univalued Member elided
                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDisplayPowerInfoEXT)))
                                                       <*> pure (vkPowerState (c :: VkDisplayPowerInfoEXT))
-- No documentation found for TopLevel "DisplayPowerStateEXT"
type DisplayPowerStateEXT = VkDisplayPowerStateEXT
-- No documentation found for TopLevel "SwapchainCounterCreateInfoEXT"
data SwapchainCounterCreateInfoEXT = SwapchainCounterCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "SwapchainCounterCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SwapchainCounterCreateInfoEXT" "surfaceCounters"
  vkSurfaceCounters :: SurfaceCounterFlagsEXT
  }
  deriving (Show, Eq)
withCStructSwapchainCounterCreateInfoEXT :: SwapchainCounterCreateInfoEXT -> (VkSwapchainCounterCreateInfoEXT -> IO a) -> IO a
withCStructSwapchainCounterCreateInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: SwapchainCounterCreateInfoEXT)) (\pPNext -> cont (VkSwapchainCounterCreateInfoEXT VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT pPNext (vkSurfaceCounters (from :: SwapchainCounterCreateInfoEXT))))
fromCStructSwapchainCounterCreateInfoEXT :: VkSwapchainCounterCreateInfoEXT -> IO SwapchainCounterCreateInfoEXT
fromCStructSwapchainCounterCreateInfoEXT c = SwapchainCounterCreateInfoEXT <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSwapchainCounterCreateInfoEXT)))
                                                                           <*> pure (vkSurfaceCounters (c :: VkSwapchainCounterCreateInfoEXT))

-- | Wrapper for vkDisplayPowerControlEXT
displayPowerControlEXT :: Device ->  DisplayKHR ->  DisplayPowerInfoEXT ->  IO ()
displayPowerControlEXT = \(Device device commandTable) -> \display -> \displayPowerInfo -> (\a -> withCStructDisplayPowerInfoEXT a . flip with) displayPowerInfo (\pDisplayPowerInfo -> Graphics.Vulkan.C.Dynamic.displayPowerControlEXT commandTable device display pDisplayPowerInfo >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))

-- | Wrapper for vkGetSwapchainCounterEXT
getSwapchainCounterEXT :: Device ->  SwapchainKHR ->  SurfaceCounterFlagBitsEXT ->  IO (Word64)
getSwapchainCounterEXT = \(Device device commandTable) -> \swapchain -> \counter -> alloca (\pCounterValue -> Graphics.Vulkan.C.Dynamic.getSwapchainCounterEXT commandTable device swapchain counter pCounterValue >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pCounterValue)))

-- | Wrapper for vkRegisterDeviceEventEXT
registerDeviceEventEXT :: Device ->  DeviceEventInfoEXT ->  Maybe AllocationCallbacks ->  IO (Fence)
registerDeviceEventEXT = \(Device device commandTable) -> \deviceEventInfo -> \allocator -> alloca (\pFence -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructDeviceEventInfoEXT a . flip with) deviceEventInfo (\pDeviceEventInfo -> Graphics.Vulkan.C.Dynamic.registerDeviceEventEXT commandTable device pDeviceEventInfo pAllocator pFence >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pFence)))))

-- | Wrapper for vkRegisterDisplayEventEXT
registerDisplayEventEXT :: Device ->  DisplayKHR ->  DisplayEventInfoEXT ->  Maybe AllocationCallbacks ->  IO (Fence)
registerDisplayEventEXT = \(Device device commandTable) -> \display -> \displayEventInfo -> \allocator -> alloca (\pFence -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructDisplayEventInfoEXT a . flip with) displayEventInfo (\pDisplayEventInfo -> Graphics.Vulkan.C.Dynamic.registerDisplayEventEXT commandTable device display pDisplayEventInfo pAllocator pFence >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pFence)))))
