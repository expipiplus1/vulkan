{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_display_control
  ( VkDeviceEventInfoEXT(..)
  , VkDeviceEventTypeEXT(..)
  , pattern VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT
  , VkDisplayEventInfoEXT(..)
  , VkDisplayEventTypeEXT(..)
  , pattern VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT
  , VkDisplayPowerInfoEXT(..)
  , VkDisplayPowerStateEXT(..)
  , pattern VK_DISPLAY_POWER_STATE_OFF_EXT
  , pattern VK_DISPLAY_POWER_STATE_SUSPEND_EXT
  , pattern VK_DISPLAY_POWER_STATE_ON_EXT
  , VkSwapchainCounterCreateInfoEXT(..)
  , FN_vkDisplayPowerControlEXT
  , PFN_vkDisplayPowerControlEXT
  , vkDisplayPowerControlEXT
  , FN_vkGetSwapchainCounterEXT
  , PFN_vkGetSwapchainCounterEXT
  , vkGetSwapchainCounterEXT
  , FN_vkRegisterDeviceEventEXT
  , PFN_vkRegisterDeviceEventEXT
  , vkRegisterDeviceEventEXT
  , FN_vkRegisterDisplayEventEXT
  , PFN_vkRegisterDisplayEventEXT
  , vkRegisterDisplayEventEXT
  , pattern VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME
  , pattern VK_EXT_DISPLAY_CONTROL_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT
  ) where

import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word64
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
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkFence
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter
  ( VkSurfaceCounterFlagBitsEXT(..)
  , VkSurfaceCounterFlagsEXT
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayKHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkSwapchainKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkDeviceEventInfoEXT - Describe a device event to create
--
-- = Description
--
-- Unresolved directive in VkDeviceEventInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkDeviceEventInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkDeviceEventInfoEXT = VkDeviceEventInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDeviceEventInfoEXT" "deviceEvent"
  vkDeviceEvent :: VkDeviceEventTypeEXT
  }
  deriving (Eq, Show)

instance Storable VkDeviceEventInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDeviceEventInfoEXT <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceEventInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceEventInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkDeviceEvent (poked :: VkDeviceEventInfoEXT))

instance Zero VkDeviceEventInfoEXT where
  zero = VkDeviceEventInfoEXT VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT
                              zero
                              zero

-- ** VkDeviceEventTypeEXT

-- | VkDeviceEventTypeEXT - Events that can occur on a device object
--
-- = See Also
--
-- No cross-references are available
newtype VkDeviceEventTypeEXT = VkDeviceEventTypeEXT Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkDeviceEventTypeEXT where
  showsPrec _ VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT = showString "VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT"
  showsPrec p (VkDeviceEventTypeEXT x) = showParen (p >= 11) (showString "VkDeviceEventTypeEXT " . showsPrec 11 x)

instance Read VkDeviceEventTypeEXT where
  readPrec = parens ( choose [ ("VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT", pure VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDeviceEventTypeEXT")
                        v <- step readPrec
                        pure (VkDeviceEventTypeEXT v)
                        )
                    )

-- | 'VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT' specifies that the fence is
-- signaled when a display is plugged into or unplugged from the specified
-- device. Applications /can/ use this notification to determine when they
-- need to re-enumerate the available displays on a device.
pattern VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT :: VkDeviceEventTypeEXT
pattern VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT = VkDeviceEventTypeEXT 0

-- | VkDisplayEventInfoEXT - Describe a display event to create
--
-- = Description
--
-- Unresolved directive in VkDisplayEventInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkDisplayEventInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkDisplayEventInfoEXT = VkDisplayEventInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @displayEvent@ is a 'VkDisplayEventTypeEXT' specifying when the fence
  -- will be signaled.
  vkDisplayEvent :: VkDisplayEventTypeEXT
  }
  deriving (Eq, Show)

instance Storable VkDisplayEventInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDisplayEventInfoEXT <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayEventInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayEventInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkDisplayEvent (poked :: VkDisplayEventInfoEXT))

instance Zero VkDisplayEventInfoEXT where
  zero = VkDisplayEventInfoEXT VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT
                               zero
                               zero

-- ** VkDisplayEventTypeEXT

-- | VkDisplayEventTypeEXT - Events that can occur on a display object
--
-- = See Also
--
-- No cross-references are available
newtype VkDisplayEventTypeEXT = VkDisplayEventTypeEXT Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkDisplayEventTypeEXT where
  showsPrec _ VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT = showString "VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT"
  showsPrec p (VkDisplayEventTypeEXT x) = showParen (p >= 11) (showString "VkDisplayEventTypeEXT " . showsPrec 11 x)

instance Read VkDisplayEventTypeEXT where
  readPrec = parens ( choose [ ("VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT", pure VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDisplayEventTypeEXT")
                        v <- step readPrec
                        pure (VkDisplayEventTypeEXT v)
                        )
                    )

-- | 'VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT' specifies that the fence is
-- signaled when the first pixel of the next display refresh cycle leaves
-- the display engine for the display.
pattern VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT :: VkDisplayEventTypeEXT
pattern VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT = VkDisplayEventTypeEXT 0

-- | VkDisplayPowerInfoEXT - Describe the power state of a display
--
-- = Description
--
-- Unresolved directive in VkDisplayPowerInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkDisplayPowerInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkDisplayPowerInfoEXT = VkDisplayPowerInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @powerState@ is a 'VkDisplayPowerStateEXT' value specifying the new
  -- power state of the display.
  vkPowerState :: VkDisplayPowerStateEXT
  }
  deriving (Eq, Show)

instance Storable VkDisplayPowerInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDisplayPowerInfoEXT <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayPowerInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayPowerInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkPowerState (poked :: VkDisplayPowerInfoEXT))

instance Zero VkDisplayPowerInfoEXT where
  zero = VkDisplayPowerInfoEXT VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT
                               zero
                               zero

-- ** VkDisplayPowerStateEXT

-- | VkDisplayPowerStateEXT - Possible power states for a display
--
-- = See Also
--
-- No cross-references are available
newtype VkDisplayPowerStateEXT = VkDisplayPowerStateEXT Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkDisplayPowerStateEXT where
  showsPrec _ VK_DISPLAY_POWER_STATE_OFF_EXT = showString "VK_DISPLAY_POWER_STATE_OFF_EXT"
  showsPrec _ VK_DISPLAY_POWER_STATE_SUSPEND_EXT = showString "VK_DISPLAY_POWER_STATE_SUSPEND_EXT"
  showsPrec _ VK_DISPLAY_POWER_STATE_ON_EXT = showString "VK_DISPLAY_POWER_STATE_ON_EXT"
  showsPrec p (VkDisplayPowerStateEXT x) = showParen (p >= 11) (showString "VkDisplayPowerStateEXT " . showsPrec 11 x)

instance Read VkDisplayPowerStateEXT where
  readPrec = parens ( choose [ ("VK_DISPLAY_POWER_STATE_OFF_EXT",     pure VK_DISPLAY_POWER_STATE_OFF_EXT)
                             , ("VK_DISPLAY_POWER_STATE_SUSPEND_EXT", pure VK_DISPLAY_POWER_STATE_SUSPEND_EXT)
                             , ("VK_DISPLAY_POWER_STATE_ON_EXT",      pure VK_DISPLAY_POWER_STATE_ON_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDisplayPowerStateEXT")
                        v <- step readPrec
                        pure (VkDisplayPowerStateEXT v)
                        )
                    )

-- | 'VK_DISPLAY_POWER_STATE_OFF_EXT' specifies that the display is powered
-- down.
pattern VK_DISPLAY_POWER_STATE_OFF_EXT :: VkDisplayPowerStateEXT
pattern VK_DISPLAY_POWER_STATE_OFF_EXT = VkDisplayPowerStateEXT 0

-- | 'VK_DISPLAY_POWER_STATE_SUSPEND_EXT' specifies that the display is put
-- into a low power mode, from which it /may/ be able to transition back to
-- 'VK_DISPLAY_POWER_STATE_ON_EXT' more quickly than if it were in
-- 'VK_DISPLAY_POWER_STATE_OFF_EXT'. This state /may/ be the same as
-- 'VK_DISPLAY_POWER_STATE_OFF_EXT'.
pattern VK_DISPLAY_POWER_STATE_SUSPEND_EXT :: VkDisplayPowerStateEXT
pattern VK_DISPLAY_POWER_STATE_SUSPEND_EXT = VkDisplayPowerStateEXT 1

-- | 'VK_DISPLAY_POWER_STATE_ON_EXT' specifies that the display is powered
-- on.
pattern VK_DISPLAY_POWER_STATE_ON_EXT :: VkDisplayPowerStateEXT
pattern VK_DISPLAY_POWER_STATE_ON_EXT = VkDisplayPowerStateEXT 2

-- | VkSwapchainCounterCreateInfoEXT - Specify the surface counters desired
--
-- == Valid Usage
--
-- -   The bits in @surfaceCounters@ /must/ be supported by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'::@surface@,
--     as reported by
--     'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.vkGetPhysicalDeviceSurfaceCapabilities2EXT'.
--
-- Unresolved directive in VkSwapchainCounterCreateInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkSwapchainCounterCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkSwapchainCounterCreateInfoEXT = VkSwapchainCounterCreateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @surfaceCounters@ is a bitmask of
  -- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.VkSurfaceCounterFlagBitsEXT'
  -- specifying surface counters to enable for the swapchain.
  vkSurfaceCounters :: VkSurfaceCounterFlagsEXT
  }
  deriving (Eq, Show)

instance Storable VkSwapchainCounterCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSwapchainCounterCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSwapchainCounterCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSwapchainCounterCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkSurfaceCounters (poked :: VkSwapchainCounterCreateInfoEXT))

instance Zero VkSwapchainCounterCreateInfoEXT where
  zero = VkSwapchainCounterCreateInfoEXT VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT
                                         zero
                                         zero

-- | vkDisplayPowerControlEXT - Set the power state of a display
--
-- = Parameters
--
-- -   @device@ is a logical device associated with @display@.
--
-- -   @display@ is the display whose power state is modified.
--
-- -   @pDisplayPowerInfo@ is an instance of 'VkDisplayPowerInfoEXT'
--     specifying the new power state of @display@.
--
-- = Description
--
-- Unresolved directive in vkDisplayPowerControlEXT.txt -
-- include::{generated}\/validity\/protos\/vkDisplayPowerControlEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDisplayPowerControlEXT" vkDisplayPowerControlEXT :: ("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayPowerInfo" ::: Ptr VkDisplayPowerInfoEXT) -> IO VkResult
#else
vkDisplayPowerControlEXT :: DeviceCmds -> ("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayPowerInfo" ::: Ptr VkDisplayPowerInfoEXT) -> IO VkResult
vkDisplayPowerControlEXT deviceCmds = mkVkDisplayPowerControlEXT (pVkDisplayPowerControlEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDisplayPowerControlEXT
  :: FunPtr (("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayPowerInfo" ::: Ptr VkDisplayPowerInfoEXT) -> IO VkResult) -> (("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayPowerInfo" ::: Ptr VkDisplayPowerInfoEXT) -> IO VkResult)
#endif

type FN_vkDisplayPowerControlEXT = ("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayPowerInfo" ::: Ptr VkDisplayPowerInfoEXT) -> IO VkResult
type PFN_vkDisplayPowerControlEXT = FunPtr FN_vkDisplayPowerControlEXT

-- | vkGetSwapchainCounterEXT - Query the current value of a surface counter
--
-- = Parameters
--
-- -   @device@ is the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' associated
--     with @swapchain@.
--
-- -   @swapchain@ is the swapchain from which to query the counter value.
--
-- -   @counter@ is the counter to query.
--
-- -   @pCounterValue@ will return the current value of the counter.
--
-- = Description
--
-- If a counter is not available because the swapchain is out of date, the
-- implementation /may/ return
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_ERROR_OUT_OF_DATE_KHR'.
--
-- == Valid Usage
--
-- -   One or more present commands on @swapchain@ /must/ have been
--     processed by the presentation engine.
--
-- Unresolved directive in vkGetSwapchainCounterEXT.txt -
-- include::{generated}\/validity\/protos\/vkGetSwapchainCounterEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetSwapchainCounterEXT" vkGetSwapchainCounterEXT :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("counter" ::: VkSurfaceCounterFlagBitsEXT) -> ("pCounterValue" ::: Ptr Word64) -> IO VkResult
#else
vkGetSwapchainCounterEXT :: DeviceCmds -> ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("counter" ::: VkSurfaceCounterFlagBitsEXT) -> ("pCounterValue" ::: Ptr Word64) -> IO VkResult
vkGetSwapchainCounterEXT deviceCmds = mkVkGetSwapchainCounterEXT (pVkGetSwapchainCounterEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSwapchainCounterEXT
  :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("counter" ::: VkSurfaceCounterFlagBitsEXT) -> ("pCounterValue" ::: Ptr Word64) -> IO VkResult) -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("counter" ::: VkSurfaceCounterFlagBitsEXT) -> ("pCounterValue" ::: Ptr Word64) -> IO VkResult)
#endif

type FN_vkGetSwapchainCounterEXT = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("counter" ::: VkSurfaceCounterFlagBitsEXT) -> ("pCounterValue" ::: Ptr Word64) -> IO VkResult
type PFN_vkGetSwapchainCounterEXT = FunPtr FN_vkGetSwapchainCounterEXT

-- | vkRegisterDeviceEventEXT - Signal a fence when a device event occurs
--
-- = Parameters
--
-- -   @device@ is a logical device on which the event /may/ occur.
--
-- -   @pDeviceEventInfo@ is a pointer to an instance of the
--     'VkDeviceEventInfoEXT' structure describing the event of interest to
--     the application.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pFence@ points to a handle in which the resulting fence object is
--     returned.
--
-- = Description
--
-- Unresolved directive in vkRegisterDeviceEventEXT.txt -
-- include::{generated}\/validity\/protos\/vkRegisterDeviceEventEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkRegisterDeviceEventEXT" vkRegisterDeviceEventEXT :: ("device" ::: VkDevice) -> ("pDeviceEventInfo" ::: Ptr VkDeviceEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult
#else
vkRegisterDeviceEventEXT :: DeviceCmds -> ("device" ::: VkDevice) -> ("pDeviceEventInfo" ::: Ptr VkDeviceEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult
vkRegisterDeviceEventEXT deviceCmds = mkVkRegisterDeviceEventEXT (pVkRegisterDeviceEventEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkRegisterDeviceEventEXT
  :: FunPtr (("device" ::: VkDevice) -> ("pDeviceEventInfo" ::: Ptr VkDeviceEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pDeviceEventInfo" ::: Ptr VkDeviceEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult)
#endif

type FN_vkRegisterDeviceEventEXT = ("device" ::: VkDevice) -> ("pDeviceEventInfo" ::: Ptr VkDeviceEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult
type PFN_vkRegisterDeviceEventEXT = FunPtr FN_vkRegisterDeviceEventEXT

-- | vkRegisterDisplayEventEXT - Signal a fence when a display event occurs
--
-- = Parameters
--
-- -   @device@ is a logical device associated with @display@
--
-- -   @display@ is the display on which the event /may/ occur.
--
-- -   @pDisplayEventInfo@ is a pointer to an instance of the
--     'VkDisplayEventInfoEXT' structure describing the event of interest
--     to the application.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pFence@ points to a handle in which the resulting fence object is
--     returned.
--
-- = Description
--
-- Unresolved directive in vkRegisterDisplayEventEXT.txt -
-- include::{generated}\/validity\/protos\/vkRegisterDisplayEventEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkRegisterDisplayEventEXT" vkRegisterDisplayEventEXT :: ("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayEventInfo" ::: Ptr VkDisplayEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult
#else
vkRegisterDisplayEventEXT :: DeviceCmds -> ("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayEventInfo" ::: Ptr VkDisplayEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult
vkRegisterDisplayEventEXT deviceCmds = mkVkRegisterDisplayEventEXT (pVkRegisterDisplayEventEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkRegisterDisplayEventEXT
  :: FunPtr (("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayEventInfo" ::: Ptr VkDisplayEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult) -> (("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayEventInfo" ::: Ptr VkDisplayEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult)
#endif

type FN_vkRegisterDisplayEventEXT = ("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayEventInfo" ::: Ptr VkDisplayEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult
type PFN_vkRegisterDisplayEventEXT = FunPtr FN_vkRegisterDisplayEventEXT

-- No documentation found for TopLevel "VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME"
pattern VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME = "VK_EXT_display_control"

-- No documentation found for TopLevel "VK_EXT_DISPLAY_CONTROL_SPEC_VERSION"
pattern VK_EXT_DISPLAY_CONTROL_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DISPLAY_CONTROL_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT = VkStructureType 1000091001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT = VkStructureType 1000091002

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT = VkStructureType 1000091000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT = VkStructureType 1000091003
