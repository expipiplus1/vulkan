{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_display_control
  ( VkDisplayPowerStateEXT(..)
  , pattern VK_DISPLAY_POWER_STATE_OFF_EXT
  , pattern VK_DISPLAY_POWER_STATE_SUSPEND_EXT
  , pattern VK_DISPLAY_POWER_STATE_ON_EXT
  , VkDeviceEventTypeEXT(..)
  , pattern VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT
  , VkDisplayEventTypeEXT(..)
  , pattern VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT
  , pattern VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT
  , pattern VK_EXT_DISPLAY_CONTROL_SPEC_VERSION
  , pattern VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME
  , vkDisplayPowerControlEXT
  , vkRegisterDeviceEventEXT
  , vkRegisterDisplayEventEXT
  , vkGetSwapchainCounterEXT
  , VkDisplayPowerInfoEXT(..)
  , VkDeviceEventInfoEXT(..)
  , VkDisplayEventInfoEXT(..)
  , VkSwapchainCounterCreateInfoEXT(..)
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
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
  )
import Graphics.Vulkan.NamedType
  ( (:::)
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


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.Core10.Queue
  ( VkFence
  )
import Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter
  ( VkSurfaceCounterFlagsEXT
  , VkSurfaceCounterFlagBitsEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_display
  ( VkDisplayKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( VkSwapchainKHR
  )


-- ** VkDisplayPowerStateEXT

-- | 
newtype VkDisplayPowerStateEXT = VkDisplayPowerStateEXT Int32
  deriving (Eq, Ord, Storable)

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

-- | 
pattern VK_DISPLAY_POWER_STATE_OFF_EXT :: VkDisplayPowerStateEXT
pattern VK_DISPLAY_POWER_STATE_OFF_EXT = VkDisplayPowerStateEXT 0

-- | 
pattern VK_DISPLAY_POWER_STATE_SUSPEND_EXT :: VkDisplayPowerStateEXT
pattern VK_DISPLAY_POWER_STATE_SUSPEND_EXT = VkDisplayPowerStateEXT 1

-- | 
pattern VK_DISPLAY_POWER_STATE_ON_EXT :: VkDisplayPowerStateEXT
pattern VK_DISPLAY_POWER_STATE_ON_EXT = VkDisplayPowerStateEXT 2
-- ** VkDeviceEventTypeEXT

-- | 
newtype VkDeviceEventTypeEXT = VkDeviceEventTypeEXT Int32
  deriving (Eq, Ord, Storable)

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

-- | 
pattern VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT :: VkDeviceEventTypeEXT
pattern VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT = VkDeviceEventTypeEXT 0
-- ** VkDisplayEventTypeEXT

-- | 
newtype VkDisplayEventTypeEXT = VkDisplayEventTypeEXT Int32
  deriving (Eq, Ord, Storable)

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

-- | 
pattern VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT :: VkDisplayEventTypeEXT
pattern VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT = VkDisplayEventTypeEXT 0
-- | Nothing
pattern VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT = VkStructureType 1000091000
-- | Nothing
pattern VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT = VkStructureType 1000091001
-- | Nothing
pattern VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT = VkStructureType 1000091002
-- | Nothing
pattern VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT = VkStructureType 1000091003
pattern VK_EXT_DISPLAY_CONTROL_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DISPLAY_CONTROL_SPEC_VERSION = 1
pattern VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME = "VK_EXT_display_control"
-- | 
foreign import ccall "vkDisplayPowerControlEXT" vkDisplayPowerControlEXT :: ("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayPowerInfo" ::: Ptr VkDisplayPowerInfoEXT) -> IO VkResult
-- | 
foreign import ccall "vkRegisterDeviceEventEXT" vkRegisterDeviceEventEXT :: ("device" ::: VkDevice) -> ("pDeviceEventInfo" ::: Ptr VkDeviceEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult
-- | 
foreign import ccall "vkRegisterDisplayEventEXT" vkRegisterDisplayEventEXT :: ("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayEventInfo" ::: Ptr VkDisplayEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult
-- | 
foreign import ccall "vkGetSwapchainCounterEXT" vkGetSwapchainCounterEXT :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("counter" ::: VkSurfaceCounterFlagBitsEXT) -> ("pCounterValue" ::: Ptr Word64) -> IO VkResult
-- | TODO: Struct comments
data VkDisplayPowerInfoEXT = VkDisplayPowerInfoEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkPowerState :: VkDisplayPowerStateEXT
  }
  deriving (Eq, Show)

instance Storable VkDisplayPowerInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDisplayPowerInfoEXT <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayPowerInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkDisplayPowerInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkPowerState (poked :: VkDisplayPowerInfoEXT))
-- | TODO: Struct comments
data VkDeviceEventInfoEXT = VkDeviceEventInfoEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkDeviceEvent :: VkDeviceEventTypeEXT
  }
  deriving (Eq, Show)

instance Storable VkDeviceEventInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDeviceEventInfoEXT <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceEventInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkDeviceEventInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkDeviceEvent (poked :: VkDeviceEventInfoEXT))
-- | TODO: Struct comments
data VkDisplayEventInfoEXT = VkDisplayEventInfoEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkDisplayEvent :: VkDisplayEventTypeEXT
  }
  deriving (Eq, Show)

instance Storable VkDisplayEventInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDisplayEventInfoEXT <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayEventInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkDisplayEventInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkDisplayEvent (poked :: VkDisplayEventInfoEXT))
-- | TODO: Struct comments
data VkSwapchainCounterCreateInfoEXT = VkSwapchainCounterCreateInfoEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkSurfaceCounters :: VkSurfaceCounterFlagsEXT
  }
  deriving (Eq, Show)

instance Storable VkSwapchainCounterCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSwapchainCounterCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSwapchainCounterCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkSwapchainCounterCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkSurfaceCounters (poked :: VkSwapchainCounterCreateInfoEXT))
