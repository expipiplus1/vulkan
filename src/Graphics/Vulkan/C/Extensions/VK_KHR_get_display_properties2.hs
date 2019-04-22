{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2
  ( VkDisplayModeProperties2KHR(..)
  , VkDisplayPlaneCapabilities2KHR(..)
  , VkDisplayPlaneInfo2KHR(..)
  , VkDisplayPlaneProperties2KHR(..)
  , VkDisplayProperties2KHR(..)
  , FN_vkGetDisplayModeProperties2KHR
  , PFN_vkGetDisplayModeProperties2KHR
  , vkGetDisplayModeProperties2KHR
  , FN_vkGetDisplayPlaneCapabilities2KHR
  , PFN_vkGetDisplayPlaneCapabilities2KHR
  , vkGetDisplayPlaneCapabilities2KHR
  , FN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR
  , PFN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR
  , vkGetPhysicalDeviceDisplayPlaneProperties2KHR
  , FN_vkGetPhysicalDeviceDisplayProperties2KHR
  , PFN_vkGetPhysicalDeviceDisplayProperties2KHR
  , vkGetPhysicalDeviceDisplayProperties2KHR
  , pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME
  , pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR
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
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkPhysicalDevice
  )
import Graphics.Vulkan.C.Dynamic
  ( InstanceCmds(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayModePropertiesKHR(..)
  , VkDisplayPlaneCapabilitiesKHR(..)
  , VkDisplayPlanePropertiesKHR(..)
  , VkDisplayPropertiesKHR(..)
  , VkDisplayKHR
  , VkDisplayModeKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkDisplayModeProperties2KHR - Structure describing an available display
-- mode
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayModePropertiesKHR',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetDisplayModeProperties2KHR'
data VkDisplayModeProperties2KHR = VkDisplayModeProperties2KHR
  { -- | @sType@ /must/ be 'VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR'
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @displayModeProperties@ is an instance of the
  -- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayModePropertiesKHR'
  -- structure.
  vkDisplayModeProperties :: VkDisplayModePropertiesKHR
  }
  deriving (Eq, Show)

instance Storable VkDisplayModeProperties2KHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDisplayModeProperties2KHR <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayModeProperties2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayModeProperties2KHR))
                *> poke (ptr `plusPtr` 16) (vkDisplayModeProperties (poked :: VkDisplayModeProperties2KHR))

instance Zero VkDisplayModeProperties2KHR where
  zero = VkDisplayModeProperties2KHR VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR
                                     zero
                                     zero

-- | VkDisplayPlaneCapabilities2KHR - Structure describing the capabilities
-- of a mode and plane combination
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPlaneCapabilitiesKHR',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetDisplayPlaneCapabilities2KHR'
data VkDisplayPlaneCapabilities2KHR = VkDisplayPlaneCapabilities2KHR
  { -- | @sType@ /must/ be 'VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR'
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @capabilities@ is an instance of the
  -- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPlaneCapabilitiesKHR'
  -- structure.
  vkCapabilities :: VkDisplayPlaneCapabilitiesKHR
  }
  deriving (Eq, Show)

instance Storable VkDisplayPlaneCapabilities2KHR where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek ptr = VkDisplayPlaneCapabilities2KHR <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayPlaneCapabilities2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayPlaneCapabilities2KHR))
                *> poke (ptr `plusPtr` 16) (vkCapabilities (poked :: VkDisplayPlaneCapabilities2KHR))

instance Zero VkDisplayPlaneCapabilities2KHR where
  zero = VkDisplayPlaneCapabilities2KHR VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR
                                        zero
                                        zero

-- | VkDisplayPlaneInfo2KHR - Structure defining the intended configuration
-- of a display plane
--
-- = Description
--
-- __Note__
--
-- This parameter also implicitly specifies a display.
--
-- -   @planeIndex@ is the plane which the application intends to use with
--     the display.
--
-- The members of 'VkDisplayPlaneInfo2KHR' correspond to the arguments to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetDisplayPlaneCapabilitiesKHR',
-- with @sType@ and @pNext@ added for extensibility.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be 'VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @mode@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayModeKHR'
--     handle
--
-- == Host Synchronization
--
-- -   Host access to @mode@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayModeKHR',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetDisplayPlaneCapabilities2KHR'
data VkDisplayPlaneInfo2KHR = VkDisplayPlaneInfo2KHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @mode@ is the display mode the application intends to program when using
  -- the specified plane.
  vkMode :: VkDisplayModeKHR
  , -- No documentation found for Nested "VkDisplayPlaneInfo2KHR" "planeIndex"
  vkPlaneIndex :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDisplayPlaneInfo2KHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDisplayPlaneInfo2KHR <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayPlaneInfo2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayPlaneInfo2KHR))
                *> poke (ptr `plusPtr` 16) (vkMode (poked :: VkDisplayPlaneInfo2KHR))
                *> poke (ptr `plusPtr` 24) (vkPlaneIndex (poked :: VkDisplayPlaneInfo2KHR))

instance Zero VkDisplayPlaneInfo2KHR where
  zero = VkDisplayPlaneInfo2KHR VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR
                                zero
                                zero
                                zero

-- | VkDisplayPlaneProperties2KHR - Structure describing an available display
-- plane
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPlanePropertiesKHR',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetPhysicalDeviceDisplayPlaneProperties2KHR'
data VkDisplayPlaneProperties2KHR = VkDisplayPlaneProperties2KHR
  { -- | @sType@ /must/ be 'VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR'
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @displayPlaneProperties@ is an instance of the
  -- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPlanePropertiesKHR'
  -- structure.
  vkDisplayPlaneProperties :: VkDisplayPlanePropertiesKHR
  }
  deriving (Eq, Show)

instance Storable VkDisplayPlaneProperties2KHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDisplayPlaneProperties2KHR <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayPlaneProperties2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayPlaneProperties2KHR))
                *> poke (ptr `plusPtr` 16) (vkDisplayPlaneProperties (poked :: VkDisplayPlaneProperties2KHR))

instance Zero VkDisplayPlaneProperties2KHR where
  zero = VkDisplayPlaneProperties2KHR VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR
                                      zero
                                      zero

-- | VkDisplayProperties2KHR - Structure describing an available display
-- device
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPropertiesKHR',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetPhysicalDeviceDisplayProperties2KHR'
data VkDisplayProperties2KHR = VkDisplayProperties2KHR
  { -- | @sType@ /must/ be 'VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR'
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @displayProperties@ is an instance of the
  -- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPropertiesKHR'
  -- structure.
  vkDisplayProperties :: VkDisplayPropertiesKHR
  }
  deriving (Eq, Show)

instance Storable VkDisplayProperties2KHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkDisplayProperties2KHR <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayProperties2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayProperties2KHR))
                *> poke (ptr `plusPtr` 16) (vkDisplayProperties (poked :: VkDisplayProperties2KHR))

instance Zero VkDisplayProperties2KHR where
  zero = VkDisplayProperties2KHR VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR
                                 zero
                                 zero

-- | vkGetDisplayModeProperties2KHR - Query information about the available
-- display modes.
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device associated with @display@.
--
-- -   @display@ is the display to query.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     display modes available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'VkDisplayModeProperties2KHR' structures.
--
-- = Description
--
-- 'vkGetDisplayModeProperties2KHR' behaves similarly to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetDisplayModePropertiesKHR',
-- with the ability to return extended information via chained output
-- structures.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @display@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayKHR' handle
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ 'VkDisplayModeProperties2KHR'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayKHR',
-- 'VkDisplayModeProperties2KHR',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDisplayModeProperties2KHR" vkGetDisplayModeProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModeProperties2KHR) -> IO VkResult
#else
vkGetDisplayModeProperties2KHR :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModeProperties2KHR) -> IO VkResult
vkGetDisplayModeProperties2KHR deviceCmds = mkVkGetDisplayModeProperties2KHR (pVkGetDisplayModeProperties2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayModeProperties2KHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModeProperties2KHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModeProperties2KHR) -> IO VkResult)
#endif

type FN_vkGetDisplayModeProperties2KHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModeProperties2KHR) -> IO VkResult
type PFN_vkGetDisplayModeProperties2KHR = FunPtr FN_vkGetDisplayModeProperties2KHR

-- | vkGetDisplayPlaneCapabilities2KHR - Query capabilities of a mode and
-- plane combination
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device associated with
--     @pDisplayPlaneInfo@.
--
-- -   @pDisplayPlaneInfo@ is a pointer to an instance of the
--     'VkDisplayPlaneInfo2KHR' structure describing the plane and mode.
--
-- -   @pCapabilities@ is a pointer to a 'VkDisplayPlaneCapabilities2KHR'
--     structure in which the capabilities are returned.
--
-- = Description
--
-- 'vkGetDisplayPlaneCapabilities2KHR' behaves similarly to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetDisplayPlaneCapabilitiesKHR',
-- with the ability to specify extended inputs via chained input
-- structures, and to return extended information via chained output
-- structures.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'VkDisplayPlaneCapabilities2KHR', 'VkDisplayPlaneInfo2KHR',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDisplayPlaneCapabilities2KHR" vkGetDisplayPlaneCapabilities2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pDisplayPlaneInfo" ::: Ptr VkDisplayPlaneInfo2KHR) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilities2KHR) -> IO VkResult
#else
vkGetDisplayPlaneCapabilities2KHR :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pDisplayPlaneInfo" ::: Ptr VkDisplayPlaneInfo2KHR) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilities2KHR) -> IO VkResult
vkGetDisplayPlaneCapabilities2KHR deviceCmds = mkVkGetDisplayPlaneCapabilities2KHR (pVkGetDisplayPlaneCapabilities2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayPlaneCapabilities2KHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pDisplayPlaneInfo" ::: Ptr VkDisplayPlaneInfo2KHR) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilities2KHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pDisplayPlaneInfo" ::: Ptr VkDisplayPlaneInfo2KHR) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilities2KHR) -> IO VkResult)
#endif

type FN_vkGetDisplayPlaneCapabilities2KHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("pDisplayPlaneInfo" ::: Ptr VkDisplayPlaneInfo2KHR) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilities2KHR) -> IO VkResult
type PFN_vkGetDisplayPlaneCapabilities2KHR = FunPtr FN_vkGetDisplayPlaneCapabilities2KHR

-- | vkGetPhysicalDeviceDisplayPlaneProperties2KHR - Query information about
-- the available display planes.
--
-- = Parameters
--
-- -   @physicalDevice@ is a physical device.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     display planes available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'VkDisplayPlaneProperties2KHR' structures.
--
-- = Description
--
-- 'vkGetPhysicalDeviceDisplayPlaneProperties2KHR' behaves similarly to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetPhysicalDeviceDisplayPlanePropertiesKHR',
-- with the ability to return extended information via chained output
-- structures.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ 'VkDisplayPlaneProperties2KHR'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'VkDisplayPlaneProperties2KHR',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceDisplayPlaneProperties2KHR" vkGetPhysicalDeviceDisplayPlaneProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlaneProperties2KHR) -> IO VkResult
#else
vkGetPhysicalDeviceDisplayPlaneProperties2KHR :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlaneProperties2KHR) -> IO VkResult
vkGetPhysicalDeviceDisplayPlaneProperties2KHR deviceCmds = mkVkGetPhysicalDeviceDisplayPlaneProperties2KHR (pVkGetPhysicalDeviceDisplayPlaneProperties2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDisplayPlaneProperties2KHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlaneProperties2KHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlaneProperties2KHR) -> IO VkResult)
#endif

type FN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlaneProperties2KHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR = FunPtr FN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR

-- | vkGetPhysicalDeviceDisplayProperties2KHR - Query information about the
-- available displays
--
-- = Parameters
--
-- -   @physicalDevice@ is a physical device.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     display devices available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'VkDisplayProperties2KHR' structures.
--
-- = Description
--
-- 'vkGetPhysicalDeviceDisplayProperties2KHR' behaves similarly to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetPhysicalDeviceDisplayPropertiesKHR',
-- with the ability to return extended information via chained output
-- structures.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ 'VkDisplayProperties2KHR' structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'VkDisplayProperties2KHR',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceDisplayProperties2KHR" vkGetPhysicalDeviceDisplayProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayProperties2KHR) -> IO VkResult
#else
vkGetPhysicalDeviceDisplayProperties2KHR :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayProperties2KHR) -> IO VkResult
vkGetPhysicalDeviceDisplayProperties2KHR deviceCmds = mkVkGetPhysicalDeviceDisplayProperties2KHR (pVkGetPhysicalDeviceDisplayProperties2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDisplayProperties2KHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayProperties2KHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayProperties2KHR) -> IO VkResult)
#endif

type FN_vkGetPhysicalDeviceDisplayProperties2KHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayProperties2KHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceDisplayProperties2KHR = FunPtr FN_vkGetPhysicalDeviceDisplayProperties2KHR

-- No documentation found for TopLevel "VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME"
pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME = "VK_KHR_get_display_properties2"

-- No documentation found for TopLevel "VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION"
pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION :: Integral a => a
pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR = VkStructureType 1000121002

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR = VkStructureType 1000121004

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR = VkStructureType 1000121003

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR = VkStructureType 1000121001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR = VkStructureType 1000121000
