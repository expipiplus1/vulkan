{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive
  ( HMONITOR
  , VkFullScreenExclusiveEXT(..)
  , pattern VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT
  , pattern VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT
  , pattern VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT
  , pattern VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT
  , VkSurfaceCapabilitiesFullScreenExclusiveEXT(..)
  , VkSurfaceFullScreenExclusiveInfoEXT(..)
  , VkSurfaceFullScreenExclusiveWin32InfoEXT(..)
  , FN_vkAcquireFullScreenExclusiveModeEXT
  , PFN_vkAcquireFullScreenExclusiveModeEXT
  , vkAcquireFullScreenExclusiveModeEXT
  , FN_vkGetPhysicalDeviceSurfacePresentModes2EXT
  , PFN_vkGetPhysicalDeviceSurfacePresentModes2EXT
  , vkGetPhysicalDeviceSurfacePresentModes2EXT
  , FN_vkReleaseFullScreenExclusiveModeEXT
  , PFN_vkReleaseFullScreenExclusiveModeEXT
  , vkReleaseFullScreenExclusiveModeEXT
  , pattern VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT
  , pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME
  , pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT
  , pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT
  , vkGetDeviceGroupSurfacePresentModes2EXT
  ) where

import Data.Int
  ( Int32
  )
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
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  , VkPhysicalDevice
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  , InstanceCmds(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2
  ( VkPhysicalDeviceSurfaceInfo2KHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkPresentModeKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkSwapchainKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_device_group
  ( vkGetDeviceGroupSurfacePresentModes2EXT
  )


-- No documentation found for TopLevel "HMONITOR"
type HMONITOR = Ptr ()
  

-- ** VkFullScreenExclusiveEXT

-- | VkFullScreenExclusiveEXT - Hint values an application can specify
-- affecting full-screen transition behavior
--
-- = See Also
--
-- 'VkSurfaceFullScreenExclusiveInfoEXT'
newtype VkFullScreenExclusiveEXT = VkFullScreenExclusiveEXT Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkFullScreenExclusiveEXT where
  showsPrec _ VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT = showString "VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT"
  showsPrec _ VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT = showString "VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT"
  showsPrec _ VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT = showString "VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT"
  showsPrec _ VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT = showString "VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT"
  showsPrec p (VkFullScreenExclusiveEXT x) = showParen (p >= 11) (showString "VkFullScreenExclusiveEXT " . showsPrec 11 x)

instance Read VkFullScreenExclusiveEXT where
  readPrec = parens ( choose [ ("VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT",                pure VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT)
                             , ("VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT",                pure VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT)
                             , ("VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT",             pure VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT)
                             , ("VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT", pure VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFullScreenExclusiveEXT")
                        v <- step readPrec
                        pure (VkFullScreenExclusiveEXT v)
                        )
                    )

-- | 'VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT' indicates the implementation
-- /should/ determine the appropriate full-screen method by whatever means
-- it deems appropriate.
pattern VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT :: VkFullScreenExclusiveEXT
pattern VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT = VkFullScreenExclusiveEXT 0

-- | 'VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT' indicates the implementation
-- /may/ use full-screen exclusive mechanisms when available. Such
-- mechanisms /may/ result in better performance and\/or the availability
-- of different presentation capabilities, but /may/ require a more
-- disruptive transition during swapchain initialization, first
-- presentation and\/or destruction.
pattern VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT :: VkFullScreenExclusiveEXT
pattern VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT = VkFullScreenExclusiveEXT 1

-- | 'VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT' indicates the implementation
-- /should/ avoid using full-screen mechanisms which rely on disruptive
-- transitions.
pattern VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT :: VkFullScreenExclusiveEXT
pattern VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT = VkFullScreenExclusiveEXT 2

-- | 'VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT' indicates the
-- application will manage full-screen exclusive mode by using the
-- 'vkAcquireFullScreenExclusiveModeEXT' and
-- 'vkReleaseFullScreenExclusiveModeEXT' commands.
pattern VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT :: VkFullScreenExclusiveEXT
pattern VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT = VkFullScreenExclusiveEXT 3

-- | VkSurfaceCapabilitiesFullScreenExclusiveEXT - Structure describing full
-- screen exclusive capabilities of a surface
--
-- = Description
--
-- This structure /can/ be included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.VkSurfaceCapabilities2KHR'
-- to determine support for exclusive full-screen access. If
-- @fullScreenExclusiveSupported@ is
-- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', it indicates that exclusive
-- full-screen access is not obtainable for this surface.
--
-- Applications /must/ not attempt to create swapchains with
-- 'VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT' set if
-- @fullScreenExclusiveSupported@ is
-- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkSurfaceCapabilitiesFullScreenExclusiveEXT = VkSurfaceCapabilitiesFullScreenExclusiveEXT
  { -- | @sType@ /must/ be
  -- 'VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT'
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSurfaceCapabilitiesFullScreenExclusiveEXT" "fullScreenExclusiveSupported"
  vkFullScreenExclusiveSupported :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkSurfaceCapabilitiesFullScreenExclusiveEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSurfaceCapabilitiesFullScreenExclusiveEXT <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSurfaceCapabilitiesFullScreenExclusiveEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSurfaceCapabilitiesFullScreenExclusiveEXT))
                *> poke (ptr `plusPtr` 16) (vkFullScreenExclusiveSupported (poked :: VkSurfaceCapabilitiesFullScreenExclusiveEXT))

instance Zero VkSurfaceCapabilitiesFullScreenExclusiveEXT where
  zero = VkSurfaceCapabilitiesFullScreenExclusiveEXT VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT
                                                     zero
                                                     zero

-- | VkSurfaceFullScreenExclusiveInfoEXT - Structure specifying the preferred
-- full-screen transition behavior
--
-- = Description
--
-- If this structure is not present, @fullScreenExclusive@ is considered to
-- be 'VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'VkFullScreenExclusiveEXT',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkSurfaceFullScreenExclusiveInfoEXT = VkSurfaceFullScreenExclusiveInfoEXT
  { -- | @sType@ /must/ be
  -- 'VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT'
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @fullScreenExclusive@ /must/ be a valid 'VkFullScreenExclusiveEXT' value
  vkFullScreenExclusive :: VkFullScreenExclusiveEXT
  }
  deriving (Eq, Show)

instance Storable VkSurfaceFullScreenExclusiveInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSurfaceFullScreenExclusiveInfoEXT <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSurfaceFullScreenExclusiveInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSurfaceFullScreenExclusiveInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkFullScreenExclusive (poked :: VkSurfaceFullScreenExclusiveInfoEXT))

instance Zero VkSurfaceFullScreenExclusiveInfoEXT where
  zero = VkSurfaceFullScreenExclusiveInfoEXT VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT
                                             zero
                                             zero

-- | VkSurfaceFullScreenExclusiveWin32InfoEXT - Structure specifying
-- additional creation parameters specific to Win32 fullscreen exclusive
-- mode
--
-- = Description
--
-- __Note__
--
-- If @hmonitor@ is invalidated (e.g. the monitor is unplugged) during the
-- lifetime of a swapchain created with this structure, operations on that
-- swapchain will return
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_ERROR_OUT_OF_DATE_KHR'.
--
-- __Note__
--
-- It’s the responsibility of the application to change the display
-- settings of the targeted Win32 display using the appropriate platform
-- APIs. Such changes /may/ alter the surface capabilities reported for the
-- created surface.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkSurfaceFullScreenExclusiveWin32InfoEXT = VkSurfaceFullScreenExclusiveWin32InfoEXT
  { -- | @sType@ /must/ be
  -- 'VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT'
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @hmonitor@ /must/ be a valid 'HMONITOR'
  vkHmonitor :: HMONITOR
  }
  deriving (Eq, Show)

instance Storable VkSurfaceFullScreenExclusiveWin32InfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSurfaceFullScreenExclusiveWin32InfoEXT <$> peek (ptr `plusPtr` 0)
                                                      <*> peek (ptr `plusPtr` 8)
                                                      <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSurfaceFullScreenExclusiveWin32InfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSurfaceFullScreenExclusiveWin32InfoEXT))
                *> poke (ptr `plusPtr` 16) (vkHmonitor (poked :: VkSurfaceFullScreenExclusiveWin32InfoEXT))

instance Zero VkSurfaceFullScreenExclusiveWin32InfoEXT where
  zero = VkSurfaceFullScreenExclusiveWin32InfoEXT VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT
                                                  zero
                                                  zero

-- | vkAcquireFullScreenExclusiveModeEXT - Acquire full-screen exclusive mode
-- for a swapchain
--
-- = Parameters
--
-- -   @device@ is the device associated with @swapchain@.
--
-- -   @swapchain@ is the swapchain to acquire exclusive full-screen access
--     for.
--
-- == Valid Usage
--
-- -   @swapchain@ /must/ not be in the retired state
--
-- -   @swapchain@ /must/ be a swapchain created with an instance of
--     'VkSurfaceFullScreenExclusiveInfoEXT', with @fullScreenExclusive@
--     set to 'VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT'
--
-- -   @swapchain@ /must/ not currently have exclusive full-screen access
--
-- A return value of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' indicates
-- that the @swapchain@ successfully acquired exclusive full-screen access.
-- The swapchain will retain this exclusivity until either the application
-- releases exclusive full-screen access with
-- 'vkReleaseFullScreenExclusiveModeEXT', destroys the swapchain, or if any
-- of the swapchain commands return
-- 'VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT' indicating that the mode
-- was lost because of platform-specific changes.
--
-- If the swapchain was unable to acquire exclusive full-screen access to
-- the display then
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_INITIALIZATION_FAILED' is
-- returned. An application /can/ attempt to acquire exclusive full-screen
-- access again for the same swapchain even if this command fails, or if
-- 'VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT' has been returned by a
-- swapchain command.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @swapchain@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
--     handle
--
-- -   Both of @device@, and @swapchain@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance'
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
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_INITIALIZATION_FAILED'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkAcquireFullScreenExclusiveModeEXT" vkAcquireFullScreenExclusiveModeEXT :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult
#else
vkAcquireFullScreenExclusiveModeEXT :: DeviceCmds -> ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult
vkAcquireFullScreenExclusiveModeEXT deviceCmds = mkVkAcquireFullScreenExclusiveModeEXT (pVkAcquireFullScreenExclusiveModeEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireFullScreenExclusiveModeEXT
  :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult)
#endif

type FN_vkAcquireFullScreenExclusiveModeEXT = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult
type PFN_vkAcquireFullScreenExclusiveModeEXT = FunPtr FN_vkAcquireFullScreenExclusiveModeEXT

-- | vkGetPhysicalDeviceSurfacePresentModes2EXT - Query supported
-- presentation modes
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device that will be associated with
--     the swapchain to be created, as described for
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR'.
--
-- -   @pSurfaceInfo@ points to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.VkPhysicalDeviceSurfaceInfo2KHR'
--     structure, describing the surface and other fixed parameters that
--     would be consumed by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR'.
--
-- -   @pPresentModeCount@ is a pointer to an integer related to the number
--     of presentation modes available or queried, as described below.
--
-- -   @pPresentModes@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkPresentModeKHR'
--     values, indicating the supported presentation modes.
--
-- = Description
--
-- 'vkGetPhysicalDeviceSurfacePresentModes2EXT' behaves similarly to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfacePresentModesKHR',
-- with the ability to specify extended inputs via chained input
-- structures.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @pSurfaceInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.VkPhysicalDeviceSurfaceInfo2KHR'
--     structure
--
-- -   @pPresentModeCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPresentModeCount@ is not @0@, and
--     @pPresentModes@ is not @NULL@, @pPresentModes@ /must/ be a valid
--     pointer to an array of @pPresentModeCount@
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkPresentModeKHR'
--     values
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
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.VkPhysicalDeviceSurfaceInfo2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkPresentModeKHR'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceSurfacePresentModes2EXT" vkGetPhysicalDeviceSurfacePresentModes2EXT :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult
#else
vkGetPhysicalDeviceSurfacePresentModes2EXT :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult
vkGetPhysicalDeviceSurfacePresentModes2EXT deviceCmds = mkVkGetPhysicalDeviceSurfacePresentModes2EXT (pVkGetPhysicalDeviceSurfacePresentModes2EXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfacePresentModes2EXT
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult)
#endif

type FN_vkGetPhysicalDeviceSurfacePresentModes2EXT = ("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceSurfacePresentModes2EXT = FunPtr FN_vkGetPhysicalDeviceSurfacePresentModes2EXT

-- | vkReleaseFullScreenExclusiveModeEXT - Release full-screen exclusive mode
-- from a swapchain
--
-- = Parameters
--
-- -   @device@ is the device associated with @swapchain@.
--
-- -   @swapchain@ is the swapchain to release exclusive full-screen access
--     from.
--
-- = Description
--
-- __Note__
--
-- Applications will not be able to present to @swapchain@ after this call
-- until exclusive full-screen access is reacquired. This is usually useful
-- to handle when an application is minimised or otherwise intends to stop
-- presenting for a time.
--
-- == Valid Usage
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkReleaseFullScreenExclusiveModeEXT" vkReleaseFullScreenExclusiveModeEXT :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult
#else
vkReleaseFullScreenExclusiveModeEXT :: DeviceCmds -> ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult
vkReleaseFullScreenExclusiveModeEXT deviceCmds = mkVkReleaseFullScreenExclusiveModeEXT (pVkReleaseFullScreenExclusiveModeEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkReleaseFullScreenExclusiveModeEXT
  :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult)
#endif

type FN_vkReleaseFullScreenExclusiveModeEXT = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult
type PFN_vkReleaseFullScreenExclusiveModeEXT = FunPtr FN_vkReleaseFullScreenExclusiveModeEXT

-- No documentation found for Nested "VkResult" "VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT"
pattern VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT :: VkResult
pattern VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT = VkResult (-1000255000)

-- No documentation found for TopLevel "VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME"
pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME = "VK_EXT_full_screen_exclusive"

-- No documentation found for TopLevel "VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION"
pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION :: Integral a => a
pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION = 3

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT"
pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT = VkStructureType 1000255002

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT = VkStructureType 1000255000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT"
pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT = VkStructureType 1000255001
