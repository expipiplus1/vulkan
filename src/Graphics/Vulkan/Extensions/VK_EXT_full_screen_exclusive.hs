{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_full_screen_exclusive
  ( FullScreenExclusiveEXT
  , pattern FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT
  , pattern FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT
  , pattern FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT
  , pattern FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT
  , withCStructSurfaceCapabilitiesFullScreenExclusiveEXT
  , fromCStructSurfaceCapabilitiesFullScreenExclusiveEXT
  , SurfaceCapabilitiesFullScreenExclusiveEXT(..)
  , withCStructSurfaceFullScreenExclusiveInfoEXT
  , fromCStructSurfaceFullScreenExclusiveInfoEXT
  , SurfaceFullScreenExclusiveInfoEXT(..)
  , withCStructSurfaceFullScreenExclusiveWin32InfoEXT
  , fromCStructSurfaceFullScreenExclusiveWin32InfoEXT
  , SurfaceFullScreenExclusiveWin32InfoEXT(..)
  , acquireFullScreenExclusiveModeEXT
  , getNumPhysicalDeviceSurfacePresentModes2EXT
  , getPhysicalDeviceSurfacePresentModes2EXT
  , getAllPhysicalDeviceSurfacePresentModes2EXT
  , releaseFullScreenExclusiveModeEXT
  , pattern EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME
  , pattern EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION
#if defined(VK_USE_PLATFORM_WIN32)
  , getDeviceGroupSurfacePresentModes2EXT
#endif
  , pattern STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT
  , pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT
  , pattern ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT
  , pattern STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  , nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive
  ( VkFullScreenExclusiveEXT(..)
  , VkSurfaceCapabilitiesFullScreenExclusiveEXT(..)
  , VkSurfaceFullScreenExclusiveInfoEXT(..)
  , VkSurfaceFullScreenExclusiveWin32InfoEXT(..)
  , HMONITOR
  , vkAcquireFullScreenExclusiveModeEXT
  , vkGetPhysicalDeviceSurfacePresentModes2EXT
  , vkReleaseFullScreenExclusiveModeEXT
  , pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME
  , pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION
  , pattern VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT
  , pattern VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT
  , pattern VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT
  , pattern VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT
  , pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , PhysicalDevice(..)
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2
  ( PhysicalDeviceSurfaceInfo2KHR(..)
  , withCStructPhysicalDeviceSurfaceInfo2KHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( PresentModeKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( SwapchainKHR
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT
  , pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT
  , pattern STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT
  , pattern STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT
  )

#if defined(VK_USE_PLATFORM_WIN32)
import Graphics.Vulkan.Extensions.VK_KHR_device_group
  ( getDeviceGroupSurfacePresentModes2EXT
  )
#endif


-- | VkFullScreenExclusiveEXT - Hint values an application can specify
-- affecting full-screen transition behavior
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VkSurfaceFullScreenExclusiveInfoEXT'
type FullScreenExclusiveEXT = VkFullScreenExclusiveEXT


{-# complete FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT, FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT, FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT, FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT :: FullScreenExclusiveEXT #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT'
-- indicates the implementation /should/ determine the appropriate
-- full-screen method by whatever means it deems appropriate.
pattern FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT :: (a ~ FullScreenExclusiveEXT) => a
pattern FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT = VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT'
-- indicates the implementation /may/ use full-screen exclusive mechanisms
-- when available. Such mechanisms /may/ result in better performance
-- and\/or the availability of different presentation capabilities, but
-- /may/ require a more disruptive transition during swapchain
-- initialization, first presentation and\/or destruction.
pattern FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT :: (a ~ FullScreenExclusiveEXT) => a
pattern FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT = VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT'
-- indicates the implementation /should/ avoid using full-screen mechanisms
-- which rely on disruptive transitions.
pattern FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT :: (a ~ FullScreenExclusiveEXT) => a
pattern FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT = VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT'
-- indicates the application will manage full-screen exclusive mode by
-- using the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.vkAcquireFullScreenExclusiveModeEXT'
-- and
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.vkReleaseFullScreenExclusiveModeEXT'
-- commands.
pattern FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT :: (a ~ FullScreenExclusiveEXT) => a
pattern FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT = VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT


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
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT'
-- set if @fullScreenExclusiveSupported@ is
-- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data SurfaceCapabilitiesFullScreenExclusiveEXT = SurfaceCapabilitiesFullScreenExclusiveEXT
  { -- Univalued member elided
  -- No documentation found for Nested "SurfaceCapabilitiesFullScreenExclusiveEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceCapabilitiesFullScreenExclusiveEXT" "fullScreenExclusiveSupported"
  fullScreenExclusiveSupported :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSurfaceCapabilitiesFullScreenExclusiveEXT' and
-- marshal a 'SurfaceCapabilitiesFullScreenExclusiveEXT' into it. The 'VkSurfaceCapabilitiesFullScreenExclusiveEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSurfaceCapabilitiesFullScreenExclusiveEXT :: SurfaceCapabilitiesFullScreenExclusiveEXT -> (VkSurfaceCapabilitiesFullScreenExclusiveEXT -> IO a) -> IO a
withCStructSurfaceCapabilitiesFullScreenExclusiveEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: SurfaceCapabilitiesFullScreenExclusiveEXT)) (\pPNext -> cont (VkSurfaceCapabilitiesFullScreenExclusiveEXT VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT pPNext (boolToBool32 (fullScreenExclusiveSupported (marshalled :: SurfaceCapabilitiesFullScreenExclusiveEXT)))))

-- | A function to read a 'VkSurfaceCapabilitiesFullScreenExclusiveEXT' and all additional
-- structures in the pointer chain into a 'SurfaceCapabilitiesFullScreenExclusiveEXT'.
fromCStructSurfaceCapabilitiesFullScreenExclusiveEXT :: VkSurfaceCapabilitiesFullScreenExclusiveEXT -> IO SurfaceCapabilitiesFullScreenExclusiveEXT
fromCStructSurfaceCapabilitiesFullScreenExclusiveEXT c = SurfaceCapabilitiesFullScreenExclusiveEXT <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSurfaceCapabilitiesFullScreenExclusiveEXT)))
                                                                                                   <*> pure (bool32ToBool (vkFullScreenExclusiveSupported (c :: VkSurfaceCapabilitiesFullScreenExclusiveEXT)))

instance Zero SurfaceCapabilitiesFullScreenExclusiveEXT where
  zero = SurfaceCapabilitiesFullScreenExclusiveEXT Nothing
                                                   False



-- | VkSurfaceFullScreenExclusiveInfoEXT - Structure specifying the preferred
-- full-screen transition behavior
--
-- = Description
--
-- If this structure is not present, @fullScreenExclusive@ is considered to
-- be
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VkFullScreenExclusiveEXT',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data SurfaceFullScreenExclusiveInfoEXT = SurfaceFullScreenExclusiveInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "SurfaceFullScreenExclusiveInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceFullScreenExclusiveInfoEXT" "fullScreenExclusive"
  fullScreenExclusive :: FullScreenExclusiveEXT
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSurfaceFullScreenExclusiveInfoEXT' and
-- marshal a 'SurfaceFullScreenExclusiveInfoEXT' into it. The 'VkSurfaceFullScreenExclusiveInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSurfaceFullScreenExclusiveInfoEXT :: SurfaceFullScreenExclusiveInfoEXT -> (VkSurfaceFullScreenExclusiveInfoEXT -> IO a) -> IO a
withCStructSurfaceFullScreenExclusiveInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: SurfaceFullScreenExclusiveInfoEXT)) (\pPNext -> cont (VkSurfaceFullScreenExclusiveInfoEXT VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT pPNext (fullScreenExclusive (marshalled :: SurfaceFullScreenExclusiveInfoEXT))))

-- | A function to read a 'VkSurfaceFullScreenExclusiveInfoEXT' and all additional
-- structures in the pointer chain into a 'SurfaceFullScreenExclusiveInfoEXT'.
fromCStructSurfaceFullScreenExclusiveInfoEXT :: VkSurfaceFullScreenExclusiveInfoEXT -> IO SurfaceFullScreenExclusiveInfoEXT
fromCStructSurfaceFullScreenExclusiveInfoEXT c = SurfaceFullScreenExclusiveInfoEXT <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSurfaceFullScreenExclusiveInfoEXT)))
                                                                                   <*> pure (vkFullScreenExclusive (c :: VkSurfaceFullScreenExclusiveInfoEXT))

instance Zero SurfaceFullScreenExclusiveInfoEXT where
  zero = SurfaceFullScreenExclusiveInfoEXT Nothing
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
data SurfaceFullScreenExclusiveWin32InfoEXT = SurfaceFullScreenExclusiveWin32InfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "SurfaceFullScreenExclusiveWin32InfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceFullScreenExclusiveWin32InfoEXT" "hmonitor"
  hmonitor :: HMONITOR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSurfaceFullScreenExclusiveWin32InfoEXT' and
-- marshal a 'SurfaceFullScreenExclusiveWin32InfoEXT' into it. The 'VkSurfaceFullScreenExclusiveWin32InfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSurfaceFullScreenExclusiveWin32InfoEXT :: SurfaceFullScreenExclusiveWin32InfoEXT -> (VkSurfaceFullScreenExclusiveWin32InfoEXT -> IO a) -> IO a
withCStructSurfaceFullScreenExclusiveWin32InfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: SurfaceFullScreenExclusiveWin32InfoEXT)) (\pPNext -> cont (VkSurfaceFullScreenExclusiveWin32InfoEXT VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT pPNext (hmonitor (marshalled :: SurfaceFullScreenExclusiveWin32InfoEXT))))

-- | A function to read a 'VkSurfaceFullScreenExclusiveWin32InfoEXT' and all additional
-- structures in the pointer chain into a 'SurfaceFullScreenExclusiveWin32InfoEXT'.
fromCStructSurfaceFullScreenExclusiveWin32InfoEXT :: VkSurfaceFullScreenExclusiveWin32InfoEXT -> IO SurfaceFullScreenExclusiveWin32InfoEXT
fromCStructSurfaceFullScreenExclusiveWin32InfoEXT c = SurfaceFullScreenExclusiveWin32InfoEXT <$> -- Univalued Member elided
                                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSurfaceFullScreenExclusiveWin32InfoEXT)))
                                                                                             <*> pure (vkHmonitor (c :: VkSurfaceFullScreenExclusiveWin32InfoEXT))

instance Zero SurfaceFullScreenExclusiveWin32InfoEXT where
  zero = SurfaceFullScreenExclusiveWin32InfoEXT Nothing
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
--     'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VkSurfaceFullScreenExclusiveInfoEXT',
--     with @fullScreenExclusive@ set to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT'
--
-- -   @swapchain@ /must/ not currently have exclusive full-screen access
--
-- A return value of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' indicates
-- that the @swapchain@ successfully acquired exclusive full-screen access.
-- The swapchain will retain this exclusivity until either the application
-- releases exclusive full-screen access with
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.vkReleaseFullScreenExclusiveModeEXT',
-- destroys the swapchain, or if any of the swapchain commands return
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT'
-- indicating that the mode was lost because of platform-specific changes.
--
-- If the swapchain was unable to acquire exclusive full-screen access to
-- the display then
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_INITIALIZATION_FAILED' is
-- returned. An application /can/ attempt to acquire exclusive full-screen
-- access again for the same swapchain even if this command fails, or if
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT'
-- has been returned by a swapchain command.
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
acquireFullScreenExclusiveModeEXT :: Device ->  SwapchainKHR ->  IO ()
acquireFullScreenExclusiveModeEXT = \(Device device' commandTable) -> \swapchain' -> vkAcquireFullScreenExclusiveModeEXT commandTable device' swapchain' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ()))


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
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.vkGetPhysicalDeviceSurfacePresentModes2EXT'
-- behaves similarly to
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
getNumPhysicalDeviceSurfacePresentModes2EXT :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  IO (VkResult, Word32)
getNumPhysicalDeviceSurfacePresentModes2EXT = \(PhysicalDevice physicalDevice' commandTable) -> \surfaceInfo' -> alloca (\pPresentModeCount' -> (\marshalled -> withCStructPhysicalDeviceSurfaceInfo2KHR marshalled . flip with) surfaceInfo' (\pSurfaceInfo' -> vkGetPhysicalDeviceSurfacePresentModes2EXT commandTable physicalDevice' pSurfaceInfo' pPresentModeCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pPresentModeCount'))))

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
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.vkGetPhysicalDeviceSurfacePresentModes2EXT'
-- behaves similarly to
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
getPhysicalDeviceSurfacePresentModes2EXT :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  Word32 ->  IO (VkResult, Vector PresentModeKHR)
getPhysicalDeviceSurfacePresentModes2EXT = \(PhysicalDevice physicalDevice' commandTable) -> \surfaceInfo' -> \presentModeCount' -> allocaArray (fromIntegral presentModeCount') (\pPresentModes' -> with presentModeCount' (\pPresentModeCount' -> (\marshalled -> withCStructPhysicalDeviceSurfaceInfo2KHR marshalled . flip with) surfaceInfo' (\pSurfaceInfo' -> vkGetPhysicalDeviceSurfacePresentModes2EXT commandTable physicalDevice' pSurfaceInfo' pPresentModeCount' pPresentModes' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM (peekElemOff pPresentModes') =<< (fromIntegral <$> (peek pPresentModeCount'))))))))
-- | Returns all the values available from 'getPhysicalDeviceSurfacePresentModes2EXT'.
getAllPhysicalDeviceSurfacePresentModes2EXT :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  IO (Vector PresentModeKHR)
getAllPhysicalDeviceSurfacePresentModes2EXT physicalDevice' pSurfaceInfo' =
  snd <$> getNumPhysicalDeviceSurfacePresentModes2EXT physicalDevice' pSurfaceInfo'
    >>= \num -> snd <$> getPhysicalDeviceSurfacePresentModes2EXT physicalDevice' pSurfaceInfo' num



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
releaseFullScreenExclusiveModeEXT :: Device ->  SwapchainKHR ->  IO ()
releaseFullScreenExclusiveModeEXT = \(Device device' commandTable) -> \swapchain' -> vkReleaseFullScreenExclusiveModeEXT commandTable device' swapchain' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ()))

-- No documentation found for TopLevel "VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME"
pattern EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME = VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION"
pattern EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION :: Integral a => a
pattern EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION = VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION
