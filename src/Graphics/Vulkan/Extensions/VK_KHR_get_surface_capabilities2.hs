{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2
  ( withCStructPhysicalDeviceSurfaceInfo2KHR
  , fromCStructPhysicalDeviceSurfaceInfo2KHR
  , PhysicalDeviceSurfaceInfo2KHR(..)
  , withCStructSurfaceCapabilities2KHR
  , fromCStructSurfaceCapabilities2KHR
  , SurfaceCapabilities2KHR(..)
  , withCStructSurfaceFormat2KHR
  , fromCStructSurfaceFormat2KHR
  , SurfaceFormat2KHR(..)
  , getPhysicalDeviceSurfaceCapabilities2KHR
  , getNumPhysicalDeviceSurfaceFormats2KHR
  , getPhysicalDeviceSurfaceFormats2KHR
  , getAllPhysicalDeviceSurfaceFormats2KHR
  , pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION
  , pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
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
import Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2
  ( VkPhysicalDeviceSurfaceInfo2KHR(..)
  , VkSurfaceCapabilities2KHR(..)
  , VkSurfaceFormat2KHR(..)
  , vkGetPhysicalDeviceSurfaceCapabilities2KHR
  , vkGetPhysicalDeviceSurfaceFormats2KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( SurfaceCapabilitiesKHR(..)
  , SurfaceFormatKHR(..)
  , SurfaceKHR
  , fromCStructSurfaceCapabilitiesKHR
  , fromCStructSurfaceFormatKHR
  , withCStructSurfaceCapabilitiesKHR
  , withCStructSurfaceFormatKHR
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2
  ( pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
  , pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION
  )



-- | VkPhysicalDeviceSurfaceInfo2KHR - Structure specifying a surface and
-- related swapchain creation parameters
--
-- = Description
--
-- The members of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.VkPhysicalDeviceSurfaceInfo2KHR'
-- correspond to the arguments to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceCapabilitiesKHR',
-- with @sType@ and @pNext@ added for extensibility.
--
-- Additional capabilities of a surface /may/ be available to swapchains
-- created with different full-screen exclusive settings - particularly if
-- exclusive full-screen access is application controlled. These additional
-- capabilities /can/ be queried by including the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VkSurfaceFullScreenExclusiveInfoEXT'
-- structure in the @pNext@ chain of this structure when used to query
-- surface properties. Additionally, for Win32 surfaces with application
-- controlled exclusive full-screen access, chaining a valid instance of
-- the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VkSurfaceFullScreenExclusiveWin32InfoEXT'
-- structure /may/ also report additional surface capabilities. These
-- additional capabilities only apply to swapchains created with the same
-- parameters passed into the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'.
--
-- == Valid Usage
--
-- -   If the @pNext@ chain includes an instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VkSurfaceFullScreenExclusiveInfoEXT'
--     with its @fullScreenExclusive@ member set to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT',
--     and @surface@ was created using
--     'Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface.vkCreateWin32SurfaceKHR',
--     an instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VkSurfaceFullScreenExclusiveWin32InfoEXT'
--     /must/ be present in the @pNext@ chain
--
-- Unresolved directive in VkPhysicalDeviceSurfaceInfo2KHR.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceSurfaceInfo2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceSurfaceInfo2KHR = PhysicalDeviceSurfaceInfo2KHR
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceSurfaceInfo2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceSurfaceInfo2KHR" "surface"
  surface :: SurfaceKHR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceSurfaceInfo2KHR' and
-- marshal a 'PhysicalDeviceSurfaceInfo2KHR' into it. The 'VkPhysicalDeviceSurfaceInfo2KHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceSurfaceInfo2KHR :: PhysicalDeviceSurfaceInfo2KHR -> (VkPhysicalDeviceSurfaceInfo2KHR -> IO a) -> IO a
withCStructPhysicalDeviceSurfaceInfo2KHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceSurfaceInfo2KHR)) (\pPNext -> cont (VkPhysicalDeviceSurfaceInfo2KHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR pPNext (surface (marshalled :: PhysicalDeviceSurfaceInfo2KHR))))

-- | A function to read a 'VkPhysicalDeviceSurfaceInfo2KHR' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceSurfaceInfo2KHR'.
fromCStructPhysicalDeviceSurfaceInfo2KHR :: VkPhysicalDeviceSurfaceInfo2KHR -> IO PhysicalDeviceSurfaceInfo2KHR
fromCStructPhysicalDeviceSurfaceInfo2KHR c = PhysicalDeviceSurfaceInfo2KHR <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceSurfaceInfo2KHR)))
                                                                           <*> pure (vkSurface (c :: VkPhysicalDeviceSurfaceInfo2KHR))

instance Zero PhysicalDeviceSurfaceInfo2KHR where
  zero = PhysicalDeviceSurfaceInfo2KHR Nothing
                                       zero



-- | VkSurfaceCapabilities2KHR - Structure describing capabilities of a
-- surface
--
-- = Description
--
-- Unresolved directive in VkSurfaceCapabilities2KHR.txt -
-- include::{generated}\/validity\/structs\/VkSurfaceCapabilities2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data SurfaceCapabilities2KHR = SurfaceCapabilities2KHR
  { -- Univalued member elided
  -- No documentation found for Nested "SurfaceCapabilities2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceCapabilities2KHR" "surfaceCapabilities"
  surfaceCapabilities :: SurfaceCapabilitiesKHR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSurfaceCapabilities2KHR' and
-- marshal a 'SurfaceCapabilities2KHR' into it. The 'VkSurfaceCapabilities2KHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSurfaceCapabilities2KHR :: SurfaceCapabilities2KHR -> (VkSurfaceCapabilities2KHR -> IO a) -> IO a
withCStructSurfaceCapabilities2KHR marshalled cont = withCStructSurfaceCapabilitiesKHR (surfaceCapabilities (marshalled :: SurfaceCapabilities2KHR)) (\surfaceCapabilities'' -> maybeWith withSomeVkStruct (next (marshalled :: SurfaceCapabilities2KHR)) (\pPNext -> cont (VkSurfaceCapabilities2KHR VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR pPNext surfaceCapabilities'')))

-- | A function to read a 'VkSurfaceCapabilities2KHR' and all additional
-- structures in the pointer chain into a 'SurfaceCapabilities2KHR'.
fromCStructSurfaceCapabilities2KHR :: VkSurfaceCapabilities2KHR -> IO SurfaceCapabilities2KHR
fromCStructSurfaceCapabilities2KHR c = SurfaceCapabilities2KHR <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSurfaceCapabilities2KHR)))
                                                               <*> (fromCStructSurfaceCapabilitiesKHR (vkSurfaceCapabilities (c :: VkSurfaceCapabilities2KHR)))

instance Zero SurfaceCapabilities2KHR where
  zero = SurfaceCapabilities2KHR Nothing
                                 zero



-- | VkSurfaceFormat2KHR - Structure describing a supported swapchain format
-- tuple
--
-- = Description
--
-- Unresolved directive in VkSurfaceFormat2KHR.txt -
-- include::{generated}\/validity\/structs\/VkSurfaceFormat2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data SurfaceFormat2KHR = SurfaceFormat2KHR
  { -- Univalued member elided
  -- No documentation found for Nested "SurfaceFormat2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceFormat2KHR" "surfaceFormat"
  surfaceFormat :: SurfaceFormatKHR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSurfaceFormat2KHR' and
-- marshal a 'SurfaceFormat2KHR' into it. The 'VkSurfaceFormat2KHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSurfaceFormat2KHR :: SurfaceFormat2KHR -> (VkSurfaceFormat2KHR -> IO a) -> IO a
withCStructSurfaceFormat2KHR marshalled cont = withCStructSurfaceFormatKHR (surfaceFormat (marshalled :: SurfaceFormat2KHR)) (\surfaceFormat'' -> maybeWith withSomeVkStruct (next (marshalled :: SurfaceFormat2KHR)) (\pPNext -> cont (VkSurfaceFormat2KHR VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR pPNext surfaceFormat'')))

-- | A function to read a 'VkSurfaceFormat2KHR' and all additional
-- structures in the pointer chain into a 'SurfaceFormat2KHR'.
fromCStructSurfaceFormat2KHR :: VkSurfaceFormat2KHR -> IO SurfaceFormat2KHR
fromCStructSurfaceFormat2KHR c = SurfaceFormat2KHR <$> -- Univalued Member elided
                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSurfaceFormat2KHR)))
                                                   <*> (fromCStructSurfaceFormatKHR (vkSurfaceFormat (c :: VkSurfaceFormat2KHR)))

instance Zero SurfaceFormat2KHR where
  zero = SurfaceFormat2KHR Nothing
                           zero



-- | vkGetPhysicalDeviceSurfaceCapabilities2KHR - Reports capabilities of a
-- surface on a physical device
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
-- -   @pSurfaceCapabilities@ points to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.VkSurfaceCapabilities2KHR'
--     structure in which the capabilities are returned.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.vkGetPhysicalDeviceSurfaceCapabilities2KHR'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceCapabilitiesKHR',
-- with the ability to specify extended inputs via chained input
-- structures, and to return extended information via chained output
-- structures.
--
-- == Valid Usage
--
-- -   If an instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VkSurfaceCapabilitiesFullScreenExclusiveEXT'
--     is included in the @pNext@ chain of @pSurfaceCapabilities@, an
--     instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VkSurfaceFullScreenExclusiveWin32InfoEXT'
--     /must/ be included in the @pNext@ chain of @pSurfaceInfo@.
--
-- Unresolved directive in vkGetPhysicalDeviceSurfaceCapabilities2KHR.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceSurfaceCapabilities2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getPhysicalDeviceSurfaceCapabilities2KHR :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  IO (SurfaceCapabilities2KHR)
getPhysicalDeviceSurfaceCapabilities2KHR = \(PhysicalDevice physicalDevice' commandTable) -> \surfaceInfo' -> alloca (\pSurfaceCapabilities' -> (\marshalled -> withCStructPhysicalDeviceSurfaceInfo2KHR marshalled . flip with) surfaceInfo' (\pSurfaceInfo' -> vkGetPhysicalDeviceSurfaceCapabilities2KHR commandTable physicalDevice' pSurfaceInfo' pSurfaceCapabilities' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((fromCStructSurfaceCapabilities2KHR <=< peek) pSurfaceCapabilities'))))


-- | vkGetPhysicalDeviceSurfaceFormats2KHR - Query color formats supported by
-- surface
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
-- -   @pSurfaceFormatCount@ is a pointer to an integer related to the
--     number of format tuples available or queried, as described below.
--
-- -   @pSurfaceFormats@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.VkSurfaceFormat2KHR'
--     structures.
--
-- = Description
--
-- If @pSurfaceFormats@ is @NULL@, then the number of format tuples
-- supported for the given @surface@ is returned in @pSurfaceFormatCount@.
-- The number of format tuples supported will be greater than or equal to
-- 1. Otherwise, @pSurfaceFormatCount@ /must/ point to a variable set by
-- the user to the number of elements in the @pSurfaceFormats@ array, and
-- on return the variable is overwritten with the number of structures
-- actually written to @pSurfaceFormats@. If the value of
-- @pSurfaceFormatCount@ is less than the number of format tuples
-- supported, at most @pSurfaceFormatCount@ structures will be written. If
-- @pSurfaceFormatCount@ is smaller than the number of format tuples
-- supported for the surface parameters described in @pSurfaceInfo@,
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' to indicate that not all
-- the available values were returned.
--
-- Unresolved directive in vkGetPhysicalDeviceSurfaceFormats2KHR.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceSurfaceFormats2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getNumPhysicalDeviceSurfaceFormats2KHR :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  IO (VkResult, Word32)
getNumPhysicalDeviceSurfaceFormats2KHR = \(PhysicalDevice physicalDevice' commandTable) -> \surfaceInfo' -> alloca (\pSurfaceFormatCount' -> (\marshalled -> withCStructPhysicalDeviceSurfaceInfo2KHR marshalled . flip with) surfaceInfo' (\pSurfaceInfo' -> vkGetPhysicalDeviceSurfaceFormats2KHR commandTable physicalDevice' pSurfaceInfo' pSurfaceFormatCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pSurfaceFormatCount'))))

-- | vkGetPhysicalDeviceSurfaceFormats2KHR - Query color formats supported by
-- surface
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
-- -   @pSurfaceFormatCount@ is a pointer to an integer related to the
--     number of format tuples available or queried, as described below.
--
-- -   @pSurfaceFormats@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.VkSurfaceFormat2KHR'
--     structures.
--
-- = Description
--
-- If @pSurfaceFormats@ is @NULL@, then the number of format tuples
-- supported for the given @surface@ is returned in @pSurfaceFormatCount@.
-- The number of format tuples supported will be greater than or equal to
-- 1. Otherwise, @pSurfaceFormatCount@ /must/ point to a variable set by
-- the user to the number of elements in the @pSurfaceFormats@ array, and
-- on return the variable is overwritten with the number of structures
-- actually written to @pSurfaceFormats@. If the value of
-- @pSurfaceFormatCount@ is less than the number of format tuples
-- supported, at most @pSurfaceFormatCount@ structures will be written. If
-- @pSurfaceFormatCount@ is smaller than the number of format tuples
-- supported for the surface parameters described in @pSurfaceInfo@,
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' to indicate that not all
-- the available values were returned.
--
-- Unresolved directive in vkGetPhysicalDeviceSurfaceFormats2KHR.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceSurfaceFormats2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getPhysicalDeviceSurfaceFormats2KHR :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  Word32 ->  IO (VkResult, Vector SurfaceFormat2KHR)
getPhysicalDeviceSurfaceFormats2KHR = \(PhysicalDevice physicalDevice' commandTable) -> \surfaceInfo' -> \surfaceFormatCount' -> allocaArray (fromIntegral surfaceFormatCount') (\pSurfaceFormats' -> with surfaceFormatCount' (\pSurfaceFormatCount' -> (\marshalled -> withCStructPhysicalDeviceSurfaceInfo2KHR marshalled . flip with) surfaceInfo' (\pSurfaceInfo' -> vkGetPhysicalDeviceSurfaceFormats2KHR commandTable physicalDevice' pSurfaceInfo' pSurfaceFormatCount' pSurfaceFormats' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM ((\p -> fromCStructSurfaceFormat2KHR <=< peekElemOff p) pSurfaceFormats') =<< (fromIntegral <$> (peek pSurfaceFormatCount'))))))))
-- | Returns all the values available from 'getPhysicalDeviceSurfaceFormats2KHR'.
getAllPhysicalDeviceSurfaceFormats2KHR :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  IO (Vector SurfaceFormat2KHR)
getAllPhysicalDeviceSurfaceFormats2KHR physicalDevice' pSurfaceInfo' =
  snd <$> getNumPhysicalDeviceSurfaceFormats2KHR physicalDevice' pSurfaceInfo'
    >>= \num -> snd <$> getPhysicalDeviceSurfaceFormats2KHR physicalDevice' pSurfaceInfo' num

