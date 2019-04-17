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
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkAcquireFullScreenExclusiveModeEXT
#endif
  , FN_vkAcquireFullScreenExclusiveModeEXT
  , PFN_vkAcquireFullScreenExclusiveModeEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetPhysicalDeviceSurfacePresentModes2EXT
#endif
  , FN_vkGetPhysicalDeviceSurfacePresentModes2EXT
  , PFN_vkGetPhysicalDeviceSurfacePresentModes2EXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkReleaseFullScreenExclusiveModeEXT
#endif
  , FN_vkReleaseFullScreenExclusiveModeEXT
  , PFN_vkReleaseFullScreenExclusiveModeEXT
  , pattern VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT
  , pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME
  , pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT
  , pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetDeviceGroupSurfacePresentModes2EXT
#endif
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

#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
import Graphics.Vulkan.C.Extensions.VK_KHR_device_group
  ( vkGetDeviceGroupSurfacePresentModes2EXT
  )
#endif


-- No documentation found for TopLevel "HMONITOR"
type HMONITOR = Ptr ()
  
-- ** VkFullScreenExclusiveEXT

-- | VkFullScreenExclusiveEXT - Hint values an application can specify
-- affecting full-screen transition behavior
--
-- = See Also
--
-- No cross-references are available
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

-- | @VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT@ indicates the implementation
-- /should/ determine the appropriate full-screen method by whatever means
-- it deems appropriate.
pattern VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT :: VkFullScreenExclusiveEXT
pattern VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT = VkFullScreenExclusiveEXT 0

-- | @VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT@ indicates the implementation
-- /may/ use full-screen exclusive mechanisms when available. Such
-- mechanisms /may/ result in better performance and\/or the availability
-- of different presentation capabilities, but /may/ require a more
-- disruptive transition during swapchain initialization, first
-- presentation and\/or destruction.
pattern VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT :: VkFullScreenExclusiveEXT
pattern VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT = VkFullScreenExclusiveEXT 1

-- | @VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT@ indicates the implementation
-- /should/ avoid using full-screen mechanisms which rely on disruptive
-- transitions.
pattern VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT :: VkFullScreenExclusiveEXT
pattern VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT = VkFullScreenExclusiveEXT 2

-- | @VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT@ indicates the
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
-- @fullScreenExclusiveSupported@ is @VK_FALSE@, it indicates that
-- exclusive full-screen access is not obtainable for this surface.
--
-- Applications /must/ not attempt to create swapchains with
-- @VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT@ set if
-- @fullScreenExclusiveSupported@ is @VK_FALSE@.
--
-- Unresolved directive in VkSurfaceCapabilitiesFullScreenExclusiveEXT.txt
-- -
-- include::..\/validity\/structs\/VkSurfaceCapabilitiesFullScreenExclusiveEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkSurfaceCapabilitiesFullScreenExclusiveEXT = VkSurfaceCapabilitiesFullScreenExclusiveEXT
  { -- | @sType@ is the type of this structure.
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
  zero = VkSurfaceCapabilitiesFullScreenExclusiveEXT zero
                                                     zero
                                                     zero
-- | VkSurfaceFullScreenExclusiveInfoEXT - Structure specifying the preferred
-- full-screen transition behavior
--
-- = Description
--
-- If this structure is not present, @fullScreenExclusive@ is considered to
-- be @VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT@.
--
-- Unresolved directive in VkSurfaceFullScreenExclusiveInfoEXT.txt -
-- include::..\/validity\/structs\/VkSurfaceFullScreenExclusiveInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkSurfaceFullScreenExclusiveInfoEXT = VkSurfaceFullScreenExclusiveInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @fullScreenExclusive@ is a 'VkFullScreenExclusiveEXT' value specifying
  -- the preferred full-screen transition behavior.
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
  zero = VkSurfaceFullScreenExclusiveInfoEXT zero
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
-- swapchain will return @VK_ERROR_OUT_OF_DATE_KHR@.
--
-- __Note__
--
-- It’s the responsibility of the application to change the display
-- settings of the targeted Win32 display using the appropriate platform
-- APIs. Such changes /may/ alter the surface capabilities reported for the
-- created surface.
--
-- == Valid Usage
--
-- Unresolved directive in VkSurfaceFullScreenExclusiveWin32InfoEXT.txt -
-- include::..\/validity\/structs\/VkSurfaceFullScreenExclusiveWin32InfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkSurfaceFullScreenExclusiveWin32InfoEXT = VkSurfaceFullScreenExclusiveWin32InfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @hmonitor@ /must/ be a valid @HMONITOR@
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
  zero = VkSurfaceFullScreenExclusiveWin32InfoEXT zero
                                                  zero
                                                  zero
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
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
-- A return value of @VK_SUCCESS@ indicates that the @swapchain@
-- successfully acquired exclusive full-screen access. The swapchain will
-- retain this exclusivity until either the application releases exclusive
-- full-screen access with 'vkReleaseFullScreenExclusiveModeEXT', destroys
-- the swapchain, or if any of the swapchain commands return
-- @VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT@ indicating that the mode
-- was lost because of platform-specific changes.
--
-- If the swapchain was unable to acquire exclusive full-screen access to
-- the display then @VK_ERROR_INITIALIZATION_FAILED@ is returned. An
-- application /can/ attempt to acquire exclusive full-screen access again
-- for the same swapchain even if this command fails, or if
-- @VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT@ has been returned by a
-- swapchain command.
--
-- Unresolved directive in vkAcquireFullScreenExclusiveModeEXT.txt -
-- include::..\/validity\/protos\/vkAcquireFullScreenExclusiveModeEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkAcquireFullScreenExclusiveModeEXT" vkAcquireFullScreenExclusiveModeEXT :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult

#endif
type FN_vkAcquireFullScreenExclusiveModeEXT = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult
type PFN_vkAcquireFullScreenExclusiveModeEXT = FunPtr FN_vkAcquireFullScreenExclusiveModeEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
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
-- @vkGetPhysicalDeviceSurfacePresentModes2EXT@ behaves similarly to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfacePresentModesKHR',
-- with the ability to specify extended inputs via chained input
-- structures.
--
-- Unresolved directive in vkGetPhysicalDeviceSurfacePresentModes2EXT.txt -
-- include::..\/validity\/protos\/vkGetPhysicalDeviceSurfacePresentModes2EXT.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceSurfacePresentModes2EXT" vkGetPhysicalDeviceSurfacePresentModes2EXT :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult

#endif
type FN_vkGetPhysicalDeviceSurfacePresentModes2EXT = ("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceSurfacePresentModes2EXT = FunPtr FN_vkGetPhysicalDeviceSurfacePresentModes2EXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
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
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkReleaseFullScreenExclusiveModeEXT" vkReleaseFullScreenExclusiveModeEXT :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult

#endif
type FN_vkReleaseFullScreenExclusiveModeEXT = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult
type PFN_vkReleaseFullScreenExclusiveModeEXT = FunPtr FN_vkReleaseFullScreenExclusiveModeEXT
-- | @VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT@ An operation on a
-- swapchain created with
-- @VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT@ failed as it did
-- not have exlusive full-screen access. This /may/ occur due to
-- implementation-dependent reasons, outside of the application’s control.
pattern VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT :: VkResult
pattern VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT = VkResult (-1000255000)
-- No documentation found for TopLevel "VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME"
pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME :: (Eq a ,IsString a) => a
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
