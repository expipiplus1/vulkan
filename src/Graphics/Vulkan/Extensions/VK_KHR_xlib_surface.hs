{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_xlib_surface
  ( Display(..)
  , VisualID
  , Window
  , VkXlibSurfaceCreateFlagsKHR(..)
  , pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR
  , pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION
  , pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME
  , vkCreateXlibSurfaceKHR
  , vkGetPhysicalDeviceXlibPresentationSupportKHR
  , VkXlibSurfaceCreateInfoKHR(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  , Word64
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
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkPhysicalDevice
  , VkAllocationCallbacks(..)
  , VkInstance
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( VkSurfaceKHR
  )


-- No documentation found for TopLevel "Display"
newtype Display = Display (Ptr Display)
-- No documentation found for TopLevel "VisualID"
type VisualID = Word64
-- No documentation found for TopLevel "Window"
type Window = Word64
-- ** VkXlibSurfaceCreateFlagsKHR

-- No documentation found for TopLevel "VkXlibSurfaceCreateFlagsKHR"
newtype VkXlibSurfaceCreateFlagsKHR = VkXlibSurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkXlibSurfaceCreateFlagsKHR where
  
  showsPrec p (VkXlibSurfaceCreateFlagsKHR x) = showParen (p >= 11) (showString "VkXlibSurfaceCreateFlagsKHR " . showsPrec 11 x)

instance Read VkXlibSurfaceCreateFlagsKHR where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkXlibSurfaceCreateFlagsKHR")
                        v <- step readPrec
                        pure (VkXlibSurfaceCreateFlagsKHR v)
                        )
                    )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR = VkStructureType 1000004000
-- No documentation found for TopLevel "VK_KHR_XLIB_SURFACE_SPEC_VERSION"
pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION = 6
-- No documentation found for TopLevel "VK_KHR_XLIB_SURFACE_EXTENSION_NAME"
pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME = "VK_KHR_xlib_surface"
-- | vkCreateXlibSurfaceKHR - Create a
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceKHR' object for an
-- X11 window, using the Xlib client-side library
--
-- = Parameters
-- #_parameters#
--
-- -   @instance@ is the instance to associate the surface with.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     @VkXlibSurfaceCreateInfoKHR@ structure containing the parameters
--     affecting the creation of the surface object.
--
-- -   @pAllocator@ is the allocator used for host memory allocated for the
--     surface object when there is no more specific allocator available
--     (see <{html_spec_relative}#memory-allocation Memory Allocation>).
--
-- -   @pSurface@ points to a @VkSurfaceKHR@ handle in which the created
--     surface object is returned.
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid @VkInstance@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkXlibSurfaceCreateInfoKHR@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pSurface@ /must/ be a valid pointer to a @VkSurfaceKHR@ handle
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkInstance',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceKHR',
-- 'VkXlibSurfaceCreateInfoKHR'
foreign import ccall "vkCreateXlibSurfaceKHR" vkCreateXlibSurfaceKHR :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXlibSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
-- | vkGetPhysicalDeviceXlibPresentationSupportKHR - Query physical device
-- for presentation to X11 server using Xlib
--
-- = Parameters
-- #_parameters#
--
-- -   @physicalDevice@ is the physical device.
--
-- -   @queueFamilyIndex@ is the queue family index.
--
-- -   @dpy@ is a pointer to an Xlib @Display@ connection to the server.
--
-- -   @visualId@ is an X11 visual (@VisualID@).
--
-- = Description
-- #_description#
--
-- This platform-specific function /can/ be called prior to creating a
-- surface.
--
-- == Valid Usage
--
-- -   @queueFamilyIndex@ /must/ be less than @pQueueFamilyPropertyCount@
--     returned by @vkGetPhysicalDeviceQueueFamilyProperties@ for the given
--     @physicalDevice@
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @dpy@ /must/ be a valid pointer to a @Display@ value
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice'
foreign import ccall "vkGetPhysicalDeviceXlibPresentationSupportKHR" vkGetPhysicalDeviceXlibPresentationSupportKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("dpy" ::: Ptr Display) -> ("visualID" ::: VisualID) -> IO VkBool32
-- | VkXlibSurfaceCreateInfoKHR - Structure specifying parameters of a newly
-- created Xlib surface object
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   @dpy@ /must/ point to a valid Xlib @Display@.
--
-- -   @window@ /must/ be a valid Xlib @Window@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'VkXlibSurfaceCreateFlagsKHR', 'vkCreateXlibSurfaceKHR'
data VkXlibSurfaceCreateInfoKHR = VkXlibSurfaceCreateInfoKHR
  { -- No documentation found for Nested "VkXlibSurfaceCreateInfoKHR" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkXlibSurfaceCreateInfoKHR" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkXlibSurfaceCreateInfoKHR" "vkFlags"
  vkFlags :: VkXlibSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "VkXlibSurfaceCreateInfoKHR" "vkDpy"
  vkDpy :: Ptr Display
  , -- No documentation found for Nested "VkXlibSurfaceCreateInfoKHR" "vkWindow"
  vkWindow :: Window
  }
  deriving (Eq, Show)

instance Storable VkXlibSurfaceCreateInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkXlibSurfaceCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 24)
                                        <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkXlibSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkXlibSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkXlibSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkDpy (poked :: VkXlibSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkWindow (poked :: VkXlibSurfaceCreateInfoKHR))
