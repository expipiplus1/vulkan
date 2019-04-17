{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( Display(..)
  , VisualID
  , VkXlibSurfaceCreateFlagsKHR(..)
  , VkXlibSurfaceCreateInfoKHR(..)
  , Window
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCreateXlibSurfaceKHR
#endif
  , FN_vkCreateXlibSurfaceKHR
  , PFN_vkCreateXlibSurfaceKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetPhysicalDeviceXlibPresentationSupportKHR
#endif
  , FN_vkGetPhysicalDeviceXlibPresentationSupportKHR
  , PFN_vkGetPhysicalDeviceXlibPresentationSupportKHR
  , pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR
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
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkInstance
  , VkPhysicalDevice
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkSurfaceKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "Display"
newtype Display = Display (Ptr Display)
  deriving (Storable)
-- No documentation found for TopLevel "VisualID"
type VisualID = Word64
  
-- ** VkXlibSurfaceCreateFlagsKHR

-- No documentation found for TopLevel "VkXlibSurfaceCreateFlagsKHR"
newtype VkXlibSurfaceCreateFlagsKHR = VkXlibSurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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


-- | VkXlibSurfaceCreateInfoKHR - Structure specifying parameters of a newly
-- created Xlib surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- No cross-references are available
data VkXlibSurfaceCreateInfoKHR = VkXlibSurfaceCreateInfoKHR
  { -- | @sType@ /must/ be @VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR@
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @flags@ /must/ be @0@
  vkFlags :: VkXlibSurfaceCreateFlagsKHR
  , -- | @dpy@ /must/ point to a valid Xlib @Display@.
  vkDpy :: Ptr Display
  , -- | @window@ /must/ be a valid Xlib @Window@.
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

instance Zero VkXlibSurfaceCreateInfoKHR where
  zero = VkXlibSurfaceCreateInfoKHR zero
                                    zero
                                    zero
                                    zero
                                    zero
-- No documentation found for TopLevel "Window"
type Window = Word64
  
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkCreateXlibSurfaceKHR - Create a
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' object for an
-- X11 window, using the Xlib client-side library
--
-- = Parameters
--
-- -   @instance@ is the instance to associate the surface with.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     @VkXlibSurfaceCreateInfoKHR@ structure containing the parameters
--     affecting the creation of the surface object.
--
-- -   @pAllocator@ is the allocator used for host memory allocated for the
--     surface object when there is no more specific allocator available
--     (see
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
--
-- -   @pSurface@ points to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' handle in
--     which the created surface object is returned.
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
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateXlibSurfaceKHR" vkCreateXlibSurfaceKHR :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXlibSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult

#endif
type FN_vkCreateXlibSurfaceKHR = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXlibSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateXlibSurfaceKHR = FunPtr FN_vkCreateXlibSurfaceKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkGetPhysicalDeviceXlibPresentationSupportKHR - Query physical device
-- for presentation to X11 server using Xlib
--
-- = Parameters
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
--
-- This platform-specific function /can/ be called prior to creating a
-- surface.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceXlibPresentationSupportKHR" vkGetPhysicalDeviceXlibPresentationSupportKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("dpy" ::: Ptr Display) -> ("visualID" ::: VisualID) -> IO VkBool32

#endif
type FN_vkGetPhysicalDeviceXlibPresentationSupportKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("dpy" ::: Ptr Display) -> ("visualID" ::: VisualID) -> IO VkBool32
type PFN_vkGetPhysicalDeviceXlibPresentationSupportKHR = FunPtr FN_vkGetPhysicalDeviceXlibPresentationSupportKHR
-- No documentation found for TopLevel "VK_KHR_XLIB_SURFACE_EXTENSION_NAME"
pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME = "VK_KHR_xlib_surface"
-- No documentation found for TopLevel "VK_KHR_XLIB_SURFACE_SPEC_VERSION"
pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION = 6
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR = VkStructureType 1000004000
