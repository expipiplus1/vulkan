{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_xcb_surface
  ( Xcb_connection_t
  , Xcb_visualid_t
  , Xcb_window_t
  , VkXcbSurfaceCreateFlagsKHR(..)
  , pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR
  , pattern VK_KHR_XCB_SURFACE_SPEC_VERSION
  , pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME
  , vkCreateXcbSurfaceKHR
  , vkGetPhysicalDeviceXcbPresentationSupportKHR
  , VkXcbSurfaceCreateInfoKHR(..)
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
  )
import Foreign.Ptr
  ( Ptr
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
  ( VkAllocationCallbacks(..)
  , VkInstance
  , VkPhysicalDevice
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( VkSurfaceKHR
  )


-- | Opaque data
data Xcb_connection_t
-- No documentation found for TopLevel "Xcb_visualid_t"
type Xcb_visualid_t = Word32
-- No documentation found for TopLevel "Xcb_window_t"
type Xcb_window_t = Word32
-- ** VkXcbSurfaceCreateFlagsKHR

-- No documentation found for TopLevel "VkXcbSurfaceCreateFlagsKHR"
newtype VkXcbSurfaceCreateFlagsKHR = VkXcbSurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkXcbSurfaceCreateFlagsKHR where
  
  showsPrec p (VkXcbSurfaceCreateFlagsKHR x) = showParen (p >= 11) (showString "VkXcbSurfaceCreateFlagsKHR " . showsPrec 11 x)

instance Read VkXcbSurfaceCreateFlagsKHR where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkXcbSurfaceCreateFlagsKHR")
                        v <- step readPrec
                        pure (VkXcbSurfaceCreateFlagsKHR v)
                        )
                    )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR = VkStructureType 1000005000
-- No documentation found for TopLevel "VK_KHR_XCB_SURFACE_SPEC_VERSION"
pattern VK_KHR_XCB_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_XCB_SURFACE_SPEC_VERSION = 6
-- No documentation found for TopLevel "VK_KHR_XCB_SURFACE_EXTENSION_NAME"
pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME = "VK_KHR_xcb_surface"
-- | vkCreateXcbSurfaceKHR - Create a
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceKHR' object for a
-- X11 window, using the XCB client-side library
--
-- = Parameters
--
-- -   @instance@ is the instance to associate the surface with.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     @VkXcbSurfaceCreateInfoKHR@ structure containing parameters
--     affecting the creation of the surface object.
--
-- -   @pAllocator@ is the allocator used for host memory allocated for the
--     surface object when there is no more specific allocator available
--     (see [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)).
--
-- -   @pSurface@ points to a @VkSurfaceKHR@ handle in which the created
--     surface object is returned.
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid @VkInstance@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkXcbSurfaceCreateInfoKHR@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pSurface@ /must/ be a valid pointer to a @VkSurfaceKHR@ handle
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkInstance',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceKHR',
-- 'VkXcbSurfaceCreateInfoKHR'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateXcbSurfaceKHR" vkCreateXcbSurfaceKHR :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXcbSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
-- | vkGetPhysicalDeviceXcbPresentationSupportKHR - Query physical device for
-- presentation to X11 server using XCB
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device.
--
-- -   @queueFamilyIndex@ is the queue family index.
--
-- -   @connection@ is a pointer to an @xcb_connection_t@ to the X server.
--     @visual_id@ is an X11 visual (@xcb_visualid_t@).
--
-- = Description
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
-- -   @connection@ /must/ be a valid pointer to a @xcb_connection_t@ value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceXcbPresentationSupportKHR" vkGetPhysicalDeviceXcbPresentationSupportKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("connection" ::: Ptr Xcb_connection_t) -> ("visual_id" ::: Xcb_visualid_t) -> IO VkBool32
-- | VkXcbSurfaceCreateInfoKHR - Structure specifying parameters of a newly
-- created Xcb surface object
--
-- == Valid Usage
--
-- -   @connection@ /must/ point to a valid X11 @xcb_connection_t@.
--
-- -   @window@ /must/ be a valid X11 @xcb_window_t@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'VkXcbSurfaceCreateFlagsKHR', 'vkCreateXcbSurfaceKHR'
data VkXcbSurfaceCreateInfoKHR = VkXcbSurfaceCreateInfoKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkXcbSurfaceCreateFlagsKHR
  , -- | @connection@ is a pointer to an @xcb_connection_t@ to the X server.
  vkConnection :: Ptr Xcb_connection_t
  , -- | @window@ is the @xcb_window_t@ for the X11 window to associate the
  -- surface with.
  vkWindow :: Xcb_window_t
  }
  deriving (Eq, Show)

instance Storable VkXcbSurfaceCreateInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkXcbSurfaceCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 24)
                                       <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkXcbSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkXcbSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkXcbSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkConnection (poked :: VkXcbSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkWindow (poked :: VkXcbSurfaceCreateInfoKHR))
