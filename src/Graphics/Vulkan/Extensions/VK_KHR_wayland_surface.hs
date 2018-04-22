{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_wayland_surface
  ( Wl_display
  , Wl_surface
  , VkWaylandSurfaceCreateFlagsKHR(..)
  , pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR
  , pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION
  , pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME
  , vkCreateWaylandSurfaceKHR
  , vkGetPhysicalDeviceWaylandPresentationSupportKHR
  , VkWaylandSurfaceCreateInfoKHR(..)
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
data Wl_display
-- | Opaque data
data Wl_surface
-- ** VkWaylandSurfaceCreateFlagsKHR

-- No documentation found for TopLevel "VkWaylandSurfaceCreateFlagsKHR"
newtype VkWaylandSurfaceCreateFlagsKHR = VkWaylandSurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkWaylandSurfaceCreateFlagsKHR where
  
  showsPrec p (VkWaylandSurfaceCreateFlagsKHR x) = showParen (p >= 11) (showString "VkWaylandSurfaceCreateFlagsKHR " . showsPrec 11 x)

instance Read VkWaylandSurfaceCreateFlagsKHR where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkWaylandSurfaceCreateFlagsKHR")
                        v <- step readPrec
                        pure (VkWaylandSurfaceCreateFlagsKHR v)
                        )
                    )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR = VkStructureType 1000006000
-- No documentation found for TopLevel "VK_KHR_WAYLAND_SURFACE_SPEC_VERSION"
pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION = 6
-- No documentation found for TopLevel "VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME"
pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME = "VK_KHR_wayland_surface"
-- | vkCreateWaylandSurfaceKHR - Create a
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceKHR' object for a
-- Wayland window
--
-- = Parameters
--
-- -   @instance@ is the instance to associate the surface with.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkWaylandSurfaceCreateInfoKHR' structure containing parameters
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
--     @VkWaylandSurfaceCreateInfoKHR@ structure
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
-- 'VkWaylandSurfaceCreateInfoKHR'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateWaylandSurfaceKHR" vkCreateWaylandSurfaceKHR :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkWaylandSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
-- | vkGetPhysicalDeviceWaylandPresentationSupportKHR - Query physical device
-- for presentation to Wayland
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device.
--
-- -   @queueFamilyIndex@ is the queue family index.
--
-- -   @display@ is a pointer to the @wl_display@ associated with a Wayland
--     compositor.
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
-- -   @display@ /must/ be a valid pointer to a @wl_display@ value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceWaylandPresentationSupportKHR" vkGetPhysicalDeviceWaylandPresentationSupportKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("display" ::: Ptr Wl_display) -> IO VkBool32
-- | VkWaylandSurfaceCreateInfoKHR - Structure specifying parameters of a
-- newly created Wayland surface object
--
-- == Valid Usage
--
-- -   @display@ /must/ point to a valid Wayland @wl_display@.
--
-- -   @surface@ /must/ point to a valid Wayland @wl_surface@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'VkWaylandSurfaceCreateFlagsKHR', 'vkCreateWaylandSurfaceKHR'
data VkWaylandSurfaceCreateInfoKHR = VkWaylandSurfaceCreateInfoKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkWaylandSurfaceCreateFlagsKHR
  , -- | @display@ and @surface@ are pointers to the Wayland @wl_display@ and
  -- @wl_surface@ to associate the surface with.
  vkDisplay :: Ptr Wl_display
  , -- No documentation found for Nested "VkWaylandSurfaceCreateInfoKHR" "surface"
  vkSurface :: Ptr Wl_surface
  }
  deriving (Eq, Show)

instance Storable VkWaylandSurfaceCreateInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkWaylandSurfaceCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 24)
                                           <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkWaylandSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkWaylandSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkWaylandSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkDisplay (poked :: VkWaylandSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkSurface (poked :: VkWaylandSurfaceCreateInfoKHR))
