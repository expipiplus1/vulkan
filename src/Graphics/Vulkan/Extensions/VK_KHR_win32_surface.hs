{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_win32_surface
  ( HINSTANCE
  , HWND
  , VkWin32SurfaceCreateFlagsKHR(..)
  , pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR
  , pattern VK_KHR_WIN32_SURFACE_SPEC_VERSION
  , pattern VK_KHR_WIN32_SURFACE_EXTENSION_NAME
  , vkCreateWin32SurfaceKHR
  , vkGetPhysicalDeviceWin32PresentationSupportKHR
  , VkWin32SurfaceCreateInfoKHR(..)
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


-- No documentation found for TopLevel "HINSTANCE"
type HINSTANCE = Ptr ()
-- No documentation found for TopLevel "HWND"
type HWND = Ptr ()
-- ** VkWin32SurfaceCreateFlagsKHR

-- No documentation found for TopLevel "VkWin32SurfaceCreateFlagsKHR"
newtype VkWin32SurfaceCreateFlagsKHR = VkWin32SurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkWin32SurfaceCreateFlagsKHR where
  
  showsPrec p (VkWin32SurfaceCreateFlagsKHR x) = showParen (p >= 11) (showString "VkWin32SurfaceCreateFlagsKHR " . showsPrec 11 x)

instance Read VkWin32SurfaceCreateFlagsKHR where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkWin32SurfaceCreateFlagsKHR")
                        v <- step readPrec
                        pure (VkWin32SurfaceCreateFlagsKHR v)
                        )
                    )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR = VkStructureType 1000009000
-- No documentation found for TopLevel "VK_KHR_WIN32_SURFACE_SPEC_VERSION"
pattern VK_KHR_WIN32_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_WIN32_SURFACE_SPEC_VERSION = 6
-- No documentation found for TopLevel "VK_KHR_WIN32_SURFACE_EXTENSION_NAME"
pattern VK_KHR_WIN32_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_WIN32_SURFACE_EXTENSION_NAME = "VK_KHR_win32_surface"
-- | vkCreateWin32SurfaceKHR - Create a
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceKHR' object for an
-- Win32 native window
--
-- = Parameters
-- #_parameters#
--
-- -   @instance@ is the instance to associate the surface with.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     @VkWin32SurfaceCreateInfoKHR@ structure containing parameters
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
--     @VkWin32SurfaceCreateInfoKHR@ structure
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
-- 'VkWin32SurfaceCreateInfoKHR'
foreign import ccall "vkCreateWin32SurfaceKHR" vkCreateWin32SurfaceKHR :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkWin32SurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
-- | vkGetPhysicalDeviceWin32PresentationSupportKHR - query queue family
-- support for presentation on a Win32 display
--
-- = Parameters
-- #_parameters#
--
-- -   @physicalDevice@ is the physical device.
--
-- -   @queueFamilyIndex@ is the queue family index.
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
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice'
foreign import ccall "vkGetPhysicalDeviceWin32PresentationSupportKHR" vkGetPhysicalDeviceWin32PresentationSupportKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> IO VkBool32
-- | VkWin32SurfaceCreateInfoKHR - Structure specifying parameters of a newly
-- created Win32 surface object
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   @hinstance@ /must/ be a valid Win32 @HINSTANCE@.
--
-- -   @hwnd@ /must/ be a valid Win32 @HWND@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'VkWin32SurfaceCreateFlagsKHR', 'vkCreateWin32SurfaceKHR'
data VkWin32SurfaceCreateInfoKHR = VkWin32SurfaceCreateInfoKHR
  { -- No documentation found for Nested "VkWin32SurfaceCreateInfoKHR" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkWin32SurfaceCreateInfoKHR" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkWin32SurfaceCreateInfoKHR" "vkFlags"
  vkFlags :: VkWin32SurfaceCreateFlagsKHR
  , -- No documentation found for Nested "VkWin32SurfaceCreateInfoKHR" "vkHinstance"
  vkHinstance :: HINSTANCE
  , -- No documentation found for Nested "VkWin32SurfaceCreateInfoKHR" "vkHwnd"
  vkHwnd :: HWND
  }
  deriving (Eq, Show)

instance Storable VkWin32SurfaceCreateInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkWin32SurfaceCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
                                         <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkWin32SurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkWin32SurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkWin32SurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHinstance (poked :: VkWin32SurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkHwnd (poked :: VkWin32SurfaceCreateInfoKHR))
