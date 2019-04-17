{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface
  ( HINSTANCE
  , HWND
  , VkWin32SurfaceCreateFlagsKHR(..)
  , VkWin32SurfaceCreateInfoKHR(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCreateWin32SurfaceKHR
#endif
  , FN_vkCreateWin32SurfaceKHR
  , PFN_vkCreateWin32SurfaceKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetPhysicalDeviceWin32PresentationSupportKHR
#endif
  , FN_vkGetPhysicalDeviceWin32PresentationSupportKHR
  , PFN_vkGetPhysicalDeviceWin32PresentationSupportKHR
  , pattern VK_KHR_WIN32_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_WIN32_SURFACE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR
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


-- No documentation found for TopLevel "HINSTANCE"
type HINSTANCE = Ptr ()
  
-- No documentation found for TopLevel "HWND"
type HWND = Ptr ()
  
-- ** VkWin32SurfaceCreateFlagsKHR

-- No documentation found for TopLevel "VkWin32SurfaceCreateFlagsKHR"
newtype VkWin32SurfaceCreateFlagsKHR = VkWin32SurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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


-- | VkWin32SurfaceCreateInfoKHR - Structure specifying parameters of a newly
-- created Win32 surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- No cross-references are available
data VkWin32SurfaceCreateInfoKHR = VkWin32SurfaceCreateInfoKHR
  { -- | @sType@ /must/ be @VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR@
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @flags@ /must/ be @0@
  vkFlags :: VkWin32SurfaceCreateFlagsKHR
  , -- | @hinstance@ /must/ be a valid Win32 @HINSTANCE@.
  vkHinstance :: HINSTANCE
  , -- | @hwnd@ /must/ be a valid Win32 @HWND@.
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

instance Zero VkWin32SurfaceCreateInfoKHR where
  zero = VkWin32SurfaceCreateInfoKHR zero
                                     zero
                                     zero
                                     zero
                                     zero
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkCreateWin32SurfaceKHR - Create a
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' object for an
-- Win32 native window
--
-- = Parameters
--
-- -   @instance@ is the instance to associate the surface with.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     @VkWin32SurfaceCreateInfoKHR@ structure containing parameters
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
--     @VkWin32SurfaceCreateInfoKHR@ structure
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
  "vkCreateWin32SurfaceKHR" vkCreateWin32SurfaceKHR :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkWin32SurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult

#endif
type FN_vkCreateWin32SurfaceKHR = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkWin32SurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateWin32SurfaceKHR = FunPtr FN_vkCreateWin32SurfaceKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkGetPhysicalDeviceWin32PresentationSupportKHR - query queue family
-- support for presentation on a Win32 display
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device.
--
-- -   @queueFamilyIndex@ is the queue family index.
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
  "vkGetPhysicalDeviceWin32PresentationSupportKHR" vkGetPhysicalDeviceWin32PresentationSupportKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> IO VkBool32

#endif
type FN_vkGetPhysicalDeviceWin32PresentationSupportKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> IO VkBool32
type PFN_vkGetPhysicalDeviceWin32PresentationSupportKHR = FunPtr FN_vkGetPhysicalDeviceWin32PresentationSupportKHR
-- No documentation found for TopLevel "VK_KHR_WIN32_SURFACE_EXTENSION_NAME"
pattern VK_KHR_WIN32_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_WIN32_SURFACE_EXTENSION_NAME = "VK_KHR_win32_surface"
-- No documentation found for TopLevel "VK_KHR_WIN32_SURFACE_SPEC_VERSION"
pattern VK_KHR_WIN32_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_WIN32_SURFACE_SPEC_VERSION = 6
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR = VkStructureType 1000009000
