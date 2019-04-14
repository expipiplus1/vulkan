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


-- No documentation found for TopLevel "VkXlibSurfaceCreateInfoKHR"
data VkXlibSurfaceCreateInfoKHR = VkXlibSurfaceCreateInfoKHR
  { -- No documentation found for Nested "VkXlibSurfaceCreateInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkXlibSurfaceCreateInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkXlibSurfaceCreateInfoKHR" "flags"
  vkFlags :: VkXlibSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "VkXlibSurfaceCreateInfoKHR" "dpy"
  vkDpy :: Ptr Display
  , -- No documentation found for Nested "VkXlibSurfaceCreateInfoKHR" "window"
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
-- No documentation found for TopLevel "Window"
type Window = Word64
  
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCreateXlibSurfaceKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateXlibSurfaceKHR" vkCreateXlibSurfaceKHR :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXlibSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult

#endif
type FN_vkCreateXlibSurfaceKHR = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXlibSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateXlibSurfaceKHR = FunPtr FN_vkCreateXlibSurfaceKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetPhysicalDeviceXlibPresentationSupportKHR"
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
