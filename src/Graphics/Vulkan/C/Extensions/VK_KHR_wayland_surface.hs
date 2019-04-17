{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface
  ( VkWaylandSurfaceCreateFlagsKHR(..)
  , VkWaylandSurfaceCreateInfoKHR(..)
  , Wl_display
  , Wl_surface
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCreateWaylandSurfaceKHR
#endif
  , FN_vkCreateWaylandSurfaceKHR
  , PFN_vkCreateWaylandSurfaceKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetPhysicalDeviceWaylandPresentationSupportKHR
#endif
  , FN_vkGetPhysicalDeviceWaylandPresentationSupportKHR
  , PFN_vkGetPhysicalDeviceWaylandPresentationSupportKHR
  , pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR
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


-- ** VkWaylandSurfaceCreateFlagsKHR

-- No documentation found for TopLevel "VkWaylandSurfaceCreateFlagsKHR"
newtype VkWaylandSurfaceCreateFlagsKHR = VkWaylandSurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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


-- No documentation found for TopLevel "VkWaylandSurfaceCreateInfoKHR"
data VkWaylandSurfaceCreateInfoKHR = VkWaylandSurfaceCreateInfoKHR
  { -- No documentation found for Nested "VkWaylandSurfaceCreateInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkWaylandSurfaceCreateInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkWaylandSurfaceCreateInfoKHR" "flags"
  vkFlags :: VkWaylandSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "VkWaylandSurfaceCreateInfoKHR" "display"
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

instance Zero VkWaylandSurfaceCreateInfoKHR where
  zero = VkWaylandSurfaceCreateInfoKHR zero
                                       zero
                                       zero
                                       zero
                                       zero
-- | Opaque data
data Wl_display
-- | Opaque data
data Wl_surface
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCreateWaylandSurfaceKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateWaylandSurfaceKHR" vkCreateWaylandSurfaceKHR :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkWaylandSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult

#endif
type FN_vkCreateWaylandSurfaceKHR = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkWaylandSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateWaylandSurfaceKHR = FunPtr FN_vkCreateWaylandSurfaceKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetPhysicalDeviceWaylandPresentationSupportKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceWaylandPresentationSupportKHR" vkGetPhysicalDeviceWaylandPresentationSupportKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("display" ::: Ptr Wl_display) -> IO VkBool32

#endif
type FN_vkGetPhysicalDeviceWaylandPresentationSupportKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("display" ::: Ptr Wl_display) -> IO VkBool32
type PFN_vkGetPhysicalDeviceWaylandPresentationSupportKHR = FunPtr FN_vkGetPhysicalDeviceWaylandPresentationSupportKHR
-- No documentation found for TopLevel "VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME"
pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME = "VK_KHR_wayland_surface"
-- No documentation found for TopLevel "VK_KHR_WAYLAND_SURFACE_SPEC_VERSION"
pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION = 6
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR = VkStructureType 1000006000
