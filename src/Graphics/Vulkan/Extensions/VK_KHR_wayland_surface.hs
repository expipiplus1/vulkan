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


data Wl_display
data Wl_surface
-- ** VkWaylandSurfaceCreateFlagsKHR

-- | 
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


-- | Nothing
pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR = VkStructureType 1000006000
pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION = 6
pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME = "VK_KHR_wayland_surface"
-- | 
foreign import ccall "vkCreateWaylandSurfaceKHR" vkCreateWaylandSurfaceKHR :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkWaylandSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
-- | 
foreign import ccall "vkGetPhysicalDeviceWaylandPresentationSupportKHR" vkGetPhysicalDeviceWaylandPresentationSupportKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("display" ::: Ptr Wl_display) -> IO VkBool32
-- | TODO: Struct comments
data VkWaylandSurfaceCreateInfoKHR = VkWaylandSurfaceCreateInfoKHR
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkFlags :: VkWaylandSurfaceCreateFlagsKHR
  , vkDisplay :: Ptr Wl_display
  , vkSurface :: Ptr Wl_surface
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkWaylandSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkWaylandSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkDisplay (poked :: VkWaylandSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkSurface (poked :: VkWaylandSurfaceCreateInfoKHR))
