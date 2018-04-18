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


newtype Display = Display (Ptr Display)
type VisualID = Word64
type Window = Word64
-- ** VkXlibSurfaceCreateFlagsKHR

-- | 
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


-- | Nothing
pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR = VkStructureType 1000004000
pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION = 6
pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME = "VK_KHR_xlib_surface"
-- | 
foreign import ccall "vkCreateXlibSurfaceKHR" vkCreateXlibSurfaceKHR :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXlibSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
-- | 
foreign import ccall "vkGetPhysicalDeviceXlibPresentationSupportKHR" vkGetPhysicalDeviceXlibPresentationSupportKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("dpy" ::: Ptr Display) -> ("visualID" ::: VisualID) -> IO VkBool32
-- | TODO: Struct comments
data VkXlibSurfaceCreateInfoKHR = VkXlibSurfaceCreateInfoKHR
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkFlags :: VkXlibSurfaceCreateFlagsKHR
  , vkDpy :: Ptr Display
  , vkWindow :: Window
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkXlibSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkXlibSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkDpy (poked :: VkXlibSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkWindow (poked :: VkXlibSurfaceCreateInfoKHR))
