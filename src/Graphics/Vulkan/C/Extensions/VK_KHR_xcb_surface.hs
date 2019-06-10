{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface
  ( VkXcbSurfaceCreateFlagsKHR(..)
  , VkXcbSurfaceCreateInfoKHR(..)
  , Xcb_connection_t
  , Xcb_visualid_t
  , Xcb_window_t
  , FN_vkCreateXcbSurfaceKHR
  , PFN_vkCreateXcbSurfaceKHR
  , vkCreateXcbSurfaceKHR
  , FN_vkGetPhysicalDeviceXcbPresentationSupportKHR
  , PFN_vkGetPhysicalDeviceXcbPresentationSupportKHR
  , vkGetPhysicalDeviceXcbPresentationSupportKHR
  , pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_XCB_SURFACE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR
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
import Graphics.Vulkan.C.Dynamic
  ( InstanceCmds(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkSurfaceKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkXcbSurfaceCreateFlagsKHR

-- No documentation found for TopLevel "VkXcbSurfaceCreateFlagsKHR"
newtype VkXcbSurfaceCreateFlagsKHR = VkXcbSurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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



-- No documentation found for TopLevel "VkXcbSurfaceCreateInfoKHR"
data VkXcbSurfaceCreateInfoKHR = VkXcbSurfaceCreateInfoKHR
  { -- No documentation found for Nested "VkXcbSurfaceCreateInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkXcbSurfaceCreateInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkXcbSurfaceCreateInfoKHR" "flags"
  vkFlags :: VkXcbSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "VkXcbSurfaceCreateInfoKHR" "connection"
  vkConnection :: Ptr Xcb_connection_t
  , -- No documentation found for Nested "VkXcbSurfaceCreateInfoKHR" "window"
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

instance Zero VkXcbSurfaceCreateInfoKHR where
  zero = VkXcbSurfaceCreateInfoKHR VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR
                                   zero
                                   zero
                                   zero
                                   zero

-- | Opaque data
data Xcb_connection_t

-- No documentation found for TopLevel "Xcb_visualid_t"
type Xcb_visualid_t = Word32
  

-- No documentation found for TopLevel "Xcb_window_t"
type Xcb_window_t = Word32
  

-- No documentation found for TopLevel "vkCreateXcbSurfaceKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateXcbSurfaceKHR" vkCreateXcbSurfaceKHR :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXcbSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
#else
vkCreateXcbSurfaceKHR :: InstanceCmds -> ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXcbSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
vkCreateXcbSurfaceKHR deviceCmds = mkVkCreateXcbSurfaceKHR (pVkCreateXcbSurfaceKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateXcbSurfaceKHR
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXcbSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXcbSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif

type FN_vkCreateXcbSurfaceKHR = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXcbSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateXcbSurfaceKHR = FunPtr FN_vkCreateXcbSurfaceKHR

-- No documentation found for TopLevel "vkGetPhysicalDeviceXcbPresentationSupportKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceXcbPresentationSupportKHR" vkGetPhysicalDeviceXcbPresentationSupportKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("connection" ::: Ptr Xcb_connection_t) -> ("visual_id" ::: Xcb_visualid_t) -> IO VkBool32
#else
vkGetPhysicalDeviceXcbPresentationSupportKHR :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("connection" ::: Ptr Xcb_connection_t) -> ("visual_id" ::: Xcb_visualid_t) -> IO VkBool32
vkGetPhysicalDeviceXcbPresentationSupportKHR deviceCmds = mkVkGetPhysicalDeviceXcbPresentationSupportKHR (pVkGetPhysicalDeviceXcbPresentationSupportKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceXcbPresentationSupportKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("connection" ::: Ptr Xcb_connection_t) -> ("visual_id" ::: Xcb_visualid_t) -> IO VkBool32) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("connection" ::: Ptr Xcb_connection_t) -> ("visual_id" ::: Xcb_visualid_t) -> IO VkBool32)
#endif

type FN_vkGetPhysicalDeviceXcbPresentationSupportKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("connection" ::: Ptr Xcb_connection_t) -> ("visual_id" ::: Xcb_visualid_t) -> IO VkBool32
type PFN_vkGetPhysicalDeviceXcbPresentationSupportKHR = FunPtr FN_vkGetPhysicalDeviceXcbPresentationSupportKHR

-- No documentation found for TopLevel "VK_KHR_XCB_SURFACE_EXTENSION_NAME"
pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME = "VK_KHR_xcb_surface"

-- No documentation found for TopLevel "VK_KHR_XCB_SURFACE_SPEC_VERSION"
pattern VK_KHR_XCB_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_XCB_SURFACE_SPEC_VERSION = 6

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR = VkStructureType 1000005000
