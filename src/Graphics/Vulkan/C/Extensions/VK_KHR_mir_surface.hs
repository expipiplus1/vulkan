{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_mir_surface
  ( MirConnection
  , MirSurface
  , VkMirSurfaceCreateFlagsKHR(..)
  , VkMirSurfaceCreateInfoKHR(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCreateMirSurfaceKHR
#endif
  , FN_vkCreateMirSurfaceKHR
  , PFN_vkCreateMirSurfaceKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetPhysicalDeviceMirPresentationSupportKHR
#endif
  , FN_vkGetPhysicalDeviceMirPresentationSupportKHR
  , PFN_vkGetPhysicalDeviceMirPresentationSupportKHR
  , pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_MIR_SURFACE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR
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


-- | Opaque data
data MirConnection
-- | Opaque data
data MirSurface
-- ** VkMirSurfaceCreateFlagsKHR

-- No documentation found for TopLevel "VkMirSurfaceCreateFlagsKHR"
newtype VkMirSurfaceCreateFlagsKHR = VkMirSurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkMirSurfaceCreateFlagsKHR where
  
  showsPrec p (VkMirSurfaceCreateFlagsKHR x) = showParen (p >= 11) (showString "VkMirSurfaceCreateFlagsKHR " . showsPrec 11 x)

instance Read VkMirSurfaceCreateFlagsKHR where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkMirSurfaceCreateFlagsKHR")
                        v <- step readPrec
                        pure (VkMirSurfaceCreateFlagsKHR v)
                        )
                    )


-- No documentation found for TopLevel "VkMirSurfaceCreateInfoKHR"
data VkMirSurfaceCreateInfoKHR = VkMirSurfaceCreateInfoKHR
  { -- No documentation found for Nested "VkMirSurfaceCreateInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMirSurfaceCreateInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMirSurfaceCreateInfoKHR" "flags"
  vkFlags :: VkMirSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "VkMirSurfaceCreateInfoKHR" "connection"
  vkConnection :: Ptr MirConnection
  , -- No documentation found for Nested "VkMirSurfaceCreateInfoKHR" "mirSurface"
  vkMirSurface :: Ptr MirSurface
  }
  deriving (Eq, Show)

instance Storable VkMirSurfaceCreateInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkMirSurfaceCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 24)
                                       <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMirSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMirSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkMirSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkConnection (poked :: VkMirSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkMirSurface (poked :: VkMirSurfaceCreateInfoKHR))
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCreateMirSurfaceKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateMirSurfaceKHR" vkCreateMirSurfaceKHR :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMirSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult

#endif
type FN_vkCreateMirSurfaceKHR = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMirSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateMirSurfaceKHR = FunPtr FN_vkCreateMirSurfaceKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetPhysicalDeviceMirPresentationSupportKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceMirPresentationSupportKHR" vkGetPhysicalDeviceMirPresentationSupportKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("connection" ::: Ptr MirConnection) -> IO VkBool32

#endif
type FN_vkGetPhysicalDeviceMirPresentationSupportKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("connection" ::: Ptr MirConnection) -> IO VkBool32
type PFN_vkGetPhysicalDeviceMirPresentationSupportKHR = FunPtr FN_vkGetPhysicalDeviceMirPresentationSupportKHR
-- No documentation found for TopLevel "VK_KHR_MIR_SURFACE_EXTENSION_NAME"
pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME = "VK_KHR_mir_surface"
-- No documentation found for TopLevel "VK_KHR_MIR_SURFACE_SPEC_VERSION"
pattern VK_KHR_MIR_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_MIR_SURFACE_SPEC_VERSION = 4
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR = VkStructureType 1000007000
