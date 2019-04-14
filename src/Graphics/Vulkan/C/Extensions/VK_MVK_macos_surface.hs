{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface
  ( VkMacOSSurfaceCreateFlagsMVK(..)
  , VkMacOSSurfaceCreateInfoMVK(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCreateMacOSSurfaceMVK
#endif
  , FN_vkCreateMacOSSurfaceMVK
  , PFN_vkCreateMacOSSurfaceMVK
  , pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME
  , pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
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
  ( VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkInstance
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkSurfaceKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkMacOSSurfaceCreateFlagsMVK

-- No documentation found for TopLevel "VkMacOSSurfaceCreateFlagsMVK"
newtype VkMacOSSurfaceCreateFlagsMVK = VkMacOSSurfaceCreateFlagsMVK VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkMacOSSurfaceCreateFlagsMVK where
  
  showsPrec p (VkMacOSSurfaceCreateFlagsMVK x) = showParen (p >= 11) (showString "VkMacOSSurfaceCreateFlagsMVK " . showsPrec 11 x)

instance Read VkMacOSSurfaceCreateFlagsMVK where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkMacOSSurfaceCreateFlagsMVK")
                        v <- step readPrec
                        pure (VkMacOSSurfaceCreateFlagsMVK v)
                        )
                    )


-- No documentation found for TopLevel "VkMacOSSurfaceCreateInfoMVK"
data VkMacOSSurfaceCreateInfoMVK = VkMacOSSurfaceCreateInfoMVK
  { -- No documentation found for Nested "VkMacOSSurfaceCreateInfoMVK" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMacOSSurfaceCreateInfoMVK" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMacOSSurfaceCreateInfoMVK" "flags"
  vkFlags :: VkMacOSSurfaceCreateFlagsMVK
  , -- No documentation found for Nested "VkMacOSSurfaceCreateInfoMVK" "pView"
  vkPView :: Ptr ()
  }
  deriving (Eq, Show)

instance Storable VkMacOSSurfaceCreateInfoMVK where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkMacOSSurfaceCreateInfoMVK <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMacOSSurfaceCreateInfoMVK))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMacOSSurfaceCreateInfoMVK))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkMacOSSurfaceCreateInfoMVK))
                *> poke (ptr `plusPtr` 24) (vkPView (poked :: VkMacOSSurfaceCreateInfoMVK))
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCreateMacOSSurfaceMVK"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateMacOSSurfaceMVK" vkCreateMacOSSurfaceMVK :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMacOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult

#endif
type FN_vkCreateMacOSSurfaceMVK = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMacOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateMacOSSurfaceMVK = FunPtr FN_vkCreateMacOSSurfaceMVK
-- No documentation found for TopLevel "VK_MVK_MACOS_SURFACE_EXTENSION_NAME"
pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME = "VK_MVK_macos_surface"
-- No documentation found for TopLevel "VK_MVK_MACOS_SURFACE_SPEC_VERSION"
pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION = 2
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK"
pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK :: VkStructureType
pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK = VkStructureType 1000123000
