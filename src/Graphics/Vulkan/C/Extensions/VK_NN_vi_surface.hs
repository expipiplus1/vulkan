{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NN_vi_surface
  ( VkViSurfaceCreateFlagsNN(..)
  , VkViSurfaceCreateInfoNN(..)
  , FN_vkCreateViSurfaceNN
  , PFN_vkCreateViSurfaceNN
  , vkCreateViSurfaceNN
  , pattern VK_NN_VI_SURFACE_EXTENSION_NAME
  , pattern VK_NN_VI_SURFACE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN
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
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkInstance
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


-- ** VkViSurfaceCreateFlagsNN

-- No documentation found for TopLevel "VkViSurfaceCreateFlagsNN"
newtype VkViSurfaceCreateFlagsNN = VkViSurfaceCreateFlagsNN VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkViSurfaceCreateFlagsNN where
  
  showsPrec p (VkViSurfaceCreateFlagsNN x) = showParen (p >= 11) (showString "VkViSurfaceCreateFlagsNN " . showsPrec 11 x)

instance Read VkViSurfaceCreateFlagsNN where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkViSurfaceCreateFlagsNN")
                        v <- step readPrec
                        pure (VkViSurfaceCreateFlagsNN v)
                        )
                    )



-- No documentation found for TopLevel "VkViSurfaceCreateInfoNN"
data VkViSurfaceCreateInfoNN = VkViSurfaceCreateInfoNN
  { -- No documentation found for Nested "VkViSurfaceCreateInfoNN" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkViSurfaceCreateInfoNN" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkViSurfaceCreateInfoNN" "flags"
  vkFlags :: VkViSurfaceCreateFlagsNN
  , -- No documentation found for Nested "VkViSurfaceCreateInfoNN" "window"
  vkWindow :: Ptr ()
  }
  deriving (Eq, Show)

instance Storable VkViSurfaceCreateInfoNN where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkViSurfaceCreateInfoNN <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkViSurfaceCreateInfoNN))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkViSurfaceCreateInfoNN))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkViSurfaceCreateInfoNN))
                *> poke (ptr `plusPtr` 24) (vkWindow (poked :: VkViSurfaceCreateInfoNN))

instance Zero VkViSurfaceCreateInfoNN where
  zero = VkViSurfaceCreateInfoNN VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN
                                 zero
                                 zero
                                 zero

-- No documentation found for TopLevel "vkCreateViSurfaceNN"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateViSurfaceNN" vkCreateViSurfaceNN :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkViSurfaceCreateInfoNN) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
#else
vkCreateViSurfaceNN :: InstanceCmds -> ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkViSurfaceCreateInfoNN) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
vkCreateViSurfaceNN deviceCmds = mkVkCreateViSurfaceNN (pVkCreateViSurfaceNN deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateViSurfaceNN
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkViSurfaceCreateInfoNN) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkViSurfaceCreateInfoNN) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif

type FN_vkCreateViSurfaceNN = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkViSurfaceCreateInfoNN) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateViSurfaceNN = FunPtr FN_vkCreateViSurfaceNN

-- No documentation found for TopLevel "VK_NN_VI_SURFACE_EXTENSION_NAME"
pattern VK_NN_VI_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_NN_VI_SURFACE_EXTENSION_NAME = "VK_NN_vi_surface"

-- No documentation found for TopLevel "VK_NN_VI_SURFACE_SPEC_VERSION"
pattern VK_NN_VI_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_NN_VI_SURFACE_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN"
pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN :: VkStructureType
pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN = VkStructureType 1000062000
