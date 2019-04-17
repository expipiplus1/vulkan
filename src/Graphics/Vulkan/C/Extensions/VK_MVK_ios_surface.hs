{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface
  ( VkIOSSurfaceCreateFlagsMVK(..)
  , VkIOSSurfaceCreateInfoMVK(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCreateIOSSurfaceMVK
#endif
  , FN_vkCreateIOSSurfaceMVK
  , PFN_vkCreateIOSSurfaceMVK
  , pattern VK_MVK_IOS_SURFACE_EXTENSION_NAME
  , pattern VK_MVK_IOS_SURFACE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK
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
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkSurfaceKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkIOSSurfaceCreateFlagsMVK

-- No documentation found for TopLevel "VkIOSSurfaceCreateFlagsMVK"
newtype VkIOSSurfaceCreateFlagsMVK = VkIOSSurfaceCreateFlagsMVK VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkIOSSurfaceCreateFlagsMVK where
  
  showsPrec p (VkIOSSurfaceCreateFlagsMVK x) = showParen (p >= 11) (showString "VkIOSSurfaceCreateFlagsMVK " . showsPrec 11 x)

instance Read VkIOSSurfaceCreateFlagsMVK where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkIOSSurfaceCreateFlagsMVK")
                        v <- step readPrec
                        pure (VkIOSSurfaceCreateFlagsMVK v)
                        )
                    )


-- No documentation found for TopLevel "VkIOSSurfaceCreateInfoMVK"
data VkIOSSurfaceCreateInfoMVK = VkIOSSurfaceCreateInfoMVK
  { -- No documentation found for Nested "VkIOSSurfaceCreateInfoMVK" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkIOSSurfaceCreateInfoMVK" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkIOSSurfaceCreateInfoMVK" "flags"
  vkFlags :: VkIOSSurfaceCreateFlagsMVK
  , -- No documentation found for Nested "VkIOSSurfaceCreateInfoMVK" "pView"
  vkPView :: Ptr ()
  }
  deriving (Eq, Show)

instance Storable VkIOSSurfaceCreateInfoMVK where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkIOSSurfaceCreateInfoMVK <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkIOSSurfaceCreateInfoMVK))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkIOSSurfaceCreateInfoMVK))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkIOSSurfaceCreateInfoMVK))
                *> poke (ptr `plusPtr` 24) (vkPView (poked :: VkIOSSurfaceCreateInfoMVK))

instance Zero VkIOSSurfaceCreateInfoMVK where
  zero = VkIOSSurfaceCreateInfoMVK zero
                                   zero
                                   zero
                                   zero
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCreateIOSSurfaceMVK"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateIOSSurfaceMVK" vkCreateIOSSurfaceMVK :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkIOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult

#endif
type FN_vkCreateIOSSurfaceMVK = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkIOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateIOSSurfaceMVK = FunPtr FN_vkCreateIOSSurfaceMVK
-- No documentation found for TopLevel "VK_MVK_IOS_SURFACE_EXTENSION_NAME"
pattern VK_MVK_IOS_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_MVK_IOS_SURFACE_EXTENSION_NAME = "VK_MVK_ios_surface"
-- No documentation found for TopLevel "VK_MVK_IOS_SURFACE_SPEC_VERSION"
pattern VK_MVK_IOS_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_MVK_IOS_SURFACE_SPEC_VERSION = 2
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK"
pattern VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK :: VkStructureType
pattern VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK = VkStructureType 1000122000
