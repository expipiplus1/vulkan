{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_android_surface
  ( ANativeWindow
  , VkAndroidSurfaceCreateFlagsKHR(..)
  , VkAndroidSurfaceCreateInfoKHR(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCreateAndroidSurfaceKHR
#endif
  , FN_vkCreateAndroidSurfaceKHR
  , PFN_vkCreateAndroidSurfaceKHR
  , pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR
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


-- | Opaque data
data ANativeWindow
-- ** VkAndroidSurfaceCreateFlagsKHR

-- No documentation found for TopLevel "VkAndroidSurfaceCreateFlagsKHR"
newtype VkAndroidSurfaceCreateFlagsKHR = VkAndroidSurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkAndroidSurfaceCreateFlagsKHR where
  
  showsPrec p (VkAndroidSurfaceCreateFlagsKHR x) = showParen (p >= 11) (showString "VkAndroidSurfaceCreateFlagsKHR " . showsPrec 11 x)

instance Read VkAndroidSurfaceCreateFlagsKHR where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkAndroidSurfaceCreateFlagsKHR")
                        v <- step readPrec
                        pure (VkAndroidSurfaceCreateFlagsKHR v)
                        )
                    )


-- No documentation found for TopLevel "VkAndroidSurfaceCreateInfoKHR"
data VkAndroidSurfaceCreateInfoKHR = VkAndroidSurfaceCreateInfoKHR
  { -- No documentation found for Nested "VkAndroidSurfaceCreateInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkAndroidSurfaceCreateInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkAndroidSurfaceCreateInfoKHR" "flags"
  vkFlags :: VkAndroidSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "VkAndroidSurfaceCreateInfoKHR" "window"
  vkWindow :: Ptr ANativeWindow
  }
  deriving (Eq, Show)

instance Storable VkAndroidSurfaceCreateInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkAndroidSurfaceCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkAndroidSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkAndroidSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkAndroidSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkWindow (poked :: VkAndroidSurfaceCreateInfoKHR))

instance Zero VkAndroidSurfaceCreateInfoKHR where
  zero = VkAndroidSurfaceCreateInfoKHR zero
                                       zero
                                       zero
                                       zero
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCreateAndroidSurfaceKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateAndroidSurfaceKHR" vkCreateAndroidSurfaceKHR :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkAndroidSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult

#endif
type FN_vkCreateAndroidSurfaceKHR = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkAndroidSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateAndroidSurfaceKHR = FunPtr FN_vkCreateAndroidSurfaceKHR
-- No documentation found for TopLevel "VK_KHR_ANDROID_SURFACE_EXTENSION_NAME"
pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME = "VK_KHR_android_surface"
-- No documentation found for TopLevel "VK_KHR_ANDROID_SURFACE_SPEC_VERSION"
pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION = 6
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR = VkStructureType 1000008000
