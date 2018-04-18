{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_android_surface
  ( ANativeWindow
  , VkAndroidSurfaceCreateFlagsKHR(..)
  , pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR
  , pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION
  , pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME
  , vkCreateAndroidSurfaceKHR
  , VkAndroidSurfaceCreateInfoKHR(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
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
  ( VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkInstance
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( VkSurfaceKHR
  )


data ANativeWindow
-- ** VkAndroidSurfaceCreateFlagsKHR

-- | 
newtype VkAndroidSurfaceCreateFlagsKHR = VkAndroidSurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

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


-- | Nothing
pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR = VkStructureType 1000008000
pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION = 6
pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME = "VK_KHR_android_surface"
-- | 
foreign import ccall "vkCreateAndroidSurfaceKHR" vkCreateAndroidSurfaceKHR :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkAndroidSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
-- | TODO: Struct comments
data VkAndroidSurfaceCreateInfoKHR = VkAndroidSurfaceCreateInfoKHR
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkFlags :: VkAndroidSurfaceCreateFlagsKHR
  , vkWindow :: Ptr ANativeWindow
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkAndroidSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkAndroidSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkWindow (poked :: VkAndroidSurfaceCreateInfoKHR))
