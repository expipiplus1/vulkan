{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NN_vi_surface
  ( VkViSurfaceCreateFlagsNN(..)
  , pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN
  , pattern VK_NN_VI_SURFACE_SPEC_VERSION
  , pattern VK_NN_VI_SURFACE_EXTENSION_NAME
  , vkCreateViSurfaceNN
  , VkViSurfaceCreateInfoNN(..)
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


-- ** VkViSurfaceCreateFlagsNN

-- | 
newtype VkViSurfaceCreateFlagsNN = VkViSurfaceCreateFlagsNN VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

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


-- | Nothing
pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN :: VkStructureType
pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN = VkStructureType 1000062000
pattern VK_NN_VI_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_NN_VI_SURFACE_SPEC_VERSION = 1
pattern VK_NN_VI_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NN_VI_SURFACE_EXTENSION_NAME = "VK_NN_vi_surface"
-- | 
foreign import ccall "vkCreateViSurfaceNN" vkCreateViSurfaceNN :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkViSurfaceCreateInfoNN) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
-- | TODO: Struct comments
data VkViSurfaceCreateInfoNN = VkViSurfaceCreateInfoNN
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkFlags :: VkViSurfaceCreateFlagsNN
  , vkWindow :: Ptr ()
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkViSurfaceCreateInfoNN))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkViSurfaceCreateInfoNN))
                *> poke (ptr `plusPtr` 24) (vkWindow (poked :: VkViSurfaceCreateInfoNN))
