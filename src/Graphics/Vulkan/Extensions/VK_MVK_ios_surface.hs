{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_MVK_ios_surface
  ( VkIOSSurfaceCreateFlagsMVK(..)
  , pattern VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK
  , pattern VK_MVK_IOS_SURFACE_SPEC_VERSION
  , pattern VK_MVK_IOS_SURFACE_EXTENSION_NAME
  , vkCreateIOSSurfaceMVK
  , VkIOSSurfaceCreateInfoMVK(..)
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


-- ** VkIOSSurfaceCreateFlagsMVK

-- | 
newtype VkIOSSurfaceCreateFlagsMVK = VkIOSSurfaceCreateFlagsMVK VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

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


-- | Nothing
pattern VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK :: VkStructureType
pattern VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK = VkStructureType 1000122000
pattern VK_MVK_IOS_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_MVK_IOS_SURFACE_SPEC_VERSION = 2
pattern VK_MVK_IOS_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_MVK_IOS_SURFACE_EXTENSION_NAME = "VK_MVK_ios_surface"
-- | 
foreign import ccall "vkCreateIOSSurfaceMVK" vkCreateIOSSurfaceMVK :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkIOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
-- | TODO: Struct comments
data VkIOSSurfaceCreateInfoMVK = VkIOSSurfaceCreateInfoMVK
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkFlags :: VkIOSSurfaceCreateFlagsMVK
  , vkPView :: Ptr ()
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
