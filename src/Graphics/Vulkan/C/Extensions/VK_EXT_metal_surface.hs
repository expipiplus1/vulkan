{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface
  ( CAMetalLayer
  , VkMetalSurfaceCreateFlagsEXT(..)
  , VkMetalSurfaceCreateInfoEXT(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCreateMetalSurfaceEXT
#endif
  , FN_vkCreateMetalSurfaceEXT
  , PFN_vkCreateMetalSurfaceEXT
  , pattern VK_EXT_METAL_SURFACE_EXTENSION_NAME
  , pattern VK_EXT_METAL_SURFACE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT
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


-- | Opaque data
data CAMetalLayer
-- ** VkMetalSurfaceCreateFlagsEXT

-- No documentation found for TopLevel "VkMetalSurfaceCreateFlagsEXT"
newtype VkMetalSurfaceCreateFlagsEXT = VkMetalSurfaceCreateFlagsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkMetalSurfaceCreateFlagsEXT where
  
  showsPrec p (VkMetalSurfaceCreateFlagsEXT x) = showParen (p >= 11) (showString "VkMetalSurfaceCreateFlagsEXT " . showsPrec 11 x)

instance Read VkMetalSurfaceCreateFlagsEXT where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkMetalSurfaceCreateFlagsEXT")
                        v <- step readPrec
                        pure (VkMetalSurfaceCreateFlagsEXT v)
                        )
                    )


-- No documentation found for TopLevel "VkMetalSurfaceCreateInfoEXT"
data VkMetalSurfaceCreateInfoEXT = VkMetalSurfaceCreateInfoEXT
  { -- No documentation found for Nested "VkMetalSurfaceCreateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMetalSurfaceCreateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMetalSurfaceCreateInfoEXT" "flags"
  vkFlags :: VkMetalSurfaceCreateFlagsEXT
  , -- No documentation found for Nested "VkMetalSurfaceCreateInfoEXT" "pLayer"
  vkPLayer :: Ptr CAMetalLayer
  }
  deriving (Eq, Show)

instance Storable VkMetalSurfaceCreateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkMetalSurfaceCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMetalSurfaceCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMetalSurfaceCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkMetalSurfaceCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkPLayer (poked :: VkMetalSurfaceCreateInfoEXT))
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCreateMetalSurfaceEXT"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateMetalSurfaceEXT" vkCreateMetalSurfaceEXT :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMetalSurfaceCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult

#endif
type FN_vkCreateMetalSurfaceEXT = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMetalSurfaceCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateMetalSurfaceEXT = FunPtr FN_vkCreateMetalSurfaceEXT
-- No documentation found for TopLevel "VK_EXT_METAL_SURFACE_EXTENSION_NAME"
pattern VK_EXT_METAL_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_METAL_SURFACE_EXTENSION_NAME = "VK_EXT_metal_surface"
-- No documentation found for TopLevel "VK_EXT_METAL_SURFACE_SPEC_VERSION"
pattern VK_EXT_METAL_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_EXT_METAL_SURFACE_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT = VkStructureType 1000217000
