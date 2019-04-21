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
  , FN_vkCreateMetalSurfaceEXT
  , PFN_vkCreateMetalSurfaceEXT
  , vkCreateMetalSurfaceEXT
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


-- | Opaque data
data CAMetalLayer

-- ** VkMetalSurfaceCreateFlagsEXT

-- No documentation found for TopLevel "VkMetalSurfaceCreateFlagsEXT"
newtype VkMetalSurfaceCreateFlagsEXT = VkMetalSurfaceCreateFlagsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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



-- | VkMetalSurfaceCreateInfoEXT - Structure specifying parameters of a newly
-- created Metal surface object
--
-- == Valid Usage
--
-- Unresolved directive in VkMetalSurfaceCreateInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkMetalSurfaceCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkMetalSurfaceCreateInfoEXT = VkMetalSurfaceCreateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkMetalSurfaceCreateFlagsEXT
  , -- | @pLayer@ is a reference to a 'CAMetalLayer' object that represents a
  -- renderable surface.
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

instance Zero VkMetalSurfaceCreateInfoEXT where
  zero = VkMetalSurfaceCreateInfoEXT VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT
                                     zero
                                     zero
                                     zero

-- | vkCreateMetalSurfaceEXT - Create a VkSurfaceKHR object for CAMetalLayer
--
-- = Parameters
--
-- -   @instance@ is the instance with which to associate the surface.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkMetalSurfaceCreateInfoEXT' structure containing the parameters
--     affecting the creation of the surface object.
--
-- -   @pAllocator@ is the allocator used for host memory allocated for the
--     surface object when there is no more specific allocator available
--     (see
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
--
-- -   @pSurface@ points to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' handle in
--     which the created surface object is returned.
--
-- = Description
--
-- Unresolved directive in vkCreateMetalSurfaceEXT.txt -
-- include::{generated}\/validity\/protos\/vkCreateMetalSurfaceEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateMetalSurfaceEXT" vkCreateMetalSurfaceEXT :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMetalSurfaceCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
#else
vkCreateMetalSurfaceEXT :: InstanceCmds -> ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMetalSurfaceCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
vkCreateMetalSurfaceEXT deviceCmds = mkVkCreateMetalSurfaceEXT (pVkCreateMetalSurfaceEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateMetalSurfaceEXT
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMetalSurfaceCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMetalSurfaceCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
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
