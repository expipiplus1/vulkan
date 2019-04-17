{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface
  ( GgpStreamDescriptor
  , VkStreamDescriptorSurfaceCreateFlagsGGP(..)
  , VkStreamDescriptorSurfaceCreateInfoGGP(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCreateStreamDescriptorSurfaceGGP
#endif
  , FN_vkCreateStreamDescriptorSurfaceGGP
  , PFN_vkCreateStreamDescriptorSurfaceGGP
  , pattern VK_GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME
  , pattern VK_GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP
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


-- No documentation found for TopLevel "GgpStreamDescriptor"
type GgpStreamDescriptor = Word32
  
-- ** VkStreamDescriptorSurfaceCreateFlagsGGP

-- No documentation found for TopLevel "VkStreamDescriptorSurfaceCreateFlagsGGP"
newtype VkStreamDescriptorSurfaceCreateFlagsGGP = VkStreamDescriptorSurfaceCreateFlagsGGP VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkStreamDescriptorSurfaceCreateFlagsGGP where
  
  showsPrec p (VkStreamDescriptorSurfaceCreateFlagsGGP x) = showParen (p >= 11) (showString "VkStreamDescriptorSurfaceCreateFlagsGGP " . showsPrec 11 x)

instance Read VkStreamDescriptorSurfaceCreateFlagsGGP where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkStreamDescriptorSurfaceCreateFlagsGGP")
                        v <- step readPrec
                        pure (VkStreamDescriptorSurfaceCreateFlagsGGP v)
                        )
                    )


-- | VkStreamDescriptorSurfaceCreateInfoGGP - Structure specifying parameters
-- of a newly created Google Games Platform stream surface object
--
-- == Valid Usage
--
-- Unresolved directive in VkStreamDescriptorSurfaceCreateInfoGGP.txt -
-- include::..\/validity\/structs\/VkStreamDescriptorSurfaceCreateInfoGGP.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkStreamDescriptorSurfaceCreateInfoGGP = VkStreamDescriptorSurfaceCreateInfoGGP
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkStreamDescriptorSurfaceCreateFlagsGGP
  , -- | @streamDescriptor@ /must/ be a valid @GgpStreamDescriptor@
  vkStreamDescriptor :: GgpStreamDescriptor
  }
  deriving (Eq, Show)

instance Storable VkStreamDescriptorSurfaceCreateInfoGGP where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkStreamDescriptorSurfaceCreateInfoGGP <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
                                                    <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkStreamDescriptorSurfaceCreateInfoGGP))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkStreamDescriptorSurfaceCreateInfoGGP))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkStreamDescriptorSurfaceCreateInfoGGP))
                *> poke (ptr `plusPtr` 20) (vkStreamDescriptor (poked :: VkStreamDescriptorSurfaceCreateInfoGGP))

instance Zero VkStreamDescriptorSurfaceCreateInfoGGP where
  zero = VkStreamDescriptorSurfaceCreateInfoGGP zero
                                                zero
                                                zero
                                                zero
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkCreateStreamDescriptorSurfaceGGP - Create a
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' object for a
-- Google Games Platform stream
--
-- = Parameters
--
-- -   @instance@ is the instance to associate with the surface.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     @VkStreamDescriptorSurfaceCreateInfoGGP@ structure containing
--     parameters that affect the creation of the surface object.
--
-- -   @pAllocator@ is the allocator used for host memory allocated for the
--     surface object when there is no more specific allocator available
--     (see
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
--
-- -   @pSurface@ points to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' handle in
--     which the created surface object is returned.
--
-- = Description
--
-- Unresolved directive in vkCreateStreamDescriptorSurfaceGGP.txt -
-- include::..\/validity\/protos\/vkCreateStreamDescriptorSurfaceGGP.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateStreamDescriptorSurfaceGGP" vkCreateStreamDescriptorSurfaceGGP :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkStreamDescriptorSurfaceCreateInfoGGP) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult

#endif
type FN_vkCreateStreamDescriptorSurfaceGGP = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkStreamDescriptorSurfaceCreateInfoGGP) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateStreamDescriptorSurfaceGGP = FunPtr FN_vkCreateStreamDescriptorSurfaceGGP
-- No documentation found for TopLevel "VK_GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME"
pattern VK_GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME = "VK_GGP_stream_descriptor_surface"
-- No documentation found for TopLevel "VK_GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION"
pattern VK_GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP"
pattern VK_STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP :: VkStructureType
pattern VK_STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP = VkStructureType 1000049000
