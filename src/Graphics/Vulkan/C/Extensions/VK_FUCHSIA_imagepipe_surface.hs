{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_FUCHSIA_imagepipe_surface
  ( VkImagePipeSurfaceCreateFlagsFUCHSIA(..)
  , VkImagePipeSurfaceCreateInfoFUCHSIA(..)
  , Zx_handle_t
  , FN_vkCreateImagePipeSurfaceFUCHSIA
  , PFN_vkCreateImagePipeSurfaceFUCHSIA
  , vkCreateImagePipeSurfaceFUCHSIA
  , pattern VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME
  , pattern VK_FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA
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
import Graphics.Vulkan.C.Dynamic
  ( InstanceCmds(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkSurfaceKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkImagePipeSurfaceCreateFlagsFUCHSIA

-- No documentation found for TopLevel "VkImagePipeSurfaceCreateFlagsFUCHSIA"
newtype VkImagePipeSurfaceCreateFlagsFUCHSIA = VkImagePipeSurfaceCreateFlagsFUCHSIA VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkImagePipeSurfaceCreateFlagsFUCHSIA where
  
  showsPrec p (VkImagePipeSurfaceCreateFlagsFUCHSIA x) = showParen (p >= 11) (showString "VkImagePipeSurfaceCreateFlagsFUCHSIA " . showsPrec 11 x)

instance Read VkImagePipeSurfaceCreateFlagsFUCHSIA where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImagePipeSurfaceCreateFlagsFUCHSIA")
                        v <- step readPrec
                        pure (VkImagePipeSurfaceCreateFlagsFUCHSIA v)
                        )
                    )



-- | VkImagePipeSurfaceCreateInfoFUCHSIA - Structure specifying parameters of
-- a newly created ImagePipe surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'VkImagePipeSurfaceCreateFlagsFUCHSIA',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkCreateImagePipeSurfaceFUCHSIA'
data VkImagePipeSurfaceCreateInfoFUCHSIA = VkImagePipeSurfaceCreateInfoFUCHSIA
  { -- | @sType@ /must/ be
  -- 'VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA'
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @flags@ /must/ be @0@
  vkFlags :: VkImagePipeSurfaceCreateFlagsFUCHSIA
  , -- | @imagePipeHandle@ /must/ be a valid @zx_handle_t@
  vkImagePipeHandle :: Zx_handle_t
  }
  deriving (Eq, Show)

instance Storable VkImagePipeSurfaceCreateInfoFUCHSIA where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImagePipeSurfaceCreateInfoFUCHSIA <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImagePipeSurfaceCreateInfoFUCHSIA))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImagePipeSurfaceCreateInfoFUCHSIA))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkImagePipeSurfaceCreateInfoFUCHSIA))
                *> poke (ptr `plusPtr` 20) (vkImagePipeHandle (poked :: VkImagePipeSurfaceCreateInfoFUCHSIA))

instance Zero VkImagePipeSurfaceCreateInfoFUCHSIA where
  zero = VkImagePipeSurfaceCreateInfoFUCHSIA VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA
                                             zero
                                             zero
                                             zero

-- No documentation found for TopLevel "Zx_handle_t"
type Zx_handle_t = Word32
  

-- | vkCreateImagePipeSurfaceFUCHSIA - Create a
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' object for a
-- Fuchsia ImagePipe
--
-- = Parameters
--
-- -   @instance@ is the instance to associate with the surface.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkImagePipeSurfaceCreateInfoFUCHSIA' structure containing
--     parameters affecting the creation of the surface object.
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
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'VkImagePipeSurfaceCreateInfoFUCHSIA' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pSurface@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'VkImagePipeSurfaceCreateInfoFUCHSIA',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateImagePipeSurfaceFUCHSIA" vkCreateImagePipeSurfaceFUCHSIA :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkImagePipeSurfaceCreateInfoFUCHSIA) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
#else
vkCreateImagePipeSurfaceFUCHSIA :: InstanceCmds -> ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkImagePipeSurfaceCreateInfoFUCHSIA) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
vkCreateImagePipeSurfaceFUCHSIA deviceCmds = mkVkCreateImagePipeSurfaceFUCHSIA (pVkCreateImagePipeSurfaceFUCHSIA deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateImagePipeSurfaceFUCHSIA
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkImagePipeSurfaceCreateInfoFUCHSIA) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkImagePipeSurfaceCreateInfoFUCHSIA) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif

type FN_vkCreateImagePipeSurfaceFUCHSIA = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkImagePipeSurfaceCreateInfoFUCHSIA) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateImagePipeSurfaceFUCHSIA = FunPtr FN_vkCreateImagePipeSurfaceFUCHSIA

-- No documentation found for TopLevel "VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME"
pattern VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME = "VK_FUCHSIA_imagepipe_surface"

-- No documentation found for TopLevel "VK_FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION"
pattern VK_FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA"
pattern VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA = VkStructureType 1000214000
