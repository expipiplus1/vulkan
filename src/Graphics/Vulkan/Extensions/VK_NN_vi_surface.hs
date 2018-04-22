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
  ( Ptr
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

-- No documentation found for TopLevel "VkViSurfaceCreateFlagsNN"
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


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN"
pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN :: VkStructureType
pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN = VkStructureType 1000062000
-- No documentation found for TopLevel "VK_NN_VI_SURFACE_SPEC_VERSION"
pattern VK_NN_VI_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_NN_VI_SURFACE_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_NN_VI_SURFACE_EXTENSION_NAME"
pattern VK_NN_VI_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NN_VI_SURFACE_EXTENSION_NAME = "VK_NN_vi_surface"
-- | vkCreateViSurfaceNN - Create a
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceKHR' object for a VI
-- layer
--
-- = Parameters
--
-- -   @instance@ is the instance with which to associate the surface.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     @VkViSurfaceCreateInfoNN@ structure containing parameters affecting
--     the creation of the surface object.
--
-- -   @pAllocator@ is the allocator used for host memory allocated for the
--     surface object when there is no more specific allocator available
--     (see [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)).
--
-- -   @pSurface@ points to a @VkSurfaceKHR@ handle in which the created
--     surface object is returned.
--
-- = Description
--
-- During the lifetime of a surface created using a particular
-- @nn@::@vi@::@NativeWindowHandle@ any attempts to create another surface
-- for the same @nn@::@vi@::@Layer@ and any attempts to connect to the same
-- @nn@::@vi@::@Layer@ through other platform mechanisms will have
-- undefined results.
--
-- The @currentExtent@ of a VI surface is always undefined. Applications
-- are expected to choose an appropriate size for the swapchain’s
-- @imageExtent@ (e.g., by matching the the result of a call to
-- @nn@::@vi@::@GetDisplayResolution@).
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid @VkInstance@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkViSurfaceCreateInfoNN@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pSurface@ /must/ be a valid pointer to a @VkSurfaceKHR@ handle
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_NATIVE_WINDOW_IN_USE_KHR@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkInstance',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceKHR',
-- 'VkViSurfaceCreateInfoNN'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateViSurfaceNN" vkCreateViSurfaceNN :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkViSurfaceCreateInfoNN) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
-- | VkViSurfaceCreateInfoNN - Structure specifying parameters of a newly
-- created VI surface object
--
-- == Valid Usage
--
-- -   @window@ /must/ be a valid @nn@::@vi@::@NativeWindowHandle@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'VkViSurfaceCreateFlagsNN', 'vkCreateViSurfaceNN'
data VkViSurfaceCreateInfoNN = VkViSurfaceCreateInfoNN
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkViSurfaceCreateFlagsNN
  , -- | @window@ is the @nn@::@vi@::@NativeWindowHandle@ for the
  -- @nn@::@vi@::@Layer@ with which to associate the surface.
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
