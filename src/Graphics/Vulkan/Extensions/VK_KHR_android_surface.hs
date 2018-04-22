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


-- | Opaque data
data ANativeWindow
-- ** VkAndroidSurfaceCreateFlagsKHR

-- No documentation found for TopLevel "VkAndroidSurfaceCreateFlagsKHR"
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


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR = VkStructureType 1000008000
-- No documentation found for TopLevel "VK_KHR_ANDROID_SURFACE_SPEC_VERSION"
pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION = 6
-- No documentation found for TopLevel "VK_KHR_ANDROID_SURFACE_EXTENSION_NAME"
pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME = "VK_KHR_android_surface"
-- | vkCreateAndroidSurfaceKHR - Create a
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceKHR' object for an
-- Android native window
--
-- = Parameters
--
-- -   @instance@ is the instance to associate the surface with.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     @VkAndroidSurfaceCreateInfoKHR@ structure containing parameters
--     affecting the creation of the surface object.
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
-- @ANativeWindow@ handle any attempts to create another surface for the
-- same @ANativeWindow@ and any attempts to connect to the same
-- @ANativeWindow@ through other platform mechanisms will fail.
--
-- __Note__
--
-- In particular, only one @VkSurfaceKHR@ /can/ exist at a time for a given
-- window. Similarly, a native window /cannot/ be used by both a
-- @VkSurfaceKHR@ and @EGLSurface@ simultaneously.
--
-- If successful, @vkCreateAndroidSurfaceKHR@ increments the
-- @ANativeWindow@’s reference count, and @vkDestroySurfaceKHR@ will
-- decrement it.
--
-- On Android, when a swapchain’s @imageExtent@ does not match the
-- surface’s @currentExtent@, the presentable images will be scaled to the
-- surface’s dimensions during presentation. @minImageExtent@ is (1,1), and
-- @maxImageExtent@ is the maximum image size supported by the consumer.
-- For the system compositor, @currentExtent@ is the window size (i.e. the
-- consumer’s preferred size).
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid @VkInstance@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkAndroidSurfaceCreateInfoKHR@ structure
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
-- 'VkAndroidSurfaceCreateInfoKHR',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkInstance',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceKHR'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateAndroidSurfaceKHR" vkCreateAndroidSurfaceKHR :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkAndroidSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
-- | VkAndroidSurfaceCreateInfoKHR - Structure specifying parameters of a
-- newly created Android surface object
--
-- == Valid Usage
--
-- -   @window@ /must/ point to a valid Android @ANativeWindow@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- = See Also
--
-- 'VkAndroidSurfaceCreateFlagsKHR',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkCreateAndroidSurfaceKHR'
data VkAndroidSurfaceCreateInfoKHR = VkAndroidSurfaceCreateInfoKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkAndroidSurfaceCreateFlagsKHR
  , -- | @window@ is a pointer to the @ANativeWindow@ to associate the surface
  -- with.
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
