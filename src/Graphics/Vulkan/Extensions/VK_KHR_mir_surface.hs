{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_mir_surface
  ( MirConnection
  , MirSurface
  , VkMirSurfaceCreateFlagsKHR(..)
  , pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR
  , pattern VK_KHR_MIR_SURFACE_SPEC_VERSION
  , pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME
  , vkCreateMirSurfaceKHR
  , vkGetPhysicalDeviceMirPresentationSupportKHR
  , VkMirSurfaceCreateInfoKHR(..)
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
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkInstance
  , VkPhysicalDevice
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( VkSurfaceKHR
  )


-- | Opaque data
data MirConnection
-- | Opaque data
data MirSurface
-- ** VkMirSurfaceCreateFlagsKHR

-- No documentation found for TopLevel "VkMirSurfaceCreateFlagsKHR"
newtype VkMirSurfaceCreateFlagsKHR = VkMirSurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkMirSurfaceCreateFlagsKHR where
  
  showsPrec p (VkMirSurfaceCreateFlagsKHR x) = showParen (p >= 11) (showString "VkMirSurfaceCreateFlagsKHR " . showsPrec 11 x)

instance Read VkMirSurfaceCreateFlagsKHR where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkMirSurfaceCreateFlagsKHR")
                        v <- step readPrec
                        pure (VkMirSurfaceCreateFlagsKHR v)
                        )
                    )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR = VkStructureType 1000007000
-- No documentation found for TopLevel "VK_KHR_MIR_SURFACE_SPEC_VERSION"
pattern VK_KHR_MIR_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_MIR_SURFACE_SPEC_VERSION = 4
-- No documentation found for TopLevel "VK_KHR_MIR_SURFACE_EXTENSION_NAME"
pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME = "VK_KHR_mir_surface"
-- | vkCreateMirSurfaceKHR - Create a
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceKHR' object for a
-- Mir window
--
-- = Parameters
--
-- -   @instance@ is the instance to associate the surface with.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkMirSurfaceCreateInfoKHR' structure containing parameters
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
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid @VkInstance@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkMirSurfaceCreateInfoKHR@ structure
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
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkInstance',
-- 'VkMirSurfaceCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceKHR'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateMirSurfaceKHR" vkCreateMirSurfaceKHR :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMirSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
-- | vkGetPhysicalDeviceMirPresentationSupportKHR - Query physical device for
-- presentation to Mir
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device.
--
-- -   @queueFamilyIndex@ is the queue family index.
--
-- -   @connection@ is a pointer to the @MirConnection@, and identifies the
--     desired Mir compositor.
--
-- = Description
--
-- This platform-specific function /can/ be called prior to creating a
-- surface.
--
-- == Valid Usage
--
-- -   @queueFamilyIndex@ /must/ be less than @pQueueFamilyPropertyCount@
--     returned by @vkGetPhysicalDeviceQueueFamilyProperties@ for the given
--     @physicalDevice@
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @connection@ /must/ be a valid pointer to a @MirConnection@ value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceMirPresentationSupportKHR" vkGetPhysicalDeviceMirPresentationSupportKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("connection" ::: Ptr MirConnection) -> IO VkBool32
-- | VkMirSurfaceCreateInfoKHR - Structure specifying parameters of a newly
-- created Mir surface object
--
-- == Valid Usage
--
-- -   @connection@ /must/ point to a valid @MirConnection@.
--
-- -   @surface@ /must/ point to a valid @MirSurface@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- = See Also
--
-- 'VkMirSurfaceCreateFlagsKHR',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCreateMirSurfaceKHR'
data VkMirSurfaceCreateInfoKHR = VkMirSurfaceCreateInfoKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkMirSurfaceCreateFlagsKHR
  , -- | @connection@ and @surface@ are pointers to the @MirConnection@ and
  -- @MirSurface@ for the window to associate the surface with.
  vkConnection :: Ptr MirConnection
  , -- No documentation found for Nested "VkMirSurfaceCreateInfoKHR" "mirSurface"
  vkMirSurface :: Ptr MirSurface
  }
  deriving (Eq, Show)

instance Storable VkMirSurfaceCreateInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkMirSurfaceCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 24)
                                       <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMirSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMirSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkMirSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkConnection (poked :: VkMirSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkMirSurface (poked :: VkMirSurfaceCreateInfoKHR))
