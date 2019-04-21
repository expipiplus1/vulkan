{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NN_vi_surface
  ( ViSurfaceCreateFlagsNN
  , withCStructViSurfaceCreateInfoNN
  , fromCStructViSurfaceCreateInfoNN
  , ViSurfaceCreateInfoNN(..)
  , createViSurfaceNN
  , pattern VK_NN_VI_SURFACE_SPEC_VERSION
  , pattern VK_NN_VI_SURFACE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( Ptr
  , castPtr
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_NN_vi_surface
  ( VkViSurfaceCreateFlagsNN(..)
  , VkViSurfaceCreateInfoNN(..)
  , vkCreateViSurfaceNN
  , pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Instance(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( SurfaceKHR
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_NN_vi_surface
  ( pattern VK_NN_VI_SURFACE_EXTENSION_NAME
  , pattern VK_NN_VI_SURFACE_SPEC_VERSION
  )


-- No documentation found for TopLevel "ViSurfaceCreateFlagsNN"
type ViSurfaceCreateFlagsNN = VkViSurfaceCreateFlagsNN


-- | VkViSurfaceCreateInfoNN - Structure specifying parameters of a newly
-- created VI surface object
--
-- == Valid Usage
--
-- Unresolved directive in VkViSurfaceCreateInfoNN.txt -
-- include::{generated}\/validity\/structs\/VkViSurfaceCreateInfoNN.txt[]
--
-- = See Also
--
-- No cross-references are available
data ViSurfaceCreateInfoNN = ViSurfaceCreateInfoNN
  { -- Univalued member elided
  -- No documentation found for Nested "ViSurfaceCreateInfoNN" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ViSurfaceCreateInfoNN" "flags"
  flags :: ViSurfaceCreateFlagsNN
  , -- No documentation found for Nested "ViSurfaceCreateInfoNN" "window"
  window :: Ptr ()
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkViSurfaceCreateInfoNN' and
-- marshal a 'ViSurfaceCreateInfoNN' into it. The 'VkViSurfaceCreateInfoNN' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructViSurfaceCreateInfoNN :: ViSurfaceCreateInfoNN -> (VkViSurfaceCreateInfoNN -> IO a) -> IO a
withCStructViSurfaceCreateInfoNN marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ViSurfaceCreateInfoNN)) (\pPNext -> cont (VkViSurfaceCreateInfoNN VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN pPNext (flags (marshalled :: ViSurfaceCreateInfoNN)) (window (marshalled :: ViSurfaceCreateInfoNN))))

-- | A function to read a 'VkViSurfaceCreateInfoNN' and all additional
-- structures in the pointer chain into a 'ViSurfaceCreateInfoNN'.
fromCStructViSurfaceCreateInfoNN :: VkViSurfaceCreateInfoNN -> IO ViSurfaceCreateInfoNN
fromCStructViSurfaceCreateInfoNN c = ViSurfaceCreateInfoNN <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkViSurfaceCreateInfoNN)))
                                                           <*> pure (vkFlags (c :: VkViSurfaceCreateInfoNN))
                                                           <*> pure (vkWindow (c :: VkViSurfaceCreateInfoNN))

instance Zero ViSurfaceCreateInfoNN where
  zero = ViSurfaceCreateInfoNN Nothing
                               zero
                               zero



-- | vkCreateViSurfaceNN - Create a
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' object for a
-- VI layer
--
-- = Parameters
--
-- -   @instance@ is the instance with which to associate the surface.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_NN_vi_surface.VkViSurfaceCreateInfoNN'
--     structure containing parameters affecting the creation of the
--     surface object.
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
-- During the lifetime of a surface created using a particular
-- @nn@::@vi@::@NativeWindowHandle@, applications /must/ not attempt to
-- create another surface for the same @nn@::@vi@::@Layer@ or attempt to
-- connect to the same @nn@::@vi@::@Layer@ through other platform
-- mechanisms.
--
-- If the native window is created with a specified size, @currentExtent@
-- will reflect that size. In this case, applications should use the same
-- size for the swapchain’s @imageExtent@. Otherwise, the @currentExtent@
-- will have the special value (0xFFFFFFFF, 0xFFFFFFFF), indicating that
-- applications are expected to choose an appropriate size for the
-- swapchain’s @imageExtent@ (e.g., by matching the result of a call to
-- @nn@::@vi@::@GetDisplayResolution@).
--
-- Unresolved directive in vkCreateViSurfaceNN.txt -
-- include::{generated}\/validity\/protos\/vkCreateViSurfaceNN.txt[]
--
-- = See Also
--
-- No cross-references are available
createViSurfaceNN :: Instance ->  ViSurfaceCreateInfoNN ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createViSurfaceNN = \(Instance instance' commandTable) -> \createInfo' -> \allocator -> alloca (\pSurface' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructViSurfaceCreateInfoNN marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateViSurfaceNN commandTable instance' pCreateInfo' pAllocator pSurface' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pSurface')))))
