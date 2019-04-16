{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_MVK_macos_surface
  ( MacOSSurfaceCreateFlagsMVK
  , withCStructMacOSSurfaceCreateInfoMVK
  , fromCStructMacOSSurfaceCreateInfoMVK
  , MacOSSurfaceCreateInfoMVK(..)
  , createMacOSSurfaceMVK
  , pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION
  , pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK
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
import qualified Graphics.Vulkan.C.Dynamic
  ( createMacOSSurfaceMVK
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface
  ( VkMacOSSurfaceCreateFlagsMVK(..)
  , VkMacOSSurfaceCreateInfoMVK(..)
  , pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK
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
import Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface
  ( pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME
  , pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION
  )


-- No documentation found for TopLevel "MacOSSurfaceCreateFlagsMVK"
type MacOSSurfaceCreateFlagsMVK = VkMacOSSurfaceCreateFlagsMVK
-- No documentation found for TopLevel "MacOSSurfaceCreateInfoMVK"
data MacOSSurfaceCreateInfoMVK = MacOSSurfaceCreateInfoMVK
  { -- Univalued Member elided
  -- No documentation found for Nested "MacOSSurfaceCreateInfoMVK" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MacOSSurfaceCreateInfoMVK" "flags"
  vkFlags :: MacOSSurfaceCreateFlagsMVK
  , -- No documentation found for Nested "MacOSSurfaceCreateInfoMVK" "pView"
  vkPView :: Ptr ()
  }
  deriving (Show, Eq)
withCStructMacOSSurfaceCreateInfoMVK :: MacOSSurfaceCreateInfoMVK -> (VkMacOSSurfaceCreateInfoMVK -> IO a) -> IO a
withCStructMacOSSurfaceCreateInfoMVK from cont = maybeWith withSomeVkStruct (vkPNext (from :: MacOSSurfaceCreateInfoMVK)) (\pPNext -> cont (VkMacOSSurfaceCreateInfoMVK VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK pPNext (vkFlags (from :: MacOSSurfaceCreateInfoMVK)) (vkPView (from :: MacOSSurfaceCreateInfoMVK))))
fromCStructMacOSSurfaceCreateInfoMVK :: VkMacOSSurfaceCreateInfoMVK -> IO MacOSSurfaceCreateInfoMVK
fromCStructMacOSSurfaceCreateInfoMVK c = MacOSSurfaceCreateInfoMVK <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMacOSSurfaceCreateInfoMVK)))
                                                                   <*> pure (vkFlags (c :: VkMacOSSurfaceCreateInfoMVK))
                                                                   <*> pure (vkPView (c :: VkMacOSSurfaceCreateInfoMVK))

-- | Wrapper for 'vkCreateMacOSSurfaceMVK'
createMacOSSurfaceMVK :: Instance ->  MacOSSurfaceCreateInfoMVK ->  Maybe AllocationCallbacks ->  IO ( SurfaceKHR )
createMacOSSurfaceMVK = \(Instance instance' commandTable) -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructMacOSSurfaceCreateInfoMVK a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createMacOSSurfaceMVK commandTable instance' pCreateInfo pAllocator pSurface >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pSurface)))))
