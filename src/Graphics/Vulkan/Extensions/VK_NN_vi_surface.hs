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
import qualified Graphics.Vulkan.C.Dynamic
  ( createViSurfaceNN
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_NN_vi_surface
  ( VkViSurfaceCreateFlagsNN(..)
  , VkViSurfaceCreateInfoNN(..)
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
-- No documentation found for TopLevel "ViSurfaceCreateInfoNN"
data ViSurfaceCreateInfoNN = ViSurfaceCreateInfoNN
  { -- Univalued Member elided
  -- No documentation found for Nested "ViSurfaceCreateInfoNN" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ViSurfaceCreateInfoNN" "flags"
  vkFlags :: ViSurfaceCreateFlagsNN
  , -- No documentation found for Nested "ViSurfaceCreateInfoNN" "window"
  vkWindow :: Ptr ()
  }
  deriving (Show, Eq)
withCStructViSurfaceCreateInfoNN :: ViSurfaceCreateInfoNN -> (VkViSurfaceCreateInfoNN -> IO a) -> IO a
withCStructViSurfaceCreateInfoNN from cont = maybeWith withSomeVkStruct (vkPNext (from :: ViSurfaceCreateInfoNN)) (\pPNext -> cont (VkViSurfaceCreateInfoNN VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN pPNext (vkFlags (from :: ViSurfaceCreateInfoNN)) (vkWindow (from :: ViSurfaceCreateInfoNN))))
fromCStructViSurfaceCreateInfoNN :: VkViSurfaceCreateInfoNN -> IO ViSurfaceCreateInfoNN
fromCStructViSurfaceCreateInfoNN c = ViSurfaceCreateInfoNN <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkViSurfaceCreateInfoNN)))
                                                           <*> pure (vkFlags (c :: VkViSurfaceCreateInfoNN))
                                                           <*> pure (vkWindow (c :: VkViSurfaceCreateInfoNN))
instance Zero ViSurfaceCreateInfoNN where
  zero = ViSurfaceCreateInfoNN Nothing
                               zero
                               zero

-- | Wrapper for 'vkCreateViSurfaceNN'
createViSurfaceNN :: Instance ->  ViSurfaceCreateInfoNN ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createViSurfaceNN = \(Instance instance' commandTable) -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructViSurfaceCreateInfoNN a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createViSurfaceNN commandTable instance' pCreateInfo pAllocator pSurface >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pSurface)))))
