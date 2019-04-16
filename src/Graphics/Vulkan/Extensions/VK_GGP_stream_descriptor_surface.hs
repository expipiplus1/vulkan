{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_GGP_stream_descriptor_surface
  ( StreamDescriptorSurfaceCreateFlagsGGP
  , withCStructStreamDescriptorSurfaceCreateInfoGGP
  , fromCStructStreamDescriptorSurfaceCreateInfoGGP
  , StreamDescriptorSurfaceCreateInfoGGP(..)
  , createStreamDescriptorSurfaceGGP
  , pattern VK_GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION
  , pattern VK_GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP
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
  ( castPtr
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createStreamDescriptorSurfaceGGP
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface
  ( VkStreamDescriptorSurfaceCreateFlagsGGP(..)
  , VkStreamDescriptorSurfaceCreateInfoGGP(..)
  , GgpStreamDescriptor
  , pattern VK_STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP
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
import Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface
  ( pattern VK_GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME
  , pattern VK_GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION
  )


-- No documentation found for TopLevel "StreamDescriptorSurfaceCreateFlagsGGP"
type StreamDescriptorSurfaceCreateFlagsGGP = VkStreamDescriptorSurfaceCreateFlagsGGP
-- No documentation found for TopLevel "StreamDescriptorSurfaceCreateInfoGGP"
data StreamDescriptorSurfaceCreateInfoGGP = StreamDescriptorSurfaceCreateInfoGGP
  { -- Univalued Member elided
  -- No documentation found for Nested "StreamDescriptorSurfaceCreateInfoGGP" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "StreamDescriptorSurfaceCreateInfoGGP" "flags"
  vkFlags :: StreamDescriptorSurfaceCreateFlagsGGP
  , -- No documentation found for Nested "StreamDescriptorSurfaceCreateInfoGGP" "streamDescriptor"
  vkStreamDescriptor :: GgpStreamDescriptor
  }
  deriving (Show, Eq)
withCStructStreamDescriptorSurfaceCreateInfoGGP :: StreamDescriptorSurfaceCreateInfoGGP -> (VkStreamDescriptorSurfaceCreateInfoGGP -> IO a) -> IO a
withCStructStreamDescriptorSurfaceCreateInfoGGP from cont = maybeWith withSomeVkStruct (vkPNext (from :: StreamDescriptorSurfaceCreateInfoGGP)) (\pPNext -> cont (VkStreamDescriptorSurfaceCreateInfoGGP VK_STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP pPNext (vkFlags (from :: StreamDescriptorSurfaceCreateInfoGGP)) (vkStreamDescriptor (from :: StreamDescriptorSurfaceCreateInfoGGP))))
fromCStructStreamDescriptorSurfaceCreateInfoGGP :: VkStreamDescriptorSurfaceCreateInfoGGP -> IO StreamDescriptorSurfaceCreateInfoGGP
fromCStructStreamDescriptorSurfaceCreateInfoGGP c = StreamDescriptorSurfaceCreateInfoGGP <$> -- Univalued Member elided
                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkStreamDescriptorSurfaceCreateInfoGGP)))
                                                                                         <*> pure (vkFlags (c :: VkStreamDescriptorSurfaceCreateInfoGGP))
                                                                                         <*> pure (vkStreamDescriptor (c :: VkStreamDescriptorSurfaceCreateInfoGGP))

-- | Wrapper for 'vkCreateStreamDescriptorSurfaceGGP'
createStreamDescriptorSurfaceGGP :: Instance ->  StreamDescriptorSurfaceCreateInfoGGP ->  Maybe AllocationCallbacks ->  IO ( SurfaceKHR )
createStreamDescriptorSurfaceGGP = \(Instance instance' commandTable) -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructStreamDescriptorSurfaceCreateInfoGGP a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createStreamDescriptorSurfaceGGP commandTable instance' pCreateInfo pAllocator pSurface >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pSurface)))))
