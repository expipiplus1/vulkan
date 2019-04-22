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
  , pattern GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME
  , pattern GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.String
  ( IsString
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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface
  ( VkStreamDescriptorSurfaceCreateFlagsGGP(..)
  , VkStreamDescriptorSurfaceCreateInfoGGP(..)
  , GgpStreamDescriptor
  , vkCreateStreamDescriptorSurfaceGGP
  , pattern VK_GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME
  , pattern VK_GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP
  )


-- No documentation found for TopLevel "StreamDescriptorSurfaceCreateFlagsGGP"
type StreamDescriptorSurfaceCreateFlagsGGP = VkStreamDescriptorSurfaceCreateFlagsGGP


-- No complete pragma for StreamDescriptorSurfaceCreateFlagsGGP as it has no patterns


-- | VkStreamDescriptorSurfaceCreateInfoGGP - Structure specifying parameters
-- of a newly created Google Games Platform stream surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface.VkStreamDescriptorSurfaceCreateFlagsGGP',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface.vkCreateStreamDescriptorSurfaceGGP'
data StreamDescriptorSurfaceCreateInfoGGP = StreamDescriptorSurfaceCreateInfoGGP
  { -- Univalued member elided
  -- No documentation found for Nested "StreamDescriptorSurfaceCreateInfoGGP" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "StreamDescriptorSurfaceCreateInfoGGP" "flags"
  flags :: StreamDescriptorSurfaceCreateFlagsGGP
  , -- No documentation found for Nested "StreamDescriptorSurfaceCreateInfoGGP" "streamDescriptor"
  streamDescriptor :: GgpStreamDescriptor
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkStreamDescriptorSurfaceCreateInfoGGP' and
-- marshal a 'StreamDescriptorSurfaceCreateInfoGGP' into it. The 'VkStreamDescriptorSurfaceCreateInfoGGP' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructStreamDescriptorSurfaceCreateInfoGGP :: StreamDescriptorSurfaceCreateInfoGGP -> (VkStreamDescriptorSurfaceCreateInfoGGP -> IO a) -> IO a
withCStructStreamDescriptorSurfaceCreateInfoGGP marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: StreamDescriptorSurfaceCreateInfoGGP)) (\pPNext -> cont (VkStreamDescriptorSurfaceCreateInfoGGP VK_STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP pPNext (flags (marshalled :: StreamDescriptorSurfaceCreateInfoGGP)) (streamDescriptor (marshalled :: StreamDescriptorSurfaceCreateInfoGGP))))

-- | A function to read a 'VkStreamDescriptorSurfaceCreateInfoGGP' and all additional
-- structures in the pointer chain into a 'StreamDescriptorSurfaceCreateInfoGGP'.
fromCStructStreamDescriptorSurfaceCreateInfoGGP :: VkStreamDescriptorSurfaceCreateInfoGGP -> IO StreamDescriptorSurfaceCreateInfoGGP
fromCStructStreamDescriptorSurfaceCreateInfoGGP c = StreamDescriptorSurfaceCreateInfoGGP <$> -- Univalued Member elided
                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkStreamDescriptorSurfaceCreateInfoGGP)))
                                                                                         <*> pure (vkFlags (c :: VkStreamDescriptorSurfaceCreateInfoGGP))
                                                                                         <*> pure (vkStreamDescriptor (c :: VkStreamDescriptorSurfaceCreateInfoGGP))

instance Zero StreamDescriptorSurfaceCreateInfoGGP where
  zero = StreamDescriptorSurfaceCreateInfoGGP Nothing
                                              zero
                                              zero



-- | vkCreateStreamDescriptorSurfaceGGP - Create a
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' object for a
-- Google Games Platform stream
--
-- = Parameters
--
-- -   @instance@ is the instance to associate with the surface.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface.VkStreamDescriptorSurfaceCreateInfoGGP'
--     structure containing parameters that affect the creation of the
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
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface.VkStreamDescriptorSurfaceCreateInfoGGP'
--     structure
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
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance',
-- 'Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface.VkStreamDescriptorSurfaceCreateInfoGGP',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR'
createStreamDescriptorSurfaceGGP :: Instance ->  StreamDescriptorSurfaceCreateInfoGGP ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createStreamDescriptorSurfaceGGP = \(Instance instance' commandTable) -> \createInfo' -> \allocator -> alloca (\pSurface' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructStreamDescriptorSurfaceCreateInfoGGP marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateStreamDescriptorSurfaceGGP commandTable instance' pCreateInfo' pAllocator pSurface' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pSurface')))))

-- No documentation found for TopLevel "VK_GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME"
pattern GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME = VK_GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION"
pattern GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION :: Integral a => a
pattern GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION = VK_GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION
