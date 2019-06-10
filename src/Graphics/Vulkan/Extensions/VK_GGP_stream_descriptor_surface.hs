{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_GGP_stream_descriptor_surface
  ( StreamDescriptorSurfaceCreateFlagsGGP
#if defined(VK_USE_PLATFORM_GGP)
  , StreamDescriptorSurfaceCreateInfoGGP(..)
#endif
  , createStreamDescriptorSurfaceGGP
  , pattern GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME
  , pattern GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP
  ) where

import Data.String
  ( IsString
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Storable
  ( peek
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface
  ( VkStreamDescriptorSurfaceCreateFlagsGGP(..)
  , vkCreateStreamDescriptorSurfaceGGP
  , pattern VK_GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME
  , pattern VK_GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface
  ( GgpStreamDescriptor
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Instance(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( SurfaceKHR
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP
  )


-- No documentation found for TopLevel "StreamDescriptorSurfaceCreateFlagsGGP"
type StreamDescriptorSurfaceCreateFlagsGGP = VkStreamDescriptorSurfaceCreateFlagsGGP


-- No complete pragma for StreamDescriptorSurfaceCreateFlagsGGP as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkStreamDescriptorSurfaceCreateInfoGGP"
data StreamDescriptorSurfaceCreateInfoGGP = StreamDescriptorSurfaceCreateInfoGGP
  { -- No documentation found for Nested "StreamDescriptorSurfaceCreateInfoGGP" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "StreamDescriptorSurfaceCreateInfoGGP" "flags"
  flags :: StreamDescriptorSurfaceCreateFlagsGGP
  , -- No documentation found for Nested "StreamDescriptorSurfaceCreateInfoGGP" "streamDescriptor"
  streamDescriptor :: GgpStreamDescriptor
  }
  deriving (Show, Eq)

instance Zero StreamDescriptorSurfaceCreateInfoGGP where
  zero = StreamDescriptorSurfaceCreateInfoGGP Nothing
                                              zero
                                              zero

#endif


-- No documentation found for TopLevel "vkCreateStreamDescriptorSurfaceGGP"
createStreamDescriptorSurfaceGGP :: Instance ->  StreamDescriptorSurfaceCreateInfoGGP ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createStreamDescriptorSurfaceGGP = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME"
pattern GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME = VK_GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION"
pattern GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION :: Integral a => a
pattern GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION = VK_GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION
