{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  SurfaceCapabilities2EXT(..)
  , 
#endif
  SurfaceCounterFlagBitsEXT
  , pattern SURFACE_COUNTER_VBLANK_EXT
  , SurfaceCounterFlagsEXT
#if defined(VK_USE_PLATFORM_GGP)
  , getPhysicalDeviceSurfaceCapabilities2EXT
#endif
  , pattern EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
  , pattern EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION
  , pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT
  , pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif
import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Alloc
  ( alloca
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peek
  )
#endif


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter
  ( VkSurfaceCounterFlagBitsEXT(..)
  , pattern VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
  , pattern VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION
  , pattern VK_SURFACE_COUNTER_VBLANK_EXT
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter
  ( vkGetPhysicalDeviceSurfaceCapabilities2EXT
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  , ImageUsageFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( CompositeAlphaFlagsKHR
  , SurfaceKHR
  , SurfaceTransformFlagBitsKHR
  , SurfaceTransformFlagsKHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  , SomeVkStruct
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSurfaceCapabilities2EXT"
data SurfaceCapabilities2EXT = SurfaceCapabilities2EXT
  { -- No documentation found for Nested "SurfaceCapabilities2EXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "minImageCount"
  minImageCount :: Word32
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "maxImageCount"
  maxImageCount :: Word32
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "currentExtent"
  currentExtent :: Extent2D
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "minImageExtent"
  minImageExtent :: Extent2D
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "maxImageExtent"
  maxImageExtent :: Extent2D
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "maxImageArrayLayers"
  maxImageArrayLayers :: Word32
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "supportedTransforms"
  supportedTransforms :: SurfaceTransformFlagsKHR
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "currentTransform"
  currentTransform :: SurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "supportedCompositeAlpha"
  supportedCompositeAlpha :: CompositeAlphaFlagsKHR
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "supportedUsageFlags"
  supportedUsageFlags :: ImageUsageFlags
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "supportedSurfaceCounters"
  supportedSurfaceCounters :: SurfaceCounterFlagsEXT
  }
  deriving (Show, Eq)

instance Zero SurfaceCapabilities2EXT where
  zero = SurfaceCapabilities2EXT Nothing
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero

#endif

-- No documentation found for TopLevel "SurfaceCounterFlagBitsEXT"
type SurfaceCounterFlagBitsEXT = VkSurfaceCounterFlagBitsEXT


{-# complete SURFACE_COUNTER_VBLANK_EXT :: SurfaceCounterFlagBitsEXT #-}


-- No documentation found for Nested "SurfaceCounterFlagBitsEXT" "SURFACE_COUNTER_VBLANK_EXT"
pattern SURFACE_COUNTER_VBLANK_EXT :: (a ~ SurfaceCounterFlagBitsEXT) => a
pattern SURFACE_COUNTER_VBLANK_EXT = VK_SURFACE_COUNTER_VBLANK_EXT

-- No documentation found for TopLevel "SurfaceCounterFlagsEXT"
type SurfaceCounterFlagsEXT = SurfaceCounterFlagBitsEXT


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfaceCapabilities2EXT"
getPhysicalDeviceSurfaceCapabilities2EXT :: PhysicalDevice ->  SurfaceKHR ->  IO (SurfaceCapabilities2EXT)
getPhysicalDeviceSurfaceCapabilities2EXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif

-- No documentation found for TopLevel "VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME"
pattern EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME = VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION"
pattern EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION :: Integral a => a
pattern EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION = VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION

-- No documentation found for TopLevel "STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT"
pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT :: VkStructureType
pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT = STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT
