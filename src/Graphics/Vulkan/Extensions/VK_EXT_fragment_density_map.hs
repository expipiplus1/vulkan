{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_fragment_density_map
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceFragmentDensityMapFeaturesEXT(..)
  , 
  PhysicalDeviceFragmentDensityMapPropertiesEXT(..)
  , RenderPassFragmentDensityMapCreateInfoEXT(..)
#endif
  , pattern EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME
  , pattern EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT
  , pattern IMAGE_CREATE_SUBSAMPLED_BIT_EXT
  , pattern IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT
  , pattern ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT
  , pattern FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT
  , pattern IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT
  , pattern IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT
  , pattern PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT
  , pattern SAMPLER_CREATE_SUBSAMPLED_BIT_EXT
  , pattern SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map
  ( pattern VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME
  , pattern VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pass
  ( AttachmentReference(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( pattern FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT
  , pattern IMAGE_CREATE_SUBSAMPLED_BIT_EXT
  , pattern IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT
  )
import Graphics.Vulkan.Core10.Image
  ( pattern IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT
  )
import Graphics.Vulkan.Core10.ImageView
  ( pattern IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT
  )
import Graphics.Vulkan.Core10.Pass
  ( pattern ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT
  )
import Graphics.Vulkan.Core10.Queue
  ( pattern PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT
  )
import Graphics.Vulkan.Core10.Sampler
  ( pattern SAMPLER_CREATE_SUBSAMPLED_BIT_EXT
  , pattern SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceFragmentDensityMapFeaturesEXT"
data PhysicalDeviceFragmentDensityMapFeaturesEXT = PhysicalDeviceFragmentDensityMapFeaturesEXT
  { -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapFeaturesEXT" "fragmentDensityMap"
  fragmentDensityMap :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapFeaturesEXT" "fragmentDensityMapDynamic"
  fragmentDensityMapDynamic :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapFeaturesEXT" "fragmentDensityMapNonSubsampledImages"
  fragmentDensityMapNonSubsampledImages :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceFragmentDensityMapFeaturesEXT where
  zero = PhysicalDeviceFragmentDensityMapFeaturesEXT Nothing
                                                     False
                                                     False
                                                     False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceFragmentDensityMapPropertiesEXT"
data PhysicalDeviceFragmentDensityMapPropertiesEXT = PhysicalDeviceFragmentDensityMapPropertiesEXT
  { -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapPropertiesEXT" "minFragmentDensityTexelSize"
  minFragmentDensityTexelSize :: Extent2D
  , -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapPropertiesEXT" "maxFragmentDensityTexelSize"
  maxFragmentDensityTexelSize :: Extent2D
  , -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapPropertiesEXT" "fragmentDensityInvocations"
  fragmentDensityInvocations :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceFragmentDensityMapPropertiesEXT where
  zero = PhysicalDeviceFragmentDensityMapPropertiesEXT Nothing
                                                       zero
                                                       zero
                                                       False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkRenderPassFragmentDensityMapCreateInfoEXT"
data RenderPassFragmentDensityMapCreateInfoEXT = RenderPassFragmentDensityMapCreateInfoEXT
  { -- No documentation found for Nested "RenderPassFragmentDensityMapCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "RenderPassFragmentDensityMapCreateInfoEXT" "fragmentDensityMapAttachment"
  fragmentDensityMapAttachment :: AttachmentReference
  }
  deriving (Show, Eq)

instance Zero RenderPassFragmentDensityMapCreateInfoEXT where
  zero = RenderPassFragmentDensityMapCreateInfoEXT Nothing
                                                   zero

#endif

-- No documentation found for TopLevel "VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME"
pattern EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME = VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION"
pattern EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION :: Integral a => a
pattern EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION = VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION
