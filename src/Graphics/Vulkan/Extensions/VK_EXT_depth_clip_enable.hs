{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_depth_clip_enable
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceDepthClipEnableFeaturesEXT(..)
  , 
#endif
  PipelineRasterizationDepthClipStateCreateFlagsEXT
#if defined(VK_USE_PLATFORM_GGP)
  , PipelineRasterizationDepthClipStateCreateInfoEXT(..)
#endif
  , pattern EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME
  , pattern EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_depth_clip_enable
  ( VkPipelineRasterizationDepthClipStateCreateFlagsEXT(..)
  , pattern VK_EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME
  , pattern VK_EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceDepthClipEnableFeaturesEXT"
data PhysicalDeviceDepthClipEnableFeaturesEXT = PhysicalDeviceDepthClipEnableFeaturesEXT
  { -- No documentation found for Nested "PhysicalDeviceDepthClipEnableFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceDepthClipEnableFeaturesEXT" "depthClipEnable"
  depthClipEnable :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceDepthClipEnableFeaturesEXT where
  zero = PhysicalDeviceDepthClipEnableFeaturesEXT Nothing
                                                  False

#endif

-- No documentation found for TopLevel "PipelineRasterizationDepthClipStateCreateFlagsEXT"
type PipelineRasterizationDepthClipStateCreateFlagsEXT = VkPipelineRasterizationDepthClipStateCreateFlagsEXT


-- No complete pragma for PipelineRasterizationDepthClipStateCreateFlagsEXT as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineRasterizationDepthClipStateCreateInfoEXT"
data PipelineRasterizationDepthClipStateCreateInfoEXT = PipelineRasterizationDepthClipStateCreateInfoEXT
  { -- No documentation found for Nested "PipelineRasterizationDepthClipStateCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineRasterizationDepthClipStateCreateInfoEXT" "flags"
  flags :: PipelineRasterizationDepthClipStateCreateFlagsEXT
  , -- No documentation found for Nested "PipelineRasterizationDepthClipStateCreateInfoEXT" "depthClipEnable"
  depthClipEnable :: Bool
  }
  deriving (Show, Eq)

instance Zero PipelineRasterizationDepthClipStateCreateInfoEXT where
  zero = PipelineRasterizationDepthClipStateCreateInfoEXT Nothing
                                                          zero
                                                          False

#endif

-- No documentation found for TopLevel "VK_EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME"
pattern EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME = VK_EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION"
pattern EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION :: Integral a => a
pattern EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION = VK_EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION
