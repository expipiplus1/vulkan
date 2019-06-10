{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_conservative_rasterization
  ( ConservativeRasterizationModeEXT
  , pattern CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT
  , pattern CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT
  , pattern CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT
#if defined(VK_USE_PLATFORM_GGP)
  , PhysicalDeviceConservativeRasterizationPropertiesEXT(..)
#endif
  , PipelineRasterizationConservativeStateCreateFlagsEXT
#if defined(VK_USE_PLATFORM_GGP)
  , PipelineRasterizationConservativeStateCreateInfoEXT(..)
#endif
  , pattern EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
  , pattern EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization
  ( VkConservativeRasterizationModeEXT(..)
  , VkPipelineRasterizationConservativeStateCreateFlagsEXT(..)
  , pattern VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT
  , pattern VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT
  , pattern VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT
  , pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
  , pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
  )


-- No documentation found for TopLevel "ConservativeRasterizationModeEXT"
type ConservativeRasterizationModeEXT = VkConservativeRasterizationModeEXT


{-# complete CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT, CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT, CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT :: ConservativeRasterizationModeEXT #-}


-- No documentation found for Nested "ConservativeRasterizationModeEXT" "CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT"
pattern CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT :: (a ~ ConservativeRasterizationModeEXT) => a
pattern CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT = VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT


-- No documentation found for Nested "ConservativeRasterizationModeEXT" "CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT"
pattern CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT :: (a ~ ConservativeRasterizationModeEXT) => a
pattern CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT = VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT


-- No documentation found for Nested "ConservativeRasterizationModeEXT" "CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT"
pattern CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT :: (a ~ ConservativeRasterizationModeEXT) => a
pattern CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT = VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceConservativeRasterizationPropertiesEXT"
data PhysicalDeviceConservativeRasterizationPropertiesEXT = PhysicalDeviceConservativeRasterizationPropertiesEXT
  { -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "primitiveOverestimationSize"
  primitiveOverestimationSize :: Float
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "maxExtraPrimitiveOverestimationSize"
  maxExtraPrimitiveOverestimationSize :: Float
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "extraPrimitiveOverestimationSizeGranularity"
  extraPrimitiveOverestimationSizeGranularity :: Float
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "primitiveUnderestimation"
  primitiveUnderestimation :: Bool
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "conservativePointAndLineRasterization"
  conservativePointAndLineRasterization :: Bool
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "degenerateTrianglesRasterized"
  degenerateTrianglesRasterized :: Bool
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "degenerateLinesRasterized"
  degenerateLinesRasterized :: Bool
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "fullyCoveredFragmentShaderInputVariable"
  fullyCoveredFragmentShaderInputVariable :: Bool
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "conservativeRasterizationPostDepthCoverage"
  conservativeRasterizationPostDepthCoverage :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceConservativeRasterizationPropertiesEXT where
  zero = PhysicalDeviceConservativeRasterizationPropertiesEXT Nothing
                                                              zero
                                                              zero
                                                              zero
                                                              False
                                                              False
                                                              False
                                                              False
                                                              False
                                                              False

#endif

-- No documentation found for TopLevel "PipelineRasterizationConservativeStateCreateFlagsEXT"
type PipelineRasterizationConservativeStateCreateFlagsEXT = VkPipelineRasterizationConservativeStateCreateFlagsEXT


-- No complete pragma for PipelineRasterizationConservativeStateCreateFlagsEXT as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineRasterizationConservativeStateCreateInfoEXT"
data PipelineRasterizationConservativeStateCreateInfoEXT = PipelineRasterizationConservativeStateCreateInfoEXT
  { -- No documentation found for Nested "PipelineRasterizationConservativeStateCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineRasterizationConservativeStateCreateInfoEXT" "flags"
  flags :: PipelineRasterizationConservativeStateCreateFlagsEXT
  , -- No documentation found for Nested "PipelineRasterizationConservativeStateCreateInfoEXT" "conservativeRasterizationMode"
  conservativeRasterizationMode :: ConservativeRasterizationModeEXT
  , -- No documentation found for Nested "PipelineRasterizationConservativeStateCreateInfoEXT" "extraPrimitiveOverestimationSize"
  extraPrimitiveOverestimationSize :: Float
  }
  deriving (Show, Eq)

instance Zero PipelineRasterizationConservativeStateCreateInfoEXT where
  zero = PipelineRasterizationConservativeStateCreateInfoEXT Nothing
                                                             zero
                                                             zero
                                                             zero

#endif

-- No documentation found for TopLevel "VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME"
pattern EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME = VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION"
pattern EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION :: Integral a => a
pattern EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION = VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION
