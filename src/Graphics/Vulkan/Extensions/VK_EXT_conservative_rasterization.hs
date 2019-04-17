{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_conservative_rasterization
  ( ConservativeRasterizationModeEXT
  , withCStructPhysicalDeviceConservativeRasterizationPropertiesEXT
  , fromCStructPhysicalDeviceConservativeRasterizationPropertiesEXT
  , PhysicalDeviceConservativeRasterizationPropertiesEXT(..)
  , PipelineRasterizationConservativeStateCreateFlagsEXT
  , withCStructPipelineRasterizationConservativeStateCreateInfoEXT
  , fromCStructPipelineRasterizationConservativeStateCreateInfoEXT
  , PipelineRasterizationConservativeStateCreateInfoEXT(..)
  , pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION
  , pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
  ) where

import Foreign.C.Types
  ( CFloat(..)
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization
  ( VkConservativeRasterizationModeEXT(..)
  , VkPhysicalDeviceConservativeRasterizationPropertiesEXT(..)
  , VkPipelineRasterizationConservativeStateCreateFlagsEXT(..)
  , VkPipelineRasterizationConservativeStateCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization
  ( pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
  , pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION
  )


-- No documentation found for TopLevel "ConservativeRasterizationModeEXT"
type ConservativeRasterizationModeEXT = VkConservativeRasterizationModeEXT
-- No documentation found for TopLevel "PhysicalDeviceConservativeRasterizationPropertiesEXT"
data PhysicalDeviceConservativeRasterizationPropertiesEXT = PhysicalDeviceConservativeRasterizationPropertiesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "primitiveOverestimationSize"
  vkPrimitiveOverestimationSize :: CFloat
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "maxExtraPrimitiveOverestimationSize"
  vkMaxExtraPrimitiveOverestimationSize :: CFloat
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "extraPrimitiveOverestimationSizeGranularity"
  vkExtraPrimitiveOverestimationSizeGranularity :: CFloat
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "primitiveUnderestimation"
  vkPrimitiveUnderestimation :: Bool
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "conservativePointAndLineRasterization"
  vkConservativePointAndLineRasterization :: Bool
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "degenerateTrianglesRasterized"
  vkDegenerateTrianglesRasterized :: Bool
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "degenerateLinesRasterized"
  vkDegenerateLinesRasterized :: Bool
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "fullyCoveredFragmentShaderInputVariable"
  vkFullyCoveredFragmentShaderInputVariable :: Bool
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "conservativeRasterizationPostDepthCoverage"
  vkConservativeRasterizationPostDepthCoverage :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceConservativeRasterizationPropertiesEXT :: PhysicalDeviceConservativeRasterizationPropertiesEXT -> (VkPhysicalDeviceConservativeRasterizationPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceConservativeRasterizationPropertiesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceConservativeRasterizationPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceConservativeRasterizationPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT pPNext (vkPrimitiveOverestimationSize (from :: PhysicalDeviceConservativeRasterizationPropertiesEXT)) (vkMaxExtraPrimitiveOverestimationSize (from :: PhysicalDeviceConservativeRasterizationPropertiesEXT)) (vkExtraPrimitiveOverestimationSizeGranularity (from :: PhysicalDeviceConservativeRasterizationPropertiesEXT)) (boolToBool32 (vkPrimitiveUnderestimation (from :: PhysicalDeviceConservativeRasterizationPropertiesEXT))) (boolToBool32 (vkConservativePointAndLineRasterization (from :: PhysicalDeviceConservativeRasterizationPropertiesEXT))) (boolToBool32 (vkDegenerateTrianglesRasterized (from :: PhysicalDeviceConservativeRasterizationPropertiesEXT))) (boolToBool32 (vkDegenerateLinesRasterized (from :: PhysicalDeviceConservativeRasterizationPropertiesEXT))) (boolToBool32 (vkFullyCoveredFragmentShaderInputVariable (from :: PhysicalDeviceConservativeRasterizationPropertiesEXT))) (boolToBool32 (vkConservativeRasterizationPostDepthCoverage (from :: PhysicalDeviceConservativeRasterizationPropertiesEXT)))))
fromCStructPhysicalDeviceConservativeRasterizationPropertiesEXT :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT -> IO PhysicalDeviceConservativeRasterizationPropertiesEXT
fromCStructPhysicalDeviceConservativeRasterizationPropertiesEXT c = PhysicalDeviceConservativeRasterizationPropertiesEXT <$> -- Univalued Member elided
                                                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT)))
                                                                                                                         <*> pure (vkPrimitiveOverestimationSize (c :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                                                                                                                         <*> pure (vkMaxExtraPrimitiveOverestimationSize (c :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                                                                                                                         <*> pure (vkExtraPrimitiveOverestimationSizeGranularity (c :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                                                                                                                         <*> pure (bool32ToBool (vkPrimitiveUnderestimation (c :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT)))
                                                                                                                         <*> pure (bool32ToBool (vkConservativePointAndLineRasterization (c :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT)))
                                                                                                                         <*> pure (bool32ToBool (vkDegenerateTrianglesRasterized (c :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT)))
                                                                                                                         <*> pure (bool32ToBool (vkDegenerateLinesRasterized (c :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT)))
                                                                                                                         <*> pure (bool32ToBool (vkFullyCoveredFragmentShaderInputVariable (c :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT)))
                                                                                                                         <*> pure (bool32ToBool (vkConservativeRasterizationPostDepthCoverage (c :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT)))
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
-- No documentation found for TopLevel "PipelineRasterizationConservativeStateCreateFlagsEXT"
type PipelineRasterizationConservativeStateCreateFlagsEXT = VkPipelineRasterizationConservativeStateCreateFlagsEXT
-- No documentation found for TopLevel "PipelineRasterizationConservativeStateCreateInfoEXT"
data PipelineRasterizationConservativeStateCreateInfoEXT = PipelineRasterizationConservativeStateCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineRasterizationConservativeStateCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineRasterizationConservativeStateCreateInfoEXT" "flags"
  vkFlags :: PipelineRasterizationConservativeStateCreateFlagsEXT
  , -- No documentation found for Nested "PipelineRasterizationConservativeStateCreateInfoEXT" "conservativeRasterizationMode"
  vkConservativeRasterizationMode :: ConservativeRasterizationModeEXT
  , -- No documentation found for Nested "PipelineRasterizationConservativeStateCreateInfoEXT" "extraPrimitiveOverestimationSize"
  vkExtraPrimitiveOverestimationSize :: CFloat
  }
  deriving (Show, Eq)
withCStructPipelineRasterizationConservativeStateCreateInfoEXT :: PipelineRasterizationConservativeStateCreateInfoEXT -> (VkPipelineRasterizationConservativeStateCreateInfoEXT -> IO a) -> IO a
withCStructPipelineRasterizationConservativeStateCreateInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PipelineRasterizationConservativeStateCreateInfoEXT)) (\pPNext -> cont (VkPipelineRasterizationConservativeStateCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT pPNext (vkFlags (from :: PipelineRasterizationConservativeStateCreateInfoEXT)) (vkConservativeRasterizationMode (from :: PipelineRasterizationConservativeStateCreateInfoEXT)) (vkExtraPrimitiveOverestimationSize (from :: PipelineRasterizationConservativeStateCreateInfoEXT))))
fromCStructPipelineRasterizationConservativeStateCreateInfoEXT :: VkPipelineRasterizationConservativeStateCreateInfoEXT -> IO PipelineRasterizationConservativeStateCreateInfoEXT
fromCStructPipelineRasterizationConservativeStateCreateInfoEXT c = PipelineRasterizationConservativeStateCreateInfoEXT <$> -- Univalued Member elided
                                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineRasterizationConservativeStateCreateInfoEXT)))
                                                                                                                       <*> pure (vkFlags (c :: VkPipelineRasterizationConservativeStateCreateInfoEXT))
                                                                                                                       <*> pure (vkConservativeRasterizationMode (c :: VkPipelineRasterizationConservativeStateCreateInfoEXT))
                                                                                                                       <*> pure (vkExtraPrimitiveOverestimationSize (c :: VkPipelineRasterizationConservativeStateCreateInfoEXT))
instance Zero PipelineRasterizationConservativeStateCreateInfoEXT where
  zero = PipelineRasterizationConservativeStateCreateInfoEXT Nothing
                                                             zero
                                                             zero
                                                             zero
