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
  , withCStructPhysicalDeviceConservativeRasterizationPropertiesEXT
  , fromCStructPhysicalDeviceConservativeRasterizationPropertiesEXT
  , PhysicalDeviceConservativeRasterizationPropertiesEXT(..)
  , PipelineRasterizationConservativeStateCreateFlagsEXT
  , withCStructPipelineRasterizationConservativeStateCreateInfoEXT
  , fromCStructPipelineRasterizationConservativeStateCreateInfoEXT
  , PipelineRasterizationConservativeStateCreateInfoEXT(..)
  , pattern EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
  , pattern EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
  ) where

import Data.String
  ( IsString
  )
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
  , pattern VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT
  , pattern VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT
  , pattern VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT
  , pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
  , pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
  )


-- | VkConservativeRasterizationModeEXT - Specify the conservative
-- rasterization mode
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization.VkPipelineRasterizationConservativeStateCreateInfoEXT'
type ConservativeRasterizationModeEXT = VkConservativeRasterizationModeEXT


{-# complete CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT, CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT, CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT :: ConservativeRasterizationModeEXT #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization.VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT'
-- specifies that conservative rasterization is disabled and rasterization
-- proceeds as normal.
pattern CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT :: (a ~ ConservativeRasterizationModeEXT) => a
pattern CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT = VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization.VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT'
-- specifies that conservative rasterization is enabled in overestimation
-- mode.
pattern CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT :: (a ~ ConservativeRasterizationModeEXT) => a
pattern CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT = VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization.VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT'
-- specifies that conservative rasterization is enabled in underestimation
-- mode.
pattern CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT :: (a ~ ConservativeRasterizationModeEXT) => a
pattern CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT = VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT


-- | VkPhysicalDeviceConservativeRasterizationPropertiesEXT - Structure
-- describing conservative raster properties that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization.VkPhysicalDeviceConservativeRasterizationPropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization.VkPhysicalDeviceConservativeRasterizationPropertiesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits and properties.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceConservativeRasterizationPropertiesEXT = PhysicalDeviceConservativeRasterizationPropertiesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "primitiveOverestimationSize"
  primitiveOverestimationSize :: CFloat
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "maxExtraPrimitiveOverestimationSize"
  maxExtraPrimitiveOverestimationSize :: CFloat
  , -- No documentation found for Nested "PhysicalDeviceConservativeRasterizationPropertiesEXT" "extraPrimitiveOverestimationSizeGranularity"
  extraPrimitiveOverestimationSizeGranularity :: CFloat
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

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceConservativeRasterizationPropertiesEXT' and
-- marshal a 'PhysicalDeviceConservativeRasterizationPropertiesEXT' into it. The 'VkPhysicalDeviceConservativeRasterizationPropertiesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceConservativeRasterizationPropertiesEXT :: PhysicalDeviceConservativeRasterizationPropertiesEXT -> (VkPhysicalDeviceConservativeRasterizationPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceConservativeRasterizationPropertiesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceConservativeRasterizationPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceConservativeRasterizationPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT pPNext (primitiveOverestimationSize (marshalled :: PhysicalDeviceConservativeRasterizationPropertiesEXT)) (maxExtraPrimitiveOverestimationSize (marshalled :: PhysicalDeviceConservativeRasterizationPropertiesEXT)) (extraPrimitiveOverestimationSizeGranularity (marshalled :: PhysicalDeviceConservativeRasterizationPropertiesEXT)) (boolToBool32 (primitiveUnderestimation (marshalled :: PhysicalDeviceConservativeRasterizationPropertiesEXT))) (boolToBool32 (conservativePointAndLineRasterization (marshalled :: PhysicalDeviceConservativeRasterizationPropertiesEXT))) (boolToBool32 (degenerateTrianglesRasterized (marshalled :: PhysicalDeviceConservativeRasterizationPropertiesEXT))) (boolToBool32 (degenerateLinesRasterized (marshalled :: PhysicalDeviceConservativeRasterizationPropertiesEXT))) (boolToBool32 (fullyCoveredFragmentShaderInputVariable (marshalled :: PhysicalDeviceConservativeRasterizationPropertiesEXT))) (boolToBool32 (conservativeRasterizationPostDepthCoverage (marshalled :: PhysicalDeviceConservativeRasterizationPropertiesEXT)))))

-- | A function to read a 'VkPhysicalDeviceConservativeRasterizationPropertiesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceConservativeRasterizationPropertiesEXT'.
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


-- | VkPipelineRasterizationConservativeStateCreateFlagsEXT - Reserved for
-- future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization.VkPipelineRasterizationConservativeStateCreateFlagsEXT'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization.VkPipelineRasterizationConservativeStateCreateInfoEXT'
type PipelineRasterizationConservativeStateCreateFlagsEXT = VkPipelineRasterizationConservativeStateCreateFlagsEXT


-- No complete pragma for PipelineRasterizationConservativeStateCreateFlagsEXT as it has no patterns


-- | VkPipelineRasterizationConservativeStateCreateInfoEXT - Structure
-- specifying conservative raster state
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization.VkConservativeRasterizationModeEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization.VkPipelineRasterizationConservativeStateCreateFlagsEXT',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PipelineRasterizationConservativeStateCreateInfoEXT = PipelineRasterizationConservativeStateCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineRasterizationConservativeStateCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineRasterizationConservativeStateCreateInfoEXT" "flags"
  flags :: PipelineRasterizationConservativeStateCreateFlagsEXT
  , -- No documentation found for Nested "PipelineRasterizationConservativeStateCreateInfoEXT" "conservativeRasterizationMode"
  conservativeRasterizationMode :: ConservativeRasterizationModeEXT
  , -- No documentation found for Nested "PipelineRasterizationConservativeStateCreateInfoEXT" "extraPrimitiveOverestimationSize"
  extraPrimitiveOverestimationSize :: CFloat
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineRasterizationConservativeStateCreateInfoEXT' and
-- marshal a 'PipelineRasterizationConservativeStateCreateInfoEXT' into it. The 'VkPipelineRasterizationConservativeStateCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineRasterizationConservativeStateCreateInfoEXT :: PipelineRasterizationConservativeStateCreateInfoEXT -> (VkPipelineRasterizationConservativeStateCreateInfoEXT -> IO a) -> IO a
withCStructPipelineRasterizationConservativeStateCreateInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PipelineRasterizationConservativeStateCreateInfoEXT)) (\pPNext -> cont (VkPipelineRasterizationConservativeStateCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT pPNext (flags (marshalled :: PipelineRasterizationConservativeStateCreateInfoEXT)) (conservativeRasterizationMode (marshalled :: PipelineRasterizationConservativeStateCreateInfoEXT)) (extraPrimitiveOverestimationSize (marshalled :: PipelineRasterizationConservativeStateCreateInfoEXT))))

-- | A function to read a 'VkPipelineRasterizationConservativeStateCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'PipelineRasterizationConservativeStateCreateInfoEXT'.
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


-- No documentation found for TopLevel "VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME"
pattern EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME = VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION"
pattern EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION :: Integral a => a
pattern EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION = VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION
