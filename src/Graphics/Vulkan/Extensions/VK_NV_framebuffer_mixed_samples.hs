{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_framebuffer_mixed_samples
  ( CoverageModulationModeNV
  , pattern COVERAGE_MODULATION_MODE_NONE_NV
  , pattern COVERAGE_MODULATION_MODE_RGB_NV
  , pattern COVERAGE_MODULATION_MODE_ALPHA_NV
  , pattern COVERAGE_MODULATION_MODE_RGBA_NV
  , PipelineCoverageModulationStateCreateFlagsNV
#if defined(VK_USE_PLATFORM_GGP)
  , PipelineCoverageModulationStateCreateInfoNV(..)
#endif
  , pattern NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME
  , pattern NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV
  ) where

import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_NV_framebuffer_mixed_samples
  ( VkCoverageModulationModeNV(..)
  , VkPipelineCoverageModulationStateCreateFlagsNV(..)
  , pattern VK_COVERAGE_MODULATION_MODE_ALPHA_NV
  , pattern VK_COVERAGE_MODULATION_MODE_NONE_NV
  , pattern VK_COVERAGE_MODULATION_MODE_RGBA_NV
  , pattern VK_COVERAGE_MODULATION_MODE_RGB_NV
  , pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME
  , pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV
  )


-- No documentation found for TopLevel "CoverageModulationModeNV"
type CoverageModulationModeNV = VkCoverageModulationModeNV


{-# complete COVERAGE_MODULATION_MODE_NONE_NV, COVERAGE_MODULATION_MODE_RGB_NV, COVERAGE_MODULATION_MODE_ALPHA_NV, COVERAGE_MODULATION_MODE_RGBA_NV :: CoverageModulationModeNV #-}


-- No documentation found for Nested "CoverageModulationModeNV" "COVERAGE_MODULATION_MODE_NONE_NV"
pattern COVERAGE_MODULATION_MODE_NONE_NV :: (a ~ CoverageModulationModeNV) => a
pattern COVERAGE_MODULATION_MODE_NONE_NV = VK_COVERAGE_MODULATION_MODE_NONE_NV


-- No documentation found for Nested "CoverageModulationModeNV" "COVERAGE_MODULATION_MODE_RGB_NV"
pattern COVERAGE_MODULATION_MODE_RGB_NV :: (a ~ CoverageModulationModeNV) => a
pattern COVERAGE_MODULATION_MODE_RGB_NV = VK_COVERAGE_MODULATION_MODE_RGB_NV


-- No documentation found for Nested "CoverageModulationModeNV" "COVERAGE_MODULATION_MODE_ALPHA_NV"
pattern COVERAGE_MODULATION_MODE_ALPHA_NV :: (a ~ CoverageModulationModeNV) => a
pattern COVERAGE_MODULATION_MODE_ALPHA_NV = VK_COVERAGE_MODULATION_MODE_ALPHA_NV


-- No documentation found for Nested "CoverageModulationModeNV" "COVERAGE_MODULATION_MODE_RGBA_NV"
pattern COVERAGE_MODULATION_MODE_RGBA_NV :: (a ~ CoverageModulationModeNV) => a
pattern COVERAGE_MODULATION_MODE_RGBA_NV = VK_COVERAGE_MODULATION_MODE_RGBA_NV

-- No documentation found for TopLevel "PipelineCoverageModulationStateCreateFlagsNV"
type PipelineCoverageModulationStateCreateFlagsNV = VkPipelineCoverageModulationStateCreateFlagsNV


-- No complete pragma for PipelineCoverageModulationStateCreateFlagsNV as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineCoverageModulationStateCreateInfoNV"
data PipelineCoverageModulationStateCreateInfoNV = PipelineCoverageModulationStateCreateInfoNV
  { -- No documentation found for Nested "PipelineCoverageModulationStateCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineCoverageModulationStateCreateInfoNV" "flags"
  flags :: PipelineCoverageModulationStateCreateFlagsNV
  , -- No documentation found for Nested "PipelineCoverageModulationStateCreateInfoNV" "coverageModulationMode"
  coverageModulationMode :: CoverageModulationModeNV
  , -- No documentation found for Nested "PipelineCoverageModulationStateCreateInfoNV" "coverageModulationTableEnable"
  coverageModulationTableEnable :: Bool
  , -- No documentation found for Nested "PipelineCoverageModulationStateCreateInfoNV" "pCoverageModulationTable"
  coverageModulationTable :: Either Word32 (Vector Float)
  }
  deriving (Show, Eq)

instance Zero PipelineCoverageModulationStateCreateInfoNV where
  zero = PipelineCoverageModulationStateCreateInfoNV Nothing
                                                     zero
                                                     zero
                                                     False
                                                     (Left 0)

#endif

-- No documentation found for TopLevel "VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME"
pattern NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME = VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION"
pattern NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION :: Integral a => a
pattern NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION = VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION
