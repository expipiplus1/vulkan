{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_fragment_coverage_to_color
  ( PipelineCoverageToColorStateCreateFlagsNV
#if defined(VK_USE_PLATFORM_GGP)
  , PipelineCoverageToColorStateCreateInfoNV(..)
#endif
  , pattern NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME
  , pattern NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV
  ) where

import Data.String
  ( IsString
  )

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
import Graphics.Vulkan.C.Extensions.VK_NV_fragment_coverage_to_color
  ( VkPipelineCoverageToColorStateCreateFlagsNV(..)
  , pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME
  , pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV
  )


-- No documentation found for TopLevel "PipelineCoverageToColorStateCreateFlagsNV"
type PipelineCoverageToColorStateCreateFlagsNV = VkPipelineCoverageToColorStateCreateFlagsNV


-- No complete pragma for PipelineCoverageToColorStateCreateFlagsNV as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineCoverageToColorStateCreateInfoNV"
data PipelineCoverageToColorStateCreateInfoNV = PipelineCoverageToColorStateCreateInfoNV
  { -- No documentation found for Nested "PipelineCoverageToColorStateCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineCoverageToColorStateCreateInfoNV" "flags"
  flags :: PipelineCoverageToColorStateCreateFlagsNV
  , -- No documentation found for Nested "PipelineCoverageToColorStateCreateInfoNV" "coverageToColorEnable"
  coverageToColorEnable :: Bool
  , -- No documentation found for Nested "PipelineCoverageToColorStateCreateInfoNV" "coverageToColorLocation"
  coverageToColorLocation :: Word32
  }
  deriving (Show, Eq)

instance Zero PipelineCoverageToColorStateCreateInfoNV where
  zero = PipelineCoverageToColorStateCreateInfoNV Nothing
                                                  zero
                                                  False
                                                  zero

#endif

-- No documentation found for TopLevel "VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME"
pattern NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME = VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION"
pattern NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION :: Integral a => a
pattern NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION = VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION
