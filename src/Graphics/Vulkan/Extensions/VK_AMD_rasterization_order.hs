{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_AMD_rasterization_order
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PipelineRasterizationStateRasterizationOrderAMD(..)
  , 
#endif
  RasterizationOrderAMD
  , pattern RASTERIZATION_ORDER_STRICT_AMD
  , pattern RASTERIZATION_ORDER_RELAXED_AMD
  , pattern AMD_RASTERIZATION_ORDER_EXTENSION_NAME
  , pattern AMD_RASTERIZATION_ORDER_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_AMD_rasterization_order
  ( VkRasterizationOrderAMD(..)
  , pattern VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME
  , pattern VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION
  , pattern VK_RASTERIZATION_ORDER_RELAXED_AMD
  , pattern VK_RASTERIZATION_ORDER_STRICT_AMD
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineRasterizationStateRasterizationOrderAMD"
data PipelineRasterizationStateRasterizationOrderAMD = PipelineRasterizationStateRasterizationOrderAMD
  { -- No documentation found for Nested "PipelineRasterizationStateRasterizationOrderAMD" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineRasterizationStateRasterizationOrderAMD" "rasterizationOrder"
  rasterizationOrder :: RasterizationOrderAMD
  }
  deriving (Show, Eq)

instance Zero PipelineRasterizationStateRasterizationOrderAMD where
  zero = PipelineRasterizationStateRasterizationOrderAMD Nothing
                                                         zero

#endif

-- No documentation found for TopLevel "RasterizationOrderAMD"
type RasterizationOrderAMD = VkRasterizationOrderAMD


{-# complete RASTERIZATION_ORDER_STRICT_AMD, RASTERIZATION_ORDER_RELAXED_AMD :: RasterizationOrderAMD #-}


-- No documentation found for Nested "RasterizationOrderAMD" "RASTERIZATION_ORDER_STRICT_AMD"
pattern RASTERIZATION_ORDER_STRICT_AMD :: (a ~ RasterizationOrderAMD) => a
pattern RASTERIZATION_ORDER_STRICT_AMD = VK_RASTERIZATION_ORDER_STRICT_AMD


-- No documentation found for Nested "RasterizationOrderAMD" "RASTERIZATION_ORDER_RELAXED_AMD"
pattern RASTERIZATION_ORDER_RELAXED_AMD :: (a ~ RasterizationOrderAMD) => a
pattern RASTERIZATION_ORDER_RELAXED_AMD = VK_RASTERIZATION_ORDER_RELAXED_AMD

-- No documentation found for TopLevel "VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME"
pattern AMD_RASTERIZATION_ORDER_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern AMD_RASTERIZATION_ORDER_EXTENSION_NAME = VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME

-- No documentation found for TopLevel "VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION"
pattern AMD_RASTERIZATION_ORDER_SPEC_VERSION :: Integral a => a
pattern AMD_RASTERIZATION_ORDER_SPEC_VERSION = VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION
