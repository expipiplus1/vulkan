{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_AMD_rasterization_order
  ( withCStructPipelineRasterizationStateRasterizationOrderAMD
  , fromCStructPipelineRasterizationStateRasterizationOrderAMD
  , PipelineRasterizationStateRasterizationOrderAMD(..)
  , RasterizationOrderAMD
  , pattern VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION
  , pattern VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD
  ) where

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
import Graphics.Vulkan.C.Extensions.VK_AMD_rasterization_order
  ( VkPipelineRasterizationStateRasterizationOrderAMD(..)
  , VkRasterizationOrderAMD(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_rasterization_order
  ( pattern VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME
  , pattern VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION
  )


-- No documentation found for TopLevel "PipelineRasterizationStateRasterizationOrderAMD"
data PipelineRasterizationStateRasterizationOrderAMD = PipelineRasterizationStateRasterizationOrderAMD
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineRasterizationStateRasterizationOrderAMD" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineRasterizationStateRasterizationOrderAMD" "rasterizationOrder"
  vkRasterizationOrder :: RasterizationOrderAMD
  }
  deriving (Show, Eq)
withCStructPipelineRasterizationStateRasterizationOrderAMD :: PipelineRasterizationStateRasterizationOrderAMD -> (VkPipelineRasterizationStateRasterizationOrderAMD -> IO a) -> IO a
withCStructPipelineRasterizationStateRasterizationOrderAMD from cont = maybeWith withSomeVkStruct (vkPNext (from :: PipelineRasterizationStateRasterizationOrderAMD)) (\pPNext -> cont (VkPipelineRasterizationStateRasterizationOrderAMD VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD pPNext (vkRasterizationOrder (from :: PipelineRasterizationStateRasterizationOrderAMD))))
fromCStructPipelineRasterizationStateRasterizationOrderAMD :: VkPipelineRasterizationStateRasterizationOrderAMD -> IO PipelineRasterizationStateRasterizationOrderAMD
fromCStructPipelineRasterizationStateRasterizationOrderAMD c = PipelineRasterizationStateRasterizationOrderAMD <$> -- Univalued Member elided
                                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineRasterizationStateRasterizationOrderAMD)))
                                                                                                               <*> pure (vkRasterizationOrder (c :: VkPipelineRasterizationStateRasterizationOrderAMD))
instance Zero PipelineRasterizationStateRasterizationOrderAMD where
  zero = PipelineRasterizationStateRasterizationOrderAMD Nothing
                                                         zero
-- No documentation found for TopLevel "RasterizationOrderAMD"
type RasterizationOrderAMD = VkRasterizationOrderAMD
