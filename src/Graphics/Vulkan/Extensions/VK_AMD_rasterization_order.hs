{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_AMD_rasterization_order
  ( withCStructPipelineRasterizationStateRasterizationOrderAMD
  , fromCStructPipelineRasterizationStateRasterizationOrderAMD
  , PipelineRasterizationStateRasterizationOrderAMD(..)
  , RasterizationOrderAMD
  , pattern RASTERIZATION_ORDER_STRICT_AMD
  , pattern RASTERIZATION_ORDER_RELAXED_AMD
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
  , pattern VK_RASTERIZATION_ORDER_RELAXED_AMD
  , pattern VK_RASTERIZATION_ORDER_STRICT_AMD
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



-- | VkPipelineRasterizationStateRasterizationOrderAMD - Structure defining
-- rasterization order for a graphics pipeline
--
-- = Description
--
-- Unresolved directive in
-- VkPipelineRasterizationStateRasterizationOrderAMD.txt -
-- include::{generated}\/validity\/structs\/VkPipelineRasterizationStateRasterizationOrderAMD.txt[]
--
-- If the
-- @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.1-extensions\/html\/vkspec.html#VK_AMD_rasterization_order@
-- device extension is not enabled or the application does not request a
-- particular rasterization order through specifying a
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_rasterization_order.VkPipelineRasterizationStateRasterizationOrderAMD'
-- structure then the rasterization order used by the graphics pipeline
-- defaults to
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_rasterization_order.VK_RASTERIZATION_ORDER_STRICT_AMD'.
--
-- = See Also
--
-- No cross-references are available
data PipelineRasterizationStateRasterizationOrderAMD = PipelineRasterizationStateRasterizationOrderAMD
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineRasterizationStateRasterizationOrderAMD" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineRasterizationStateRasterizationOrderAMD" "rasterizationOrder"
  rasterizationOrder :: RasterizationOrderAMD
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineRasterizationStateRasterizationOrderAMD' and
-- marshal a 'PipelineRasterizationStateRasterizationOrderAMD' into it. The 'VkPipelineRasterizationStateRasterizationOrderAMD' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineRasterizationStateRasterizationOrderAMD :: PipelineRasterizationStateRasterizationOrderAMD -> (VkPipelineRasterizationStateRasterizationOrderAMD -> IO a) -> IO a
withCStructPipelineRasterizationStateRasterizationOrderAMD marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PipelineRasterizationStateRasterizationOrderAMD)) (\pPNext -> cont (VkPipelineRasterizationStateRasterizationOrderAMD VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD pPNext (rasterizationOrder (marshalled :: PipelineRasterizationStateRasterizationOrderAMD))))

-- | A function to read a 'VkPipelineRasterizationStateRasterizationOrderAMD' and all additional
-- structures in the pointer chain into a 'PipelineRasterizationStateRasterizationOrderAMD'.
fromCStructPipelineRasterizationStateRasterizationOrderAMD :: VkPipelineRasterizationStateRasterizationOrderAMD -> IO PipelineRasterizationStateRasterizationOrderAMD
fromCStructPipelineRasterizationStateRasterizationOrderAMD c = PipelineRasterizationStateRasterizationOrderAMD <$> -- Univalued Member elided
                                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineRasterizationStateRasterizationOrderAMD)))
                                                                                                               <*> pure (vkRasterizationOrder (c :: VkPipelineRasterizationStateRasterizationOrderAMD))

instance Zero PipelineRasterizationStateRasterizationOrderAMD where
  zero = PipelineRasterizationStateRasterizationOrderAMD Nothing
                                                         zero


-- | VkRasterizationOrderAMD - Specify rasterization order for a graphics
-- pipeline
--
-- = See Also
--
-- No cross-references are available
type RasterizationOrderAMD = VkRasterizationOrderAMD


-- | 'Graphics.Vulkan.C.Extensions.VK_AMD_rasterization_order.VK_RASTERIZATION_ORDER_STRICT_AMD'
-- specifies that operations for each primitive in a subpass /must/ occur
-- in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#drawing-primitive-order primitive order>.
pattern RASTERIZATION_ORDER_STRICT_AMD :: (a ~ RasterizationOrderAMD) => a
pattern RASTERIZATION_ORDER_STRICT_AMD = VK_RASTERIZATION_ORDER_STRICT_AMD


-- | 'Graphics.Vulkan.C.Extensions.VK_AMD_rasterization_order.VK_RASTERIZATION_ORDER_RELAXED_AMD'
-- specifies that operations for each primitive in a subpass /may/ not
-- occur in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#drawing-primitive-order primitive order>.
pattern RASTERIZATION_ORDER_RELAXED_AMD :: (a ~ RasterizationOrderAMD) => a
pattern RASTERIZATION_ORDER_RELAXED_AMD = VK_RASTERIZATION_ORDER_RELAXED_AMD
