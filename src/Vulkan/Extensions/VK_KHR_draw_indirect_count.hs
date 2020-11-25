{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_draw_indirect_count"
module Vulkan.Extensions.VK_KHR_draw_indirect_count  ( cmdDrawIndirectCountKHR
                                                     , cmdDrawIndexedIndirectCountKHR
                                                     , KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION
                                                     , pattern KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION
                                                     , KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME
                                                     , pattern KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME
                                                     ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count (cmdDrawIndexedIndirectCount)
import Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count (cmdDrawIndirectCount)
-- No documentation found for TopLevel "vkCmdDrawIndirectCountKHR"
cmdDrawIndirectCountKHR = cmdDrawIndirectCount


-- No documentation found for TopLevel "vkCmdDrawIndexedIndirectCountKHR"
cmdDrawIndexedIndirectCountKHR = cmdDrawIndexedIndirectCount


type KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION"
pattern KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION = 1


type KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME = "VK_KHR_draw_indirect_count"

-- No documentation found for TopLevel "VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME"
pattern KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME = "VK_KHR_draw_indirect_count"

