{-# language CPP #-}
module Vulkan.Extensions.VK_AMD_draw_indirect_count  ( cmdDrawIndirectCountAMD
                                                     , cmdDrawIndexedIndirectCountAMD
                                                     , AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION
                                                     , pattern AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION
                                                     , AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME
                                                     , pattern AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME
                                                     ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count (cmdDrawIndexedIndirectCount)
import Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count (cmdDrawIndirectCount)
-- No documentation found for TopLevel "vkCmdDrawIndirectCountAMD"
cmdDrawIndirectCountAMD = cmdDrawIndirectCount


-- No documentation found for TopLevel "vkCmdDrawIndexedIndirectCountAMD"
cmdDrawIndexedIndirectCountAMD = cmdDrawIndexedIndirectCount


type AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION"
pattern AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION = 2


type AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME = "VK_AMD_draw_indirect_count"

-- No documentation found for TopLevel "VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME"
pattern AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME = "VK_AMD_draw_indirect_count"

