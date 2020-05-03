{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_maintenance1  ( pattern ERROR_OUT_OF_POOL_MEMORY_KHR
                                              , pattern FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR
                                              , pattern FORMAT_FEATURE_TRANSFER_DST_BIT_KHR
                                              , pattern IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR
                                              , trimCommandPoolKHR
                                              , CommandPoolTrimFlagsKHR
                                              , KHR_MAINTENANCE1_SPEC_VERSION
                                              , pattern KHR_MAINTENANCE1_SPEC_VERSION
                                              , KHR_MAINTENANCE1_EXTENSION_NAME
                                              , pattern KHR_MAINTENANCE1_EXTENSION_NAME
                                              ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_maintenance1 (trimCommandPool)
import Vulkan.Core11.Enums.CommandPoolTrimFlags (CommandPoolTrimFlags)
import Vulkan.Core10.Enums.Result (Result(ERROR_OUT_OF_POOL_MEMORY))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(FORMAT_FEATURE_TRANSFER_DST_BIT))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(FORMAT_FEATURE_TRANSFER_SRC_BIT))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT))
-- No documentation found for TopLevel "VK_ERROR_OUT_OF_POOL_MEMORY_KHR"
pattern ERROR_OUT_OF_POOL_MEMORY_KHR = ERROR_OUT_OF_POOL_MEMORY


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR"
pattern FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR = FORMAT_FEATURE_TRANSFER_SRC_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR"
pattern FORMAT_FEATURE_TRANSFER_DST_BIT_KHR = FORMAT_FEATURE_TRANSFER_DST_BIT


-- No documentation found for TopLevel "VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR"
pattern IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR = IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT


-- No documentation found for TopLevel "vkTrimCommandPoolKHR"
trimCommandPoolKHR = trimCommandPool


-- No documentation found for TopLevel "VkCommandPoolTrimFlagsKHR"
type CommandPoolTrimFlagsKHR = CommandPoolTrimFlags


type KHR_MAINTENANCE1_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE1_SPEC_VERSION"
pattern KHR_MAINTENANCE1_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_MAINTENANCE1_SPEC_VERSION = 2


type KHR_MAINTENANCE1_EXTENSION_NAME = "VK_KHR_maintenance1"

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE1_EXTENSION_NAME"
pattern KHR_MAINTENANCE1_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_MAINTENANCE1_EXTENSION_NAME = "VK_KHR_maintenance1"

