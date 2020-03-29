{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_host_query_reset  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT
                                                           , resetQueryPoolEXT
                                                           , PhysicalDeviceHostQueryResetFeaturesEXT
                                                           , EXT_HOST_QUERY_RESET_SPEC_VERSION
                                                           , pattern EXT_HOST_QUERY_RESET_SPEC_VERSION
                                                           , EXT_HOST_QUERY_RESET_EXTENSION_NAME
                                                           , pattern EXT_HOST_QUERY_RESET_EXTENSION_NAME
                                                           ) where

import Data.String (IsString)
import Graphics.Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset (resetQueryPool)
import Graphics.Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset (PhysicalDeviceHostQueryResetFeatures)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES


-- No documentation found for TopLevel "vkResetQueryPoolEXT"
resetQueryPoolEXT = resetQueryPool


-- No documentation found for TopLevel "VkPhysicalDeviceHostQueryResetFeaturesEXT"
type PhysicalDeviceHostQueryResetFeaturesEXT = PhysicalDeviceHostQueryResetFeatures


type EXT_HOST_QUERY_RESET_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_HOST_QUERY_RESET_SPEC_VERSION"
pattern EXT_HOST_QUERY_RESET_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_HOST_QUERY_RESET_SPEC_VERSION = 1


type EXT_HOST_QUERY_RESET_EXTENSION_NAME = "VK_EXT_host_query_reset"

-- No documentation found for TopLevel "VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME"
pattern EXT_HOST_QUERY_RESET_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_HOST_QUERY_RESET_EXTENSION_NAME = "VK_EXT_host_query_reset"

