{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_external_fence  ( pattern STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR
                                                , pattern FENCE_IMPORT_TEMPORARY_BIT_KHR
                                                , FenceImportFlagsKHR
                                                , FenceImportFlagBitsKHR
                                                , ExportFenceCreateInfoKHR
                                                , KHR_EXTERNAL_FENCE_SPEC_VERSION
                                                , pattern KHR_EXTERNAL_FENCE_SPEC_VERSION
                                                , KHR_EXTERNAL_FENCE_EXTENSION_NAME
                                                , pattern KHR_EXTERNAL_FENCE_EXTENSION_NAME
                                                ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_external_fence (ExportFenceCreateInfo)
import Vulkan.Core11.Enums.FenceImportFlagBits (FenceImportFlagBits)
import Vulkan.Core11.Enums.FenceImportFlagBits (FenceImportFlags)
import Vulkan.Core11.Enums.FenceImportFlagBits (FenceImportFlags)
import Vulkan.Core11.Enums.FenceImportFlagBits (FenceImportFlagBits(FENCE_IMPORT_TEMPORARY_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR = STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO


-- No documentation found for TopLevel "VK_FENCE_IMPORT_TEMPORARY_BIT_KHR"
pattern FENCE_IMPORT_TEMPORARY_BIT_KHR = FENCE_IMPORT_TEMPORARY_BIT


-- No documentation found for TopLevel "VkFenceImportFlagsKHR"
type FenceImportFlagsKHR = FenceImportFlags


-- No documentation found for TopLevel "VkFenceImportFlagBitsKHR"
type FenceImportFlagBitsKHR = FenceImportFlagBits


-- No documentation found for TopLevel "VkExportFenceCreateInfoKHR"
type ExportFenceCreateInfoKHR = ExportFenceCreateInfo


type KHR_EXTERNAL_FENCE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_SPEC_VERSION"
pattern KHR_EXTERNAL_FENCE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_EXTERNAL_FENCE_SPEC_VERSION = 1


type KHR_EXTERNAL_FENCE_EXTENSION_NAME = "VK_KHR_external_fence"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME"
pattern KHR_EXTERNAL_FENCE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_EXTERNAL_FENCE_EXTENSION_NAME = "VK_KHR_external_fence"

