{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_external_semaphore  ( pattern STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR
                                                    , pattern SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR
                                                    , SemaphoreImportFlagsKHR
                                                    , SemaphoreImportFlagBitsKHR
                                                    , ExportSemaphoreCreateInfoKHR
                                                    , KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION
                                                    , pattern KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION
                                                    , KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME
                                                    , pattern KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME
                                                    ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore (ExportSemaphoreCreateInfo)
import Vulkan.Core11.Enums.SemaphoreImportFlagBits (SemaphoreImportFlagBits)
import Vulkan.Core11.Enums.SemaphoreImportFlagBits (SemaphoreImportFlags)
import Vulkan.Core11.Enums.SemaphoreImportFlagBits (SemaphoreImportFlags)
import Vulkan.Core11.Enums.SemaphoreImportFlagBits (SemaphoreImportFlagBits(SEMAPHORE_IMPORT_TEMPORARY_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR = STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO


-- No documentation found for TopLevel "VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR"
pattern SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR = SEMAPHORE_IMPORT_TEMPORARY_BIT


-- No documentation found for TopLevel "VkSemaphoreImportFlagsKHR"
type SemaphoreImportFlagsKHR = SemaphoreImportFlags


-- No documentation found for TopLevel "VkSemaphoreImportFlagBitsKHR"
type SemaphoreImportFlagBitsKHR = SemaphoreImportFlagBits


-- No documentation found for TopLevel "VkExportSemaphoreCreateInfoKHR"
type ExportSemaphoreCreateInfoKHR = ExportSemaphoreCreateInfo


type KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION"
pattern KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION = 1


type KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME = "VK_KHR_external_semaphore"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME"
pattern KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME = "VK_KHR_external_semaphore"

