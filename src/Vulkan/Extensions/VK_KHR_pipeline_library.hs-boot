{-# language CPP #-}
-- | = Name
--
-- VK_KHR_pipeline_library - device extension
--
-- = Registered Extension Number
--
-- 291
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-01-08
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   See contributors to @VK_KHR_ray_tracing@
--
-- == Description
--
-- A pipeline library is a special pipeline that cannot be bound, instead
-- it defines a set of shaders and shader groups which can be linked into
-- other pipelines. This extension defines the infrastructure for pipeline
-- libraries, but does not specify the creation or usage of pipeline
-- libraries. This is left to additional dependent extensions.
--
-- == New Structures
--
-- -   'PipelineLibraryCreateInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_PIPELINE_LIBRARY_EXTENSION_NAME'
--
-- -   'KHR_PIPELINE_LIBRARY_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR'
--
-- == Version History
--
-- -   Revision 1, 2020-01-08 (Christoph Kubisch)
--
--     -   Initial draft.
--
-- = See Also
--
-- 'PipelineLibraryCreateInfoKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_library Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_pipeline_library  (PipelineLibraryCreateInfoKHR) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PipelineLibraryCreateInfoKHR

instance ToCStruct PipelineLibraryCreateInfoKHR
instance Show PipelineLibraryCreateInfoKHR

instance FromCStruct PipelineLibraryCreateInfoKHR

