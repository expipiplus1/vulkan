{-# language CPP #-}
-- | = Name
--
-- VK_AMD_mixed_attachment_samples - device extension
--
-- = VK_AMD_mixed_attachment_samples
--
-- [__Name String__]
--     @VK_AMD_mixed_attachment_samples@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     137
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__API Interactions__]
--
--     -   Interacts with VK_VERSION_1_3
--
--     -   Interacts with VK_KHR_dynamic_rendering
--
-- [__Contact__]
--
--     -   Matthaeus G. Chajdas
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMD_mixed_attachment_samples] @anteru%0A*Here describe the issue or question you have about the VK_AMD_mixed_attachment_samples extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-07-24
--
-- [__Contributors__]
--
--     -   Mais Alnasser, AMD
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Maciej Jesionowski, AMD
--
--     -   Daniel Rakos, AMD
--
-- == Description
--
-- This extension enables applications to use multisampled rendering with a
-- depth\/stencil sample count that is larger than the color sample count.
-- Having a depth\/stencil sample count larger than the color sample count
-- allows maintaining geometry and coverage information at a higher sample
-- rate than color information. All samples are depth\/stencil tested, but
-- only the first color sample count number of samples get a corresponding
-- color output.
--
-- == New Structures
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo',
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo':
--
--     -   'AttachmentSampleCountInfoAMD'
--
-- == New Enum Constants
--
-- -   'AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME'
--
-- -   'AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_AMD'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2017-07-24 (Daniel Rakos)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_AMD_mixed_attachment_samples Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_mixed_attachment_samples  (AttachmentSampleCountInfoAMD) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data AttachmentSampleCountInfoAMD

instance ToCStruct AttachmentSampleCountInfoAMD
instance Show AttachmentSampleCountInfoAMD

instance FromCStruct AttachmentSampleCountInfoAMD

