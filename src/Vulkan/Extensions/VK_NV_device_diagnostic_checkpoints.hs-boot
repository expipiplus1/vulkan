{-# language CPP #-}
-- | = Name
--
-- VK_NV_device_diagnostic_checkpoints - device extension
--
-- = VK_NV_device_diagnostic_checkpoints
--
-- [__Name String__]
--     @VK_NV_device_diagnostic_checkpoints@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     207
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_VERSION_1_3
--
--     -   Interacts with VK_KHR_synchronization2
--
-- [__Contact__]
--
--     -   Nuno Subtil
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_device_diagnostic_checkpoints] @nsubtil%0A*Here describe the issue or question you have about the VK_NV_device_diagnostic_checkpoints extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-07-16
--
-- [__Contributors__]
--
--     -   Oleg Kuznetsov, NVIDIA
--
--     -   Alex Dunn, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- This extension allows applications to insert markers in the command
-- stream and associate them with custom data.
--
-- If a device lost error occurs, the application /may/ then query the
-- implementation for the last markers to cross specific
-- implementation-defined pipeline stages, in order to narrow down which
-- commands were executing at the time and might have caused the failure.
--
-- == New Commands
--
-- -   'cmdSetCheckpointNV'
--
-- -   'getQueueCheckpointDataNV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>
-- is supported:
--
-- -   'getQueueCheckpointData2NV'
--
-- == New Structures
--
-- -   'CheckpointDataNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.QueueFamilyProperties2':
--
--     -   'QueueFamilyCheckpointPropertiesNV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>
-- is supported:
--
-- -   'CheckpointData2NV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.QueueFamilyProperties2':
--
--     -   'QueueFamilyCheckpointProperties2NV'
--
-- == New Enum Constants
--
-- -   'NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME'
--
-- -   'NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CHECKPOINT_DATA_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CHECKPOINT_DATA_2_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_2_NV'
--
-- == Version History
--
-- -   Revision 1, 2018-07-16 (Nuno Subtil)
--
--     -   Internal revisions
--
-- -   Revision 2, 2018-07-16 (Nuno Subtil)
--
--     -   ???
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_device_diagnostic_checkpoints Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints  ( CheckpointData2NV
                                                              , CheckpointDataNV
                                                              , QueueFamilyCheckpointProperties2NV
                                                              , QueueFamilyCheckpointPropertiesNV
                                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data CheckpointData2NV

instance ToCStruct CheckpointData2NV
instance Show CheckpointData2NV

instance FromCStruct CheckpointData2NV


data CheckpointDataNV

instance ToCStruct CheckpointDataNV
instance Show CheckpointDataNV

instance FromCStruct CheckpointDataNV


data QueueFamilyCheckpointProperties2NV

instance ToCStruct QueueFamilyCheckpointProperties2NV
instance Show QueueFamilyCheckpointProperties2NV

instance FromCStruct QueueFamilyCheckpointProperties2NV


data QueueFamilyCheckpointPropertiesNV

instance ToCStruct QueueFamilyCheckpointPropertiesNV
instance Show QueueFamilyCheckpointPropertiesNV

instance FromCStruct QueueFamilyCheckpointPropertiesNV

