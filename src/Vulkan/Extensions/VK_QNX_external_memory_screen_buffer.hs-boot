{-# language CPP #-}
-- | = Name
--
-- VK_QNX_external_memory_screen_buffer - device extension
--
-- == VK_QNX_external_memory_screen_buffer
--
-- [__Name String__]
--     @VK_QNX_external_memory_screen_buffer@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     530
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_sampler_ycbcr_conversion VK_KHR_sampler_ycbcr_conversion>
--              and
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory VK_KHR_external_memory>
--              and
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dedicated_allocation VK_KHR_dedicated_allocation>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_queue_family_foreign VK_EXT_queue_family_foreign>
--
-- [__Contact__]
--
--     -   Mike Gorchak
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QNX_external_memory_screen_buffer] @mgorchak-blackberry%0A*Here describe the issue or question you have about the VK_QNX_external_memory_screen_buffer extension* >
--
--     -   Aaron Ruby
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QNX_external_memory_screen_buffer] @aruby-blackberry%0A*Here describe the issue or question you have about the VK_QNX_external_memory_screen_buffer extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-05-17
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Mike Gorchak, QNX \/ Blackberry Limited
--
--     -   Aaron Ruby, QNX \/ Blackberry Limited
--
-- == Description
--
-- This extension enables an application to import QNX Screen
-- 'Screen_buffer' objects created outside of the Vulkan device into Vulkan
-- memory objects, where they can be bound to images and buffers.
--
-- Some 'Screen_buffer' images have implementation-defined /external
-- formats/ that /may/ not correspond to Vulkan formats. Sampler Y′CBCR
-- conversion /can/ be used to sample from these images and convert them to
-- a known color space.
--
-- 'Screen_buffer' is strongly typed, so naming the handle type is
-- redundant. The internal layout and therefore size of a 'Screen_buffer'
-- image may depend on native usage flags that do not have corresponding
-- Vulkan counterparts.
--
-- == New Commands
--
-- -   'getScreenBufferPropertiesQNX'
--
-- == New Structures
--
-- -   'ScreenBufferPropertiesQNX'
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo',
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo':
--
--     -   'ExternalFormatQNX'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'ImportScreenBufferInfoQNX'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX'
--
-- -   Extending 'ScreenBufferPropertiesQNX':
--
--     -   'ScreenBufferFormatPropertiesQNX'
--
-- == New Enum Constants
--
-- -   'QNX_EXTERNAL_MEMORY_SCREEN_BUFFER_EXTENSION_NAME'
--
-- -   'QNX_EXTERNAL_MEMORY_SCREEN_BUFFER_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits':
--
--     -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_SCREEN_BUFFER_BIT_QNX'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXTERNAL_FORMAT_QNX'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_SCREEN_BUFFER_INFO_QNX'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_SCREEN_BUFFER_FEATURES_QNX'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SCREEN_BUFFER_FORMAT_PROPERTIES_QNX'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SCREEN_BUFFER_PROPERTIES_QNX'
--
-- == Version History
--
-- -   Revision 1, 2023-05-17 (Mike Gorchak)
--
--     -   Initial version
--
-- == See Also
--
-- 'ExternalFormatQNX', 'ImportScreenBufferInfoQNX',
-- 'PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX',
-- 'ScreenBufferFormatPropertiesQNX', 'ScreenBufferPropertiesQNX',
-- 'getScreenBufferPropertiesQNX'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QNX_external_memory_screen_buffer Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QNX_external_memory_screen_buffer  ( ExternalFormatQNX
                                                               , ImportScreenBufferInfoQNX
                                                               , PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX
                                                               , ScreenBufferFormatPropertiesQNX
                                                               , ScreenBufferPropertiesQNX
                                                               , Screen_buffer
                                                               ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data ExternalFormatQNX

instance ToCStruct ExternalFormatQNX
instance Show ExternalFormatQNX

instance FromCStruct ExternalFormatQNX


data ImportScreenBufferInfoQNX

instance ToCStruct ImportScreenBufferInfoQNX
instance Show ImportScreenBufferInfoQNX

instance FromCStruct ImportScreenBufferInfoQNX


data PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX

instance ToCStruct PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX
instance Show PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX

instance FromCStruct PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX


data ScreenBufferFormatPropertiesQNX

instance ToCStruct ScreenBufferFormatPropertiesQNX
instance Show ScreenBufferFormatPropertiesQNX

instance FromCStruct ScreenBufferFormatPropertiesQNX


type role ScreenBufferPropertiesQNX nominal
data ScreenBufferPropertiesQNX (es :: [Type])

instance ( Extendss ScreenBufferPropertiesQNX es
         , PokeChain es ) => ToCStruct (ScreenBufferPropertiesQNX es)
instance Show (Chain es) => Show (ScreenBufferPropertiesQNX es)

instance ( Extendss ScreenBufferPropertiesQNX es
         , PeekChain es ) => FromCStruct (ScreenBufferPropertiesQNX es)


data Screen_buffer

