{-# language CPP #-}
-- | = Name
--
-- VK_ANDROID_external_memory_android_hardware_buffer - device extension
--
-- == VK_ANDROID_external_memory_android_hardware_buffer
--
-- [__Name String__]
--     @VK_ANDROID_external_memory_android_hardware_buffer@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     130
--
-- [__Revision__]
--     3
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_sampler_ycbcr_conversion@
--
--     -   Requires @VK_KHR_external_memory@
--
--     -   Requires @VK_EXT_queue_family_foreign@
--
--     -   Requires @VK_KHR_dedicated_allocation@
--
-- [__Contact__]
--
--     -   Jesse Hall
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_ANDROID_external_memory_android_hardware_buffer:%20&body=@critsec%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-08-27
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Ray Smith, ARM
--
--     -   Chad Versace, Google
--
--     -   Jesse Hall, Google
--
--     -   Tobias Hector, Imagination
--
--     -   James Jones, NVIDIA
--
--     -   Tony Zlatinski, NVIDIA
--
--     -   Matthew Netsch, Qualcomm
--
--     -   Andrew Garrard, Samsung
--
-- == Description
--
-- This extension enables an application to import Android
-- 'AHardwareBuffer' objects created outside of the Vulkan device into
-- Vulkan memory objects, where they /can/ be bound to images and buffers.
-- It also allows exporting an 'AHardwareBuffer' from a Vulkan memory
-- object for symmetry with other operating systems. But since not all
-- 'AHardwareBuffer' usages and formats have Vulkan equivalents, exporting
-- from Vulkan provides strictly less functionality than creating the
-- 'AHardwareBuffer' externally and importing it.
--
-- Some 'AHardwareBuffer' images have implementation-defined /external
-- formats/ that /may/ not correspond to Vulkan formats. Sampler Y′CBCR
-- conversion /can/ be used to sample from these images and convert them to
-- a known color space.
--
-- == New Base Types
--
-- -   'AHardwareBuffer'
--
-- == New Commands
--
-- -   'getAndroidHardwareBufferPropertiesANDROID'
--
-- -   'getMemoryAndroidHardwareBufferANDROID'
--
-- == New Structures
--
-- -   'AndroidHardwareBufferPropertiesANDROID'
--
-- -   'MemoryGetAndroidHardwareBufferInfoANDROID'
--
-- -   Extending 'AndroidHardwareBufferPropertiesANDROID':
--
--     -   'AndroidHardwareBufferFormatPropertiesANDROID'
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo',
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo':
--
--     -   'ExternalFormatANDROID'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.ImageFormatProperties2':
--
--     -   'AndroidHardwareBufferUsageANDROID'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'ImportAndroidHardwareBufferInfoANDROID'
--
-- == New Enum Constants
--
-- -   'ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME'
--
-- -   'ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits':
--
--     -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID'
--
-- == Issues
--
-- 1) Other external memory objects are represented as weakly-typed handles
-- (e.g. Win32 'Vulkan.Extensions.VK_NV_external_memory_win32.HANDLE' or
-- POSIX file descriptor), and require a handle type parameter along with
-- handles. 'AHardwareBuffer' is strongly typed, so naming the handle type
-- is redundant. Does symmetry justify adding handle type
-- parameters\/fields anyway?
--
-- __RESOLVED__: No. The handle type is already provided in places that
-- treat external memory objects generically. In the places we would add
-- it, the application code that would have to provide the handle type
-- value is already dealing with 'AHardwareBuffer'-specific
-- commands\/structures; the extra symmetry would not be enough to make
-- that code generic.
--
-- 2) The internal layout and therefore size of a 'AHardwareBuffer' image
-- may depend on native usage flags that do not have corresponding Vulkan
-- counterparts. Do we provide this info to
-- 'Vulkan.Core10.Image.createImage' somehow, or allow the allocation size
-- reported by 'Vulkan.Core10.MemoryManagement.getImageMemoryRequirements'
-- to be approximate?
--
-- __RESOLVED__: Allow the allocation size to be unspecified when
-- allocating the memory. It has to work this way for exported image memory
-- anyway, since 'AHardwareBuffer' allocation happens in
-- 'Vulkan.Core10.Memory.allocateMemory', and internally is performed by a
-- separate HAL, not the Vulkan implementation itself. There is a similar
-- issue with 'Vulkan.Core10.Image.getImageSubresourceLayout': the layout
-- is determined by the allocator HAL, so it is not known until the image
-- is bound to memory.
--
-- 3) Should the result of sampling an external-format image with the
-- suggested Y′CBCR conversion parameters yield the same results as using a
-- @samplerExternalOES@ in OpenGL ES?
--
-- __RESOLVED__: This would be desirable, so that apps converting from
-- OpenGL ES to Vulkan could get the same output given the same input. But
-- since sampling and conversion from Y′CBCR images is so loosely defined
-- in OpenGL ES, multiple implementations do it in a way that doesn’t
-- conform to Vulkan’s requirements. Modifying the OpenGL ES implementation
-- would be difficult, and would change the output of existing unmodified
-- applications. Changing the output only for applications that are being
-- modified gives developers the chance to notice and mitigate any
-- problems. Implementations are encouraged to minimize differences as much
-- as possible without causing compatibility problems for existing OpenGL
-- ES applications or violating Vulkan requirements.
--
-- 4) Should an 'AHardwareBuffer' with @AHARDWAREBUFFER_USAGE_CPU_@* usage
-- be mappable in Vulkan? Should it be possible to export an
-- @AHardwareBuffers@ with such usage?
--
-- __RESOLVED__: Optional, and mapping in Vulkan is not the same as
-- @AHardwareBuffer_lock@. The semantics of these are different: mapping in
-- memory is persistent, just gives a raw view of the memory contents, and
-- does not involve ownership. @AHardwareBuffer_lock@ gives the host
-- exclusive access to the buffer, is temporary, and allows for
-- reformatting copy-in\/copy-out. Implementations are not required to
-- support host-visible memory types for imported Android hardware buffers
-- or resources backed by them. If a host-visible memory type is supported
-- and used, the memory can be mapped in Vulkan, but doing so follows
-- Vulkan semantics: it is just a raw view of the data and does not imply
-- ownership (this means implementations must not internally call
-- @AHardwareBuffer_lock@ to implement 'Vulkan.Core10.Memory.mapMemory', or
-- assume the application has done so). Implementations are not required to
-- support linear-tiled images backed by Android hardware buffers, even if
-- the 'AHardwareBuffer' has CPU usage. There is no reliable way to
-- allocate memory in Vulkan that can be exported to a 'AHardwareBuffer'
-- with CPU usage.
--
-- 5) Android may add new 'AHardwareBuffer' formats and usage flags over
-- time. Can reference to them be added to this extension, or do they need
-- a new extension?
--
-- RESOLVED: This extension can document the interaction between the new
-- AHB formats\/usages and existing Vulkan features. No new Vulkan features
-- or implementation requirements can be added. The extension version
-- number will be incremented when this additional documentation is added,
-- but the version number does not indicate that an implementaiton supports
-- Vulkan memory or resources that map to the new 'AHardwareBuffer'
-- features: support for that must be queried with
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
-- or is implied by successfully allocating a 'AHardwareBuffer' outside of
-- Vulkan that uses the new feature and has a GPU usage flag.
--
-- In essence, these are new features added to a new Android API level,
-- rather than new Vulkan features. The extension will only document how
-- existing Vulkan features map to that new Android feature.
--
-- == Version History
--
-- -   Revision 3, 2019-08-27 (Jon Leech)
--
--     -   Update revision history to correspond to XML version number
--
-- -   Revision 2, 2018-04-09 (Petr Kraus)
--
--     -   Markup fixes and remove incorrect Draft status
--
-- -   Revision 1, 2018-03-04 (Jesse Hall)
--
--     -   Initial version
--
-- = See Also
--
-- 'AHardwareBuffer', 'AndroidHardwareBufferFormatPropertiesANDROID',
-- 'AndroidHardwareBufferPropertiesANDROID',
-- 'AndroidHardwareBufferUsageANDROID', 'ExternalFormatANDROID',
-- 'ImportAndroidHardwareBufferInfoANDROID',
-- 'MemoryGetAndroidHardwareBufferInfoANDROID',
-- 'getAndroidHardwareBufferPropertiesANDROID',
-- 'getMemoryAndroidHardwareBufferANDROID'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ANDROID_external_memory_android_hardware_buffer Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer  ( AndroidHardwareBufferFormatPropertiesANDROID
                                                                             , AndroidHardwareBufferPropertiesANDROID
                                                                             , AndroidHardwareBufferUsageANDROID
                                                                             , ExternalFormatANDROID
                                                                             , ImportAndroidHardwareBufferInfoANDROID
                                                                             , MemoryGetAndroidHardwareBufferInfoANDROID
                                                                             , AHardwareBuffer
                                                                             ) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
data AndroidHardwareBufferFormatPropertiesANDROID

instance ToCStruct AndroidHardwareBufferFormatPropertiesANDROID
instance Show AndroidHardwareBufferFormatPropertiesANDROID

instance FromCStruct AndroidHardwareBufferFormatPropertiesANDROID


type role AndroidHardwareBufferPropertiesANDROID nominal
data AndroidHardwareBufferPropertiesANDROID (es :: [Type])

instance (Extendss AndroidHardwareBufferPropertiesANDROID es, PokeChain es) => ToCStruct (AndroidHardwareBufferPropertiesANDROID es)
instance Show (Chain es) => Show (AndroidHardwareBufferPropertiesANDROID es)

instance (Extendss AndroidHardwareBufferPropertiesANDROID es, PeekChain es) => FromCStruct (AndroidHardwareBufferPropertiesANDROID es)


data AndroidHardwareBufferUsageANDROID

instance ToCStruct AndroidHardwareBufferUsageANDROID
instance Show AndroidHardwareBufferUsageANDROID

instance FromCStruct AndroidHardwareBufferUsageANDROID


data ExternalFormatANDROID

instance ToCStruct ExternalFormatANDROID
instance Show ExternalFormatANDROID

instance FromCStruct ExternalFormatANDROID


data ImportAndroidHardwareBufferInfoANDROID

instance ToCStruct ImportAndroidHardwareBufferInfoANDROID
instance Show ImportAndroidHardwareBufferInfoANDROID

instance FromCStruct ImportAndroidHardwareBufferInfoANDROID


data MemoryGetAndroidHardwareBufferInfoANDROID

instance ToCStruct MemoryGetAndroidHardwareBufferInfoANDROID
instance Show MemoryGetAndroidHardwareBufferInfoANDROID

instance FromCStruct MemoryGetAndroidHardwareBufferInfoANDROID


data AHardwareBuffer

