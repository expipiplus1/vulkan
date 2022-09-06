{-# language CPP #-}
-- | = Name
--
-- VK_SEC_amigo_profiling - device extension
--
-- == VK_SEC_amigo_profiling
--
-- [__Name String__]
--     @VK_SEC_amigo_profiling@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     486
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Contact__]
--
--     -   Ralph Potter <<data:image/png;base64, GitLab>>r_potter
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-07-29
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Ralph Potter, Samsung
--
--     -   Sangrak Oh, Samsung
--
--     -   Jinku Kang, Samsung
--
-- == Description
--
-- This extension is intended to communicate information from layered API
-- implementations such as ANGLE to internal proprietary system schedulers.
-- It has no behavioural implications beyond enabling more intelligent
-- behaviour from the system scheduler.
--
-- Application developers should avoid using this extension. It is
-- documented solely for the benefit of tools and layer developers, who may
-- need to manipulate @pNext@ chains that include these structures.
--
-- Note
--
-- There is currently no specification language written for this extension.
-- The links to APIs defined by the extension are to stubs that only
-- include generated content such as API declarations and implicit valid
-- usage statements.
--
-- Note
--
-- This extension is only intended for use in specific embedded
-- environments with known implementation details, and is therefore
-- undocumented.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceAmigoProfilingFeaturesSEC'
--
-- -   Extending 'Vulkan.Core10.Queue.SubmitInfo':
--
--     -   'AmigoProfilingSubmitInfoSEC'
--
-- == New Enum Constants
--
-- -   'SEC_AMIGO_PROFILING_EXTENSION_NAME'
--
-- -   'SEC_AMIGO_PROFILING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_AMIGO_PROFILING_SUBMIT_INFO_SEC'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_AMIGO_PROFILING_FEATURES_SEC'
--
-- == Stub API References
--
-- There is currently no specification language written for this type. This
-- section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_SEC_amigo_profiling
-- > typedef struct VkPhysicalDeviceAmigoProfilingFeaturesSEC {
-- >     VkStructureType    sType;
-- >     void*              pNext;
-- >     VkBool32           amigoProfiling;
-- > } VkPhysicalDeviceAmigoProfilingFeaturesSEC;
--
-- === Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceAmigoProfilingFeaturesSEC-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_AMIGO_PROFILING_FEATURES_SEC'
--
-- There is currently no specification language written for this type. This
-- section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_SEC_amigo_profiling
-- > typedef struct VkAmigoProfilingSubmitInfoSEC {
-- >     VkStructureType    sType;
-- >     const void*        pNext;
-- >     uint64_t           firstDrawTimestamp;
-- >     uint64_t           swapBufferTimestamp;
-- > } VkAmigoProfilingSubmitInfoSEC;
--
-- === Valid Usage (Implicit)
--
-- -   #VUID-VkAmigoProfilingSubmitInfoSEC-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_AMIGO_PROFILING_SUBMIT_INFO_SEC'
--
-- == Version History
--
-- -   Revision 1, 2022-07-29 (Ralph Potter)
--
--     -   Initial specification
--
-- == See Also
--
-- 'AmigoProfilingSubmitInfoSEC', 'PhysicalDeviceAmigoProfilingFeaturesSEC'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_SEC_amigo_profiling Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_SEC_amigo_profiling  ( AmigoProfilingSubmitInfoSEC
                                                 , PhysicalDeviceAmigoProfilingFeaturesSEC
                                                 ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data AmigoProfilingSubmitInfoSEC

instance ToCStruct AmigoProfilingSubmitInfoSEC
instance Show AmigoProfilingSubmitInfoSEC

instance FromCStruct AmigoProfilingSubmitInfoSEC


data PhysicalDeviceAmigoProfilingFeaturesSEC

instance ToCStruct PhysicalDeviceAmigoProfilingFeaturesSEC
instance Show PhysicalDeviceAmigoProfilingFeaturesSEC

instance FromCStruct PhysicalDeviceAmigoProfilingFeaturesSEC

