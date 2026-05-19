{-# language CPP #-}
-- | = Name
--
-- VK_SEC_throttle_hint - device extension
--
-- = VK_SEC_throttle_hint
--
-- [__Name String__]
--     @VK_SEC_throttle_hint@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     675
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
-- [__Contact__]
--
--     -   Ralph Potter <<data:image/png;base64, GitLab>>r_potter
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-04-09
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jonghyuk Eun, Samsung
--
--     -   Minyoung Son, Samsung
--
--     -   Jihyoung Hong, Samsung
--
--     -   Pavan Lanka, Samsung
--
--     -   Ralph Potter, Samsung
--
-- == Description
--
-- This extension is intended to convey throttle hints to the device, but
-- does not specify the throttling behavior or any minimum guarantees. This
-- extension is intended to communicate information from layered API
-- implementations such as ANGLE to internal proprietary system schedulers.
--
-- It has no behavioral implications beyond enabling more intelligent
-- behavior from the system scheduler.
--
-- Application developers should avoid using this extension. It is
-- documented solely for the benefit of tools and layer developers, who may
-- need to manipulate @pNext@ chains that include these structures.
--
-- There is currently no specification language written for this extension.
-- The links to APIs defined by the extension are to stubs that only
-- include generated content such as API declarations and implicit valid
-- usage statements.
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
--     -   'PhysicalDeviceThrottleHintFeaturesSEC'
--
-- -   Extending 'Vulkan.Core10.Queue.SubmitInfo':
--
--     -   'ThrottleHintSubmitInfoSEC'
--
-- == New Enums
--
-- -   'ThrottleHintTypeSEC'
--
-- == New Enum Constants
--
-- -   'SEC_THROTTLE_HINT_EXTENSION_NAME'
--
-- -   'SEC_THROTTLE_HINT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_THROTTLE_HINT_FEATURES_SEC'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_THROTTLE_HINT_SUBMIT_INFO_SEC'
--
-- == Stub API References
--
-- There is currently no specification language written for this type. This
-- section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_SEC_throttle_hint
-- > typedef struct VkPhysicalDeviceThrottleHintFeaturesSEC {
-- >     VkStructureType    sType;
-- >     void*              pNext;
-- >     VkBool32           throttleHint;
-- > } VkPhysicalDeviceThrottleHintFeaturesSEC;
--
-- === Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceThrottleHintFeaturesSEC-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_THROTTLE_HINT_FEATURES_SEC'
--
-- === Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structures>]
--
--     -   'Vulkan.Core10.Device.DeviceCreateInfo'
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
--
-- There is currently no specification language written for this type. This
-- section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_SEC_throttle_hint
-- > typedef struct VkThrottleHintSubmitInfoSEC {
-- >     VkStructureType          sType;
-- >     const void*              pNext;
-- >     VkThrottleHintTypeSEC    throttleHint;
-- > } VkThrottleHintSubmitInfoSEC;
--
-- === Valid Usage (Implicit)
--
-- -   #VUID-VkThrottleHintSubmitInfoSEC-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_THROTTLE_HINT_SUBMIT_INFO_SEC'
--
-- -   #VUID-VkThrottleHintSubmitInfoSEC-throttleHint-parameter#
--     @throttleHint@ /must/ be a valid 'ThrottleHintTypeSEC' value
--
-- === Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structure>]
--
--     -   'Vulkan.Core10.Queue.SubmitInfo'
--
-- There is currently no specification language written for this type. This
-- section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_SEC_throttle_hint
-- > typedef enum VkThrottleHintTypeSEC {
-- >     VK_THROTTLE_HINT_TYPE_DEFAULT_SEC = 0,
-- >     VK_THROTTLE_HINT_TYPE_LOW_SEC = 1,
-- >     VK_THROTTLE_HINT_TYPE_HIGH_SEC = 2,
-- > } VkThrottleHintTypeSEC;
--
-- == Version History
--
-- -   Revision 1, 2026-04-09 (Ralph Potter)
--
--     -   Initial specification
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_SEC_throttle_hint Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_SEC_throttle_hint  ( PhysicalDeviceThrottleHintFeaturesSEC
                                               , ThrottleHintSubmitInfoSEC
                                               ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceThrottleHintFeaturesSEC

instance ToCStruct PhysicalDeviceThrottleHintFeaturesSEC
instance Show PhysicalDeviceThrottleHintFeaturesSEC

instance FromCStruct PhysicalDeviceThrottleHintFeaturesSEC


data ThrottleHintSubmitInfoSEC

instance ToCStruct ThrottleHintSubmitInfoSEC
instance Show ThrottleHintSubmitInfoSEC

instance FromCStruct ThrottleHintSubmitInfoSEC

