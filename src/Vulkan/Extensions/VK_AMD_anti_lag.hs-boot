{-# language CPP #-}
-- | = Name
--
-- VK_AMD_anti_lag - device extension
--
-- = VK_AMD_anti_lag
--
-- [__Name String__]
--     @VK_AMD_anti_lag@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     477
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Stu Smith
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_AMD_anti_lag.adoc VK_AMD_anti_lag>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-06-06
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Stuart Smith, AMD
--
--     -   Arkadiusz Sarwa, AMD
--
-- == Description
--
-- This extension automatically paces the CPU to make sure it does not get
-- too far ahead of the GPU, reducing the latency between inputs received
-- and updates on the screen. Additionally, Anti-Lag+ offers applications
-- the ability to inform the driver when input processing begins, in order
-- to align the timing of display updates, enabling even lower latency
-- between receiving input and displaying on the screen.
--
-- == New Commands
--
-- -   'antiLagUpdateAMD'
--
-- == New Structures
--
-- -   'AntiLagDataAMD'
--
-- -   'AntiLagPresentationInfoAMD'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceAntiLagFeaturesAMD'
--
-- == New Enums
--
-- -   'AntiLagModeAMD'
--
-- -   'AntiLagStageAMD'
--
-- == New Enum Constants
--
-- -   'AMD_ANTI_LAG_EXTENSION_NAME'
--
-- -   'AMD_ANTI_LAG_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ANTI_LAG_DATA_AMD'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ANTI_LAG_PRESENTATION_INFO_AMD'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ANTI_LAG_FEATURES_AMD'
--
-- == Version History
--
-- -   Revision 1, 2024-06-06 (Arkadiusz Sarw)
--
--     -   Initial version
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_AMD_anti_lag Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_anti_lag  ( AntiLagDataAMD
                                          , AntiLagPresentationInfoAMD
                                          , PhysicalDeviceAntiLagFeaturesAMD
                                          ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data AntiLagDataAMD

instance ToCStruct AntiLagDataAMD
instance Show AntiLagDataAMD

instance FromCStruct AntiLagDataAMD


data AntiLagPresentationInfoAMD

instance ToCStruct AntiLagPresentationInfoAMD
instance Show AntiLagPresentationInfoAMD

instance FromCStruct AntiLagPresentationInfoAMD


data PhysicalDeviceAntiLagFeaturesAMD

instance ToCStruct PhysicalDeviceAntiLagFeaturesAMD
instance Show PhysicalDeviceAntiLagFeaturesAMD

instance FromCStruct PhysicalDeviceAntiLagFeaturesAMD

