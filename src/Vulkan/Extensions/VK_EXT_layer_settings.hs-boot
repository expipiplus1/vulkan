{-# language CPP #-}
-- | = Name
--
-- VK_EXT_layer_settings - instance extension
--
-- == VK_EXT_layer_settings
--
-- [__Name String__]
--     @VK_EXT_layer_settings@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     497
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Christophe Riccio
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_layer_settings] @christophe%0A*Here describe the issue or question you have about the VK_EXT_layer_settings extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_layer_settings.adoc VK_EXT_layer_settings>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-09-23
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Christophe Riccio, LunarG
--
--     -   Mark Lobodzinski, LunarG
--
--     -   Charles Giessen, LunarG
--
--     -   Spencer Fricke, LunarG
--
--     -   Juan Ramos, LunarG
--
--     -   Daniel Rakos, RasterGrid
--
--     -   Shahbaz Youssefi, Google
--
--     -   Lina Versace, Google
--
--     -   Bill Hollings, The Brenwill Workshop
--
--     -   Jon Leech, Khronos
--
--     -   Tom Olson, Arm
--
-- == Description
--
-- This extension provides a mechanism for configuring programmatically
-- through the Vulkan API the behavior of layers.
--
-- This extension provides the 'LayerSettingsCreateInfoEXT' struct that can
-- be included in the @pNext@ chain of the
-- 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo' structure passed
-- as the @pCreateInfo@ parameter of
-- 'Vulkan.Core10.DeviceInitialization.createInstance'.
--
-- The structure contains an array of 'LayerSettingEXT' structure values
-- that configure specific features of layers.
--
-- == Example
--
-- @VK_EXT_layer_settings@ is implemented by the Vulkan Profiles layer.
--
-- It allows the profiles layer tests used by the profiles layer C.I. to
-- programmatically configure the layer for each test without affecting the
-- C.I. environment, allowing to run multiple tests concurrently.
--
-- > const char* profile_file_data = JSON_TEST_FILES_PATH "VP_KHR_roadmap_2022.json";
-- > const char* profile_name_data = "VP_KHR_roadmap_2022";
-- > VkBool32 emulate_portability_data = VK_TRUE;
-- > const char* simulate_capabilities[] = {
-- >     "SIMULATE_API_VERSION_BIT",
-- >     "SIMULATE_FEATURES_BIT",
-- >     "SIMULATE_PROPERTIES_BIT",
-- >     "SIMULATE_EXTENSIONS_BIT",
-- >     "SIMULATE_FORMATS_BIT",
-- >     "SIMULATE_QUEUE_FAMILY_PROPERTIES_BIT"
-- > };
-- > const char* debug_reports[] = {
-- >     "DEBUG_REPORT_ERROR_BIT",
-- >     "DEBUG_REPORT_WARNING_BIT",
-- >     "DEBUG_REPORT_NOTIFICATION_BIT",
-- >     "DEBUG_REPORT_DEBUG_BIT"
-- > };
-- >
-- > const VkLayerSettingEXT settings[] = {
-- >      {kLayerName, kLayerSettingsProfileFile, VK_LAYER_SETTING_TYPE_STRING_EXT, 1, &profile_file_data},
-- >      {kLayerName, kLayerSettingsProfileName, VK_LAYER_SETTING_TYPE_STRING_EXT, 1, &profile_name_data},
-- >      {kLayerName, kLayerSettingsEmulatePortability, VK_LAYER_SETTING_TYPE_BOOL32_EXT, 1, &emulate_portability_data},
-- >      {kLayerName, kLayerSettingsSimulateCapabilities, VK_LAYER_SETTING_TYPE_STRING_EXT,
-- >         static_cast<uint32_t>(std::size(simulate_capabilities)), simulate_capabilities},
-- >      {kLayerName, kLayerSettingsDebugReports, VK_LAYER_SETTING_TYPE_STRING_EXT,
-- >         static_cast<uint32_t>(std::size(debug_reports)), debug_reports}
-- > };
-- >
-- > const VkLayerSettingsCreateInfoEXT layer_settings_create_info{
-- >     VK_STRUCTURE_TYPE_LAYER_SETTINGS_CREATE_INFO_EXT, nullptr,
-- >     static_cast<uint32_t>(std::size(settings)), settings};
-- >
-- > VkInstanceCreateInfo inst_create_info = {};
-- > ...
-- > inst_create_info.pNext = &layer_settings_create_info;
-- > vkCreateInstance(&inst_create_info, nullptr, &_instances);
--
-- Note
--
-- The @VK_EXT_layer_settings@ extension subsumes all the functionality
-- provided in the @VK_EXT_validation_flags@ extension and the
-- @VK_EXT_validation_features@ extension.
--
-- == New Structures
--
-- -   'LayerSettingEXT'
--
-- -   Extending 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo':
--
--     -   'LayerSettingsCreateInfoEXT'
--
-- == New Enums
--
-- -   'LayerSettingTypeEXT'
--
-- == New Enum Constants
--
-- -   'EXT_LAYER_SETTINGS_EXTENSION_NAME'
--
-- -   'EXT_LAYER_SETTINGS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_LAYER_SETTINGS_CREATE_INFO_EXT'
--
-- == Issues
--
-- -   How should application developers figure out the list of available
--     settings?
--
-- This extension does not provide a reflection API for layer settings.
-- Layer settings are described in each layer JSON manifest and the
-- documentation of each layer which implements this extension.
--
-- == Version History
--
-- -   Revision 1, 2020-06-17 (Mark Lobodzinski)
--
--     -   Initial revision for Validation layer internal usages
--
-- -   Revision 2, 2023-09-26 (Christophe Riccio)
--
--     -   Refactor APIs for any layer usages and public release
--
-- == See Also
--
-- 'LayerSettingEXT', 'LayerSettingTypeEXT', 'LayerSettingsCreateInfoEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_layer_settings Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_layer_settings  ( LayerSettingEXT
                                                , LayerSettingsCreateInfoEXT
                                                ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data LayerSettingEXT

instance ToCStruct LayerSettingEXT
instance Show LayerSettingEXT

instance FromCStruct LayerSettingEXT


data LayerSettingsCreateInfoEXT

instance ToCStruct LayerSettingsCreateInfoEXT
instance Show LayerSettingsCreateInfoEXT

instance FromCStruct LayerSettingsCreateInfoEXT

