{-# language CPP #-}
-- | = Name
--
-- VK_EXT_host_query_reset - device extension
--
-- == VK_EXT_host_query_reset
--
-- [__Name String__]
--     @VK_EXT_host_query_reset@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     262
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Bas Nieuwenhuizen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_host_query_reset] @BNieuwenhuizen%0A<<Here describe the issue or question you have about the VK_EXT_host_query_reset extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-03-06
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.2 Core
--
-- [__Contributors__]
--
--     -   Bas Nieuwenhuizen, Google
--
--     -   Jason Ekstrand, Intel
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
-- == Description
--
-- This extension adds a new function to reset queries from the host.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2, with
-- the EXT suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Commands
--
-- -   'resetQueryPoolEXT'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceHostQueryResetFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_HOST_QUERY_RESET_EXTENSION_NAME'
--
-- -   'EXT_HOST_QUERY_RESET_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2019-03-12 (Bas Nieuwenhuizen)
--
--     -   Initial draft
--
-- = See Also
--
-- 'PhysicalDeviceHostQueryResetFeaturesEXT', 'resetQueryPoolEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_query_reset Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_host_query_reset  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT
                                                  , resetQueryPoolEXT
                                                  , PhysicalDeviceHostQueryResetFeaturesEXT
                                                  , EXT_HOST_QUERY_RESET_SPEC_VERSION
                                                  , pattern EXT_HOST_QUERY_RESET_SPEC_VERSION
                                                  , EXT_HOST_QUERY_RESET_EXTENSION_NAME
                                                  , pattern EXT_HOST_QUERY_RESET_EXTENSION_NAME
                                                  ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset (resetQueryPool)
import Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset (PhysicalDeviceHostQueryResetFeatures)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES


-- No documentation found for TopLevel "vkResetQueryPoolEXT"
resetQueryPoolEXT = resetQueryPool


-- No documentation found for TopLevel "VkPhysicalDeviceHostQueryResetFeaturesEXT"
type PhysicalDeviceHostQueryResetFeaturesEXT = PhysicalDeviceHostQueryResetFeatures


type EXT_HOST_QUERY_RESET_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_HOST_QUERY_RESET_SPEC_VERSION"
pattern EXT_HOST_QUERY_RESET_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_HOST_QUERY_RESET_SPEC_VERSION = 1


type EXT_HOST_QUERY_RESET_EXTENSION_NAME = "VK_EXT_host_query_reset"

-- No documentation found for TopLevel "VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME"
pattern EXT_HOST_QUERY_RESET_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_HOST_QUERY_RESET_EXTENSION_NAME = "VK_EXT_host_query_reset"

