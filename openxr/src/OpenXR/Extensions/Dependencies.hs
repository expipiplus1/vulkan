{-# language CPP #-}
-- No documentation found for Chapter "Dependencies"
module OpenXR.Extensions.Dependencies  ( extensionDependencies
                                       , extensionCoreRequirement
                                       ) where

import Data.Word (Word32)
import Data.ByteString (ByteString)
import OpenXR.NamedType ((:::))
import OpenXR.Version (Version)
import OpenXR.Core10 (pattern API_VERSION_1_0)
import OpenXR.Extensions.XR_EXT_hand_tracking (pattern EXT_HAND_TRACKING_EXTENSION_NAME)
import OpenXR.Extensions.XR_KHR_loader_init_android (pattern KHR_LOADER_INIT_ANDROID_EXTENSION_NAME)
import OpenXR.Extensions.XR_KHR_loader_init (pattern KHR_LOADER_INIT_EXTENSION_NAME)
import OpenXR.Extensions.XR_KHR_vulkan_enable (pattern KHR_VULKAN_ENABLE_EXTENSION_NAME)
import OpenXR.Extensions.XR_KHR_vulkan_swapchain_format_list (pattern KHR_VULKAN_SWAPCHAIN_FORMAT_LIST_EXTENSION_NAME)
import OpenXR.Extensions.XR_MSFT_first_person_observer (pattern MSFT_FIRST_PERSON_OBSERVER_EXTENSION_NAME)
import OpenXR.Extensions.XR_MSFT_hand_tracking_mesh (pattern MSFT_HAND_TRACKING_MESH_EXTENSION_NAME)
import OpenXR.Extensions.XR_MSFT_perception_anchor_interop (pattern MSFT_PERCEPTION_ANCHOR_INTEROP_EXTENSION_NAME)
import OpenXR.Extensions.XR_MSFT_secondary_view_configuration (pattern MSFT_SECONDARY_VIEW_CONFIGURATION_EXTENSION_NAME)
import OpenXR.Extensions.XR_MSFT_spatial_anchor (pattern MSFT_SPATIAL_ANCHOR_EXTENSION_NAME)
-- | The set of other extensions required to use this extension
extensionDependencies :: ("extensionName" ::: ByteString) -> [ByteString]
extensionDependencies = \case
  KHR_VULKAN_SWAPCHAIN_FORMAT_LIST_EXTENSION_NAME -> [KHR_VULKAN_ENABLE_EXTENSION_NAME]
  MSFT_HAND_TRACKING_MESH_EXTENSION_NAME -> [EXT_HAND_TRACKING_EXTENSION_NAME]
  MSFT_FIRST_PERSON_OBSERVER_EXTENSION_NAME -> [MSFT_SECONDARY_VIEW_CONFIGURATION_EXTENSION_NAME]
  MSFT_PERCEPTION_ANCHOR_INTEROP_EXTENSION_NAME -> [MSFT_SPATIAL_ANCHOR_EXTENSION_NAME]
  KHR_LOADER_INIT_ANDROID_EXTENSION_NAME -> [KHR_LOADER_INIT_EXTENSION_NAME]
  _ -> []

-- | The minimum required API version to use this extension
extensionCoreRequirement :: ("extensionName" ::: ByteString) -> Version
extensionCoreRequirement = \case
  _ -> API_VERSION_1_0
