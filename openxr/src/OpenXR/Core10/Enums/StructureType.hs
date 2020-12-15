{-# language CPP #-}
-- No documentation found for Chapter "StructureType"
module OpenXR.Core10.Enums.StructureType  (StructureType( TYPE_UNKNOWN
                                                        , TYPE_API_LAYER_PROPERTIES
                                                        , TYPE_EXTENSION_PROPERTIES
                                                        , TYPE_INSTANCE_CREATE_INFO
                                                        , TYPE_SYSTEM_GET_INFO
                                                        , TYPE_SYSTEM_PROPERTIES
                                                        , TYPE_VIEW_LOCATE_INFO
                                                        , TYPE_VIEW
                                                        , TYPE_SESSION_CREATE_INFO
                                                        , TYPE_SWAPCHAIN_CREATE_INFO
                                                        , TYPE_SESSION_BEGIN_INFO
                                                        , TYPE_VIEW_STATE
                                                        , TYPE_FRAME_END_INFO
                                                        , TYPE_HAPTIC_VIBRATION
                                                        , TYPE_EVENT_DATA_BUFFER
                                                        , TYPE_EVENT_DATA_INSTANCE_LOSS_PENDING
                                                        , TYPE_EVENT_DATA_SESSION_STATE_CHANGED
                                                        , TYPE_ACTION_STATE_BOOLEAN
                                                        , TYPE_ACTION_STATE_FLOAT
                                                        , TYPE_ACTION_STATE_VECTOR2F
                                                        , TYPE_ACTION_STATE_POSE
                                                        , TYPE_ACTION_SET_CREATE_INFO
                                                        , TYPE_ACTION_CREATE_INFO
                                                        , TYPE_INSTANCE_PROPERTIES
                                                        , TYPE_FRAME_WAIT_INFO
                                                        , TYPE_COMPOSITION_LAYER_PROJECTION
                                                        , TYPE_COMPOSITION_LAYER_QUAD
                                                        , TYPE_REFERENCE_SPACE_CREATE_INFO
                                                        , TYPE_ACTION_SPACE_CREATE_INFO
                                                        , TYPE_EVENT_DATA_REFERENCE_SPACE_CHANGE_PENDING
                                                        , TYPE_VIEW_CONFIGURATION_VIEW
                                                        , TYPE_SPACE_LOCATION
                                                        , TYPE_SPACE_VELOCITY
                                                        , TYPE_FRAME_STATE
                                                        , TYPE_VIEW_CONFIGURATION_PROPERTIES
                                                        , TYPE_FRAME_BEGIN_INFO
                                                        , TYPE_COMPOSITION_LAYER_PROJECTION_VIEW
                                                        , TYPE_EVENT_DATA_EVENTS_LOST
                                                        , TYPE_INTERACTION_PROFILE_SUGGESTED_BINDING
                                                        , TYPE_EVENT_DATA_INTERACTION_PROFILE_CHANGED
                                                        , TYPE_INTERACTION_PROFILE_STATE
                                                        , TYPE_SWAPCHAIN_IMAGE_ACQUIRE_INFO
                                                        , TYPE_SWAPCHAIN_IMAGE_WAIT_INFO
                                                        , TYPE_SWAPCHAIN_IMAGE_RELEASE_INFO
                                                        , TYPE_ACTION_STATE_GET_INFO
                                                        , TYPE_HAPTIC_ACTION_INFO
                                                        , TYPE_SESSION_ACTION_SETS_ATTACH_INFO
                                                        , TYPE_ACTIONS_SYNC_INFO
                                                        , TYPE_BOUND_SOURCES_FOR_ACTION_ENUMERATE_INFO
                                                        , TYPE_INPUT_SOURCE_LOCALIZED_NAME_GET_INFO
                                                        , TYPE_BINDING_MODIFICATIONS_KHR
                                                        , TYPE_SYSTEM_COLOR_SPACE_PROPERTIES_FB
                                                        , TYPE_EVENT_DATA_DISPLAY_REFRESH_RATE_CHANGED_FB
                                                        , TYPE_COMPOSITION_LAYER_EQUIRECT2_KHR
                                                        , TYPE_VULKAN_GRAPHICS_DEVICE_GET_INFO_KHR
                                                        , TYPE_VULKAN_DEVICE_CREATE_INFO_KHR
                                                        , TYPE_VULKAN_INSTANCE_CREATE_INFO_KHR
                                                        , TYPE_LOADER_INIT_INFO_ANDROID_KHR
                                                        , TYPE_INTERACTION_PROFILE_ANALOG_THRESHOLD_VALVE
                                                        , TYPE_HOLOGRAPHIC_WINDOW_ATTACHMENT_MSFT
                                                        , TYPE_VIEW_CONFIGURATION_VIEW_FOV_EPIC
                                                        , TYPE_CONTROLLER_MODEL_STATE_MSFT
                                                        , TYPE_CONTROLLER_MODEL_NODE_STATE_MSFT
                                                        , TYPE_CONTROLLER_MODEL_PROPERTIES_MSFT
                                                        , TYPE_CONTROLLER_MODEL_NODE_PROPERTIES_MSFT
                                                        , TYPE_CONTROLLER_MODEL_KEY_STATE_MSFT
                                                        , TYPE_SECONDARY_VIEW_CONFIGURATION_SWAPCHAIN_CREATE_INFO_MSFT
                                                        , TYPE_SECONDARY_VIEW_CONFIGURATION_LAYER_INFO_MSFT
                                                        , TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_END_INFO_MSFT
                                                        , TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_STATE_MSFT
                                                        , TYPE_SECONDARY_VIEW_CONFIGURATION_STATE_MSFT
                                                        , TYPE_SECONDARY_VIEW_CONFIGURATION_SESSION_BEGIN_INFO_MSFT
                                                        , TYPE_HAND_POSE_TYPE_INFO_MSFT
                                                        , TYPE_HAND_MESH_MSFT
                                                        , TYPE_HAND_MESH_UPDATE_INFO_MSFT
                                                        , TYPE_HAND_MESH_SPACE_CREATE_INFO_MSFT
                                                        , TYPE_SYSTEM_HAND_TRACKING_MESH_PROPERTIES_MSFT
                                                        , TYPE_HAND_JOINT_VELOCITIES_EXT
                                                        , TYPE_HAND_JOINT_LOCATIONS_EXT
                                                        , TYPE_HAND_JOINTS_LOCATE_INFO_EXT
                                                        , TYPE_HAND_TRACKER_CREATE_INFO_EXT
                                                        , TYPE_SYSTEM_HAND_TRACKING_PROPERTIES_EXT
                                                        , TYPE_SPATIAL_GRAPH_NODE_SPACE_CREATE_INFO_MSFT
                                                        , TYPE_GRAPHICS_BINDING_EGL_MNDX
                                                        , TYPE_VIEW_CONFIGURATION_DEPTH_RANGE_EXT
                                                        , TYPE_SPATIAL_ANCHOR_SPACE_CREATE_INFO_MSFT
                                                        , TYPE_SPATIAL_ANCHOR_CREATE_INFO_MSFT
                                                        , TYPE_COMPOSITION_LAYER_COLOR_SCALE_BIAS_KHR
                                                        , TYPE_EVENT_DATA_MAIN_SESSION_VISIBILITY_CHANGED_EXTX
                                                        , TYPE_SESSION_CREATE_INFO_OVERLAY_EXTX
                                                        , TYPE_EVENT_DATA_VISIBILITY_MASK_CHANGED_KHR
                                                        , TYPE_VISIBILITY_MASK_KHR
                                                        , TYPE_EYE_GAZE_SAMPLE_TIME_EXT
                                                        , TYPE_SYSTEM_EYE_GAZE_INTERACTION_PROPERTIES_EXT
                                                        , TYPE_GRAPHICS_REQUIREMENTS_D3D12_KHR
                                                        , TYPE_SWAPCHAIN_IMAGE_D3D12_KHR
                                                        , TYPE_GRAPHICS_BINDING_D3D12_KHR
                                                        , TYPE_GRAPHICS_REQUIREMENTS_D3D11_KHR
                                                        , TYPE_SWAPCHAIN_IMAGE_D3D11_KHR
                                                        , TYPE_GRAPHICS_BINDING_D3D11_KHR
                                                        , TYPE_GRAPHICS_REQUIREMENTS_VULKAN_KHR
                                                        , TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR
                                                        , TYPE_GRAPHICS_BINDING_VULKAN_KHR
                                                        , TYPE_GRAPHICS_REQUIREMENTS_OPENGL_ES_KHR
                                                        , TYPE_SWAPCHAIN_IMAGE_OPENGL_ES_KHR
                                                        , TYPE_GRAPHICS_BINDING_OPENGL_ES_ANDROID_KHR
                                                        , TYPE_GRAPHICS_REQUIREMENTS_OPENGL_KHR
                                                        , TYPE_SWAPCHAIN_IMAGE_OPENGL_KHR
                                                        , TYPE_GRAPHICS_BINDING_OPENGL_WAYLAND_KHR
                                                        , TYPE_GRAPHICS_BINDING_OPENGL_XCB_KHR
                                                        , TYPE_GRAPHICS_BINDING_OPENGL_XLIB_KHR
                                                        , TYPE_GRAPHICS_BINDING_OPENGL_WIN32_KHR
                                                        , TYPE_DEBUG_UTILS_LABEL_EXT
                                                        , TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT
                                                        , TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT
                                                        , TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT
                                                        , TYPE_COMPOSITION_LAYER_EQUIRECT_KHR
                                                        , TYPE_COMPOSITION_LAYER_CYLINDER_KHR
                                                        , TYPE_EVENT_DATA_PERF_SETTINGS_EXT
                                                        , TYPE_VULKAN_SWAPCHAIN_FORMAT_LIST_CREATE_INFO_KHR
                                                        , TYPE_COMPOSITION_LAYER_DEPTH_INFO_KHR
                                                        , TYPE_INSTANCE_CREATE_INFO_ANDROID_KHR
                                                        , TYPE_COMPOSITION_LAYER_CUBE_KHR
                                                        , ..
                                                        )) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import OpenXR.Zero (Zero)
-- | XrStructureType - Values for type members of structs
--
-- = Description
--
-- Most structures containing @type@ members have a value of @type@
-- matching the type of the structure, as described more fully in
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-types Valid Usage for Structure Types>.
--
-- Note that all extension enums begin at the extension enum base of 110
-- (base 10). Each extension is assigned a block of 1000 enums, starting at
-- the enum base and arranged by the extension’s index.
--
-- = See Also
--
-- 'OpenXR.Core10.Input.ActionCreateInfo',
-- 'OpenXR.Core10.Input.ActionSetCreateInfo',
-- 'OpenXR.Core10.Space.ActionSpaceCreateInfo',
-- 'OpenXR.Core10.Input.ActionStateBoolean',
-- 'OpenXR.Core10.Input.ActionStateFloat',
-- 'OpenXR.Core10.Input.ActionStateGetInfo',
-- 'OpenXR.Core10.Input.ActionStatePose',
-- 'OpenXR.Core10.Input.ActionStateVector2f',
-- 'OpenXR.Core10.Input.ActionsSyncInfo',
-- 'OpenXR.Core10.Instance.ApiLayerProperties',
-- 'OpenXR.CStruct.Extends.BaseInStructure',
-- 'OpenXR.CStruct.Extends.BaseOutStructure',
-- 'OpenXR.Extensions.XR_KHR_binding_modification.BindingModificationBaseHeaderKHR',
-- 'OpenXR.Extensions.XR_KHR_binding_modification.BindingModificationsKHR',
-- 'OpenXR.Core10.Input.BoundSourcesForActionEnumerateInfo',
-- 'OpenXR.Core10.OtherTypes.CompositionLayerBaseHeader',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_color_scale_bias.CompositionLayerColorScaleBiasKHR',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_cube.CompositionLayerCubeKHR',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_cylinder.CompositionLayerCylinderKHR',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_depth.CompositionLayerDepthInfoKHR',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_equirect2.CompositionLayerEquirect2KHR',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_equirect.CompositionLayerEquirectKHR',
-- 'OpenXR.Core10.OtherTypes.CompositionLayerProjection',
-- 'OpenXR.Core10.OtherTypes.CompositionLayerProjectionView',
-- 'OpenXR.Core10.OtherTypes.CompositionLayerQuad',
-- 'OpenXR.Extensions.XR_MSFT_controller_model.ControllerModelKeyStateMSFT',
-- 'OpenXR.Extensions.XR_MSFT_controller_model.ControllerModelNodePropertiesMSFT',
-- 'OpenXR.Extensions.XR_MSFT_controller_model.ControllerModelNodeStateMSFT',
-- 'OpenXR.Extensions.XR_MSFT_controller_model.ControllerModelPropertiesMSFT',
-- 'OpenXR.Extensions.XR_MSFT_controller_model.ControllerModelStateMSFT',
-- 'OpenXR.Extensions.XR_EXT_debug_utils.DebugUtilsLabelEXT',
-- 'OpenXR.Extensions.XR_EXT_debug_utils.DebugUtilsMessengerCallbackDataEXT',
-- 'OpenXR.Extensions.XR_EXT_debug_utils.DebugUtilsMessengerCreateInfoEXT',
-- 'OpenXR.Extensions.XR_EXT_debug_utils.DebugUtilsObjectNameInfoEXT',
-- 'OpenXR.Core10.OtherTypes.EventDataBaseHeader',
-- 'OpenXR.Core10.Instance.EventDataBuffer',
-- 'OpenXR.Extensions.XR_FB_display_refresh_rate.EventDataDisplayRefreshRateChangedFB',
-- 'OpenXR.Core10.OtherTypes.EventDataEventsLost',
-- 'OpenXR.Core10.OtherTypes.EventDataInstanceLossPending',
-- 'OpenXR.Core10.OtherTypes.EventDataInteractionProfileChanged',
-- 'OpenXR.Extensions.XR_EXTX_overlay.EventDataMainSessionVisibilityChangedEXTX',
-- 'OpenXR.Extensions.XR_EXT_performance_settings.EventDataPerfSettingsEXT',
-- 'OpenXR.Core10.OtherTypes.EventDataReferenceSpaceChangePending',
-- 'OpenXR.Core10.OtherTypes.EventDataSessionStateChanged',
-- 'OpenXR.Extensions.XR_KHR_visibility_mask.EventDataVisibilityMaskChangedKHR',
-- 'OpenXR.Core10.Instance.ExtensionProperties',
-- 'OpenXR.Extensions.XR_EXT_eye_gaze_interaction.EyeGazeSampleTimeEXT',
-- 'OpenXR.Core10.DisplayTiming.FrameBeginInfo',
-- 'OpenXR.Core10.DisplayTiming.FrameEndInfo',
-- 'OpenXR.Core10.DisplayTiming.FrameState',
-- 'OpenXR.Core10.DisplayTiming.FrameWaitInfo',
-- 'OpenXR.Extensions.XR_KHR_D3D11_enable.GraphicsBindingD3D11KHR',
-- 'OpenXR.Extensions.XR_KHR_D3D12_enable.GraphicsBindingD3D12KHR',
-- 'OpenXR.Extensions.XR_MNDX_egl_enable.GraphicsBindingEGLMNDX',
-- 'OpenXR.Extensions.XR_KHR_opengl_es_enable.GraphicsBindingOpenGLESAndroidKHR',
-- 'OpenXR.Extensions.XR_KHR_opengl_enable.GraphicsBindingOpenGLWaylandKHR',
-- 'OpenXR.Extensions.XR_KHR_opengl_enable.GraphicsBindingOpenGLWin32KHR',
-- 'OpenXR.Extensions.XR_KHR_opengl_enable.GraphicsBindingOpenGLXcbKHR',
-- 'OpenXR.Extensions.XR_KHR_opengl_enable.GraphicsBindingOpenGLXlibKHR',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable.GraphicsBindingVulkanKHR',
-- 'OpenXR.Extensions.XR_KHR_D3D11_enable.GraphicsRequirementsD3D11KHR',
-- 'OpenXR.Extensions.XR_KHR_D3D12_enable.GraphicsRequirementsD3D12KHR',
-- 'OpenXR.Extensions.XR_KHR_opengl_es_enable.GraphicsRequirementsOpenGLESKHR',
-- 'OpenXR.Extensions.XR_KHR_opengl_enable.GraphicsRequirementsOpenGLKHR',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable.GraphicsRequirementsVulkanKHR',
-- 'OpenXR.Extensions.XR_EXT_hand_tracking.HandJointLocationsEXT',
-- 'OpenXR.Extensions.XR_EXT_hand_tracking.HandJointVelocitiesEXT',
-- 'OpenXR.Extensions.XR_EXT_hand_tracking.HandJointsLocateInfoEXT',
-- 'OpenXR.Extensions.XR_MSFT_hand_tracking_mesh.HandMeshMSFT',
-- 'OpenXR.Extensions.XR_MSFT_hand_tracking_mesh.HandMeshSpaceCreateInfoMSFT',
-- 'OpenXR.Extensions.XR_MSFT_hand_tracking_mesh.HandMeshUpdateInfoMSFT',
-- 'OpenXR.Extensions.XR_MSFT_hand_tracking_mesh.HandPoseTypeInfoMSFT',
-- 'OpenXR.Extensions.XR_EXT_hand_tracking.HandTrackerCreateInfoEXT',
-- 'OpenXR.Core10.Haptics.HapticActionInfo',
-- 'OpenXR.Core10.Haptics.HapticBaseHeader',
-- 'OpenXR.Core10.OtherTypes.HapticVibration',
-- 'OpenXR.Extensions.XR_MSFT_holographic_window_attachment.HolographicWindowAttachmentMSFT',
-- 'OpenXR.Core10.Input.InputSourceLocalizedNameGetInfo',
-- 'OpenXR.Core10.Instance.InstanceCreateInfo',
-- 'OpenXR.Extensions.XR_KHR_android_create_instance.InstanceCreateInfoAndroidKHR',
-- 'OpenXR.Core10.Instance.InstanceProperties',
-- 'OpenXR.Extensions.XR_VALVE_analog_threshold.InteractionProfileAnalogThresholdVALVE',
-- 'OpenXR.Core10.Input.InteractionProfileState',
-- 'OpenXR.Core10.Input.InteractionProfileSuggestedBinding',
-- 'OpenXR.Extensions.XR_KHR_loader_init_android.LoaderInitInfoAndroidKHR',
-- 'OpenXR.Extensions.XR_KHR_loader_init.LoaderInitInfoBaseHeaderKHR',
-- 'OpenXR.Core10.Space.ReferenceSpaceCreateInfo',
-- 'OpenXR.Extensions.XR_MSFT_secondary_view_configuration.SecondaryViewConfigurationFrameEndInfoMSFT',
-- 'OpenXR.Extensions.XR_MSFT_secondary_view_configuration.SecondaryViewConfigurationFrameStateMSFT',
-- 'OpenXR.Extensions.XR_MSFT_secondary_view_configuration.SecondaryViewConfigurationLayerInfoMSFT',
-- 'OpenXR.Extensions.XR_MSFT_secondary_view_configuration.SecondaryViewConfigurationSessionBeginInfoMSFT',
-- 'OpenXR.Extensions.XR_MSFT_secondary_view_configuration.SecondaryViewConfigurationStateMSFT',
-- 'OpenXR.Extensions.XR_MSFT_secondary_view_configuration.SecondaryViewConfigurationSwapchainCreateInfoMSFT',
-- 'OpenXR.Core10.Input.SessionActionSetsAttachInfo',
-- 'OpenXR.Core10.Session.SessionBeginInfo',
-- 'OpenXR.Core10.Device.SessionCreateInfo',
-- 'OpenXR.Extensions.XR_EXTX_overlay.SessionCreateInfoOverlayEXTX',
-- 'OpenXR.Core10.Space.SpaceLocation',
-- 'OpenXR.Core10.Space.SpaceVelocity',
-- 'OpenXR.Extensions.XR_MSFT_spatial_anchor.SpatialAnchorCreateInfoMSFT',
-- 'OpenXR.Extensions.XR_MSFT_spatial_anchor.SpatialAnchorSpaceCreateInfoMSFT',
-- 'OpenXR.Extensions.XR_MSFT_spatial_graph_bridge.SpatialGraphNodeSpaceCreateInfoMSFT',
-- 'OpenXR.Core10.Image.SwapchainCreateInfo',
-- 'OpenXR.Core10.Image.SwapchainImageAcquireInfo',
-- 'OpenXR.Core10.Image.SwapchainImageBaseHeader',
-- 'OpenXR.Extensions.XR_KHR_D3D11_enable.SwapchainImageD3D11KHR',
-- 'OpenXR.Extensions.XR_KHR_D3D12_enable.SwapchainImageD3D12KHR',
-- 'OpenXR.Extensions.XR_KHR_opengl_es_enable.SwapchainImageOpenGLESKHR',
-- 'OpenXR.Extensions.XR_KHR_opengl_enable.SwapchainImageOpenGLKHR',
-- 'OpenXR.Core10.Image.SwapchainImageReleaseInfo',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable.SwapchainImageVulkanKHR',
-- 'OpenXR.Core10.Image.SwapchainImageWaitInfo',
-- 'OpenXR.Extensions.XR_FB_color_space.SystemColorSpacePropertiesFB',
-- 'OpenXR.Extensions.XR_EXT_eye_gaze_interaction.SystemEyeGazeInteractionPropertiesEXT',
-- 'OpenXR.Core10.Device.SystemGetInfo',
-- 'OpenXR.Extensions.XR_MSFT_hand_tracking_mesh.SystemHandTrackingMeshPropertiesMSFT',
-- 'OpenXR.Extensions.XR_EXT_hand_tracking.SystemHandTrackingPropertiesEXT',
-- 'OpenXR.Core10.Device.SystemProperties',
-- 'OpenXR.Core10.DisplayTiming.View',
-- 'OpenXR.Extensions.XR_EXT_view_configuration_depth_range.ViewConfigurationDepthRangeEXT',
-- 'OpenXR.Core10.ViewConfigurations.ViewConfigurationProperties',
-- 'OpenXR.Core10.ViewConfigurations.ViewConfigurationView',
-- 'OpenXR.Extensions.XR_EPIC_view_configuration_fov.ViewConfigurationViewFovEPIC',
-- 'OpenXR.Core10.DisplayTiming.ViewLocateInfo',
-- 'OpenXR.Core10.DisplayTiming.ViewState',
-- 'OpenXR.Extensions.XR_KHR_visibility_mask.VisibilityMaskKHR',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable2.VulkanDeviceCreateInfoKHR',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable2.VulkanGraphicsDeviceGetInfoKHR',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable2.VulkanInstanceCreateInfoKHR',
-- 'OpenXR.Extensions.XR_KHR_vulkan_swapchain_format_list.VulkanSwapchainFormatListCreateInfoKHR',
-- 'OpenXR.Core10.Instance.structureTypeToString'
newtype StructureType = StructureType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "XrStructureType" "XR_TYPE_UNKNOWN"
pattern TYPE_UNKNOWN                            = StructureType 0
-- No documentation found for Nested "XrStructureType" "XR_TYPE_API_LAYER_PROPERTIES"
pattern TYPE_API_LAYER_PROPERTIES               = StructureType 1
-- No documentation found for Nested "XrStructureType" "XR_TYPE_EXTENSION_PROPERTIES"
pattern TYPE_EXTENSION_PROPERTIES               = StructureType 2
-- No documentation found for Nested "XrStructureType" "XR_TYPE_INSTANCE_CREATE_INFO"
pattern TYPE_INSTANCE_CREATE_INFO               = StructureType 3
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SYSTEM_GET_INFO"
pattern TYPE_SYSTEM_GET_INFO                    = StructureType 4
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SYSTEM_PROPERTIES"
pattern TYPE_SYSTEM_PROPERTIES                  = StructureType 5
-- No documentation found for Nested "XrStructureType" "XR_TYPE_VIEW_LOCATE_INFO"
pattern TYPE_VIEW_LOCATE_INFO                   = StructureType 6
-- No documentation found for Nested "XrStructureType" "XR_TYPE_VIEW"
pattern TYPE_VIEW                               = StructureType 7
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SESSION_CREATE_INFO"
pattern TYPE_SESSION_CREATE_INFO                = StructureType 8
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SWAPCHAIN_CREATE_INFO"
pattern TYPE_SWAPCHAIN_CREATE_INFO              = StructureType 9
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SESSION_BEGIN_INFO"
pattern TYPE_SESSION_BEGIN_INFO                 = StructureType 10
-- No documentation found for Nested "XrStructureType" "XR_TYPE_VIEW_STATE"
pattern TYPE_VIEW_STATE                         = StructureType 11
-- No documentation found for Nested "XrStructureType" "XR_TYPE_FRAME_END_INFO"
pattern TYPE_FRAME_END_INFO                     = StructureType 12
-- No documentation found for Nested "XrStructureType" "XR_TYPE_HAPTIC_VIBRATION"
pattern TYPE_HAPTIC_VIBRATION                   = StructureType 13
-- No documentation found for Nested "XrStructureType" "XR_TYPE_EVENT_DATA_BUFFER"
pattern TYPE_EVENT_DATA_BUFFER                  = StructureType 16
-- No documentation found for Nested "XrStructureType" "XR_TYPE_EVENT_DATA_INSTANCE_LOSS_PENDING"
pattern TYPE_EVENT_DATA_INSTANCE_LOSS_PENDING   = StructureType 17
-- No documentation found for Nested "XrStructureType" "XR_TYPE_EVENT_DATA_SESSION_STATE_CHANGED"
pattern TYPE_EVENT_DATA_SESSION_STATE_CHANGED   = StructureType 18
-- No documentation found for Nested "XrStructureType" "XR_TYPE_ACTION_STATE_BOOLEAN"
pattern TYPE_ACTION_STATE_BOOLEAN               = StructureType 23
-- No documentation found for Nested "XrStructureType" "XR_TYPE_ACTION_STATE_FLOAT"
pattern TYPE_ACTION_STATE_FLOAT                 = StructureType 24
-- No documentation found for Nested "XrStructureType" "XR_TYPE_ACTION_STATE_VECTOR2F"
pattern TYPE_ACTION_STATE_VECTOR2F              = StructureType 25
-- No documentation found for Nested "XrStructureType" "XR_TYPE_ACTION_STATE_POSE"
pattern TYPE_ACTION_STATE_POSE                  = StructureType 27
-- No documentation found for Nested "XrStructureType" "XR_TYPE_ACTION_SET_CREATE_INFO"
pattern TYPE_ACTION_SET_CREATE_INFO             = StructureType 28
-- No documentation found for Nested "XrStructureType" "XR_TYPE_ACTION_CREATE_INFO"
pattern TYPE_ACTION_CREATE_INFO                 = StructureType 29
-- No documentation found for Nested "XrStructureType" "XR_TYPE_INSTANCE_PROPERTIES"
pattern TYPE_INSTANCE_PROPERTIES                = StructureType 32
-- No documentation found for Nested "XrStructureType" "XR_TYPE_FRAME_WAIT_INFO"
pattern TYPE_FRAME_WAIT_INFO                    = StructureType 33
-- No documentation found for Nested "XrStructureType" "XR_TYPE_COMPOSITION_LAYER_PROJECTION"
pattern TYPE_COMPOSITION_LAYER_PROJECTION       = StructureType 35
-- No documentation found for Nested "XrStructureType" "XR_TYPE_COMPOSITION_LAYER_QUAD"
pattern TYPE_COMPOSITION_LAYER_QUAD             = StructureType 36
-- No documentation found for Nested "XrStructureType" "XR_TYPE_REFERENCE_SPACE_CREATE_INFO"
pattern TYPE_REFERENCE_SPACE_CREATE_INFO        = StructureType 37
-- No documentation found for Nested "XrStructureType" "XR_TYPE_ACTION_SPACE_CREATE_INFO"
pattern TYPE_ACTION_SPACE_CREATE_INFO           = StructureType 38
-- No documentation found for Nested "XrStructureType" "XR_TYPE_EVENT_DATA_REFERENCE_SPACE_CHANGE_PENDING"
pattern TYPE_EVENT_DATA_REFERENCE_SPACE_CHANGE_PENDING = StructureType 40
-- No documentation found for Nested "XrStructureType" "XR_TYPE_VIEW_CONFIGURATION_VIEW"
pattern TYPE_VIEW_CONFIGURATION_VIEW            = StructureType 41
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SPACE_LOCATION"
pattern TYPE_SPACE_LOCATION                     = StructureType 42
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SPACE_VELOCITY"
pattern TYPE_SPACE_VELOCITY                     = StructureType 43
-- No documentation found for Nested "XrStructureType" "XR_TYPE_FRAME_STATE"
pattern TYPE_FRAME_STATE                        = StructureType 44
-- No documentation found for Nested "XrStructureType" "XR_TYPE_VIEW_CONFIGURATION_PROPERTIES"
pattern TYPE_VIEW_CONFIGURATION_PROPERTIES      = StructureType 45
-- No documentation found for Nested "XrStructureType" "XR_TYPE_FRAME_BEGIN_INFO"
pattern TYPE_FRAME_BEGIN_INFO                   = StructureType 46
-- No documentation found for Nested "XrStructureType" "XR_TYPE_COMPOSITION_LAYER_PROJECTION_VIEW"
pattern TYPE_COMPOSITION_LAYER_PROJECTION_VIEW  = StructureType 48
-- No documentation found for Nested "XrStructureType" "XR_TYPE_EVENT_DATA_EVENTS_LOST"
pattern TYPE_EVENT_DATA_EVENTS_LOST             = StructureType 49
-- No documentation found for Nested "XrStructureType" "XR_TYPE_INTERACTION_PROFILE_SUGGESTED_BINDING"
pattern TYPE_INTERACTION_PROFILE_SUGGESTED_BINDING = StructureType 51
-- No documentation found for Nested "XrStructureType" "XR_TYPE_EVENT_DATA_INTERACTION_PROFILE_CHANGED"
pattern TYPE_EVENT_DATA_INTERACTION_PROFILE_CHANGED = StructureType 52
-- No documentation found for Nested "XrStructureType" "XR_TYPE_INTERACTION_PROFILE_STATE"
pattern TYPE_INTERACTION_PROFILE_STATE          = StructureType 53
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SWAPCHAIN_IMAGE_ACQUIRE_INFO"
pattern TYPE_SWAPCHAIN_IMAGE_ACQUIRE_INFO       = StructureType 55
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SWAPCHAIN_IMAGE_WAIT_INFO"
pattern TYPE_SWAPCHAIN_IMAGE_WAIT_INFO          = StructureType 56
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SWAPCHAIN_IMAGE_RELEASE_INFO"
pattern TYPE_SWAPCHAIN_IMAGE_RELEASE_INFO       = StructureType 57
-- No documentation found for Nested "XrStructureType" "XR_TYPE_ACTION_STATE_GET_INFO"
pattern TYPE_ACTION_STATE_GET_INFO              = StructureType 58
-- No documentation found for Nested "XrStructureType" "XR_TYPE_HAPTIC_ACTION_INFO"
pattern TYPE_HAPTIC_ACTION_INFO                 = StructureType 59
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SESSION_ACTION_SETS_ATTACH_INFO"
pattern TYPE_SESSION_ACTION_SETS_ATTACH_INFO    = StructureType 60
-- No documentation found for Nested "XrStructureType" "XR_TYPE_ACTIONS_SYNC_INFO"
pattern TYPE_ACTIONS_SYNC_INFO                  = StructureType 61
-- No documentation found for Nested "XrStructureType" "XR_TYPE_BOUND_SOURCES_FOR_ACTION_ENUMERATE_INFO"
pattern TYPE_BOUND_SOURCES_FOR_ACTION_ENUMERATE_INFO = StructureType 62
-- No documentation found for Nested "XrStructureType" "XR_TYPE_INPUT_SOURCE_LOCALIZED_NAME_GET_INFO"
pattern TYPE_INPUT_SOURCE_LOCALIZED_NAME_GET_INFO = StructureType 63
-- No documentation found for Nested "XrStructureType" "XR_TYPE_BINDING_MODIFICATIONS_KHR"
pattern TYPE_BINDING_MODIFICATIONS_KHR          = StructureType 1000120000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SYSTEM_COLOR_SPACE_PROPERTIES_FB"
pattern TYPE_SYSTEM_COLOR_SPACE_PROPERTIES_FB   = StructureType 1000108000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_EVENT_DATA_DISPLAY_REFRESH_RATE_CHANGED_FB"
pattern TYPE_EVENT_DATA_DISPLAY_REFRESH_RATE_CHANGED_FB = StructureType 1000101000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_COMPOSITION_LAYER_EQUIRECT2_KHR"
pattern TYPE_COMPOSITION_LAYER_EQUIRECT2_KHR    = StructureType 1000091000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_VULKAN_GRAPHICS_DEVICE_GET_INFO_KHR"
pattern TYPE_VULKAN_GRAPHICS_DEVICE_GET_INFO_KHR = StructureType 1000090003
-- No documentation found for Nested "XrStructureType" "XR_TYPE_VULKAN_DEVICE_CREATE_INFO_KHR"
pattern TYPE_VULKAN_DEVICE_CREATE_INFO_KHR      = StructureType 1000090001
-- No documentation found for Nested "XrStructureType" "XR_TYPE_VULKAN_INSTANCE_CREATE_INFO_KHR"
pattern TYPE_VULKAN_INSTANCE_CREATE_INFO_KHR    = StructureType 1000090000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_LOADER_INIT_INFO_ANDROID_KHR"
pattern TYPE_LOADER_INIT_INFO_ANDROID_KHR       = StructureType 1000089000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_INTERACTION_PROFILE_ANALOG_THRESHOLD_VALVE"
pattern TYPE_INTERACTION_PROFILE_ANALOG_THRESHOLD_VALVE = StructureType 1000079000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_HOLOGRAPHIC_WINDOW_ATTACHMENT_MSFT"
pattern TYPE_HOLOGRAPHIC_WINDOW_ATTACHMENT_MSFT = StructureType 1000063000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_VIEW_CONFIGURATION_VIEW_FOV_EPIC"
pattern TYPE_VIEW_CONFIGURATION_VIEW_FOV_EPIC   = StructureType 1000059000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_CONTROLLER_MODEL_STATE_MSFT"
pattern TYPE_CONTROLLER_MODEL_STATE_MSFT        = StructureType 1000055004
-- No documentation found for Nested "XrStructureType" "XR_TYPE_CONTROLLER_MODEL_NODE_STATE_MSFT"
pattern TYPE_CONTROLLER_MODEL_NODE_STATE_MSFT   = StructureType 1000055003
-- No documentation found for Nested "XrStructureType" "XR_TYPE_CONTROLLER_MODEL_PROPERTIES_MSFT"
pattern TYPE_CONTROLLER_MODEL_PROPERTIES_MSFT   = StructureType 1000055002
-- No documentation found for Nested "XrStructureType" "XR_TYPE_CONTROLLER_MODEL_NODE_PROPERTIES_MSFT"
pattern TYPE_CONTROLLER_MODEL_NODE_PROPERTIES_MSFT = StructureType 1000055001
-- No documentation found for Nested "XrStructureType" "XR_TYPE_CONTROLLER_MODEL_KEY_STATE_MSFT"
pattern TYPE_CONTROLLER_MODEL_KEY_STATE_MSFT    = StructureType 1000055000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SECONDARY_VIEW_CONFIGURATION_SWAPCHAIN_CREATE_INFO_MSFT"
pattern TYPE_SECONDARY_VIEW_CONFIGURATION_SWAPCHAIN_CREATE_INFO_MSFT = StructureType 1000053005
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SECONDARY_VIEW_CONFIGURATION_LAYER_INFO_MSFT"
pattern TYPE_SECONDARY_VIEW_CONFIGURATION_LAYER_INFO_MSFT = StructureType 1000053004
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_END_INFO_MSFT"
pattern TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_END_INFO_MSFT = StructureType 1000053003
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_STATE_MSFT"
pattern TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_STATE_MSFT = StructureType 1000053002
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SECONDARY_VIEW_CONFIGURATION_STATE_MSFT"
pattern TYPE_SECONDARY_VIEW_CONFIGURATION_STATE_MSFT = StructureType 1000053001
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SECONDARY_VIEW_CONFIGURATION_SESSION_BEGIN_INFO_MSFT"
pattern TYPE_SECONDARY_VIEW_CONFIGURATION_SESSION_BEGIN_INFO_MSFT = StructureType 1000053000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_HAND_POSE_TYPE_INFO_MSFT"
pattern TYPE_HAND_POSE_TYPE_INFO_MSFT           = StructureType 1000052004
-- No documentation found for Nested "XrStructureType" "XR_TYPE_HAND_MESH_MSFT"
pattern TYPE_HAND_MESH_MSFT                     = StructureType 1000052003
-- No documentation found for Nested "XrStructureType" "XR_TYPE_HAND_MESH_UPDATE_INFO_MSFT"
pattern TYPE_HAND_MESH_UPDATE_INFO_MSFT         = StructureType 1000052002
-- No documentation found for Nested "XrStructureType" "XR_TYPE_HAND_MESH_SPACE_CREATE_INFO_MSFT"
pattern TYPE_HAND_MESH_SPACE_CREATE_INFO_MSFT   = StructureType 1000052001
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SYSTEM_HAND_TRACKING_MESH_PROPERTIES_MSFT"
pattern TYPE_SYSTEM_HAND_TRACKING_MESH_PROPERTIES_MSFT = StructureType 1000052000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_HAND_JOINT_VELOCITIES_EXT"
pattern TYPE_HAND_JOINT_VELOCITIES_EXT          = StructureType 1000051004
-- No documentation found for Nested "XrStructureType" "XR_TYPE_HAND_JOINT_LOCATIONS_EXT"
pattern TYPE_HAND_JOINT_LOCATIONS_EXT           = StructureType 1000051003
-- No documentation found for Nested "XrStructureType" "XR_TYPE_HAND_JOINTS_LOCATE_INFO_EXT"
pattern TYPE_HAND_JOINTS_LOCATE_INFO_EXT        = StructureType 1000051002
-- No documentation found for Nested "XrStructureType" "XR_TYPE_HAND_TRACKER_CREATE_INFO_EXT"
pattern TYPE_HAND_TRACKER_CREATE_INFO_EXT       = StructureType 1000051001
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SYSTEM_HAND_TRACKING_PROPERTIES_EXT"
pattern TYPE_SYSTEM_HAND_TRACKING_PROPERTIES_EXT = StructureType 1000051000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SPATIAL_GRAPH_NODE_SPACE_CREATE_INFO_MSFT"
pattern TYPE_SPATIAL_GRAPH_NODE_SPACE_CREATE_INFO_MSFT = StructureType 1000049000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_GRAPHICS_BINDING_EGL_MNDX"
pattern TYPE_GRAPHICS_BINDING_EGL_MNDX          = StructureType 1000048004
-- No documentation found for Nested "XrStructureType" "XR_TYPE_VIEW_CONFIGURATION_DEPTH_RANGE_EXT"
pattern TYPE_VIEW_CONFIGURATION_DEPTH_RANGE_EXT = StructureType 1000046000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SPATIAL_ANCHOR_SPACE_CREATE_INFO_MSFT"
pattern TYPE_SPATIAL_ANCHOR_SPACE_CREATE_INFO_MSFT = StructureType 1000039001
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SPATIAL_ANCHOR_CREATE_INFO_MSFT"
pattern TYPE_SPATIAL_ANCHOR_CREATE_INFO_MSFT    = StructureType 1000039000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_COMPOSITION_LAYER_COLOR_SCALE_BIAS_KHR"
pattern TYPE_COMPOSITION_LAYER_COLOR_SCALE_BIAS_KHR = StructureType 1000034000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_EVENT_DATA_MAIN_SESSION_VISIBILITY_CHANGED_EXTX"
pattern TYPE_EVENT_DATA_MAIN_SESSION_VISIBILITY_CHANGED_EXTX = StructureType 1000033003
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SESSION_CREATE_INFO_OVERLAY_EXTX"
pattern TYPE_SESSION_CREATE_INFO_OVERLAY_EXTX   = StructureType 1000033000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_EVENT_DATA_VISIBILITY_MASK_CHANGED_KHR"
pattern TYPE_EVENT_DATA_VISIBILITY_MASK_CHANGED_KHR = StructureType 1000031001
-- No documentation found for Nested "XrStructureType" "XR_TYPE_VISIBILITY_MASK_KHR"
pattern TYPE_VISIBILITY_MASK_KHR                = StructureType 1000031000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_EYE_GAZE_SAMPLE_TIME_EXT"
pattern TYPE_EYE_GAZE_SAMPLE_TIME_EXT           = StructureType 1000030001
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SYSTEM_EYE_GAZE_INTERACTION_PROPERTIES_EXT"
pattern TYPE_SYSTEM_EYE_GAZE_INTERACTION_PROPERTIES_EXT = StructureType 1000030000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_GRAPHICS_REQUIREMENTS_D3D12_KHR"
pattern TYPE_GRAPHICS_REQUIREMENTS_D3D12_KHR    = StructureType 1000028002
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SWAPCHAIN_IMAGE_D3D12_KHR"
pattern TYPE_SWAPCHAIN_IMAGE_D3D12_KHR          = StructureType 1000028001
-- No documentation found for Nested "XrStructureType" "XR_TYPE_GRAPHICS_BINDING_D3D12_KHR"
pattern TYPE_GRAPHICS_BINDING_D3D12_KHR         = StructureType 1000028000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_GRAPHICS_REQUIREMENTS_D3D11_KHR"
pattern TYPE_GRAPHICS_REQUIREMENTS_D3D11_KHR    = StructureType 1000027002
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SWAPCHAIN_IMAGE_D3D11_KHR"
pattern TYPE_SWAPCHAIN_IMAGE_D3D11_KHR          = StructureType 1000027001
-- No documentation found for Nested "XrStructureType" "XR_TYPE_GRAPHICS_BINDING_D3D11_KHR"
pattern TYPE_GRAPHICS_BINDING_D3D11_KHR         = StructureType 1000027000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_GRAPHICS_REQUIREMENTS_VULKAN_KHR"
pattern TYPE_GRAPHICS_REQUIREMENTS_VULKAN_KHR   = StructureType 1000025002
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR"
pattern TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR         = StructureType 1000025001
-- No documentation found for Nested "XrStructureType" "XR_TYPE_GRAPHICS_BINDING_VULKAN_KHR"
pattern TYPE_GRAPHICS_BINDING_VULKAN_KHR        = StructureType 1000025000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_GRAPHICS_REQUIREMENTS_OPENGL_ES_KHR"
pattern TYPE_GRAPHICS_REQUIREMENTS_OPENGL_ES_KHR = StructureType 1000024003
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SWAPCHAIN_IMAGE_OPENGL_ES_KHR"
pattern TYPE_SWAPCHAIN_IMAGE_OPENGL_ES_KHR      = StructureType 1000024002
-- No documentation found for Nested "XrStructureType" "XR_TYPE_GRAPHICS_BINDING_OPENGL_ES_ANDROID_KHR"
pattern TYPE_GRAPHICS_BINDING_OPENGL_ES_ANDROID_KHR = StructureType 1000024001
-- No documentation found for Nested "XrStructureType" "XR_TYPE_GRAPHICS_REQUIREMENTS_OPENGL_KHR"
pattern TYPE_GRAPHICS_REQUIREMENTS_OPENGL_KHR   = StructureType 1000023005
-- No documentation found for Nested "XrStructureType" "XR_TYPE_SWAPCHAIN_IMAGE_OPENGL_KHR"
pattern TYPE_SWAPCHAIN_IMAGE_OPENGL_KHR         = StructureType 1000023004
-- No documentation found for Nested "XrStructureType" "XR_TYPE_GRAPHICS_BINDING_OPENGL_WAYLAND_KHR"
pattern TYPE_GRAPHICS_BINDING_OPENGL_WAYLAND_KHR = StructureType 1000023003
-- No documentation found for Nested "XrStructureType" "XR_TYPE_GRAPHICS_BINDING_OPENGL_XCB_KHR"
pattern TYPE_GRAPHICS_BINDING_OPENGL_XCB_KHR    = StructureType 1000023002
-- No documentation found for Nested "XrStructureType" "XR_TYPE_GRAPHICS_BINDING_OPENGL_XLIB_KHR"
pattern TYPE_GRAPHICS_BINDING_OPENGL_XLIB_KHR   = StructureType 1000023001
-- No documentation found for Nested "XrStructureType" "XR_TYPE_GRAPHICS_BINDING_OPENGL_WIN32_KHR"
pattern TYPE_GRAPHICS_BINDING_OPENGL_WIN32_KHR  = StructureType 1000023000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_DEBUG_UTILS_LABEL_EXT"
pattern TYPE_DEBUG_UTILS_LABEL_EXT              = StructureType 1000019003
-- No documentation found for Nested "XrStructureType" "XR_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT"
pattern TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT = StructureType 1000019002
-- No documentation found for Nested "XrStructureType" "XR_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT"
pattern TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT = StructureType 1000019001
-- No documentation found for Nested "XrStructureType" "XR_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT"
pattern TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT   = StructureType 1000019000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_COMPOSITION_LAYER_EQUIRECT_KHR"
pattern TYPE_COMPOSITION_LAYER_EQUIRECT_KHR     = StructureType 1000018000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_COMPOSITION_LAYER_CYLINDER_KHR"
pattern TYPE_COMPOSITION_LAYER_CYLINDER_KHR     = StructureType 1000017000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_EVENT_DATA_PERF_SETTINGS_EXT"
pattern TYPE_EVENT_DATA_PERF_SETTINGS_EXT       = StructureType 1000015000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_VULKAN_SWAPCHAIN_FORMAT_LIST_CREATE_INFO_KHR"
pattern TYPE_VULKAN_SWAPCHAIN_FORMAT_LIST_CREATE_INFO_KHR = StructureType 1000014000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_COMPOSITION_LAYER_DEPTH_INFO_KHR"
pattern TYPE_COMPOSITION_LAYER_DEPTH_INFO_KHR   = StructureType 1000010000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_INSTANCE_CREATE_INFO_ANDROID_KHR"
pattern TYPE_INSTANCE_CREATE_INFO_ANDROID_KHR   = StructureType 1000008000
-- No documentation found for Nested "XrStructureType" "XR_TYPE_COMPOSITION_LAYER_CUBE_KHR"
pattern TYPE_COMPOSITION_LAYER_CUBE_KHR         = StructureType 1000006000
{-# complete TYPE_UNKNOWN,
             TYPE_API_LAYER_PROPERTIES,
             TYPE_EXTENSION_PROPERTIES,
             TYPE_INSTANCE_CREATE_INFO,
             TYPE_SYSTEM_GET_INFO,
             TYPE_SYSTEM_PROPERTIES,
             TYPE_VIEW_LOCATE_INFO,
             TYPE_VIEW,
             TYPE_SESSION_CREATE_INFO,
             TYPE_SWAPCHAIN_CREATE_INFO,
             TYPE_SESSION_BEGIN_INFO,
             TYPE_VIEW_STATE,
             TYPE_FRAME_END_INFO,
             TYPE_HAPTIC_VIBRATION,
             TYPE_EVENT_DATA_BUFFER,
             TYPE_EVENT_DATA_INSTANCE_LOSS_PENDING,
             TYPE_EVENT_DATA_SESSION_STATE_CHANGED,
             TYPE_ACTION_STATE_BOOLEAN,
             TYPE_ACTION_STATE_FLOAT,
             TYPE_ACTION_STATE_VECTOR2F,
             TYPE_ACTION_STATE_POSE,
             TYPE_ACTION_SET_CREATE_INFO,
             TYPE_ACTION_CREATE_INFO,
             TYPE_INSTANCE_PROPERTIES,
             TYPE_FRAME_WAIT_INFO,
             TYPE_COMPOSITION_LAYER_PROJECTION,
             TYPE_COMPOSITION_LAYER_QUAD,
             TYPE_REFERENCE_SPACE_CREATE_INFO,
             TYPE_ACTION_SPACE_CREATE_INFO,
             TYPE_EVENT_DATA_REFERENCE_SPACE_CHANGE_PENDING,
             TYPE_VIEW_CONFIGURATION_VIEW,
             TYPE_SPACE_LOCATION,
             TYPE_SPACE_VELOCITY,
             TYPE_FRAME_STATE,
             TYPE_VIEW_CONFIGURATION_PROPERTIES,
             TYPE_FRAME_BEGIN_INFO,
             TYPE_COMPOSITION_LAYER_PROJECTION_VIEW,
             TYPE_EVENT_DATA_EVENTS_LOST,
             TYPE_INTERACTION_PROFILE_SUGGESTED_BINDING,
             TYPE_EVENT_DATA_INTERACTION_PROFILE_CHANGED,
             TYPE_INTERACTION_PROFILE_STATE,
             TYPE_SWAPCHAIN_IMAGE_ACQUIRE_INFO,
             TYPE_SWAPCHAIN_IMAGE_WAIT_INFO,
             TYPE_SWAPCHAIN_IMAGE_RELEASE_INFO,
             TYPE_ACTION_STATE_GET_INFO,
             TYPE_HAPTIC_ACTION_INFO,
             TYPE_SESSION_ACTION_SETS_ATTACH_INFO,
             TYPE_ACTIONS_SYNC_INFO,
             TYPE_BOUND_SOURCES_FOR_ACTION_ENUMERATE_INFO,
             TYPE_INPUT_SOURCE_LOCALIZED_NAME_GET_INFO,
             TYPE_BINDING_MODIFICATIONS_KHR,
             TYPE_SYSTEM_COLOR_SPACE_PROPERTIES_FB,
             TYPE_EVENT_DATA_DISPLAY_REFRESH_RATE_CHANGED_FB,
             TYPE_COMPOSITION_LAYER_EQUIRECT2_KHR,
             TYPE_VULKAN_GRAPHICS_DEVICE_GET_INFO_KHR,
             TYPE_VULKAN_DEVICE_CREATE_INFO_KHR,
             TYPE_VULKAN_INSTANCE_CREATE_INFO_KHR,
             TYPE_LOADER_INIT_INFO_ANDROID_KHR,
             TYPE_INTERACTION_PROFILE_ANALOG_THRESHOLD_VALVE,
             TYPE_HOLOGRAPHIC_WINDOW_ATTACHMENT_MSFT,
             TYPE_VIEW_CONFIGURATION_VIEW_FOV_EPIC,
             TYPE_CONTROLLER_MODEL_STATE_MSFT,
             TYPE_CONTROLLER_MODEL_NODE_STATE_MSFT,
             TYPE_CONTROLLER_MODEL_PROPERTIES_MSFT,
             TYPE_CONTROLLER_MODEL_NODE_PROPERTIES_MSFT,
             TYPE_CONTROLLER_MODEL_KEY_STATE_MSFT,
             TYPE_SECONDARY_VIEW_CONFIGURATION_SWAPCHAIN_CREATE_INFO_MSFT,
             TYPE_SECONDARY_VIEW_CONFIGURATION_LAYER_INFO_MSFT,
             TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_END_INFO_MSFT,
             TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_STATE_MSFT,
             TYPE_SECONDARY_VIEW_CONFIGURATION_STATE_MSFT,
             TYPE_SECONDARY_VIEW_CONFIGURATION_SESSION_BEGIN_INFO_MSFT,
             TYPE_HAND_POSE_TYPE_INFO_MSFT,
             TYPE_HAND_MESH_MSFT,
             TYPE_HAND_MESH_UPDATE_INFO_MSFT,
             TYPE_HAND_MESH_SPACE_CREATE_INFO_MSFT,
             TYPE_SYSTEM_HAND_TRACKING_MESH_PROPERTIES_MSFT,
             TYPE_HAND_JOINT_VELOCITIES_EXT,
             TYPE_HAND_JOINT_LOCATIONS_EXT,
             TYPE_HAND_JOINTS_LOCATE_INFO_EXT,
             TYPE_HAND_TRACKER_CREATE_INFO_EXT,
             TYPE_SYSTEM_HAND_TRACKING_PROPERTIES_EXT,
             TYPE_SPATIAL_GRAPH_NODE_SPACE_CREATE_INFO_MSFT,
             TYPE_GRAPHICS_BINDING_EGL_MNDX,
             TYPE_VIEW_CONFIGURATION_DEPTH_RANGE_EXT,
             TYPE_SPATIAL_ANCHOR_SPACE_CREATE_INFO_MSFT,
             TYPE_SPATIAL_ANCHOR_CREATE_INFO_MSFT,
             TYPE_COMPOSITION_LAYER_COLOR_SCALE_BIAS_KHR,
             TYPE_EVENT_DATA_MAIN_SESSION_VISIBILITY_CHANGED_EXTX,
             TYPE_SESSION_CREATE_INFO_OVERLAY_EXTX,
             TYPE_EVENT_DATA_VISIBILITY_MASK_CHANGED_KHR,
             TYPE_VISIBILITY_MASK_KHR,
             TYPE_EYE_GAZE_SAMPLE_TIME_EXT,
             TYPE_SYSTEM_EYE_GAZE_INTERACTION_PROPERTIES_EXT,
             TYPE_GRAPHICS_REQUIREMENTS_D3D12_KHR,
             TYPE_SWAPCHAIN_IMAGE_D3D12_KHR,
             TYPE_GRAPHICS_BINDING_D3D12_KHR,
             TYPE_GRAPHICS_REQUIREMENTS_D3D11_KHR,
             TYPE_SWAPCHAIN_IMAGE_D3D11_KHR,
             TYPE_GRAPHICS_BINDING_D3D11_KHR,
             TYPE_GRAPHICS_REQUIREMENTS_VULKAN_KHR,
             TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR,
             TYPE_GRAPHICS_BINDING_VULKAN_KHR,
             TYPE_GRAPHICS_REQUIREMENTS_OPENGL_ES_KHR,
             TYPE_SWAPCHAIN_IMAGE_OPENGL_ES_KHR,
             TYPE_GRAPHICS_BINDING_OPENGL_ES_ANDROID_KHR,
             TYPE_GRAPHICS_REQUIREMENTS_OPENGL_KHR,
             TYPE_SWAPCHAIN_IMAGE_OPENGL_KHR,
             TYPE_GRAPHICS_BINDING_OPENGL_WAYLAND_KHR,
             TYPE_GRAPHICS_BINDING_OPENGL_XCB_KHR,
             TYPE_GRAPHICS_BINDING_OPENGL_XLIB_KHR,
             TYPE_GRAPHICS_BINDING_OPENGL_WIN32_KHR,
             TYPE_DEBUG_UTILS_LABEL_EXT,
             TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT,
             TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT,
             TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT,
             TYPE_COMPOSITION_LAYER_EQUIRECT_KHR,
             TYPE_COMPOSITION_LAYER_CYLINDER_KHR,
             TYPE_EVENT_DATA_PERF_SETTINGS_EXT,
             TYPE_VULKAN_SWAPCHAIN_FORMAT_LIST_CREATE_INFO_KHR,
             TYPE_COMPOSITION_LAYER_DEPTH_INFO_KHR,
             TYPE_INSTANCE_CREATE_INFO_ANDROID_KHR,
             TYPE_COMPOSITION_LAYER_CUBE_KHR :: StructureType #-}

conNameStructureType :: String
conNameStructureType = "StructureType"

enumPrefixStructureType :: String
enumPrefixStructureType = "TYPE_"

showTableStructureType :: [(StructureType, String)]
showTableStructureType =
  [ (TYPE_UNKNOWN                           , "UNKNOWN")
  , (TYPE_API_LAYER_PROPERTIES              , "API_LAYER_PROPERTIES")
  , (TYPE_EXTENSION_PROPERTIES              , "EXTENSION_PROPERTIES")
  , (TYPE_INSTANCE_CREATE_INFO              , "INSTANCE_CREATE_INFO")
  , (TYPE_SYSTEM_GET_INFO                   , "SYSTEM_GET_INFO")
  , (TYPE_SYSTEM_PROPERTIES                 , "SYSTEM_PROPERTIES")
  , (TYPE_VIEW_LOCATE_INFO                  , "VIEW_LOCATE_INFO")
  , (TYPE_VIEW                              , "VIEW")
  , (TYPE_SESSION_CREATE_INFO               , "SESSION_CREATE_INFO")
  , (TYPE_SWAPCHAIN_CREATE_INFO             , "SWAPCHAIN_CREATE_INFO")
  , (TYPE_SESSION_BEGIN_INFO                , "SESSION_BEGIN_INFO")
  , (TYPE_VIEW_STATE                        , "VIEW_STATE")
  , (TYPE_FRAME_END_INFO                    , "FRAME_END_INFO")
  , (TYPE_HAPTIC_VIBRATION                  , "HAPTIC_VIBRATION")
  , (TYPE_EVENT_DATA_BUFFER                 , "EVENT_DATA_BUFFER")
  , (TYPE_EVENT_DATA_INSTANCE_LOSS_PENDING  , "EVENT_DATA_INSTANCE_LOSS_PENDING")
  , (TYPE_EVENT_DATA_SESSION_STATE_CHANGED  , "EVENT_DATA_SESSION_STATE_CHANGED")
  , (TYPE_ACTION_STATE_BOOLEAN              , "ACTION_STATE_BOOLEAN")
  , (TYPE_ACTION_STATE_FLOAT                , "ACTION_STATE_FLOAT")
  , (TYPE_ACTION_STATE_VECTOR2F             , "ACTION_STATE_VECTOR2F")
  , (TYPE_ACTION_STATE_POSE                 , "ACTION_STATE_POSE")
  , (TYPE_ACTION_SET_CREATE_INFO            , "ACTION_SET_CREATE_INFO")
  , (TYPE_ACTION_CREATE_INFO                , "ACTION_CREATE_INFO")
  , (TYPE_INSTANCE_PROPERTIES               , "INSTANCE_PROPERTIES")
  , (TYPE_FRAME_WAIT_INFO                   , "FRAME_WAIT_INFO")
  , (TYPE_COMPOSITION_LAYER_PROJECTION      , "COMPOSITION_LAYER_PROJECTION")
  , (TYPE_COMPOSITION_LAYER_QUAD            , "COMPOSITION_LAYER_QUAD")
  , (TYPE_REFERENCE_SPACE_CREATE_INFO       , "REFERENCE_SPACE_CREATE_INFO")
  , (TYPE_ACTION_SPACE_CREATE_INFO          , "ACTION_SPACE_CREATE_INFO")
  , (TYPE_EVENT_DATA_REFERENCE_SPACE_CHANGE_PENDING, "EVENT_DATA_REFERENCE_SPACE_CHANGE_PENDING")
  , (TYPE_VIEW_CONFIGURATION_VIEW           , "VIEW_CONFIGURATION_VIEW")
  , (TYPE_SPACE_LOCATION                    , "SPACE_LOCATION")
  , (TYPE_SPACE_VELOCITY                    , "SPACE_VELOCITY")
  , (TYPE_FRAME_STATE                       , "FRAME_STATE")
  , (TYPE_VIEW_CONFIGURATION_PROPERTIES     , "VIEW_CONFIGURATION_PROPERTIES")
  , (TYPE_FRAME_BEGIN_INFO                  , "FRAME_BEGIN_INFO")
  , (TYPE_COMPOSITION_LAYER_PROJECTION_VIEW , "COMPOSITION_LAYER_PROJECTION_VIEW")
  , (TYPE_EVENT_DATA_EVENTS_LOST            , "EVENT_DATA_EVENTS_LOST")
  , (TYPE_INTERACTION_PROFILE_SUGGESTED_BINDING, "INTERACTION_PROFILE_SUGGESTED_BINDING")
  , (TYPE_EVENT_DATA_INTERACTION_PROFILE_CHANGED, "EVENT_DATA_INTERACTION_PROFILE_CHANGED")
  , (TYPE_INTERACTION_PROFILE_STATE         , "INTERACTION_PROFILE_STATE")
  , (TYPE_SWAPCHAIN_IMAGE_ACQUIRE_INFO      , "SWAPCHAIN_IMAGE_ACQUIRE_INFO")
  , (TYPE_SWAPCHAIN_IMAGE_WAIT_INFO         , "SWAPCHAIN_IMAGE_WAIT_INFO")
  , (TYPE_SWAPCHAIN_IMAGE_RELEASE_INFO      , "SWAPCHAIN_IMAGE_RELEASE_INFO")
  , (TYPE_ACTION_STATE_GET_INFO             , "ACTION_STATE_GET_INFO")
  , (TYPE_HAPTIC_ACTION_INFO                , "HAPTIC_ACTION_INFO")
  , (TYPE_SESSION_ACTION_SETS_ATTACH_INFO   , "SESSION_ACTION_SETS_ATTACH_INFO")
  , (TYPE_ACTIONS_SYNC_INFO                 , "ACTIONS_SYNC_INFO")
  , (TYPE_BOUND_SOURCES_FOR_ACTION_ENUMERATE_INFO, "BOUND_SOURCES_FOR_ACTION_ENUMERATE_INFO")
  , (TYPE_INPUT_SOURCE_LOCALIZED_NAME_GET_INFO, "INPUT_SOURCE_LOCALIZED_NAME_GET_INFO")
  , (TYPE_BINDING_MODIFICATIONS_KHR         , "BINDING_MODIFICATIONS_KHR")
  , (TYPE_SYSTEM_COLOR_SPACE_PROPERTIES_FB  , "SYSTEM_COLOR_SPACE_PROPERTIES_FB")
  , (TYPE_EVENT_DATA_DISPLAY_REFRESH_RATE_CHANGED_FB, "EVENT_DATA_DISPLAY_REFRESH_RATE_CHANGED_FB")
  , (TYPE_COMPOSITION_LAYER_EQUIRECT2_KHR   , "COMPOSITION_LAYER_EQUIRECT2_KHR")
  , (TYPE_VULKAN_GRAPHICS_DEVICE_GET_INFO_KHR, "VULKAN_GRAPHICS_DEVICE_GET_INFO_KHR")
  , (TYPE_VULKAN_DEVICE_CREATE_INFO_KHR     , "VULKAN_DEVICE_CREATE_INFO_KHR")
  , (TYPE_VULKAN_INSTANCE_CREATE_INFO_KHR   , "VULKAN_INSTANCE_CREATE_INFO_KHR")
  , (TYPE_LOADER_INIT_INFO_ANDROID_KHR      , "LOADER_INIT_INFO_ANDROID_KHR")
  , (TYPE_INTERACTION_PROFILE_ANALOG_THRESHOLD_VALVE, "INTERACTION_PROFILE_ANALOG_THRESHOLD_VALVE")
  , (TYPE_HOLOGRAPHIC_WINDOW_ATTACHMENT_MSFT, "HOLOGRAPHIC_WINDOW_ATTACHMENT_MSFT")
  , (TYPE_VIEW_CONFIGURATION_VIEW_FOV_EPIC  , "VIEW_CONFIGURATION_VIEW_FOV_EPIC")
  , (TYPE_CONTROLLER_MODEL_STATE_MSFT       , "CONTROLLER_MODEL_STATE_MSFT")
  , (TYPE_CONTROLLER_MODEL_NODE_STATE_MSFT  , "CONTROLLER_MODEL_NODE_STATE_MSFT")
  , (TYPE_CONTROLLER_MODEL_PROPERTIES_MSFT  , "CONTROLLER_MODEL_PROPERTIES_MSFT")
  , (TYPE_CONTROLLER_MODEL_NODE_PROPERTIES_MSFT, "CONTROLLER_MODEL_NODE_PROPERTIES_MSFT")
  , (TYPE_CONTROLLER_MODEL_KEY_STATE_MSFT   , "CONTROLLER_MODEL_KEY_STATE_MSFT")
  , ( TYPE_SECONDARY_VIEW_CONFIGURATION_SWAPCHAIN_CREATE_INFO_MSFT
    , "SECONDARY_VIEW_CONFIGURATION_SWAPCHAIN_CREATE_INFO_MSFT"
    )
  , (TYPE_SECONDARY_VIEW_CONFIGURATION_LAYER_INFO_MSFT, "SECONDARY_VIEW_CONFIGURATION_LAYER_INFO_MSFT")
  , (TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_END_INFO_MSFT, "SECONDARY_VIEW_CONFIGURATION_FRAME_END_INFO_MSFT")
  , (TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_STATE_MSFT, "SECONDARY_VIEW_CONFIGURATION_FRAME_STATE_MSFT")
  , (TYPE_SECONDARY_VIEW_CONFIGURATION_STATE_MSFT     , "SECONDARY_VIEW_CONFIGURATION_STATE_MSFT")
  , (TYPE_SECONDARY_VIEW_CONFIGURATION_SESSION_BEGIN_INFO_MSFT, "SECONDARY_VIEW_CONFIGURATION_SESSION_BEGIN_INFO_MSFT")
  , (TYPE_HAND_POSE_TYPE_INFO_MSFT                    , "HAND_POSE_TYPE_INFO_MSFT")
  , (TYPE_HAND_MESH_MSFT                              , "HAND_MESH_MSFT")
  , (TYPE_HAND_MESH_UPDATE_INFO_MSFT                  , "HAND_MESH_UPDATE_INFO_MSFT")
  , (TYPE_HAND_MESH_SPACE_CREATE_INFO_MSFT            , "HAND_MESH_SPACE_CREATE_INFO_MSFT")
  , (TYPE_SYSTEM_HAND_TRACKING_MESH_PROPERTIES_MSFT   , "SYSTEM_HAND_TRACKING_MESH_PROPERTIES_MSFT")
  , (TYPE_HAND_JOINT_VELOCITIES_EXT                   , "HAND_JOINT_VELOCITIES_EXT")
  , (TYPE_HAND_JOINT_LOCATIONS_EXT                    , "HAND_JOINT_LOCATIONS_EXT")
  , (TYPE_HAND_JOINTS_LOCATE_INFO_EXT                 , "HAND_JOINTS_LOCATE_INFO_EXT")
  , (TYPE_HAND_TRACKER_CREATE_INFO_EXT                , "HAND_TRACKER_CREATE_INFO_EXT")
  , (TYPE_SYSTEM_HAND_TRACKING_PROPERTIES_EXT         , "SYSTEM_HAND_TRACKING_PROPERTIES_EXT")
  , (TYPE_SPATIAL_GRAPH_NODE_SPACE_CREATE_INFO_MSFT   , "SPATIAL_GRAPH_NODE_SPACE_CREATE_INFO_MSFT")
  , (TYPE_GRAPHICS_BINDING_EGL_MNDX                   , "GRAPHICS_BINDING_EGL_MNDX")
  , (TYPE_VIEW_CONFIGURATION_DEPTH_RANGE_EXT          , "VIEW_CONFIGURATION_DEPTH_RANGE_EXT")
  , (TYPE_SPATIAL_ANCHOR_SPACE_CREATE_INFO_MSFT       , "SPATIAL_ANCHOR_SPACE_CREATE_INFO_MSFT")
  , (TYPE_SPATIAL_ANCHOR_CREATE_INFO_MSFT             , "SPATIAL_ANCHOR_CREATE_INFO_MSFT")
  , (TYPE_COMPOSITION_LAYER_COLOR_SCALE_BIAS_KHR      , "COMPOSITION_LAYER_COLOR_SCALE_BIAS_KHR")
  , (TYPE_EVENT_DATA_MAIN_SESSION_VISIBILITY_CHANGED_EXTX, "EVENT_DATA_MAIN_SESSION_VISIBILITY_CHANGED_EXTX")
  , (TYPE_SESSION_CREATE_INFO_OVERLAY_EXTX            , "SESSION_CREATE_INFO_OVERLAY_EXTX")
  , (TYPE_EVENT_DATA_VISIBILITY_MASK_CHANGED_KHR      , "EVENT_DATA_VISIBILITY_MASK_CHANGED_KHR")
  , (TYPE_VISIBILITY_MASK_KHR                         , "VISIBILITY_MASK_KHR")
  , (TYPE_EYE_GAZE_SAMPLE_TIME_EXT                    , "EYE_GAZE_SAMPLE_TIME_EXT")
  , (TYPE_SYSTEM_EYE_GAZE_INTERACTION_PROPERTIES_EXT  , "SYSTEM_EYE_GAZE_INTERACTION_PROPERTIES_EXT")
  , (TYPE_GRAPHICS_REQUIREMENTS_D3D12_KHR             , "GRAPHICS_REQUIREMENTS_D3D12_KHR")
  , (TYPE_SWAPCHAIN_IMAGE_D3D12_KHR                   , "SWAPCHAIN_IMAGE_D3D12_KHR")
  , (TYPE_GRAPHICS_BINDING_D3D12_KHR                  , "GRAPHICS_BINDING_D3D12_KHR")
  , (TYPE_GRAPHICS_REQUIREMENTS_D3D11_KHR             , "GRAPHICS_REQUIREMENTS_D3D11_KHR")
  , (TYPE_SWAPCHAIN_IMAGE_D3D11_KHR                   , "SWAPCHAIN_IMAGE_D3D11_KHR")
  , (TYPE_GRAPHICS_BINDING_D3D11_KHR                  , "GRAPHICS_BINDING_D3D11_KHR")
  , (TYPE_GRAPHICS_REQUIREMENTS_VULKAN_KHR            , "GRAPHICS_REQUIREMENTS_VULKAN_KHR")
  , (TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR                  , "SWAPCHAIN_IMAGE_VULKAN_KHR")
  , (TYPE_GRAPHICS_BINDING_VULKAN_KHR                 , "GRAPHICS_BINDING_VULKAN_KHR")
  , (TYPE_GRAPHICS_REQUIREMENTS_OPENGL_ES_KHR         , "GRAPHICS_REQUIREMENTS_OPENGL_ES_KHR")
  , (TYPE_SWAPCHAIN_IMAGE_OPENGL_ES_KHR               , "SWAPCHAIN_IMAGE_OPENGL_ES_KHR")
  , (TYPE_GRAPHICS_BINDING_OPENGL_ES_ANDROID_KHR      , "GRAPHICS_BINDING_OPENGL_ES_ANDROID_KHR")
  , (TYPE_GRAPHICS_REQUIREMENTS_OPENGL_KHR            , "GRAPHICS_REQUIREMENTS_OPENGL_KHR")
  , (TYPE_SWAPCHAIN_IMAGE_OPENGL_KHR                  , "SWAPCHAIN_IMAGE_OPENGL_KHR")
  , (TYPE_GRAPHICS_BINDING_OPENGL_WAYLAND_KHR         , "GRAPHICS_BINDING_OPENGL_WAYLAND_KHR")
  , (TYPE_GRAPHICS_BINDING_OPENGL_XCB_KHR             , "GRAPHICS_BINDING_OPENGL_XCB_KHR")
  , (TYPE_GRAPHICS_BINDING_OPENGL_XLIB_KHR            , "GRAPHICS_BINDING_OPENGL_XLIB_KHR")
  , (TYPE_GRAPHICS_BINDING_OPENGL_WIN32_KHR           , "GRAPHICS_BINDING_OPENGL_WIN32_KHR")
  , (TYPE_DEBUG_UTILS_LABEL_EXT                       , "DEBUG_UTILS_LABEL_EXT")
  , (TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT       , "DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT")
  , (TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT     , "DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT")
  , (TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT            , "DEBUG_UTILS_OBJECT_NAME_INFO_EXT")
  , (TYPE_COMPOSITION_LAYER_EQUIRECT_KHR              , "COMPOSITION_LAYER_EQUIRECT_KHR")
  , (TYPE_COMPOSITION_LAYER_CYLINDER_KHR              , "COMPOSITION_LAYER_CYLINDER_KHR")
  , (TYPE_EVENT_DATA_PERF_SETTINGS_EXT                , "EVENT_DATA_PERF_SETTINGS_EXT")
  , (TYPE_VULKAN_SWAPCHAIN_FORMAT_LIST_CREATE_INFO_KHR, "VULKAN_SWAPCHAIN_FORMAT_LIST_CREATE_INFO_KHR")
  , (TYPE_COMPOSITION_LAYER_DEPTH_INFO_KHR            , "COMPOSITION_LAYER_DEPTH_INFO_KHR")
  , (TYPE_INSTANCE_CREATE_INFO_ANDROID_KHR            , "INSTANCE_CREATE_INFO_ANDROID_KHR")
  , (TYPE_COMPOSITION_LAYER_CUBE_KHR                  , "COMPOSITION_LAYER_CUBE_KHR")
  ]

instance Show StructureType where
  showsPrec = enumShowsPrec enumPrefixStructureType
                            showTableStructureType
                            conNameStructureType
                            (\(StructureType x) -> x)
                            (showsPrec 11)

instance Read StructureType where
  readPrec = enumReadPrec enumPrefixStructureType showTableStructureType conNameStructureType StructureType

