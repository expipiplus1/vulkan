{-# language CPP #-}
-- No documentation found for Chapter "Handles"
module OpenXR.Core10.Handles  ( Instance(..)
                              , Instance_T
                              , Session(..)
                              , Session_T
                              , ActionSet(..)
                              , ActionSet_T
                              , Action(..)
                              , Action_T
                              , Swapchain(..)
                              , Swapchain_T
                              , Space(..)
                              , Space_T
                              ) where

import Foreign.Ptr (ptrToWordPtr)
import Foreign.Ptr (pattern WordPtr)
import OpenXR.Zero (Zero(..))
import Foreign.Ptr (Ptr)
import OpenXR.Core10.APIConstants (HasObjectType(..))
import OpenXR.Dynamic (InstanceCmds)
import OpenXR.Core10.APIConstants (IsHandle)
import OpenXR.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_ACTION))
import OpenXR.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_ACTION_SET))
import OpenXR.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_INSTANCE))
import OpenXR.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_SESSION))
import OpenXR.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_SPACE))
import OpenXR.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_SWAPCHAIN))
-- | An opaque type for representing pointers to XrInstance handles
data Instance_T
-- | XrInstance - Opaque handle to an instance object
--
-- = Description
--
-- An OpenXR instance is an object that allows an OpenXR application to
-- communicate with an OpenXR runtime. The application accomplishes this
-- communication by calling 'OpenXR.Core10.Instance.createInstance' and
-- receiving a handle to the resulting 'Instance' object.
--
-- The 'Instance' object stores and tracks OpenXR-related application
-- state, without storing any such state in the application’s global
-- address space. This allows the application to create multiple instances
-- as well as safely encapsulate the application’s OpenXR state since this
-- object is opaque to the application. OpenXR runtimes /may/ limit the
-- number of simultaneous 'Instance' objects that may be created and used,
-- but they /must/ support the creation and usage of at least one
-- 'Instance' object per process.
--
-- Physically, this state /may/ be stored in any of the OpenXR loader,
-- OpenXR API layers or the OpenXR runtime components. The exact storage
-- and distribution of this saved state is implementation-dependent, except
-- where indicated by this specification.
--
-- = See Also
--
-- 'OpenXR.Core10.Instance.InstanceCreateInfo',
-- 'OpenXR.Core10.Instance.InstanceProperties',
-- 'OpenXR.Extensions.XR_KHR_convert_timespec_time.convertTimeToTimespecTimeKHR',
-- 'OpenXR.Extensions.XR_KHR_win32_convert_performance_counter_time.convertTimeToWin32PerformanceCounterKHR',
-- 'OpenXR.Extensions.XR_KHR_convert_timespec_time.convertTimespecTimeToTimeKHR',
-- 'OpenXR.Extensions.XR_KHR_win32_convert_performance_counter_time.convertWin32PerformanceCounterToTimeKHR',
-- 'OpenXR.Core10.Input.createActionSet',
-- 'OpenXR.Extensions.XR_EXT_debug_utils.createDebugUtilsMessengerEXT',
-- 'OpenXR.Core10.Instance.createInstance',
-- 'OpenXR.Core10.Device.createSession',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable2.createVulkanDeviceKHR',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable2.createVulkanInstanceKHR',
-- 'OpenXR.Core10.Instance.destroyInstance',
-- 'OpenXR.Core10.Device.enumerateEnvironmentBlendModes',
-- 'OpenXR.Core10.ViewConfigurations.enumerateViewConfigurationViews',
-- 'OpenXR.Core10.ViewConfigurations.enumerateViewConfigurations',
-- 'OpenXR.Extensions.XR_KHR_D3D11_enable.getD3D11GraphicsRequirementsKHR',
-- 'OpenXR.Extensions.XR_KHR_D3D12_enable.getD3D12GraphicsRequirementsKHR',
-- 'OpenXR.Core10.Instance.getInstanceProcAddr',
-- 'OpenXR.Core10.Instance.getInstanceProperties',
-- 'OpenXR.Extensions.XR_KHR_opengl_es_enable.getOpenGLESGraphicsRequirementsKHR',
-- 'OpenXR.Extensions.XR_KHR_opengl_enable.getOpenGLGraphicsRequirementsKHR',
-- 'OpenXR.Core10.Device.getSystem',
-- 'OpenXR.Core10.Device.getSystemProperties',
-- 'OpenXR.Core10.ViewConfigurations.getViewConfigurationProperties',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable.getVulkanDeviceExtensionsKHR',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable2.getVulkanGraphicsDevice2KHR',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable.getVulkanGraphicsDeviceKHR',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable2.getVulkanGraphicsRequirements2KHR',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable.getVulkanGraphicsRequirementsKHR',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable.getVulkanInstanceExtensionsKHR',
-- 'OpenXR.Core10.SemanticPaths.pathToString',
-- 'OpenXR.Core10.Instance.pollEvent',
-- 'OpenXR.Core10.Instance.resultToString',
-- 'OpenXR.Extensions.XR_EXT_debug_utils.setDebugUtilsObjectNameEXT',
-- 'OpenXR.Core10.SemanticPaths.stringToPath',
-- 'OpenXR.Core10.Instance.structureTypeToString',
-- 'OpenXR.Extensions.XR_EXT_debug_utils.submitDebugUtilsMessageEXT',
-- 'OpenXR.Core10.Input.suggestInteractionProfileBindings'
data Instance = Instance
  { instanceHandle :: Ptr Instance_T
  , instanceCmds :: InstanceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero Instance where
  zero = Instance zero zero
instance HasObjectType Instance where
  objectTypeAndHandle (Instance (ptrToWordPtr -> WordPtr h) _) = (OBJECT_TYPE_INSTANCE, fromIntegral h)


-- | An opaque type for representing pointers to XrSession handles
data Session_T
-- | XrSession - Opaque handle to a session object
--
-- = Description
--
-- A session represents an application’s intention to display XR content to
-- the user.
--
-- First, the application creates a session by choosing a
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#system system>
-- and a graphics API and calling 'OpenXR.Core10.Device.createSession',
-- which creates a session in the
-- 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_IDLE' state. The
-- application then sets up an 'OpenXR.Core10.Instance.pollEvent' loop to
-- monitor for session state changes delivered through the
-- 'OpenXR.Core10.OtherTypes.EventDataSessionStateChanged' event. When the
-- runtime determines that the system is ready to start transitioning to
-- this session’s XR content, it notifies the application that its session
-- has moved into the
-- 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_READY' state. When the
-- application is ready to proceed and display its XR content, it calls
-- 'OpenXR.Core10.Session.beginSession' and
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#sync_frame_loop starts its frame loop>,
-- which starts its session running. While the session is running, the
-- application is expected to continuously run its frame loop by calling
-- 'OpenXR.Core10.DisplayTiming.waitFrame',
-- 'OpenXR.Core10.DisplayTiming.beginFrame' and
-- 'OpenXR.Core10.DisplayTiming.endFrame' each frame, to establish
-- synchronization with the runtime. Once the runtime is synchronized with
-- the application’s frame loop and ready to display its frames, the
-- session will move into the
-- 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_SYNCHRONIZED' state. For
-- frames where 'OpenXR.Core10.DisplayTiming.waitFrame' returns an
-- 'OpenXR.Core10.DisplayTiming.FrameState' with @shouldRender@ set to
-- true, the application should render its composition layers and submit
-- them to 'OpenXR.Core10.DisplayTiming.endFrame'. If the application
-- desires to leave a running session, it should call the
-- 'OpenXR.Core10.Session.requestExitSession' function to request that the
-- runtime transition its session to the
-- 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_STOPPING' state as soon
-- as possible. Once the application reaches the
-- 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_STOPPING' state, it can
-- call 'OpenXR.Core10.Session.endSession' to stop the XR session, after
-- which the session will transition through
-- 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_IDLE' to the
-- 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_EXITING' state.
--
-- #session_running#A session is considered __running__ after a successful
-- call to 'OpenXR.Core10.Session.beginSession' and remains running until
-- any call is made to 'OpenXR.Core10.Session.endSession'. Certain
-- functions are only valid to call when a session is running, such as
-- 'OpenXR.Core10.DisplayTiming.waitFrame', or else the
-- 'OpenXR.Core10.Enums.Result.ERROR_SESSION_NOT_RUNNING' error /must/ be
-- returned by the runtime.
--
-- #session_not_running#A session is considered __not running__ before a
-- successful call to 'OpenXR.Core10.Session.beginSession' and becomes not
-- running again after any call is made to
-- 'OpenXR.Core10.Session.endSession'. Certain functions are only valid to
-- call when a session is not running, such as
-- 'OpenXR.Core10.Session.beginSession', or else the
-- 'OpenXR.Core10.Enums.Result.ERROR_SESSION_RUNNING' error /must/ be
-- returned by the runtime.
--
-- If an error is returned from 'OpenXR.Core10.Session.beginSession', the
-- session remains in its current running or not running state. Calling
-- 'OpenXR.Core10.Session.endSession' always transitions a session to the
-- not running state, regardless of any errors returned.
--
-- Only running sessions may become focused sessions that receive XR input.
-- When a session
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#session_not_running is not running>,
-- the application /must/ not submit frames. This is important because
-- without a running session, the runtime no longer has to spend resources
-- on sub-systems (tracking etc.) that are no longer needed by the
-- application.
--
-- = See Also
--
-- 'OpenXR.Core10.OtherTypes.EventDataInteractionProfileChanged',
-- 'OpenXR.Core10.OtherTypes.EventDataReferenceSpaceChangePending',
-- 'OpenXR.Core10.OtherTypes.EventDataSessionStateChanged',
-- 'OpenXR.Extensions.XR_KHR_visibility_mask.EventDataVisibilityMaskChangedKHR',
-- 'OpenXR.Core10.Haptics.applyHapticFeedback',
-- 'OpenXR.Core10.Input.attachSessionActionSets',
-- 'OpenXR.Core10.DisplayTiming.beginFrame',
-- 'OpenXR.Core10.Session.beginSession',
-- 'OpenXR.Core10.Space.createActionSpace',
-- 'OpenXR.Extensions.XR_EXT_hand_tracking.createHandTrackerEXT',
-- 'OpenXR.Core10.Space.createReferenceSpace',
-- 'OpenXR.Core10.Device.createSession',
-- 'OpenXR.Extensions.XR_MSFT_perception_anchor_interop.createSpatialAnchorFromPerceptionAnchorMSFT',
-- 'OpenXR.Extensions.XR_MSFT_spatial_anchor.createSpatialAnchorMSFT',
-- 'OpenXR.Extensions.XR_MSFT_spatial_anchor.createSpatialAnchorSpaceMSFT',
-- 'OpenXR.Extensions.XR_MSFT_spatial_graph_bridge.createSpatialGraphNodeSpaceMSFT',
-- 'OpenXR.Core10.Image.createSwapchain',
-- 'OpenXR.Extensions.XR_KHR_android_surface_swapchain.createSwapchainAndroidSurfaceKHR',
-- 'OpenXR.Core10.Device.destroySession',
-- 'OpenXR.Core10.DisplayTiming.endFrame',
-- 'OpenXR.Core10.Session.endSession',
-- 'OpenXR.Core10.Input.enumerateBoundSourcesForAction',
-- 'OpenXR.Extensions.XR_FB_color_space.enumerateColorSpacesFB',
-- 'OpenXR.Extensions.XR_FB_display_refresh_rate.enumerateDisplayRefreshRatesFB',
-- 'OpenXR.Core10.Space.enumerateReferenceSpaces',
-- 'OpenXR.Core10.Image.enumerateSwapchainFormats',
-- 'OpenXR.Core10.Input.getActionStateBoolean',
-- 'OpenXR.Core10.Input.getActionStateFloat',
-- 'OpenXR.Core10.Input.getActionStatePose',
-- 'OpenXR.Core10.Input.getActionStateVector2f',
-- 'OpenXR.Extensions.XR_MSFT_controller_model.getControllerModelKeyMSFT',
-- 'OpenXR.Extensions.XR_MSFT_controller_model.getControllerModelPropertiesMSFT',
-- 'OpenXR.Extensions.XR_MSFT_controller_model.getControllerModelStateMSFT',
-- 'OpenXR.Core10.Input.getCurrentInteractionProfile',
-- 'OpenXR.Extensions.XR_FB_display_refresh_rate.getDisplayRefreshRateFB',
-- 'OpenXR.Core10.Input.getInputSourceLocalizedName',
-- 'OpenXR.Core10.Space.getReferenceSpaceBoundsRect',
-- 'OpenXR.Extensions.XR_KHR_visibility_mask.getVisibilityMaskKHR',
-- 'OpenXR.Extensions.XR_MSFT_controller_model.loadControllerModelMSFT',
-- 'OpenXR.Core10.DisplayTiming.locateViews',
-- 'OpenXR.Extensions.XR_EXT_performance_settings.perfSettingsSetPerformanceLevelEXT',
-- 'OpenXR.Extensions.XR_FB_display_refresh_rate.requestDisplayRefreshRateFB',
-- 'OpenXR.Core10.Session.requestExitSession',
-- 'OpenXR.Extensions.XR_EXT_debug_utils.sessionBeginDebugUtilsLabelRegionEXT',
-- 'OpenXR.Extensions.XR_EXT_debug_utils.sessionEndDebugUtilsLabelRegionEXT',
-- 'OpenXR.Extensions.XR_EXT_debug_utils.sessionInsertDebugUtilsLabelEXT',
-- 'OpenXR.Extensions.XR_KHR_android_thread_settings.setAndroidApplicationThreadKHR',
-- 'OpenXR.Extensions.XR_FB_color_space.setColorSpaceFB',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#xrSetInputDeviceActiveEXT xrSetInputDeviceActiveEXT>,
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#xrSetInputDeviceLocationEXT xrSetInputDeviceLocationEXT>,
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#xrSetInputDeviceStateBoolEXT xrSetInputDeviceStateBoolEXT>,
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#xrSetInputDeviceStateFloatEXT xrSetInputDeviceStateFloatEXT>,
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#xrSetInputDeviceStateVector2fEXT xrSetInputDeviceStateVector2fEXT>,
-- 'OpenXR.Core10.Haptics.stopHapticFeedback',
-- 'OpenXR.Core10.Input.syncActions',
-- 'OpenXR.Extensions.XR_EXT_thermal_query.thermalGetTemperatureTrendEXT',
-- 'OpenXR.Extensions.XR_MSFT_perception_anchor_interop.tryGetPerceptionAnchorFromSpatialAnchorMSFT',
-- 'OpenXR.Core10.DisplayTiming.waitFrame'
data Session = Session
  { sessionHandle :: Ptr Session_T
  , instanceCmds :: InstanceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero Session where
  zero = Session zero zero
instance HasObjectType Session where
  objectTypeAndHandle (Session (ptrToWordPtr -> WordPtr h) _) = (OBJECT_TYPE_SESSION, fromIntegral h)


-- | An opaque type for representing pointers to XrActionSet handles
data ActionSet_T
-- | XrActionSet - Opaque handle to an action set
--
-- = Description
--
-- Action sets are application-defined collections of actions. They are
-- attached to a given 'Session' with a
-- 'OpenXR.Core10.Input.attachSessionActionSets' call. They are enabled or
-- disabled by the application via 'OpenXR.Core10.Input.syncActions'
-- depending on the current application context. For example, a game may
-- have one set of actions that apply to controlling a character and
-- another set for navigating a menu system. When these actions are grouped
-- into two 'ActionSet' handles they can be selectively enabled and
-- disabled using a single function call.
--
-- Actions are passed a handle to their 'ActionSet' when they are created.
--
-- Action sets are created by calling
-- 'OpenXR.Core10.Input.createActionSet':
--
-- = See Also
--
-- 'OpenXR.Core10.Input.ActiveActionSet',
-- 'OpenXR.Core10.Input.SessionActionSetsAttachInfo',
-- 'OpenXR.Core10.Input.createAction',
-- 'OpenXR.Core10.Input.createActionSet',
-- 'OpenXR.Core10.Input.destroyActionSet'
data ActionSet = ActionSet
  { actionSetHandle :: Ptr ActionSet_T
  , instanceCmds :: InstanceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero ActionSet where
  zero = ActionSet zero zero
instance HasObjectType ActionSet where
  objectTypeAndHandle (ActionSet (ptrToWordPtr -> WordPtr h) _) = (OBJECT_TYPE_ACTION_SET, fromIntegral h)


-- | An opaque type for representing pointers to XrAction handles
data Action_T
-- | XrAction - Opaque handle to an action
--
-- = Description
--
-- Action handles are used to refer to individual actions when retrieving
-- action data, creating action spaces, or sending haptic events.
--
-- = See Also
--
-- 'OpenXR.Core10.Space.ActionSpaceCreateInfo',
-- 'OpenXR.Core10.Input.ActionStateGetInfo',
-- 'OpenXR.Core10.Input.ActionSuggestedBinding',
-- 'OpenXR.Core10.Enums.ActionType.ActionType',
-- 'OpenXR.Core10.Input.BoundSourcesForActionEnumerateInfo',
-- 'OpenXR.Core10.Haptics.HapticActionInfo',
-- 'OpenXR.Extensions.XR_VALVE_analog_threshold.InteractionProfileAnalogThresholdVALVE',
-- 'OpenXR.Core10.Input.createAction', 'OpenXR.Core10.Input.destroyAction'
data Action = Action
  { actionHandle :: Ptr Action_T
  , instanceCmds :: InstanceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero Action where
  zero = Action zero zero
instance HasObjectType Action where
  objectTypeAndHandle (Action (ptrToWordPtr -> WordPtr h) _) = (OBJECT_TYPE_ACTION, fromIntegral h)


-- | An opaque type for representing pointers to XrSwapchain handles
data Swapchain_T
-- | XrSwapchain - Opaque handle to a swapchain object
--
-- = Description
--
-- Normal XR applications will want to present rendered images to the user.
-- To allow this, the runtime provides images organized in swapchains for
-- the application to render into. The runtime /must/ allow applications to
-- create multiple swapchains.
--
-- Swapchain image format support by the runtime is specified by the
-- 'OpenXR.Core10.Image.enumerateSwapchainFormats' function. Runtimes
-- /should/ support @R8G8B8A8@ and @R8G8B8A8@ @sRGB@ formats if possible.
--
-- Swapchain images /can/ be 2D or 2D Array.
--
-- Rendering operations involving composition of submitted layers should be
-- assumed to be internally performed by the runtime in linear color space.
-- Images submitted in sRGB color space must be created using an
-- API-specific sRGB format (e.g. @DXGI_FORMAT_R8G8B8A8_UNORM_SRGB@,
-- @GL_SRGB8_ALPHA8@, @VK_FORMAT_R8G8B8A8_SRGB@) to apply automatic
-- sRGB-to-linear conversion when read by the runtime. All other formats
-- will be treated as linear values.
--
-- Note
--
-- OpenXR applications should avoid submitting linear encoded 8 bit color
-- data (e.g. @DXGI_FORMAT_R8G8B8A8_UNORM@) whenever possible as it may
-- result in color banding.
--
-- Gritz, L. and d’Eon, E. 2007. The Importance of Being Linear. In: H.
-- Nguyen, ed., /GPU Gems 3/. Addison-Wesley Professional.
-- <https://developer.nvidia.com/gpugems/gpugems3/part-iv-image-effects/chapter-24-importance-being-linear>
--
-- Note
--
-- DXGI resources will be created with their associated TYPELESS format,
-- but the runtime will use the application-specified format for reading
-- the data.
--
-- = See Also
--
-- 'OpenXR.Extensions.XR_KHR_composition_layer_cube.CompositionLayerCubeKHR',
-- 'OpenXR.Core10.OtherTypes.SwapchainSubImage',
-- 'OpenXR.Core10.Image.acquireSwapchainImage',
-- 'OpenXR.Core10.Image.createSwapchain',
-- 'OpenXR.Extensions.XR_KHR_android_surface_swapchain.createSwapchainAndroidSurfaceKHR',
-- 'OpenXR.Core10.Image.destroySwapchain',
-- 'OpenXR.Core10.Image.enumerateSwapchainFormats',
-- 'OpenXR.Core10.Image.enumerateSwapchainImages',
-- 'OpenXR.Core10.Image.releaseSwapchainImage',
-- 'OpenXR.Core10.Image.waitSwapchainImage'
data Swapchain = Swapchain
  { swapchainHandle :: Ptr Swapchain_T
  , instanceCmds :: InstanceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero Swapchain where
  zero = Swapchain zero zero
instance HasObjectType Swapchain where
  objectTypeAndHandle (Swapchain (ptrToWordPtr -> WordPtr h) _) = (OBJECT_TYPE_SWAPCHAIN, fromIntegral h)


-- | An opaque type for representing pointers to XrSpace handles
data Space_T
-- | XrSpace - Opaque handle to a space
--
-- = Description
--
-- Spaces are represented by 'Space' handles, which the application creates
-- and then uses in API calls. Whenever an application calls a function
-- that returns coordinates, it provides an 'Space' to specify the frame of
-- reference in which those coordinates will be expressed. Similarly, when
-- providing coordinates to a function, the application specifies which
-- 'Space' the runtime should use to interpret those coordinates.
--
-- OpenXR defines a set of well-known __reference spaces__ that
-- applications use to bootstrap their spatial reasoning. These reference
-- spaces are: @VIEW@, @LOCAL@ and @STAGE@. Each reference space has a
-- well-defined meaning, which establishes where its origin is positioned
-- and how its axes are oriented.
--
-- Runtimes whose tracking systems improve their understanding of the world
-- over time /may/ track spaces independently. For example, even though a
-- @LOCAL@ space and a @STAGE@ space each map their origin to a static
-- position in the world, a runtime with an inside-out tracking system
-- /may/ introduce slight adjustments to the origin of each space on a
-- continuous basis to keep each origin in place.
--
-- Beyond well-known reference spaces, runtimes expose other
-- independently-tracked spaces, such as a pose action space that tracks
-- the pose of a motion controller over time.
--
-- When one or both spaces are tracking a dynamic object, passing in an
-- updated time to 'OpenXR.Core10.Space.locateSpace' each frame will result
-- in an updated relative pose. For example, the location of the left
-- hand’s pose action space in the @STAGE@ reference space will change each
-- frame as the user’s hand moves relative to the stage’s predefined origin
-- on the floor. In other XR APIs, it is common to report the \"pose\" of
-- an object relative to some presumed underlying global space. This API is
-- careful to not explicitly define such an underlying global space,
-- because it does not apply to all systems. Some systems will support no
-- @STAGE@ space, while others may support a @STAGE@ space that switches
-- between various physical stages with dynamic availability. To satisfy
-- this wide variability, \"poses\" are always described as the
-- relationship between two spaces.
--
-- Some devices improve their understanding of the world as the device is
-- used. The location returned by 'OpenXR.Core10.Space.locateSpace' in
-- later frames /may/ change over time, even for spaces that track static
-- objects, as either the target space or base space adjusts its origin.
--
-- Composition layers submitted by the application include an 'Space' for
-- the runtime to use to position that layer over time. Composition layers
-- whose 'Space' is relative to the @VIEW@ reference space are implicitly
-- \"head-locked\", even if they may not be \"display-locked\" for
-- non-head-mounted form factors.
--
-- = See Also
--
-- 'OpenXR.Core10.Space.ActionSpaceCreateInfo',
-- 'OpenXR.Core10.OtherTypes.CompositionLayerBaseHeader',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_cube.CompositionLayerCubeKHR',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_cylinder.CompositionLayerCylinderKHR',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_equirect2.CompositionLayerEquirect2KHR',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_equirect.CompositionLayerEquirectKHR',
-- 'OpenXR.Core10.OtherTypes.CompositionLayerProjection',
-- 'OpenXR.Core10.OtherTypes.CompositionLayerQuad',
-- 'OpenXR.Extensions.XR_EXT_hand_tracking.HandJointsLocateInfoEXT',
-- 'OpenXR.Core10.Space.ReferenceSpaceCreateInfo',
-- 'OpenXR.Extensions.XR_MSFT_spatial_anchor.SpatialAnchorCreateInfoMSFT',
-- 'OpenXR.Core10.DisplayTiming.ViewLocateInfo',
-- 'OpenXR.Core10.Space.createActionSpace',
-- 'OpenXR.Extensions.XR_MSFT_hand_tracking_mesh.createHandMeshSpaceMSFT',
-- 'OpenXR.Core10.Space.createReferenceSpace',
-- 'OpenXR.Extensions.XR_MSFT_spatial_anchor.createSpatialAnchorSpaceMSFT',
-- 'OpenXR.Extensions.XR_MSFT_spatial_graph_bridge.createSpatialGraphNodeSpaceMSFT',
-- 'OpenXR.Core10.Space.destroySpace', 'OpenXR.Core10.Space.locateSpace',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#xrSetInputDeviceLocationEXT xrSetInputDeviceLocationEXT>
data Space = Space
  { spaceHandle :: Ptr Space_T
  , instanceCmds :: InstanceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero Space where
  zero = Space zero zero
instance HasObjectType Space where
  objectTypeAndHandle (Space (ptrToWordPtr -> WordPtr h) _) = (OBJECT_TYPE_SPACE, fromIntegral h)

