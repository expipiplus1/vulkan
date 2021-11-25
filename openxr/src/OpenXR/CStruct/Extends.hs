{-# language CPP #-}
-- No documentation found for Chapter "Extends"
module OpenXR.CStruct.Extends  ( BaseInStructure(..)
                               , BaseOutStructure(..)
                               , Extends
                               , PeekChain(..)
                               , PokeChain(..)
                               , Chain
                               , Extendss
                               , SomeStruct(..)
                               , extendSomeStruct
                               , withSomeStruct
                               , withSomeCStruct
                               , peekSomeCStruct
                               , pokeSomeCStruct
                               , forgetExtensions
                               , Extensible(..)
                               , pattern (::&)
                               , pattern (:&)
                               , SomeChild(..)
                               , withSomeChild
                               , lowerChildPointer
                               , Inherits
                               , Inheritable(..)
                               ) where

import Data.Maybe (fromMaybe)
import Type.Reflection (typeRep)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Base (join)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (Ptr)
import GHC.TypeLits (ErrorMessage(..))
import GHC.TypeLits (TypeError)
import Data.Kind (Constraint)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import {-# SOURCE #-} OpenXR.Core10.Input (ActionCreateInfo)
import {-# SOURCE #-} OpenXR.Core10.Input (ActionSetCreateInfo)
import {-# SOURCE #-} OpenXR.Core10.Space (ActionSpaceCreateInfo)
import {-# SOURCE #-} OpenXR.Core10.Input (ActionStateBoolean)
import {-# SOURCE #-} OpenXR.Core10.Input (ActionStateFloat)
import {-# SOURCE #-} OpenXR.Core10.Input (ActionStateGetInfo)
import {-# SOURCE #-} OpenXR.Core10.Input (ActionStatePose)
import {-# SOURCE #-} OpenXR.Core10.Input (ActionStateVector2f)
import {-# SOURCE #-} OpenXR.Core10.Input (ActionSuggestedBinding)
import {-# SOURCE #-} OpenXR.Core10.Input (ActionsSyncInfo)
import {-# SOURCE #-} OpenXR.Core10.Input (ActiveActionSet)
import {-# SOURCE #-} OpenXR.Core10.Instance (ApiLayerProperties)
import {-# SOURCE #-} OpenXR.Core10.Instance (ApplicationInfo)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_binding_modification (BindingModificationBaseHeaderKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_binding_modification (BindingModificationsKHR)
import {-# SOURCE #-} OpenXR.Core10.Input (BoundSourcesForActionEnumerateInfo)
import {-# SOURCE #-} OpenXR.Core10.OtherTypes (Color4f)
import {-# SOURCE #-} OpenXR.Core10.OtherTypes (CompositionLayerBaseHeader)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_composition_layer_color_scale_bias (CompositionLayerColorScaleBiasKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_composition_layer_cube (CompositionLayerCubeKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_composition_layer_cylinder (CompositionLayerCylinderKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_composition_layer_depth (CompositionLayerDepthInfoKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_composition_layer_equirect2 (CompositionLayerEquirect2KHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_composition_layer_equirect (CompositionLayerEquirectKHR)
import {-# SOURCE #-} OpenXR.Core10.OtherTypes (CompositionLayerProjection)
import {-# SOURCE #-} OpenXR.Core10.OtherTypes (CompositionLayerProjectionView)
import {-# SOURCE #-} OpenXR.Core10.OtherTypes (CompositionLayerQuad)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_controller_model (ControllerModelKeyStateMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_controller_model (ControllerModelNodePropertiesMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_controller_model (ControllerModelNodeStateMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_controller_model (ControllerModelPropertiesMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_controller_model (ControllerModelStateMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_debug_utils (DebugUtilsLabelEXT)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_debug_utils (DebugUtilsMessengerCallbackDataEXT)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_debug_utils (DebugUtilsMessengerCreateInfoEXT)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_debug_utils (DebugUtilsObjectNameInfoEXT)
import {-# SOURCE #-} OpenXR.Core10.OtherTypes (EventDataBaseHeader)
import {-# SOURCE #-} OpenXR.Core10.Instance (EventDataBuffer)
import {-# SOURCE #-} OpenXR.Extensions.XR_FB_display_refresh_rate (EventDataDisplayRefreshRateChangedFB)
import {-# SOURCE #-} OpenXR.Core10.OtherTypes (EventDataEventsLost)
import {-# SOURCE #-} OpenXR.Core10.OtherTypes (EventDataInstanceLossPending)
import {-# SOURCE #-} OpenXR.Core10.OtherTypes (EventDataInteractionProfileChanged)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXTX_overlay (EventDataMainSessionVisibilityChangedEXTX)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_performance_settings (EventDataPerfSettingsEXT)
import {-# SOURCE #-} OpenXR.Core10.OtherTypes (EventDataReferenceSpaceChangePending)
import {-# SOURCE #-} OpenXR.Core10.OtherTypes (EventDataSessionStateChanged)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_visibility_mask (EventDataVisibilityMaskChangedKHR)
import {-# SOURCE #-} OpenXR.Core10.Instance (ExtensionProperties)
import {-# SOURCE #-} OpenXR.Core10.FundamentalTypes (Extent2Df)
import {-# SOURCE #-} OpenXR.Core10.FundamentalTypes (Extent2Di)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_eye_gaze_interaction (EyeGazeSampleTimeEXT)
import {-# SOURCE #-} OpenXR.Core10.OtherTypes (Fovf)
import {-# SOURCE #-} OpenXR.Core10.DisplayTiming (FrameBeginInfo)
import {-# SOURCE #-} OpenXR.Core10.DisplayTiming (FrameEndInfo)
import {-# SOURCE #-} OpenXR.Core10.DisplayTiming (FrameState)
import {-# SOURCE #-} OpenXR.Core10.DisplayTiming (FrameWaitInfo)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_D3D11_enable (GraphicsBindingD3D11KHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_D3D12_enable (GraphicsBindingD3D12KHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_MNDX_egl_enable (GraphicsBindingEGLMNDX)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_opengl_es_enable (GraphicsBindingOpenGLESAndroidKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_opengl_enable (GraphicsBindingOpenGLWaylandKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_opengl_enable (GraphicsBindingOpenGLWin32KHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_opengl_enable (GraphicsBindingOpenGLXcbKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_opengl_enable (GraphicsBindingOpenGLXlibKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_vulkan_enable (GraphicsBindingVulkanKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_D3D11_enable (GraphicsRequirementsD3D11KHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_D3D12_enable (GraphicsRequirementsD3D12KHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_opengl_es_enable (GraphicsRequirementsOpenGLESKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_opengl_enable (GraphicsRequirementsOpenGLKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_vulkan_enable (GraphicsRequirementsVulkanKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_hand_tracking (HandJointLocationEXT)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_hand_tracking (HandJointLocationsEXT)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_hand_tracking (HandJointVelocitiesEXT)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_hand_tracking (HandJointVelocityEXT)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_hand_tracking (HandJointsLocateInfoEXT)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_hand_tracking_mesh (HandMeshIndexBufferMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_hand_tracking_mesh (HandMeshMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_hand_tracking_mesh (HandMeshSpaceCreateInfoMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_hand_tracking_mesh (HandMeshUpdateInfoMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_hand_tracking_mesh (HandMeshVertexBufferMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_hand_tracking_mesh (HandMeshVertexMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_hand_tracking_mesh (HandPoseTypeInfoMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_hand_tracking (HandTrackerCreateInfoEXT)
import {-# SOURCE #-} OpenXR.Core10.Haptics (HapticActionInfo)
import {-# SOURCE #-} OpenXR.Core10.Haptics (HapticBaseHeader)
import {-# SOURCE #-} OpenXR.Core10.OtherTypes (HapticVibration)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_holographic_window_attachment (HolographicWindowAttachmentMSFT)
import {-# SOURCE #-} OpenXR.Core10.Input (InputSourceLocalizedNameGetInfo)
import {-# SOURCE #-} OpenXR.Core10.Instance (InstanceCreateInfo)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_android_create_instance (InstanceCreateInfoAndroidKHR)
import {-# SOURCE #-} OpenXR.Core10.Instance (InstanceProperties)
import {-# SOURCE #-} OpenXR.Extensions.XR_VALVE_analog_threshold (InteractionProfileAnalogThresholdVALVE)
import {-# SOURCE #-} OpenXR.Core10.Input (InteractionProfileState)
import {-# SOURCE #-} OpenXR.Core10.Input (InteractionProfileSuggestedBinding)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_loader_init_android (LoaderInitInfoAndroidKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_loader_init (LoaderInitInfoBaseHeaderKHR)
import {-# SOURCE #-} OpenXR.Core10.FundamentalTypes (Offset2Df)
import {-# SOURCE #-} OpenXR.Core10.FundamentalTypes (Offset2Di)
import {-# SOURCE #-} OpenXR.Core10.Space (Posef)
import {-# SOURCE #-} OpenXR.Core10.Space (Quaternionf)
import {-# SOURCE #-} OpenXR.Core10.FundamentalTypes (Rect2Df)
import {-# SOURCE #-} OpenXR.Core10.FundamentalTypes (Rect2Di)
import {-# SOURCE #-} OpenXR.Core10.Space (ReferenceSpaceCreateInfo)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_secondary_view_configuration (SecondaryViewConfigurationFrameEndInfoMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_secondary_view_configuration (SecondaryViewConfigurationFrameStateMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_secondary_view_configuration (SecondaryViewConfigurationLayerInfoMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_secondary_view_configuration (SecondaryViewConfigurationSessionBeginInfoMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_secondary_view_configuration (SecondaryViewConfigurationStateMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_secondary_view_configuration (SecondaryViewConfigurationSwapchainCreateInfoMSFT)
import {-# SOURCE #-} OpenXR.Core10.Input (SessionActionSetsAttachInfo)
import {-# SOURCE #-} OpenXR.Core10.Session (SessionBeginInfo)
import {-# SOURCE #-} OpenXR.Core10.Device (SessionCreateInfo)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXTX_overlay (SessionCreateInfoOverlayEXTX)
import {-# SOURCE #-} OpenXR.Core10.Space (SpaceLocation)
import {-# SOURCE #-} OpenXR.Core10.Space (SpaceVelocity)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_spatial_anchor (SpatialAnchorCreateInfoMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_spatial_anchor (SpatialAnchorSpaceCreateInfoMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_spatial_graph_bridge (SpatialGraphNodeSpaceCreateInfoMSFT)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Enums.StructureType (StructureType(..))
import {-# SOURCE #-} OpenXR.Core10.Image (SwapchainCreateInfo)
import {-# SOURCE #-} OpenXR.Core10.Image (SwapchainImageAcquireInfo)
import {-# SOURCE #-} OpenXR.Core10.Image (SwapchainImageBaseHeader)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_D3D11_enable (SwapchainImageD3D11KHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_D3D12_enable (SwapchainImageD3D12KHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_opengl_es_enable (SwapchainImageOpenGLESKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_opengl_enable (SwapchainImageOpenGLKHR)
import {-# SOURCE #-} OpenXR.Core10.Image (SwapchainImageReleaseInfo)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_vulkan_enable (SwapchainImageVulkanKHR)
import {-# SOURCE #-} OpenXR.Core10.Image (SwapchainImageWaitInfo)
import {-# SOURCE #-} OpenXR.Core10.OtherTypes (SwapchainSubImage)
import {-# SOURCE #-} OpenXR.Extensions.XR_FB_color_space (SystemColorSpacePropertiesFB)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_eye_gaze_interaction (SystemEyeGazeInteractionPropertiesEXT)
import {-# SOURCE #-} OpenXR.Core10.Device (SystemGetInfo)
import {-# SOURCE #-} OpenXR.Core10.Device (SystemGraphicsProperties)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_hand_tracking_mesh (SystemHandTrackingMeshPropertiesMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_hand_tracking (SystemHandTrackingPropertiesEXT)
import {-# SOURCE #-} OpenXR.Core10.Device (SystemProperties)
import {-# SOURCE #-} OpenXR.Core10.Device (SystemTrackingProperties)
import {-# SOURCE #-} OpenXR.Core10.Input (Vector2f)
import {-# SOURCE #-} OpenXR.Core10.Space (Vector3f)
import {-# SOURCE #-} OpenXR.Core10.OtherTypes (Vector4f)
import {-# SOURCE #-} OpenXR.Core10.DisplayTiming (View)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_view_configuration_depth_range (ViewConfigurationDepthRangeEXT)
import {-# SOURCE #-} OpenXR.Core10.ViewConfigurations (ViewConfigurationProperties)
import {-# SOURCE #-} OpenXR.Core10.ViewConfigurations (ViewConfigurationView)
import {-# SOURCE #-} OpenXR.Extensions.XR_EPIC_view_configuration_fov (ViewConfigurationViewFovEPIC)
import {-# SOURCE #-} OpenXR.Core10.DisplayTiming (ViewLocateInfo)
import {-# SOURCE #-} OpenXR.Core10.DisplayTiming (ViewState)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_visibility_mask (VisibilityMaskKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_vulkan_enable2 (VulkanDeviceCreateInfoKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_vulkan_enable2 (VulkanGraphicsDeviceGetInfoKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_vulkan_enable2 (VulkanInstanceCreateInfoKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_vulkan_swapchain_format_list (VulkanSwapchainFormatListCreateInfoKHR)
-- | XrBaseInStructure - Convenience type for iterating (read only)
--
-- == Member Descriptions
--
-- = Description
--
-- 'BaseInStructure' can be used to facilitate iterating through a
-- read-only structure pointer chain.
--
-- = See Also
--
-- 'BaseInStructure', 'BaseOutStructure',
-- 'OpenXR.Core10.Enums.StructureType.StructureType'
data BaseInStructure = BaseInStructure
  { -- | @type@ is the 'OpenXR.Core10.Enums.StructureType.StructureType' of this
    -- structure. This base structure itself has no associated
    -- 'OpenXR.Core10.Enums.StructureType.StructureType' value.
    type' :: StructureType
  , -- | @next@ is @NULL@ or a pointer to the next structure in a structure
    -- chain. No such structures are defined in core OpenXR.
    next :: Ptr BaseInStructure
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BaseInStructure)
#endif
deriving instance Show BaseInStructure

instance ToCStruct BaseInStructure where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BaseInStructure{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (type')
    poke ((p `plusPtr` 8 :: Ptr (Ptr BaseInStructure))) (next)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (zero)
    poke ((p `plusPtr` 8 :: Ptr (Ptr BaseInStructure))) (zero)
    f

instance FromCStruct BaseInStructure where
  peekCStruct p = do
    type' <- peek @StructureType ((p `plusPtr` 0 :: Ptr StructureType))
    next <- peek @(Ptr BaseInStructure) ((p `plusPtr` 8 :: Ptr (Ptr BaseInStructure)))
    pure $ BaseInStructure
             type' next

instance Storable BaseInStructure where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BaseInStructure where
  zero = BaseInStructure
           zero
           zero


-- | XrBaseOutStructure - Convenience type for iterating (mutable)
--
-- == Member Descriptions
--
-- = Description
--
-- 'BaseOutStructure' can be used to facilitate iterating through a
-- structure pointer chain that returns data back to the application.
--
-- = See Also
--
-- 'BaseInStructure', 'BaseOutStructure',
-- 'OpenXR.Core10.Enums.StructureType.StructureType'
data BaseOutStructure = BaseOutStructure
  { -- | @type@ is the 'OpenXR.Core10.Enums.StructureType.StructureType' of this
    -- structure. This base structure itself has no associated
    -- 'OpenXR.Core10.Enums.StructureType.StructureType' value.
    type' :: StructureType
  , -- | @next@ is @NULL@ or a pointer to the next structure in a structure
    -- chain. No such structures are defined in core OpenXR.
    next :: Ptr BaseOutStructure
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BaseOutStructure)
#endif
deriving instance Show BaseOutStructure

instance ToCStruct BaseOutStructure where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BaseOutStructure{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (type')
    poke ((p `plusPtr` 8 :: Ptr (Ptr BaseOutStructure))) (next)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (zero)
    poke ((p `plusPtr` 8 :: Ptr (Ptr BaseOutStructure))) (zero)
    f

instance FromCStruct BaseOutStructure where
  peekCStruct p = do
    type' <- peek @StructureType ((p `plusPtr` 0 :: Ptr StructureType))
    next <- peek @(Ptr BaseOutStructure) ((p `plusPtr` 8 :: Ptr (Ptr BaseOutStructure)))
    pure $ BaseOutStructure
             type' next

instance Storable BaseOutStructure where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BaseOutStructure where
  zero = BaseOutStructure
           zero
           zero


type family Extends (a :: [Type] -> Type) (b :: Type) :: Constraint where
  Extends CompositionLayerBaseHeader CompositionLayerColorScaleBiasKHR = ()
  Extends CompositionLayerProjectionView CompositionLayerDepthInfoKHR = ()
  Extends FrameEndInfo SecondaryViewConfigurationFrameEndInfoMSFT = ()
  Extends FrameState SecondaryViewConfigurationFrameStateMSFT = ()
  Extends HandJointLocationsEXT HandJointVelocitiesEXT = ()
  Extends HandTrackerCreateInfoEXT HandPoseTypeInfoMSFT = ()
  Extends InstanceCreateInfo InstanceCreateInfoAndroidKHR = ()
  Extends InstanceCreateInfo DebugUtilsMessengerCreateInfoEXT = ()
  Extends InteractionProfileSuggestedBinding InteractionProfileAnalogThresholdVALVE = ()
  Extends InteractionProfileSuggestedBinding BindingModificationsKHR = ()
  Extends SessionBeginInfo SecondaryViewConfigurationSessionBeginInfoMSFT = ()
  Extends SessionCreateInfo GraphicsBindingOpenGLWin32KHR = ()
  Extends SessionCreateInfo GraphicsBindingOpenGLXlibKHR = ()
  Extends SessionCreateInfo GraphicsBindingOpenGLXcbKHR = ()
  Extends SessionCreateInfo GraphicsBindingOpenGLWaylandKHR = ()
  Extends SessionCreateInfo GraphicsBindingD3D11KHR = ()
  Extends SessionCreateInfo GraphicsBindingD3D12KHR = ()
  Extends SessionCreateInfo GraphicsBindingOpenGLESAndroidKHR = ()
  Extends SessionCreateInfo GraphicsBindingVulkanKHR = ()
  Extends SessionCreateInfo SessionCreateInfoOverlayEXTX = ()
  Extends SessionCreateInfo GraphicsBindingEGLMNDX = ()
  Extends SessionCreateInfo HolographicWindowAttachmentMSFT = ()
  Extends SpaceLocation SpaceVelocity = ()
  Extends SpaceLocation EyeGazeSampleTimeEXT = ()
  Extends SwapchainCreateInfo SecondaryViewConfigurationSwapchainCreateInfoMSFT = ()
  Extends SystemProperties SystemEyeGazeInteractionPropertiesEXT = ()
  Extends SystemProperties SystemHandTrackingPropertiesEXT = ()
  Extends SystemProperties SystemHandTrackingMeshPropertiesMSFT = ()
  Extends ViewConfigurationView ViewConfigurationDepthRangeEXT = ()
  Extends ViewConfigurationView ViewConfigurationViewFovEPIC = ()
  Extends a b = TypeError (ShowType a :<>: Text " is not extended by " :<>: ShowType b)

data SomeStruct (a :: [Type] -> Type) where
  SomeStruct
    :: forall a es
     . (Extendss a es, PokeChain es, Show (Chain es))
    => a es
    -> SomeStruct a

deriving instance (forall es. Show (Chain es) => Show (a es)) => Show (SomeStruct a)

-- | The constraint is so on this instance to encourage type inference
instance Zero (a '[]) => Zero (SomeStruct a) where
  zero = SomeStruct (zero :: a '[])

-- | Forget which extensions a pointed-to struct has by casting the pointer
forgetExtensions :: Ptr (a es) -> Ptr (SomeStruct a)
forgetExtensions = castPtr

-- | Add an extension to the beginning of the struct chain
--
-- This can be used to optionally extend structs based on some condition (for
-- example, an extension or layer being available)
extendSomeStruct
  :: (Extensible a, Extends a e, ToCStruct e, Show e)
  => e
  -> SomeStruct a
  -> SomeStruct a
extendSomeStruct e (SomeStruct a) = SomeStruct (setNext a (e, getNext a))

-- | Consume a 'SomeStruct' value
withSomeStruct
  :: forall a b
   . SomeStruct a
  -> (forall es . (Extendss a es, PokeChain es, Show (Chain es)) => a es -> b)
  -> b
withSomeStruct (SomeStruct s) f = f s

-- | Write the C representation of some extended @a@ and use the pointer,
-- the pointer must not be returned from the continuation.
withSomeCStruct
  :: forall a b
   . (forall es . (Extendss a es, PokeChain es) => ToCStruct (a es))
  => SomeStruct a
  -> (forall es . (Extendss a es, PokeChain es) => Ptr (a es) -> IO b)
  -> IO b
withSomeCStruct s f = withSomeStruct s (`withCStruct` f)

-- | Given some memory for the head of the chain, allocate and poke the
-- tail and run an action.
pokeSomeCStruct
  :: (forall es . (Extendss a es, PokeChain es) => ToCStruct (a es))
  => Ptr (SomeStruct a)
  -- ^ Pointer to some memory at least the size of the head of the struct
  -- chain.
  -> SomeStruct a
  -- ^ The struct to poke
  -> IO b
  -- ^ Computation to run while the poked tail is valid
  -> IO b
pokeSomeCStruct p (SomeStruct s) = pokeCStruct (castPtr p) s

-- | Given a pointer to a struct with an unknown chain, peek the struct and
-- its chain.
peekSomeCStruct
  :: forall a
   . (Extensible a, forall es . (Extendss a es, PeekChain es) => FromCStruct (a es))
  => Ptr (SomeStruct a)
  -> IO (SomeStruct a)
peekSomeCStruct p = do
  head'  <- peekCStruct (castPtr @_ @(a '[]) p)
  pNext <- peek @(Ptr BaseOutStructure) (p `plusPtr` 8)
  peekSomeChain @a pNext $ \tail' -> SomeStruct (setNext head' tail')

peekSomeChain
  :: forall a b
   . (Extensible a)
  => Ptr BaseOutStructure
  -> (  forall es
      . (Extendss a es, PokeChain es, Show (Chain es))
     => Chain es
     -> b
     )
  -> IO b
peekSomeChain p c = if p == nullPtr
  then pure (c ())
  else do
    baseOut <- peek p
    join
      $ peekChainHead @a (case baseOut of BaseOutStructure{type'} -> type')
                         (castPtr @BaseOutStructure @() p)
      $ \head' -> peekSomeChain @a (case baseOut of BaseOutStructure{next} -> next)
                                  (\tail' -> c (head', tail'))

peekChainHead
  :: forall a b
   . Extensible a
  => StructureType
  -> Ptr ()
  -> (forall e . (Extends a e, ToCStruct e, Show e) => e -> b)
  -> IO b
peekChainHead ty p c = case ty of
  TYPE_GRAPHICS_BINDING_OPENGL_WIN32_KHR -> go @GraphicsBindingOpenGLWin32KHR
  TYPE_GRAPHICS_BINDING_OPENGL_XLIB_KHR -> go @GraphicsBindingOpenGLXlibKHR
  TYPE_GRAPHICS_BINDING_OPENGL_XCB_KHR -> go @GraphicsBindingOpenGLXcbKHR
  TYPE_GRAPHICS_BINDING_OPENGL_WAYLAND_KHR -> go @GraphicsBindingOpenGLWaylandKHR
  TYPE_GRAPHICS_BINDING_D3D11_KHR -> go @GraphicsBindingD3D11KHR
  TYPE_GRAPHICS_BINDING_D3D12_KHR -> go @GraphicsBindingD3D12KHR
  TYPE_GRAPHICS_BINDING_OPENGL_ES_ANDROID_KHR -> go @GraphicsBindingOpenGLESAndroidKHR
  TYPE_GRAPHICS_BINDING_VULKAN_KHR -> go @GraphicsBindingVulkanKHR
  TYPE_SPACE_VELOCITY -> go @SpaceVelocity
  TYPE_COMPOSITION_LAYER_DEPTH_INFO_KHR -> go @CompositionLayerDepthInfoKHR
  TYPE_INSTANCE_CREATE_INFO_ANDROID_KHR -> go @InstanceCreateInfoAndroidKHR
  TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT -> go @DebugUtilsMessengerCreateInfoEXT
  TYPE_SESSION_CREATE_INFO_OVERLAY_EXTX -> go @SessionCreateInfoOverlayEXTX
  TYPE_VIEW_CONFIGURATION_DEPTH_RANGE_EXT -> go @ViewConfigurationDepthRangeEXT
  TYPE_VIEW_CONFIGURATION_VIEW_FOV_EPIC -> go @ViewConfigurationViewFovEPIC
  TYPE_INTERACTION_PROFILE_ANALOG_THRESHOLD_VALVE -> go @InteractionProfileAnalogThresholdVALVE
  TYPE_BINDING_MODIFICATIONS_KHR -> go @BindingModificationsKHR
  TYPE_SYSTEM_EYE_GAZE_INTERACTION_PROPERTIES_EXT -> go @SystemEyeGazeInteractionPropertiesEXT
  TYPE_EYE_GAZE_SAMPLE_TIME_EXT -> go @EyeGazeSampleTimeEXT
  TYPE_GRAPHICS_BINDING_EGL_MNDX -> go @GraphicsBindingEGLMNDX
  TYPE_SYSTEM_HAND_TRACKING_PROPERTIES_EXT -> go @SystemHandTrackingPropertiesEXT
  TYPE_HAND_JOINT_VELOCITIES_EXT -> go @HandJointVelocitiesEXT
  TYPE_SYSTEM_HAND_TRACKING_MESH_PROPERTIES_MSFT -> go @SystemHandTrackingMeshPropertiesMSFT
  TYPE_HAND_POSE_TYPE_INFO_MSFT -> go @HandPoseTypeInfoMSFT
  TYPE_SECONDARY_VIEW_CONFIGURATION_SESSION_BEGIN_INFO_MSFT -> go @SecondaryViewConfigurationSessionBeginInfoMSFT
  TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_STATE_MSFT -> go @SecondaryViewConfigurationFrameStateMSFT
  TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_END_INFO_MSFT -> go @SecondaryViewConfigurationFrameEndInfoMSFT
  TYPE_SECONDARY_VIEW_CONFIGURATION_SWAPCHAIN_CREATE_INFO_MSFT -> go @SecondaryViewConfigurationSwapchainCreateInfoMSFT
  TYPE_HOLOGRAPHIC_WINDOW_ATTACHMENT_MSFT -> go @HolographicWindowAttachmentMSFT
  TYPE_COMPOSITION_LAYER_COLOR_SCALE_BIAS_KHR -> go @CompositionLayerColorScaleBiasKHR
  t -> throwIO $ IOError Nothing InvalidArgument "peekChainHead" ("Unrecognized struct type: " <> show t) Nothing Nothing
 where
  go :: forall e . (Typeable e, FromCStruct e, ToCStruct e, Show e) => IO b
  go =
    let r = extends @a @e Proxy $ do
          head' <- peekCStruct @e (castPtr p)
          pure $ c head'
    in  fromMaybe
          (throwIO $ IOError
            Nothing
            InvalidArgument
            "peekChainHead"
            (  "Illegal struct extension of "
            <> extensibleTypeName @a
            <> " with "
            <> show ty
            )
            Nothing
            Nothing
          )
          r

class Extensible (a :: [Type] -> Type) where
  extensibleTypeName :: String
  -- ^ For error reporting an invalid extension
  getNext :: a es -> Chain es
  setNext :: a ds -> Chain es -> a es
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends a e => b) -> Maybe b

type family Chain (xs :: [a]) = (r :: a) | r -> xs where
  Chain '[]    = ()
  Chain (x:xs) = (x, Chain xs)

-- | A pattern synonym to separate the head of a struct chain from the
-- tail, use in conjunction with ':&' to extract several members.
--
-- @
-- Head{..} ::& () <- returningNoTail a b c
-- -- Equivalent to
-- Head{..} <- returningNoTail @'[] a b c
-- @
--
-- @
-- Head{..} ::& Foo{..} :& Bar{..} :& () <- returningWithTail a b c
-- @
--
-- @
-- myFun (Head{..} :&& Foo{..} :& ())
-- @
pattern (::&) :: Extensible a => a es' -> Chain es -> a es
pattern a ::& es <- (\a -> (a, getNext a) -> (a, es))
  where a ::& es = setNext a es
infix 6 ::&
{-# complete (::&) :: GraphicsBindingOpenGLWin32KHR #-}
{-# complete (::&) :: GraphicsBindingOpenGLXlibKHR #-}
{-# complete (::&) :: GraphicsBindingOpenGLXcbKHR #-}
{-# complete (::&) :: GraphicsBindingOpenGLWaylandKHR #-}
{-# complete (::&) :: GraphicsBindingD3D11KHR #-}
{-# complete (::&) :: GraphicsBindingD3D12KHR #-}
{-# complete (::&) :: GraphicsBindingOpenGLESAndroidKHR #-}
{-# complete (::&) :: GraphicsBindingVulkanKHR #-}
{-# complete (::&) :: SpaceVelocity #-}
{-# complete (::&) :: CompositionLayerDepthInfoKHR #-}
{-# complete (::&) :: InstanceCreateInfoAndroidKHR #-}
{-# complete (::&) :: DebugUtilsMessengerCreateInfoEXT #-}
{-# complete (::&) :: SessionCreateInfoOverlayEXTX #-}
{-# complete (::&) :: ViewConfigurationDepthRangeEXT #-}
{-# complete (::&) :: ViewConfigurationViewFovEPIC #-}
{-# complete (::&) :: InteractionProfileAnalogThresholdVALVE #-}
{-# complete (::&) :: BindingModificationsKHR #-}
{-# complete (::&) :: SystemEyeGazeInteractionPropertiesEXT #-}
{-# complete (::&) :: EyeGazeSampleTimeEXT #-}
{-# complete (::&) :: GraphicsBindingEGLMNDX #-}
{-# complete (::&) :: SystemHandTrackingPropertiesEXT #-}
{-# complete (::&) :: HandJointVelocitiesEXT #-}
{-# complete (::&) :: SystemHandTrackingMeshPropertiesMSFT #-}
{-# complete (::&) :: HandPoseTypeInfoMSFT #-}
{-# complete (::&) :: SecondaryViewConfigurationSessionBeginInfoMSFT #-}
{-# complete (::&) :: SecondaryViewConfigurationFrameStateMSFT #-}
{-# complete (::&) :: SecondaryViewConfigurationFrameEndInfoMSFT #-}
{-# complete (::&) :: SecondaryViewConfigurationSwapchainCreateInfoMSFT #-}
{-# complete (::&) :: HolographicWindowAttachmentMSFT #-}
{-# complete (::&) :: CompositionLayerColorScaleBiasKHR #-}

-- | View the head and tail of a 'Chain', see '::&'
--
-- Equivalent to @(,)@
pattern (:&) :: e -> Chain es -> Chain (e:es)
pattern e :& es = (e, es)
infixr 7 :&
{-# complete (:&) #-}

type family Extendss (p :: [Type] -> Type) (xs :: [Type]) :: Constraint where
  Extendss p '[]      = ()
  Extendss p (x : xs) = (Extends p x, Extendss p xs)

class PokeChain es where
  withChain :: Chain es -> (Ptr (Chain es) -> IO a) -> IO a
  withZeroChain :: (Ptr (Chain es) -> IO a) -> IO a

instance PokeChain '[] where
  withChain () f = f nullPtr
  withZeroChain f = f nullPtr

instance (ToCStruct e, PokeChain es) => PokeChain (e:es) where
  withChain (e, es) f = evalContT $ do
    t <- ContT $ withChain es
    h <- ContT $ withCStruct e
    lift $ linkChain h t
    lift $ f (castPtr h)
  withZeroChain f = evalContT $ do
    t <- ContT $ withZeroChain @es
    h <- ContT $ withZeroCStruct @e
    lift $ linkChain h t
    lift $ f (castPtr h)

class PeekChain es where
  peekChain :: Ptr (Chain es) -> IO (Chain es)

instance PeekChain '[] where
  peekChain _ = pure ()

instance (FromCStruct e, PeekChain es) => PeekChain (e:es) where
  peekChain p = do
    h <- peekCStruct @e (castPtr p)
    tPtr <- peek (p `plusPtr` 8)
    t <- peekChain tPtr
    pure (h, t)

linkChain :: Ptr a -> Ptr b -> IO ()
linkChain head' tail' = poke (head' `plusPtr` 8) tail'

data SomeChild (a :: Type) where
  SomeChild :: forall a b . (Inherits a b, Typeable b, ToCStruct b, Show b) => b -> SomeChild a
deriving instance Show (SomeChild a)

type family Inherits (a :: Type) (b :: Type) :: Constraint where
  Inherits SwapchainImageBaseHeader SwapchainImageD3D12KHR = ()
  Inherits SwapchainImageBaseHeader SwapchainImageD3D11KHR = ()
  Inherits SwapchainImageBaseHeader SwapchainImageVulkanKHR = ()
  Inherits SwapchainImageBaseHeader SwapchainImageOpenGLESKHR = ()
  Inherits SwapchainImageBaseHeader SwapchainImageOpenGLKHR = ()
  Inherits (CompositionLayerBaseHeader '[]) CompositionLayerEquirect2KHR = ()
  Inherits (CompositionLayerBaseHeader '[]) CompositionLayerEquirectKHR = ()
  Inherits (CompositionLayerBaseHeader '[]) CompositionLayerCubeKHR = ()
  Inherits (CompositionLayerBaseHeader '[]) CompositionLayerCylinderKHR = ()
  Inherits (CompositionLayerBaseHeader '[]) CompositionLayerQuad = ()
  Inherits (CompositionLayerBaseHeader '[]) CompositionLayerProjection = ()
  Inherits HapticBaseHeader HapticVibration = ()
  Inherits EventDataBaseHeader EventDataDisplayRefreshRateChangedFB = ()
  Inherits EventDataBaseHeader EventDataMainSessionVisibilityChangedEXTX = ()
  Inherits EventDataBaseHeader EventDataInteractionProfileChanged = ()
  Inherits EventDataBaseHeader EventDataVisibilityMaskChangedKHR = ()
  Inherits EventDataBaseHeader EventDataPerfSettingsEXT = ()
  Inherits EventDataBaseHeader EventDataReferenceSpaceChangePending = ()
  Inherits EventDataBaseHeader EventDataSessionStateChanged = ()
  Inherits EventDataBaseHeader EventDataInstanceLossPending = ()
  Inherits EventDataBaseHeader EventDataEventsLost = ()
  Inherits LoaderInitInfoBaseHeaderKHR LoaderInitInfoAndroidKHR = ()
  Inherits parent child =
    TypeError (ShowType parent :<>: Text " is not inherited by " :<>: ShowType child)

class Inheritable (a :: Type) where
  peekSomeCChild :: Ptr (SomeChild a) -> IO (SomeChild a)

withSomeChild :: SomeChild a -> (Ptr (SomeChild a) -> IO b) -> IO b
withSomeChild (SomeChild c) f = withCStruct c (f . lowerChildPointer)

lowerChildPointer :: Inherits a b => Ptr b -> Ptr (SomeChild a)
lowerChildPointer = castPtr

