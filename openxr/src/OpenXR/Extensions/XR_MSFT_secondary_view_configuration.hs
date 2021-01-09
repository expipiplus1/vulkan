{-# language CPP #-}
-- | = Name
--
-- XR_MSFT_secondary_view_configuration - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_secondary_view_configuration  XR_MSFT_secondary_view_configuration>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 54
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'SecondaryViewConfigurationFrameEndInfoMSFT',
-- 'SecondaryViewConfigurationFrameStateMSFT',
-- 'SecondaryViewConfigurationLayerInfoMSFT',
-- 'SecondaryViewConfigurationSessionBeginInfoMSFT',
-- 'SecondaryViewConfigurationStateMSFT',
-- 'SecondaryViewConfigurationSwapchainCreateInfoMSFT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_secondary_view_configuration OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_MSFT_secondary_view_configuration  ( SecondaryViewConfigurationSessionBeginInfoMSFT(..)
                                                               , SecondaryViewConfigurationStateMSFT(..)
                                                               , SecondaryViewConfigurationFrameStateMSFT(..)
                                                               , SecondaryViewConfigurationFrameEndInfoMSFT(..)
                                                               , SecondaryViewConfigurationLayerInfoMSFT(..)
                                                               , SecondaryViewConfigurationSwapchainCreateInfoMSFT(..)
                                                               , MSFT_secondary_view_configuration_SPEC_VERSION
                                                               , pattern MSFT_secondary_view_configuration_SPEC_VERSION
                                                               , MSFT_SECONDARY_VIEW_CONFIGURATION_EXTENSION_NAME
                                                               , pattern MSFT_SECONDARY_VIEW_CONFIGURATION_EXTENSION_NAME
                                                               ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import OpenXR.CStruct.Utils (advancePtrBytes)
import OpenXR.Core10.FundamentalTypes (bool32ToBool)
import OpenXR.Core10.FundamentalTypes (boolToBool32)
import OpenXR.CStruct.Extends (withSomeChild)
import OpenXR.Core10.FundamentalTypes (Bool32)
import OpenXR.Core10.OtherTypes (CompositionLayerBaseHeader)
import OpenXR.Core10.Enums.EnvironmentBlendMode (EnvironmentBlendMode)
import OpenXR.CStruct.Extends (Inheritable(peekSomeCChild))
import OpenXR.CStruct.Extends (SomeChild)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Enums.ViewConfigurationType (ViewConfigurationType)
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_END_INFO_MSFT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_STATE_MSFT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SECONDARY_VIEW_CONFIGURATION_LAYER_INFO_MSFT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SECONDARY_VIEW_CONFIGURATION_SESSION_BEGIN_INFO_MSFT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SECONDARY_VIEW_CONFIGURATION_STATE_MSFT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SECONDARY_VIEW_CONFIGURATION_SWAPCHAIN_CREATE_INFO_MSFT))
-- | XrSecondaryViewConfigurationSessionBeginInfoMSFT - Describes an
-- extension structure to 'OpenXR.Core10.Session.beginSession' indicating
-- supported view configuration types.
--
-- == Member Descriptions
--
-- = Description
--
-- If there are any duplicated view configuration types in the array of
-- @enabledViewConfigurationTypes@, the runtime /must/ return error
-- 'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'.
--
-- If there are any primary view configuration types in the array of
-- @enabledViewConfigurationTypes@, the runtime /must/ return error
-- 'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'.
--
-- If there are any secondary view configuration types not returned by
-- 'OpenXR.Core10.ViewConfigurations.enumerateViewConfigurations' in the
-- array of @enabledViewConfigurationTypes@, the runtime /must/ return
-- error
-- 'OpenXR.Core10.Enums.Result.ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrSecondaryViewConfigurationSessionBeginInfoMSFT-extension-notenabled#
--     The @@ extension /must/ be enabled prior to using
--     'SecondaryViewConfigurationSessionBeginInfoMSFT'
--
-- -   #VUID-XrSecondaryViewConfigurationSessionBeginInfoMSFT-type-type#
--     @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SECONDARY_VIEW_CONFIGURATION_SESSION_BEGIN_INFO_MSFT'
--
-- -   #VUID-XrSecondaryViewConfigurationSessionBeginInfoMSFT-next-next#
--     @next@ /must/ be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrSecondaryViewConfigurationSessionBeginInfoMSFT-enabledViewConfigurationTypes-parameter#
--     @enabledViewConfigurationTypes@ /must/ be a pointer to an array of
--     @viewConfigurationCount@ valid
--     'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType'
--     values
--
-- -   #VUID-XrSecondaryViewConfigurationSessionBeginInfoMSFT-viewConfigurationCount-arraylength#
--     The @viewConfigurationCount@ parameter /must/ be greater than @0@
--
-- = See Also
--
-- 'OpenXR.Core10.Session.SessionBeginInfo',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType'
data SecondaryViewConfigurationSessionBeginInfoMSFT = SecondaryViewConfigurationSessionBeginInfoMSFT
  { -- | @enabledViewConfigurationTypes@ is an array of enabled secondary view
    -- configuration types that application supports.
    enabledViewConfigurationTypes :: Vector ViewConfigurationType }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SecondaryViewConfigurationSessionBeginInfoMSFT)
#endif
deriving instance Show SecondaryViewConfigurationSessionBeginInfoMSFT

instance ToCStruct SecondaryViewConfigurationSessionBeginInfoMSFT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SecondaryViewConfigurationSessionBeginInfoMSFT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SECONDARY_VIEW_CONFIGURATION_SESSION_BEGIN_INFO_MSFT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (enabledViewConfigurationTypes)) :: Word32))
    pEnabledViewConfigurationTypes' <- ContT $ allocaBytesAligned @ViewConfigurationType ((Data.Vector.length (enabledViewConfigurationTypes)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pEnabledViewConfigurationTypes' `plusPtr` (4 * (i)) :: Ptr ViewConfigurationType) (e)) (enabledViewConfigurationTypes)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ViewConfigurationType))) (pEnabledViewConfigurationTypes')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SECONDARY_VIEW_CONFIGURATION_SESSION_BEGIN_INFO_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SecondaryViewConfigurationSessionBeginInfoMSFT where
  peekCStruct p = do
    viewConfigurationCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    enabledViewConfigurationTypes <- peek @(Ptr ViewConfigurationType) ((p `plusPtr` 24 :: Ptr (Ptr ViewConfigurationType)))
    enabledViewConfigurationTypes' <- generateM (fromIntegral viewConfigurationCount) (\i -> peek @ViewConfigurationType ((enabledViewConfigurationTypes `advancePtrBytes` (4 * (i)) :: Ptr ViewConfigurationType)))
    pure $ SecondaryViewConfigurationSessionBeginInfoMSFT
             enabledViewConfigurationTypes'

instance Zero SecondaryViewConfigurationSessionBeginInfoMSFT where
  zero = SecondaryViewConfigurationSessionBeginInfoMSFT
           mempty


-- | XrSecondaryViewConfigurationStateMSFT - Returns the state of an enabled
-- secondary view configuration.
--
-- == Member Descriptions
--
-- = Description
--
-- When a secondary view configuration becomes active, the application
-- /should/ render its secondary views as soon as possible, by getting
-- their view transforms and FOV using
-- 'OpenXR.Core10.DisplayTiming.locateViews' and then submitting
-- composition layers to 'OpenXR.Core10.DisplayTiming.endFrame' through the
-- 'SecondaryViewConfigurationFrameEndInfoMSFT' extension structure. When a
-- secondary view configuration changes from inactive to active, the
-- runtime /may/ change
-- 'OpenXR.Core10.ViewConfigurations.ViewConfigurationView' of the given
-- view configuration such as the recommended image width or height. An
-- application /should/ query for latest
-- 'OpenXR.Core10.ViewConfigurations.ViewConfigurationView' through
-- 'OpenXR.Core10.ViewConfigurations.enumerateViewConfigurationViews'
-- function for the secondary view configuration and consider recreating
-- swapchain images if necessary. The runtime /must/ not change the
-- 'OpenXR.Core10.ViewConfigurations.ViewConfigurationView', including
-- recommended image width and height of a secondary view configuration
-- when @active@ remains true until the secondary view configuration
-- deactivated or the session has ended.
--
-- If necessary, the application /can/ take longer than a frame duration to
-- prepare by calling 'OpenXR.Core10.DisplayTiming.endFrame' without
-- submitting layers for that secondary view configuration until ready. The
-- runtime /should/ delay the underlying scenario managed by the secondary
-- view configuration until the application begins submitting frames with
-- layers for that configuration. The active secondary view configuration
-- composed output is undefined if the application stops submitting frames
-- with layers for a secondary view configuration while @active@ remains
-- true.
--
-- When the runtime intends to conclude a secondary view configuration, for
-- example when user stops video capture, the runtime makes the view
-- configuration inactive by setting the corresponding @active@ in the
-- 'SecondaryViewConfigurationStateMSFT' structure to false.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrSecondaryViewConfigurationStateMSFT-extension-notenabled#
--     The @@ extension /must/ be enabled prior to using
--     'SecondaryViewConfigurationStateMSFT'
--
-- -   #VUID-XrSecondaryViewConfigurationStateMSFT-type-type# @type@ /must/
--     be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SECONDARY_VIEW_CONFIGURATION_STATE_MSFT'
--
-- -   #VUID-XrSecondaryViewConfigurationStateMSFT-next-next# @next@ /must/
--     be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrSecondaryViewConfigurationStateMSFT-viewConfigurationType-parameter#
--     @viewConfigurationType@ /must/ be a valid
--     'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType'
--     value
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >,
-- 'OpenXR.Core10.DisplayTiming.FrameState',
-- 'SecondaryViewConfigurationFrameStateMSFT',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType'
data SecondaryViewConfigurationStateMSFT = SecondaryViewConfigurationStateMSFT
  { -- | @viewConfigurationType@ is a
    -- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType' that
    -- represents the returned state.
    viewConfigurationType :: ViewConfigurationType
  , -- | @active@ is an
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >
    -- returns whether the secondary view configuration is active and
    -- displaying frames to users.
    active :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SecondaryViewConfigurationStateMSFT)
#endif
deriving instance Show SecondaryViewConfigurationStateMSFT

instance ToCStruct SecondaryViewConfigurationStateMSFT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SecondaryViewConfigurationStateMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SECONDARY_VIEW_CONFIGURATION_STATE_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ViewConfigurationType)) (viewConfigurationType)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (active))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SECONDARY_VIEW_CONFIGURATION_STATE_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ViewConfigurationType)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct SecondaryViewConfigurationStateMSFT where
  peekCStruct p = do
    viewConfigurationType <- peek @ViewConfigurationType ((p `plusPtr` 16 :: Ptr ViewConfigurationType))
    active <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ SecondaryViewConfigurationStateMSFT
             viewConfigurationType (bool32ToBool active)

instance Storable SecondaryViewConfigurationStateMSFT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SecondaryViewConfigurationStateMSFT where
  zero = SecondaryViewConfigurationStateMSFT
           zero
           zero


-- | XrSecondaryViewConfigurationFrameStateMSFT - Extension structure to
-- xrWaitFrame to return a list of secondary view configuration states.
--
-- == Member Descriptions
--
-- = Description
--
-- The array size @viewConfigurationCount@ in the
-- 'SecondaryViewConfigurationFrameStateMSFT' structure /must/ be the same
-- as the array size enabled through
-- 'SecondaryViewConfigurationSessionBeginInfoMSFT' when calling
-- 'OpenXR.Core10.Session.beginSession' earlier, otherwise the runtime
-- /must/ return error
-- 'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrSecondaryViewConfigurationFrameStateMSFT-extension-notenabled#
--     The @@ extension /must/ be enabled prior to using
--     'SecondaryViewConfigurationFrameStateMSFT'
--
-- -   #VUID-XrSecondaryViewConfigurationFrameStateMSFT-type-type# @type@
--     /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_STATE_MSFT'
--
-- -   #VUID-XrSecondaryViewConfigurationFrameStateMSFT-next-next# @next@
--     /must/ be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrSecondaryViewConfigurationFrameStateMSFT-viewConfigurationStates-parameter#
--     @viewConfigurationStates@ /must/ be a pointer to an array of
--     @viewConfigurationCount@ 'SecondaryViewConfigurationStateMSFT'
--     structures
--
-- -   #VUID-XrSecondaryViewConfigurationFrameStateMSFT-viewConfigurationCount-arraylength#
--     The @viewConfigurationCount@ parameter /must/ be greater than @0@
--
-- = See Also
--
-- 'OpenXR.Core10.DisplayTiming.FrameState',
-- 'SecondaryViewConfigurationStateMSFT',
-- 'OpenXR.Core10.Enums.StructureType.StructureType'
data SecondaryViewConfigurationFrameStateMSFT = SecondaryViewConfigurationFrameStateMSFT
  { -- | @viewConfigurationCount@ is the number of elements in
    -- @viewConfigurationStates@.
    viewConfigurationCount :: Word32
  , -- | @viewConfigurationStates@ is an array of
    -- 'SecondaryViewConfigurationStateMSFT' structures.
    viewConfigurationStates :: Ptr SecondaryViewConfigurationStateMSFT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SecondaryViewConfigurationFrameStateMSFT)
#endif
deriving instance Show SecondaryViewConfigurationFrameStateMSFT

instance ToCStruct SecondaryViewConfigurationFrameStateMSFT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SecondaryViewConfigurationFrameStateMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_STATE_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (viewConfigurationCount)
    poke ((p `plusPtr` 24 :: Ptr (Ptr SecondaryViewConfigurationStateMSFT))) (viewConfigurationStates)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_STATE_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr (Ptr SecondaryViewConfigurationStateMSFT))) (zero)
    f

instance FromCStruct SecondaryViewConfigurationFrameStateMSFT where
  peekCStruct p = do
    viewConfigurationCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    viewConfigurationStates <- peek @(Ptr SecondaryViewConfigurationStateMSFT) ((p `plusPtr` 24 :: Ptr (Ptr SecondaryViewConfigurationStateMSFT)))
    pure $ SecondaryViewConfigurationFrameStateMSFT
             viewConfigurationCount viewConfigurationStates

instance Storable SecondaryViewConfigurationFrameStateMSFT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SecondaryViewConfigurationFrameStateMSFT where
  zero = SecondaryViewConfigurationFrameStateMSFT
           zero
           zero


-- | XrSecondaryViewConfigurationFrameEndInfoMSFT - Submit an array of
-- 'SecondaryViewConfigurationLayerInfoMSFT', one for each secondary view
-- configuration.
--
-- == Member Descriptions
--
-- = Description
--
-- The view configuration type in each
-- 'SecondaryViewConfigurationLayerInfoMSFT' must be one of the view
-- configurations enabled when calling 'OpenXR.Core10.Session.beginSession'
-- in 'SecondaryViewConfigurationSessionBeginInfoMSFT', or else the runtime
-- /must/ return error
-- 'OpenXR.Core10.Enums.Result.ERROR_SECONDARY_VIEW_CONFIGURATION_TYPE_NOT_ENABLED_MSFT'.
--
-- The view configuration type in each
-- 'SecondaryViewConfigurationLayerInfoMSFT' must not be the primary view
-- configuration in this session, or else the runtime /must/ return error
-- 'OpenXR.Core10.Enums.Result.ERROR_LAYER_INVALID'. The primary view
-- configuration layers continue to be submitted through
-- 'OpenXR.Core10.DisplayTiming.FrameEndInfo' directly.
--
-- If the view configuration is not active, as indicated in
-- 'SecondaryViewConfigurationFrameStateMSFT', the composition layers
-- submitted to this view configuration /may/ be ignored by the runtime.
-- Applications /should/ avoid rendering into secondary views when the view
-- configuration is inactive.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrSecondaryViewConfigurationFrameEndInfoMSFT-extension-notenabled#
--     The @@ extension /must/ be enabled prior to using
--     'SecondaryViewConfigurationFrameEndInfoMSFT'
--
-- -   #VUID-XrSecondaryViewConfigurationFrameEndInfoMSFT-type-type# @type@
--     /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_END_INFO_MSFT'
--
-- -   #VUID-XrSecondaryViewConfigurationFrameEndInfoMSFT-next-next# @next@
--     /must/ be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrSecondaryViewConfigurationFrameEndInfoMSFT-viewConfigurationLayersInfo-parameter#
--     @viewConfigurationLayersInfo@ /must/ be a pointer to an array of
--     @viewConfigurationCount@ valid
--     'SecondaryViewConfigurationLayerInfoMSFT' structures
--
-- -   #VUID-XrSecondaryViewConfigurationFrameEndInfoMSFT-viewConfigurationCount-arraylength#
--     The @viewConfigurationCount@ parameter /must/ be greater than @0@
--
-- = See Also
--
-- 'SecondaryViewConfigurationLayerInfoMSFT',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.DisplayTiming.endFrame'
data SecondaryViewConfigurationFrameEndInfoMSFT = SecondaryViewConfigurationFrameEndInfoMSFT
  { -- | @viewConfigurationLayersInfo@ is an array of
    -- 'SecondaryViewConfigurationLayerInfoMSFT', containing composition layers
    -- to be submitted for the specified active view configuration.
    viewConfigurationLayersInfo :: Vector SecondaryViewConfigurationLayerInfoMSFT }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SecondaryViewConfigurationFrameEndInfoMSFT)
#endif
deriving instance Show SecondaryViewConfigurationFrameEndInfoMSFT

instance ToCStruct SecondaryViewConfigurationFrameEndInfoMSFT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SecondaryViewConfigurationFrameEndInfoMSFT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_END_INFO_MSFT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (viewConfigurationLayersInfo)) :: Word32))
    pViewConfigurationLayersInfo' <- ContT $ allocaBytesAligned @SecondaryViewConfigurationLayerInfoMSFT ((Data.Vector.length (viewConfigurationLayersInfo)) * 40) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pViewConfigurationLayersInfo' `plusPtr` (40 * (i)) :: Ptr SecondaryViewConfigurationLayerInfoMSFT) (e) . ($ ())) (viewConfigurationLayersInfo)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr SecondaryViewConfigurationLayerInfoMSFT))) (pViewConfigurationLayersInfo')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_END_INFO_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SecondaryViewConfigurationFrameEndInfoMSFT where
  peekCStruct p = do
    viewConfigurationCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    viewConfigurationLayersInfo <- peek @(Ptr SecondaryViewConfigurationLayerInfoMSFT) ((p `plusPtr` 24 :: Ptr (Ptr SecondaryViewConfigurationLayerInfoMSFT)))
    viewConfigurationLayersInfo' <- generateM (fromIntegral viewConfigurationCount) (\i -> peekCStruct @SecondaryViewConfigurationLayerInfoMSFT ((viewConfigurationLayersInfo `advancePtrBytes` (40 * (i)) :: Ptr SecondaryViewConfigurationLayerInfoMSFT)))
    pure $ SecondaryViewConfigurationFrameEndInfoMSFT
             viewConfigurationLayersInfo'

instance Zero SecondaryViewConfigurationFrameEndInfoMSFT where
  zero = SecondaryViewConfigurationFrameEndInfoMSFT
           mempty


-- | XrSecondaryViewConfigurationLayerInfoMSFT - Describe an array of
-- composition layers to be submitted to given
-- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType'.
--
-- == Member Descriptions
--
-- = Description
--
-- This structure is similar to the
-- 'OpenXR.Core10.DisplayTiming.FrameEndInfo' structure, with an extra
-- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType' field
-- to specify the view configuration for which the submitted layers will be
-- rendered.
--
-- The application /should/ render its content for both the primary and
-- secondary view configurations using the same @predictedDisplayTime@
-- reported by 'OpenXR.Core10.DisplayTiming.waitFrame'. The runtime /must/
-- treat both the primary views and secondary views as being submitted for
-- the same @displayTime@ specified in the call to
-- 'OpenXR.Core10.DisplayTiming.endFrame'.
--
-- For layers such as quad layers whose content is identical across view
-- configurations, the application /can/ submit the same
-- 'OpenXR.Core10.OtherTypes.CompositionLayerBaseHeader' structures to
-- multiple view configurations in the same
-- 'OpenXR.Core10.DisplayTiming.endFrame' function call.
--
-- For each frame, the application /should/ only render and submit layers
-- for the secondary view configurations that were active that frame, as
-- indicated in the 'SecondaryViewConfigurationFrameStateMSFT' filled in
-- for that frame’s 'OpenXR.Core10.DisplayTiming.waitFrame' call. The
-- runtime /must/ ignore composition layers submitted for an inactive view
-- configuration.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrSecondaryViewConfigurationLayerInfoMSFT-extension-notenabled#
--     The @@ extension /must/ be enabled prior to using
--     'SecondaryViewConfigurationLayerInfoMSFT'
--
-- -   #VUID-XrSecondaryViewConfigurationLayerInfoMSFT-type-type# @type@
--     /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SECONDARY_VIEW_CONFIGURATION_LAYER_INFO_MSFT'
--
-- -   #VUID-XrSecondaryViewConfigurationLayerInfoMSFT-next-next# @next@
--     /must/ be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrSecondaryViewConfigurationLayerInfoMSFT-viewConfigurationType-parameter#
--     @viewConfigurationType@ /must/ be a valid
--     'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType'
--     value
--
-- -   #VUID-XrSecondaryViewConfigurationLayerInfoMSFT-environmentBlendMode-parameter#
--     @environmentBlendMode@ /must/ be a valid
--     'OpenXR.Core10.Enums.EnvironmentBlendMode.EnvironmentBlendMode'
--     value
--
-- -   #VUID-XrSecondaryViewConfigurationLayerInfoMSFT-layers-parameter#
--     @layers@ /must/ be a pointer to an array of @layerCount@ valid
--     'OpenXR.Core10.OtherTypes.CompositionLayerBaseHeader'-based
--     structures. See also:
--     'OpenXR.Extensions.XR_KHR_composition_layer_cube.CompositionLayerCubeKHR',
--     'OpenXR.Extensions.XR_KHR_composition_layer_cylinder.CompositionLayerCylinderKHR',
--     'OpenXR.Extensions.XR_KHR_composition_layer_equirect2.CompositionLayerEquirect2KHR',
--     'OpenXR.Extensions.XR_KHR_composition_layer_equirect.CompositionLayerEquirectKHR',
--     'OpenXR.Core10.OtherTypes.CompositionLayerProjection',
--     'OpenXR.Core10.OtherTypes.CompositionLayerQuad'
--
-- -   #VUID-XrSecondaryViewConfigurationLayerInfoMSFT-layerCount-arraylength#
--     The @layerCount@ parameter /must/ be greater than @0@
--
-- = See Also
--
-- 'OpenXR.Core10.OtherTypes.CompositionLayerBaseHeader',
-- 'OpenXR.Core10.Enums.EnvironmentBlendMode.EnvironmentBlendMode',
-- 'SecondaryViewConfigurationFrameEndInfoMSFT',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType',
-- 'OpenXR.Core10.DisplayTiming.endFrame'
data SecondaryViewConfigurationLayerInfoMSFT = SecondaryViewConfigurationLayerInfoMSFT
  { -- | @viewConfigurationType@ is
    -- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType' to
    -- which the composition layers will be displayed.
    viewConfigurationType :: ViewConfigurationType
  , -- | @environmentBlendMode@ is the
    -- 'OpenXR.Core10.Enums.EnvironmentBlendMode.EnvironmentBlendMode' value
    -- representing the desired
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#environment_blend_mode environment blend mode>
    -- for this view configuration.
    environmentBlendMode :: EnvironmentBlendMode
  , -- | @layers@ is a pointer to an array of
    -- 'OpenXR.Core10.OtherTypes.CompositionLayerBaseHeader' pointers.
    layers :: Vector (SomeChild (CompositionLayerBaseHeader '[]))
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SecondaryViewConfigurationLayerInfoMSFT)
#endif
deriving instance Show SecondaryViewConfigurationLayerInfoMSFT

instance ToCStruct SecondaryViewConfigurationLayerInfoMSFT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SecondaryViewConfigurationLayerInfoMSFT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SECONDARY_VIEW_CONFIGURATION_LAYER_INFO_MSFT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr ViewConfigurationType)) (viewConfigurationType)
    lift $ poke ((p `plusPtr` 20 :: Ptr EnvironmentBlendMode)) (environmentBlendMode)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (layers)) :: Word32))
    pLayers' <- ContT $ allocaBytesAligned @(Ptr _) ((Data.Vector.length (layers)) * 8) 8
    Data.Vector.imapM_ (\i e -> do
      layers'' <- ContT $ withSomeChild (e)
      lift $ poke (pLayers' `plusPtr` (8 * (i)) :: Ptr (Ptr _)) layers'') (layers)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr (Ptr _)))) (pLayers')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SECONDARY_VIEW_CONFIGURATION_LAYER_INFO_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ViewConfigurationType)) (zero)
    poke ((p `plusPtr` 20 :: Ptr EnvironmentBlendMode)) (zero)
    f

instance FromCStruct SecondaryViewConfigurationLayerInfoMSFT where
  peekCStruct p = do
    viewConfigurationType <- peek @ViewConfigurationType ((p `plusPtr` 16 :: Ptr ViewConfigurationType))
    environmentBlendMode <- peek @EnvironmentBlendMode ((p `plusPtr` 20 :: Ptr EnvironmentBlendMode))
    layerCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    layers <- peek @(Ptr (Ptr _)) ((p `plusPtr` 32 :: Ptr (Ptr (Ptr _))))
    layers' <- generateM (fromIntegral layerCount) (\i -> peekSomeCChild =<< peek ((layers `advancePtrBytes` (8 * (i)) :: Ptr (Ptr _))))
    pure $ SecondaryViewConfigurationLayerInfoMSFT
             viewConfigurationType environmentBlendMode layers'

instance Zero SecondaryViewConfigurationLayerInfoMSFT where
  zero = SecondaryViewConfigurationLayerInfoMSFT
           zero
           zero
           mempty


-- | XrSecondaryViewConfigurationSwapchainCreateInfoMSFT - Hint to runtime
-- that the created swapchain image will be used for given secondary view
-- configuration.
--
-- == Member Descriptions
--
-- = Description
--
-- If this structure is not present in the
-- 'OpenXR.Core10.Image.SwapchainCreateInfo' next chain when calling
-- 'OpenXR.Core10.Image.createSwapchain', the runtime /should/ optimize the
-- created swapchain for the primary view configuration of the session.
--
-- If the application submits a swapchain image created with one view
-- configuration type to a composition layer for another view
-- configuration, the runtime /may/ need to copy the resource across view
-- configurations. However, the runtime /must/ correctly compose the image
-- regardless which view configuration type was hinted when swapchain image
-- was created.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrSecondaryViewConfigurationSwapchainCreateInfoMSFT-extension-notenabled#
--     The @@ extension /must/ be enabled prior to using
--     'SecondaryViewConfigurationSwapchainCreateInfoMSFT'
--
-- -   #VUID-XrSecondaryViewConfigurationSwapchainCreateInfoMSFT-type-type#
--     @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SECONDARY_VIEW_CONFIGURATION_SWAPCHAIN_CREATE_INFO_MSFT'
--
-- -   #VUID-XrSecondaryViewConfigurationSwapchainCreateInfoMSFT-next-next#
--     @next@ /must/ be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrSecondaryViewConfigurationSwapchainCreateInfoMSFT-viewConfigurationType-parameter#
--     @viewConfigurationType@ /must/ be a valid
--     'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType'
--     value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Image.SwapchainCreateInfo',
-- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType'
data SecondaryViewConfigurationSwapchainCreateInfoMSFT = SecondaryViewConfigurationSwapchainCreateInfoMSFT
  { -- | @viewConfigurationType@ is the secondary view configuration type the
    -- application is intending to use this swapchain for.
    viewConfigurationType :: ViewConfigurationType }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SecondaryViewConfigurationSwapchainCreateInfoMSFT)
#endif
deriving instance Show SecondaryViewConfigurationSwapchainCreateInfoMSFT

instance ToCStruct SecondaryViewConfigurationSwapchainCreateInfoMSFT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SecondaryViewConfigurationSwapchainCreateInfoMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SECONDARY_VIEW_CONFIGURATION_SWAPCHAIN_CREATE_INFO_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ViewConfigurationType)) (viewConfigurationType)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SECONDARY_VIEW_CONFIGURATION_SWAPCHAIN_CREATE_INFO_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ViewConfigurationType)) (zero)
    f

instance FromCStruct SecondaryViewConfigurationSwapchainCreateInfoMSFT where
  peekCStruct p = do
    viewConfigurationType <- peek @ViewConfigurationType ((p `plusPtr` 16 :: Ptr ViewConfigurationType))
    pure $ SecondaryViewConfigurationSwapchainCreateInfoMSFT
             viewConfigurationType

instance Storable SecondaryViewConfigurationSwapchainCreateInfoMSFT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SecondaryViewConfigurationSwapchainCreateInfoMSFT where
  zero = SecondaryViewConfigurationSwapchainCreateInfoMSFT
           zero


type MSFT_secondary_view_configuration_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_MSFT_secondary_view_configuration_SPEC_VERSION"
pattern MSFT_secondary_view_configuration_SPEC_VERSION :: forall a . Integral a => a
pattern MSFT_secondary_view_configuration_SPEC_VERSION = 1


type MSFT_SECONDARY_VIEW_CONFIGURATION_EXTENSION_NAME = "XR_MSFT_secondary_view_configuration"

-- No documentation found for TopLevel "XR_MSFT_SECONDARY_VIEW_CONFIGURATION_EXTENSION_NAME"
pattern MSFT_SECONDARY_VIEW_CONFIGURATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern MSFT_SECONDARY_VIEW_CONFIGURATION_EXTENSION_NAME = "XR_MSFT_secondary_view_configuration"

