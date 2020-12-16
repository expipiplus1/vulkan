{-# language CPP #-}
-- No documentation found for Chapter "ViewConfigurations"
module OpenXR.Core10.ViewConfigurations  ( enumerateViewConfigurations
                                         , getViewConfigurationProperties
                                         , enumerateViewConfigurationViews
                                         , ViewConfigurationView(..)
                                         , ViewConfigurationProperties(..)
                                         ) where

import OpenXR.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import OpenXR.CStruct.Utils (advancePtrBytes)
import OpenXR.Core10.FundamentalTypes (bool32ToBool)
import OpenXR.Core10.FundamentalTypes (boolToBool32)
import OpenXR.CStruct.Extends (forgetExtensions)
import OpenXR.NamedType ((:::))
import OpenXR.Core10.FundamentalTypes (Bool32)
import OpenXR.CStruct.Extends (Chain)
import OpenXR.CStruct.Extends (Extends)
import OpenXR.CStruct.Extends (Extendss)
import OpenXR.CStruct.Extends (Extensible(..))
import OpenXR.Core10.Handles (Instance)
import OpenXR.Core10.Handles (Instance(..))
import OpenXR.Dynamic (InstanceCmds(pXrEnumerateViewConfigurationViews))
import OpenXR.Dynamic (InstanceCmds(pXrEnumerateViewConfigurations))
import OpenXR.Dynamic (InstanceCmds(pXrGetViewConfigurationProperties))
import OpenXR.Core10.Handles (Instance_T)
import OpenXR.Exception (OpenXrException(..))
import OpenXR.CStruct.Extends (PeekChain)
import OpenXR.CStruct.Extends (PeekChain(..))
import OpenXR.CStruct.Extends (PokeChain)
import OpenXR.CStruct.Extends (PokeChain(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.CStruct.Extends (SomeStruct)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Device (SystemId)
import OpenXR.Core10.Device (SystemId(..))
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_view_configuration_depth_range (ViewConfigurationDepthRangeEXT)
import OpenXR.Core10.Enums.ViewConfigurationType (ViewConfigurationType)
import OpenXR.Core10.Enums.ViewConfigurationType (ViewConfigurationType(..))
import {-# SOURCE #-} OpenXR.Extensions.XR_EPIC_view_configuration_fov (ViewConfigurationViewFovEPIC)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_VIEW_CONFIGURATION_PROPERTIES))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_VIEW_CONFIGURATION_VIEW))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrEnumerateViewConfigurations
  :: FunPtr (Ptr Instance_T -> SystemId -> Word32 -> Ptr Word32 -> Ptr ViewConfigurationType -> IO Result) -> Ptr Instance_T -> SystemId -> Word32 -> Ptr Word32 -> Ptr ViewConfigurationType -> IO Result

-- | xrEnumerateViewConfigurations - Enumerates supported view configurations
--
-- == Parameter Descriptions
--
-- -   @instance@ is the instance from which @systemId@ was retrieved.
--
-- -   @systemId@ is the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
--     whose view configurations will be enumerated.
--
-- -   @viewConfigurationsTypeCapacityInput@ is the capacity of the
--     @viewConfigurations@ array, or 0 to indicate a request to retrieve
--     the required capacity.
--
-- -   @viewConfigurationsTypeCountOutput@ is a pointer to the count of
--     @viewConfigurations@ written, or a pointer to the required capacity
--     in the case that @viewConfigurationsTypeCapacityInput@ is 0.
--
-- -   @viewConfigurationsTypes@ is a pointer to an array of
--     'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType'
--     values, but /can/ be @NULL@ if @viewConfigurationsTypeCapacityInput@
--     is 0.
--
-- -   See
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#buffer-size-parameters Buffer Size Parameters>
--     chapter for a detailed description of retrieving the required
--     @viewConfigurations@ size.
--
-- = Description
--
-- 'enumerateViewConfigurations' enumerates the view configuration types
-- supported by the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >.
-- The supported set for that system /must/ not change during the lifetime
-- of its 'OpenXR.Core10.Handles.Instance'. The returned list of primary
-- view configurations /should/ be in order from what the runtime
-- considered highest to lowest user preference. Thus the first enumerated
-- view configuration type /should/ be the one the runtime prefers the
-- application to use if possible.
--
-- Runtimes /must/ always return identical buffer contents from this
-- enumeration for the given @systemId@ and for the lifetime of the
-- instance.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrEnumerateViewConfigurations-instance-parameter# @instance@
--     /must/ be a valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrEnumerateViewConfigurations-viewConfigurationTypeCountOutput-parameter#
--     @viewConfigurationTypeCountOutput@ /must/ be a pointer to a
--     @uint32_t@ value
--
-- -   #VUID-xrEnumerateViewConfigurations-viewConfigurationTypes-parameter#
--     If @viewConfigurationTypeCapacityInput@ is not @0@,
--     @viewConfigurationTypes@ /must/ be a pointer to an array of
--     @viewConfigurationTypeCapacityInput@
--     'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType'
--     values
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SYSTEM_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Instance',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType'
enumerateViewConfigurations :: forall io
                             . (MonadIO io)
                            => -- No documentation found for Nested "xrEnumerateViewConfigurations" "instance"
                               Instance
                            -> -- No documentation found for Nested "xrEnumerateViewConfigurations" "systemId"
                               SystemId
                            -> io (("viewConfigurationTypes" ::: Vector ViewConfigurationType))
enumerateViewConfigurations instance' systemId = liftIO . evalContT $ do
  let xrEnumerateViewConfigurationsPtr = pXrEnumerateViewConfigurations (instanceCmds (instance' :: Instance))
  lift $ unless (xrEnumerateViewConfigurationsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrEnumerateViewConfigurations is null" Nothing Nothing
  let xrEnumerateViewConfigurations' = mkXrEnumerateViewConfigurations xrEnumerateViewConfigurationsPtr
  let instance'' = instanceHandle (instance')
  pViewConfigurationTypeCountOutput <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "xrEnumerateViewConfigurations" (xrEnumerateViewConfigurations' instance'' (systemId) (0) (pViewConfigurationTypeCountOutput) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  viewConfigurationTypeCountOutput <- lift $ peek @Word32 pViewConfigurationTypeCountOutput
  pViewConfigurationTypes <- ContT $ bracket (callocBytes @ViewConfigurationType ((fromIntegral (viewConfigurationTypeCountOutput)) * 4)) free
  r' <- lift $ traceAroundEvent "xrEnumerateViewConfigurations" (xrEnumerateViewConfigurations' instance'' (systemId) ((viewConfigurationTypeCountOutput)) (pViewConfigurationTypeCountOutput) (pViewConfigurationTypes))
  lift $ when (r' < SUCCESS) (throwIO (OpenXrException r'))
  viewConfigurationTypeCountOutput' <- lift $ peek @Word32 pViewConfigurationTypeCountOutput
  viewConfigurationTypes' <- lift $ generateM (fromIntegral (viewConfigurationTypeCountOutput')) (\i -> peek @ViewConfigurationType ((pViewConfigurationTypes `advancePtrBytes` (4 * (i)) :: Ptr ViewConfigurationType)))
  pure $ (viewConfigurationTypes')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetViewConfigurationProperties
  :: FunPtr (Ptr Instance_T -> SystemId -> ViewConfigurationType -> Ptr ViewConfigurationProperties -> IO Result) -> Ptr Instance_T -> SystemId -> ViewConfigurationType -> Ptr ViewConfigurationProperties -> IO Result

-- | xrGetViewConfigurationProperties - Gets information for a view
-- configuration
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'getViewConfigurationProperties' queries properties of an individual
-- view configuration. Applications /must/ use one of the supported view
-- configuration types returned by 'enumerateViewConfigurations'. If
-- @viewConfigurationType@ is not supported by this
-- 'OpenXR.Core10.Handles.Instance' the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED'.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SYSTEM_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Instance',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- 'ViewConfigurationProperties',
-- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType'
getViewConfigurationProperties :: forall io
                                . (MonadIO io)
                               => -- | @instance@ is the instance from which @systemId@ was retrieved.
                                  --
                                  -- #VUID-xrGetViewConfigurationProperties-instance-parameter# @instance@
                                  -- /must/ be a valid 'OpenXR.Core10.Handles.Instance' handle
                                  Instance
                               -> -- | @systemId@ is the
                                  -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
                                  -- whose view configuration is being queried.
                                  SystemId
                               -> -- | @viewConfigurationType@ is the
                                  -- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType' of the
                                  -- configuration to get.
                                  --
                                  -- #VUID-xrGetViewConfigurationProperties-viewConfigurationType-parameter#
                                  -- @viewConfigurationType@ /must/ be a valid
                                  -- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType' value
                                  ViewConfigurationType
                               -> io (ViewConfigurationProperties)
getViewConfigurationProperties instance' systemId viewConfigurationType = liftIO . evalContT $ do
  let xrGetViewConfigurationPropertiesPtr = pXrGetViewConfigurationProperties (instanceCmds (instance' :: Instance))
  lift $ unless (xrGetViewConfigurationPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetViewConfigurationProperties is null" Nothing Nothing
  let xrGetViewConfigurationProperties' = mkXrGetViewConfigurationProperties xrGetViewConfigurationPropertiesPtr
  pConfigurationProperties <- ContT (withZeroCStruct @ViewConfigurationProperties)
  r <- lift $ traceAroundEvent "xrGetViewConfigurationProperties" (xrGetViewConfigurationProperties' (instanceHandle (instance')) (systemId) (viewConfigurationType) (pConfigurationProperties))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  configurationProperties <- lift $ peekCStruct @ViewConfigurationProperties pConfigurationProperties
  pure $ (configurationProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrEnumerateViewConfigurationViews
  :: FunPtr (Ptr Instance_T -> SystemId -> ViewConfigurationType -> Word32 -> Ptr Word32 -> Ptr (SomeStruct ViewConfigurationView) -> IO Result) -> Ptr Instance_T -> SystemId -> ViewConfigurationType -> Word32 -> Ptr Word32 -> Ptr (SomeStruct ViewConfigurationView) -> IO Result

-- | xrEnumerateViewConfigurationViews - Gets view configuration views
--
-- == Parameter Descriptions
--
-- = Description
--
-- Each 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType'
-- defines the number of views associated with it. Applications can query
-- more details of each view element using
-- 'enumerateViewConfigurationViews'. If the supplied
-- @viewConfigurationType@ is not supported by this
-- 'OpenXR.Core10.Handles.Instance' and
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED'.
--
-- Runtimes /must/ always return identical buffer contents from this
-- enumeration for the given @systemId@ and @viewConfigurationType@ for the
-- lifetime of the instance.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrEnumerateViewConfigurationViews-instance-parameter#
--     @instance@ /must/ be a valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrEnumerateViewConfigurationViews-viewConfigurationType-parameter#
--     @viewConfigurationType@ /must/ be a valid
--     'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType'
--     value
--
-- -   #VUID-xrEnumerateViewConfigurationViews-viewCountOutput-parameter#
--     @viewCountOutput@ /must/ be a pointer to a @uint32_t@ value
--
-- -   #VUID-xrEnumerateViewConfigurationViews-views-parameter# If
--     @viewCapacityInput@ is not @0@, @views@ /must/ be a pointer to an
--     array of @viewCapacityInput@ 'ViewConfigurationView' structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SYSTEM_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Instance',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType',
-- 'ViewConfigurationView', 'getViewConfigurationProperties'
enumerateViewConfigurationViews :: forall a io
                                 . (Extendss ViewConfigurationView a, PokeChain a, PeekChain a, MonadIO io)
                                => -- | @instance@ is the instance from which @systemId@ was retrieved.
                                   Instance
                                -> -- | @systemId@ is the
                                   -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
                                   -- whose view configuration is being queried.
                                   SystemId
                                -> -- | @viewConfigurationType@ is the
                                   -- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType' of the
                                   -- configuration to get.
                                   ViewConfigurationType
                                -> io (("views" ::: Vector (ViewConfigurationView a)))
enumerateViewConfigurationViews instance' systemId viewConfigurationType = liftIO . evalContT $ do
  let xrEnumerateViewConfigurationViewsPtr = pXrEnumerateViewConfigurationViews (instanceCmds (instance' :: Instance))
  lift $ unless (xrEnumerateViewConfigurationViewsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrEnumerateViewConfigurationViews is null" Nothing Nothing
  let xrEnumerateViewConfigurationViews' = mkXrEnumerateViewConfigurationViews xrEnumerateViewConfigurationViewsPtr
  let instance'' = instanceHandle (instance')
  pViewCountOutput <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "xrEnumerateViewConfigurationViews" (xrEnumerateViewConfigurationViews' instance'' (systemId) (viewConfigurationType) (0) (pViewCountOutput) (forgetExtensions (nullPtr)))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  viewCountOutput <- lift $ peek @Word32 pViewCountOutput
  pViews <- ContT $ bracket (callocBytes @(ViewConfigurationView _) ((fromIntegral (viewCountOutput)) * 40)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pViews `advancePtrBytes` (i * 40) :: Ptr (ViewConfigurationView _)) . ($ ())) [0..(fromIntegral (viewCountOutput)) - 1]
  r' <- lift $ traceAroundEvent "xrEnumerateViewConfigurationViews" (xrEnumerateViewConfigurationViews' instance'' (systemId) (viewConfigurationType) ((viewCountOutput)) (pViewCountOutput) (forgetExtensions ((pViews))))
  lift $ when (r' < SUCCESS) (throwIO (OpenXrException r'))
  viewCountOutput' <- lift $ peek @Word32 pViewCountOutput
  views' <- lift $ generateM (fromIntegral (viewCountOutput')) (\i -> peekCStruct @(ViewConfigurationView _) (((pViews) `advancePtrBytes` (40 * (i)) :: Ptr (ViewConfigurationView _))))
  pure $ (views')


-- | XrViewConfigurationView - Individual view configuration
--
-- == Member Descriptions
--
-- = Description
--
-- See 'OpenXR.Core10.OtherTypes.SwapchainSubImage' for more information
-- about @imageRect@ values, and 'OpenXR.Core10.Image.SwapchainCreateInfo'
-- for more information about creating swapchains appropriately sized to
-- support those @imageRect@ values.
--
-- The array of 'ViewConfigurationView' returned by the runtime /must/
-- adhere to the rules defined in
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#view_configuration_type >,
-- such as the count and association to the left and right eyes.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'ViewConfigurationProperties',
-- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType',
-- 'enumerateViewConfigurationViews'
data ViewConfigurationView (es :: [Type]) = ViewConfigurationView
  { -- | @next@ is @NULL@ or a pointer to the next structure in a structure
    -- chain. No such structures are defined in core OpenXR.
    --
    -- #VUID-XrViewConfigurationView-next-next# @next@ /must/ be @NULL@ or a
    -- valid pointer to the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>.
    -- See also:
    -- 'OpenXR.Extensions.XR_EXT_view_configuration_depth_range.ViewConfigurationDepthRangeEXT',
    -- 'OpenXR.Extensions.XR_EPIC_view_configuration_fov.ViewConfigurationViewFovEPIC'
    next :: Chain es
  , -- | @recommendedImageRectWidth@ is the optimal width of @imageRect@ to use
    -- when rendering this view into a swapchain.
    recommendedImageRectWidth :: Word32
  , -- | @maxImageRectWidth@ is the maximum width of @imageRect@ supported when
    -- rendering this view into a swapchain.
    maxImageRectWidth :: Word32
  , -- | @recommendedImageRectHeight@ is the optimal height of @imageRect@ to use
    -- when rendering this view into a swapchain.
    recommendedImageRectHeight :: Word32
  , -- | @maxImageRectHeight@ is the maximum height of @imageRect@ supported when
    -- rendering this view into a swapchain.
    maxImageRectHeight :: Word32
  , -- | @recommendedSwapchainSampleCount@ is the recommended number of sub-data
    -- element samples to create for each swapchain image that will be rendered
    -- into for this view.
    recommendedSwapchainSampleCount :: Word32
  , -- | @maxSwapchainSampleCount@ is the maximum number of sub-data element
    -- samples supported for swapchain images that will be rendered into for
    -- this view.
    maxSwapchainSampleCount :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ViewConfigurationView (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ViewConfigurationView es)

instance Extensible ViewConfigurationView where
  extensibleTypeName = "ViewConfigurationView"
  setNext x next = x{next = next}
  getNext ViewConfigurationView{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ViewConfigurationView e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ViewConfigurationViewFovEPIC = Just f
    | Just Refl <- eqT @e @ViewConfigurationDepthRangeEXT = Just f
    | otherwise = Nothing

instance (Extendss ViewConfigurationView es, PokeChain es) => ToCStruct (ViewConfigurationView es) where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ViewConfigurationView{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VIEW_CONFIGURATION_VIEW)
    next'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) next''
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (recommendedImageRectWidth)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (maxImageRectWidth)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (recommendedImageRectHeight)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (maxImageRectHeight)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (recommendedSwapchainSampleCount)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (maxSwapchainSampleCount)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VIEW_CONFIGURATION_VIEW)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    lift $ f

instance (Extendss ViewConfigurationView es, PeekChain es) => FromCStruct (ViewConfigurationView es) where
  peekCStruct p = do
    next <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next' <- peekChain (castPtr next)
    recommendedImageRectWidth <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxImageRectWidth <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    recommendedImageRectHeight <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    maxImageRectHeight <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    recommendedSwapchainSampleCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    maxSwapchainSampleCount <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    pure $ ViewConfigurationView
             next' recommendedImageRectWidth maxImageRectWidth recommendedImageRectHeight maxImageRectHeight recommendedSwapchainSampleCount maxSwapchainSampleCount

instance es ~ '[] => Zero (ViewConfigurationView es) where
  zero = ViewConfigurationView
           ()
           zero
           zero
           zero
           zero
           zero
           zero


-- | XrViewConfigurationProperties - Detailed configuration properties for an
-- XrViewConfigurationProperties
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >,
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType',
-- 'ViewConfigurationView', 'getViewConfigurationProperties'
data ViewConfigurationProperties = ViewConfigurationProperties
  { -- | @viewConfigurationType@ is the
    -- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType' of the
    -- configuration.
    --
    -- #VUID-XrViewConfigurationProperties-viewConfigurationType-parameter#
    -- @viewConfigurationType@ /must/ be a valid
    -- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType' value
    viewConfigurationType :: ViewConfigurationType
  , -- | @fovMutable@ indicates if the view field of view can be modified by the
    -- application.
    fovMutable :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ViewConfigurationProperties)
#endif
deriving instance Show ViewConfigurationProperties

instance ToCStruct ViewConfigurationProperties where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ViewConfigurationProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VIEW_CONFIGURATION_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ViewConfigurationType)) (viewConfigurationType)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (fovMutable))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VIEW_CONFIGURATION_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ViewConfigurationType)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct ViewConfigurationProperties where
  peekCStruct p = do
    viewConfigurationType <- peek @ViewConfigurationType ((p `plusPtr` 16 :: Ptr ViewConfigurationType))
    fovMutable <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ ViewConfigurationProperties
             viewConfigurationType (bool32ToBool fovMutable)

instance Storable ViewConfigurationProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ViewConfigurationProperties where
  zero = ViewConfigurationProperties
           zero
           zero

