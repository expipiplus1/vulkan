{-# language CPP #-}
-- | = Name
--
-- XR_FB_display_refresh_rate - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_FB_display_refresh_rate  XR_FB_display_refresh_rate>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 102
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
-- 'EventDataDisplayRefreshRateChangedFB',
-- 'enumerateDisplayRefreshRatesFB', 'getDisplayRefreshRateFB',
-- 'requestDisplayRefreshRateFB'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_FB_display_refresh_rate OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_FB_display_refresh_rate  ( enumerateDisplayRefreshRatesFB
                                                     , getDisplayRefreshRateFB
                                                     , requestDisplayRefreshRateFB
                                                     , EventDataDisplayRefreshRateChangedFB(..)
                                                     , FB_display_refresh_rate_SPEC_VERSION
                                                     , pattern FB_display_refresh_rate_SPEC_VERSION
                                                     , FB_DISPLAY_REFRESH_RATE_EXTENSION_NAME
                                                     , pattern FB_DISPLAY_REFRESH_RATE_EXTENSION_NAME
                                                     ) where

import OpenXR.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Foreign.C.Types (CFloat(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
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
import OpenXR.NamedType ((:::))
import OpenXR.Core10.OtherTypes (EventDataBaseHeader(..))
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.Dynamic (InstanceCmds(pXrEnumerateDisplayRefreshRatesFB))
import OpenXR.Dynamic (InstanceCmds(pXrGetDisplayRefreshRateFB))
import OpenXR.Dynamic (InstanceCmds(pXrRequestDisplayRefreshRateFB))
import OpenXR.Core10.OtherTypes (IsEventData(..))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Handles (Session)
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Session_T)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_EVENT_DATA_DISPLAY_REFRESH_RATE_CHANGED_FB))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrEnumerateDisplayRefreshRatesFB
  :: FunPtr (Ptr Session_T -> Word32 -> Ptr Word32 -> Ptr CFloat -> IO Result) -> Ptr Session_T -> Word32 -> Ptr Word32 -> Ptr CFloat -> IO Result

-- | xrEnumerateDisplayRefreshRatesFB - Enumerates display refresh rates
--
-- == Parameter Descriptions
--
-- -   @session@ is the session that enumerates the supported display
--     refresh rates.
--
-- -   @displayRefreshRateCapacityInput@ is the capacity of the
--     @displayRefreshRates@, or 0 to retrieve the required capacity.
--
-- -   @displayRefreshRateCountOutput@ is a pointer to the count of @float@
--     @displayRefreshRates@ written, or a pointer to the required capacity
--     in the case that @displayRefreshRateCapacityInput@ is @0@.
--
-- -   @displayRefreshRates@ is a pointer to an array of @float@ display
--     refresh rates, but /can/ be @NULL@ if
--     @displayRefreshRateCapacityInput@ is @0@.
--
-- -   See
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#buffer-size-parameters Buffer Size Parameters>
--     chapter for a detailed description of retrieving the required
--     @displayRefreshRates@ size.
--
-- = Description
--
-- 'enumerateDisplayRefreshRatesFB' enumerates the display refresh rates
-- supported by the current session. Display refresh rates /must/ be in
-- order from lowest to highest supported display refresh rates. Runtimes
-- /must/ always return identical buffer contents from this enumeration for
-- the lifetime of the session.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrEnumerateDisplayRefreshRatesFB-extension-notenabled# The @@
--     extension /must/ be enabled prior to calling
--     'enumerateDisplayRefreshRatesFB'
--
-- -   #VUID-xrEnumerateDisplayRefreshRatesFB-session-parameter# @session@
--     /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrEnumerateDisplayRefreshRatesFB-displayRefreshRateCountOutput-parameter#
--     @displayRefreshRateCountOutput@ /must/ be a pointer to a @uint32_t@
--     value
--
-- -   #VUID-xrEnumerateDisplayRefreshRatesFB-displayRefreshRates-parameter#
--     If @displayRefreshRateCapacityInput@ is not @0@,
--     @displayRefreshRates@ /must/ be a pointer to an array of
--     @displayRefreshRateCapacityInput@ @float@ values
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
--     -   'OpenXR.Core10.Enums.Result.SESSION_LOSS_PENDING'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session', 'getDisplayRefreshRateFB',
-- 'requestDisplayRefreshRateFB'
enumerateDisplayRefreshRatesFB :: forall io
                                . (MonadIO io)
                               => -- No documentation found for Nested "xrEnumerateDisplayRefreshRatesFB" "session"
                                  Session
                               -> io (Result, ("displayRefreshRates" ::: Vector Float))
enumerateDisplayRefreshRatesFB session = liftIO . evalContT $ do
  let xrEnumerateDisplayRefreshRatesFBPtr = pXrEnumerateDisplayRefreshRatesFB (instanceCmds (session :: Session))
  lift $ unless (xrEnumerateDisplayRefreshRatesFBPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrEnumerateDisplayRefreshRatesFB is null" Nothing Nothing
  let xrEnumerateDisplayRefreshRatesFB' = mkXrEnumerateDisplayRefreshRatesFB xrEnumerateDisplayRefreshRatesFBPtr
  let session' = sessionHandle (session)
  pDisplayRefreshRateCountOutput <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "xrEnumerateDisplayRefreshRatesFB" (xrEnumerateDisplayRefreshRatesFB' session' (0) (pDisplayRefreshRateCountOutput) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  displayRefreshRateCountOutput <- lift $ peek @Word32 pDisplayRefreshRateCountOutput
  pDisplayRefreshRates <- ContT $ bracket (callocBytes @CFloat ((fromIntegral (displayRefreshRateCountOutput)) * 4)) free
  r' <- lift $ traceAroundEvent "xrEnumerateDisplayRefreshRatesFB" (xrEnumerateDisplayRefreshRatesFB' session' ((displayRefreshRateCountOutput)) (pDisplayRefreshRateCountOutput) (pDisplayRefreshRates))
  lift $ when (r' < SUCCESS) (throwIO (OpenXrException r'))
  displayRefreshRateCountOutput' <- lift $ peek @Word32 pDisplayRefreshRateCountOutput
  displayRefreshRates' <- lift $ generateM (fromIntegral (displayRefreshRateCountOutput')) (\i -> do
    displayRefreshRatesElem <- peek @CFloat ((pDisplayRefreshRates `advancePtrBytes` (4 * (i)) :: Ptr CFloat))
    pure $ coerce @CFloat @Float displayRefreshRatesElem)
  pure $ ((r'), displayRefreshRates')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetDisplayRefreshRateFB
  :: FunPtr (Ptr Session_T -> Ptr CFloat -> IO Result) -> Ptr Session_T -> Ptr CFloat -> IO Result

-- | xrGetDisplayRefreshRateFB - Get the current display refresh rate
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'getDisplayRefreshRateFB' retrieves the current display refresh rate.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrGetDisplayRefreshRateFB-extension-notenabled# The @@
--     extension /must/ be enabled prior to calling
--     'getDisplayRefreshRateFB'
--
-- -   #VUID-xrGetDisplayRefreshRateFB-session-parameter# @session@ /must/
--     be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrGetDisplayRefreshRateFB-displayRefreshRate-parameter#
--     @displayRefreshRate@ /must/ be a pointer to a @float@ value
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
--     -   'OpenXR.Core10.Enums.Result.SESSION_LOSS_PENDING'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session', 'enumerateDisplayRefreshRatesFB',
-- 'requestDisplayRefreshRateFB'
getDisplayRefreshRateFB :: forall io
                         . (MonadIO io)
                        => -- | @session@ is the 'OpenXR.Core10.Handles.Session' to query.
                           Session
                        -> io (Result, ("displayRefreshRate" ::: Float))
getDisplayRefreshRateFB session = liftIO . evalContT $ do
  let xrGetDisplayRefreshRateFBPtr = pXrGetDisplayRefreshRateFB (instanceCmds (session :: Session))
  lift $ unless (xrGetDisplayRefreshRateFBPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetDisplayRefreshRateFB is null" Nothing Nothing
  let xrGetDisplayRefreshRateFB' = mkXrGetDisplayRefreshRateFB xrGetDisplayRefreshRateFBPtr
  pDisplayRefreshRate <- ContT $ bracket (callocBytes @CFloat 4) free
  r <- lift $ traceAroundEvent "xrGetDisplayRefreshRateFB" (xrGetDisplayRefreshRateFB' (sessionHandle (session)) (pDisplayRefreshRate))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  displayRefreshRate <- lift $ peek @CFloat pDisplayRefreshRate
  pure $ (r, (coerce @CFloat @Float displayRefreshRate))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrRequestDisplayRefreshRateFB
  :: FunPtr (Ptr Session_T -> CFloat -> IO Result) -> Ptr Session_T -> CFloat -> IO Result

-- | xrRequestDisplayRefreshRateFB - Request a display refresh rate
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'requestDisplayRefreshRateFB' provides a mechanism for an application to
-- request the system to dynamically change the display refresh rate to the
-- application preferred value. The runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_DISPLAY_REFRESH_RATE_UNSUPPORTED_FB'
-- if @displayRefreshRate@ is not either @0.0f@ or one of the values
-- enumerated by 'enumerateDisplayRefreshRatesFB'. A display refresh rate
-- of @0.0f@ indicates the application has no preference.
--
-- Note that this is only a request and does not guarantee the system will
-- switch to the requested display refresh rate.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrRequestDisplayRefreshRateFB-extension-notenabled# The @@
--     extension /must/ be enabled prior to calling
--     'requestDisplayRefreshRateFB'
--
-- -   #VUID-xrRequestDisplayRefreshRateFB-session-parameter# @session@
--     /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
--     -   'OpenXR.Core10.Enums.Result.SESSION_LOSS_PENDING'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_DISPLAY_REFRESH_RATE_UNSUPPORTED_FB'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FEATURE_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session', 'enumerateDisplayRefreshRatesFB',
-- 'getDisplayRefreshRateFB'
requestDisplayRefreshRateFB :: forall io
                             . (MonadIO io)
                            => -- | @session@ is a valid 'OpenXR.Core10.Handles.Session' handle.
                               Session
                            -> -- | @displayRefreshRate@ is @0.0f@ or a supported display refresh rate.
                               -- Supported display refresh rates are indicated by
                               -- 'enumerateDisplayRefreshRatesFB'.
                               ("displayRefreshRate" ::: Float)
                            -> io (Result)
requestDisplayRefreshRateFB session displayRefreshRate = liftIO $ do
  let xrRequestDisplayRefreshRateFBPtr = pXrRequestDisplayRefreshRateFB (instanceCmds (session :: Session))
  unless (xrRequestDisplayRefreshRateFBPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrRequestDisplayRefreshRateFB is null" Nothing Nothing
  let xrRequestDisplayRefreshRateFB' = mkXrRequestDisplayRefreshRateFB xrRequestDisplayRefreshRateFBPtr
  r <- traceAroundEvent "xrRequestDisplayRefreshRateFB" (xrRequestDisplayRefreshRateFB' (sessionHandle (session)) (CFloat (displayRefreshRate)))
  when (r < SUCCESS) (throwIO (OpenXrException r))
  pure $ (r)


-- | XrEventDataDisplayRefreshRateChangedFB - Event representing display
-- refresh rate change
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrEventDataDisplayRefreshRateChangedFB-extension-notenabled#
--     The @@ extension /must/ be enabled prior to using
--     'EventDataDisplayRefreshRateChangedFB'
--
-- -   #VUID-XrEventDataDisplayRefreshRateChangedFB-type-type# @type@
--     /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_EVENT_DATA_DISPLAY_REFRESH_RATE_CHANGED_FB'
--
-- -   #VUID-XrEventDataDisplayRefreshRateChangedFB-next-next# @next@
--     /must/ be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'getDisplayRefreshRateFB'
data EventDataDisplayRefreshRateChangedFB = EventDataDisplayRefreshRateChangedFB
  { -- | @fromDisplayRefreshRate@ is the previous display refresh rate.
    fromDisplayRefreshRate :: Float
  , -- | @toDisplayRefreshRate@ is the new display refresh rate.
    toDisplayRefreshRate :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (EventDataDisplayRefreshRateChangedFB)
#endif
deriving instance Show EventDataDisplayRefreshRateChangedFB

instance IsEventData EventDataDisplayRefreshRateChangedFB where
  toEventDataBaseHeader EventDataDisplayRefreshRateChangedFB{} = EventDataBaseHeader{type' = TYPE_EVENT_DATA_DISPLAY_REFRESH_RATE_CHANGED_FB}

instance ToCStruct EventDataDisplayRefreshRateChangedFB where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p EventDataDisplayRefreshRateChangedFB{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EVENT_DATA_DISPLAY_REFRESH_RATE_CHANGED_FB)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (fromDisplayRefreshRate))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (toDisplayRefreshRate))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EVENT_DATA_DISPLAY_REFRESH_RATE_CHANGED_FB)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct EventDataDisplayRefreshRateChangedFB where
  peekCStruct p = do
    fromDisplayRefreshRate <- peek @CFloat ((p `plusPtr` 16 :: Ptr CFloat))
    toDisplayRefreshRate <- peek @CFloat ((p `plusPtr` 20 :: Ptr CFloat))
    pure $ EventDataDisplayRefreshRateChangedFB
             (coerce @CFloat @Float fromDisplayRefreshRate) (coerce @CFloat @Float toDisplayRefreshRate)

instance Storable EventDataDisplayRefreshRateChangedFB where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero EventDataDisplayRefreshRateChangedFB where
  zero = EventDataDisplayRefreshRateChangedFB
           zero
           zero


type FB_display_refresh_rate_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_FB_display_refresh_rate_SPEC_VERSION"
pattern FB_display_refresh_rate_SPEC_VERSION :: forall a . Integral a => a
pattern FB_display_refresh_rate_SPEC_VERSION = 1


type FB_DISPLAY_REFRESH_RATE_EXTENSION_NAME = "XR_FB_display_refresh_rate"

-- No documentation found for TopLevel "XR_FB_DISPLAY_REFRESH_RATE_EXTENSION_NAME"
pattern FB_DISPLAY_REFRESH_RATE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern FB_DISPLAY_REFRESH_RATE_EXTENSION_NAME = "XR_FB_display_refresh_rate"

