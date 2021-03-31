{-# language CPP #-}
-- No documentation found for Chapter "Instance"
module OpenXR.Core10.Instance  ( getInstanceProcAddr
                               , enumerateApiLayerProperties
                               , enumerateInstanceExtensionProperties
                               , createInstance
                               , withInstance
                               , destroyInstance
                               , resultToString
                               , structureTypeToString
                               , getInstanceProperties
                               , pollEvent
                               , ApiLayerProperties(..)
                               , ExtensionProperties(..)
                               , ApplicationInfo(..)
                               , InstanceCreateInfo(..)
                               , InstanceProperties(..)
                               , EventDataBuffer(..)
                               ) where

import OpenXR.CStruct.Utils (FixedArray)
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
import Foreign.Ptr (castFunPtr)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Foreign.C.Types (CChar(..))
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Ptr (Ptr(Ptr))
import Data.Word (Word32)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import OpenXR.CStruct.Utils (advancePtrBytes)
import OpenXR.CStruct.Utils (callocFixedArray)
import OpenXR.CStruct.Extends (forgetExtensions)
import OpenXR.Dynamic (getInstanceProcAddr')
import OpenXR.Dynamic (initInstanceCmds)
import OpenXR.CStruct.Utils (lowerArrayPtr)
import OpenXR.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import OpenXR.CStruct.Utils (pokeFixedLengthByteString)
import OpenXR.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import OpenXR.NamedType ((:::))
import OpenXR.CStruct.Extends (Chain)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_debug_utils (DebugUtilsMessengerCreateInfoEXT)
import OpenXR.CStruct.Extends (Extends)
import OpenXR.CStruct.Extends (Extendss)
import OpenXR.CStruct.Extends (Extensible(..))
import OpenXR.Core10.Handles (Instance)
import OpenXR.Core10.Handles (Instance(..))
import OpenXR.Core10.Handles (Instance(Instance))
import OpenXR.Dynamic (InstanceCmds(pXrDestroyInstance))
import OpenXR.Dynamic (InstanceCmds(pXrGetInstanceProcAddr))
import OpenXR.Dynamic (InstanceCmds(pXrGetInstanceProperties))
import OpenXR.Dynamic (InstanceCmds(pXrPollEvent))
import OpenXR.Dynamic (InstanceCmds(pXrResultToString))
import OpenXR.Dynamic (InstanceCmds(pXrStructureTypeToString))
import OpenXR.Core10.Enums.InstanceCreateFlagBits (InstanceCreateFlags)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_android_create_instance (InstanceCreateInfoAndroidKHR)
import OpenXR.Core10.Handles (Instance_T)
import OpenXR.Core10.APIConstants (MAX_API_LAYER_DESCRIPTION_SIZE)
import OpenXR.Core10.APIConstants (MAX_API_LAYER_NAME_SIZE)
import OpenXR.Core10.APIConstants (MAX_APPLICATION_NAME_SIZE)
import OpenXR.Core10.APIConstants (MAX_ENGINE_NAME_SIZE)
import OpenXR.Core10.APIConstants (MAX_EXTENSION_NAME_SIZE)
import OpenXR.Core10.APIConstants (MAX_RESULT_STRING_SIZE)
import OpenXR.Core10.APIConstants (MAX_RUNTIME_NAME_SIZE)
import OpenXR.Core10.APIConstants (MAX_STRUCTURE_NAME_SIZE)
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.FuncPointers (PFN_xrVoidFunction)
import OpenXR.CStruct.Extends (PeekChain)
import OpenXR.CStruct.Extends (PeekChain(..))
import OpenXR.CStruct.Extends (PokeChain)
import OpenXR.CStruct.Extends (PokeChain(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.CStruct.Extends (SomeStruct)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Enums.StructureType (StructureType(..))
import OpenXR.Version (Version)
import OpenXR.Core10.APIConstants (pattern MAX_RESULT_STRING_SIZE)
import OpenXR.Core10.APIConstants (pattern MAX_STRUCTURE_NAME_SIZE)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_API_LAYER_PROPERTIES))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_EVENT_DATA_BUFFER))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_EXTENSION_PROPERTIES))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_INSTANCE_CREATE_INFO))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_INSTANCE_PROPERTIES))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetInstanceProcAddr
  :: FunPtr (Ptr Instance_T -> Ptr CChar -> Ptr PFN_xrVoidFunction -> IO Result) -> Ptr Instance_T -> Ptr CChar -> Ptr PFN_xrVoidFunction -> IO Result

-- | xrGetInstanceProcAddr - Gets a function pointer for an OpenXR function
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'getInstanceProcAddr' itself is obtained in a platform- and loader-
-- specific manner. Typically, the loader library will export this function
-- as a function symbol, so applications /can/ link against the loader
-- library, or load it dynamically and look up the symbol using
-- platform-specific APIs. Loaders /must/ export function symbols for all
-- core OpenXR functions. Because of this, applications that use only the
-- core OpenXR functions have no need to use 'getInstanceProcAddr'.
--
-- Because an application /can/ call 'getInstanceProcAddr' before creating
-- an instance, 'getInstanceProcAddr' returns a valid function pointer when
-- the @instance@ parameter is
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_NULL_HANDLE XR_NULL_HANDLE>
-- and the @name@ parameter is one of the following strings:
--
-- == No Instance Required
--
-- -   'enumerateInstanceExtensionProperties'
--
-- -   'enumerateApiLayerProperties'
--
-- -   'createInstance'
--
-- 'getInstanceProcAddr' /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID' if @name@ is not one
-- of the above strings and @instance@ is
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_NULL_HANDLE XR_NULL_HANDLE>.
-- 'getInstanceProcAddr' /may/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID' if @name@ is not one
-- of the above strings and @instance@ is invalid but not
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_NULL_HANDLE XR_NULL_HANDLE>.
--
-- 'getInstanceProcAddr' /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED' if @instance@ is
-- a valid instance and the string specified in @name@ is not the name of
-- an OpenXR core or enabled extension function.
--
-- If @name@ is the name of an extension function, then the result returned
-- by 'getInstanceProcAddr' will depend upon how the @instance@ was
-- created. If @instance@ was created with the related extension’s name
-- appearing in the 'InstanceCreateInfo'::@enabledExtensionNames@ array,
-- then 'getInstanceProcAddr' returns a valid function pointer. If the
-- related extension’s name did not appear in the
-- 'InstanceCreateInfo'::@enabledExtensionNames@ array during the creation
-- of @instance@, then 'getInstanceProcAddr' returns
-- 'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'. Because of
-- this, function pointers returned by 'getInstanceProcAddr' using one
-- 'OpenXR.Core10.Handles.Instance' may not be valid when used with objects
-- related to a different 'OpenXR.Core10.Handles.Instance'.
--
-- The returned function pointer is of type
-- 'OpenXR.Core10.FuncPointers.PFN_xrVoidFunction', and must be cast to the
-- type of the function being queried.
--
-- The table below defines the various use cases for 'getInstanceProcAddr'
-- and return value (“fp” is “function pointer”) for each case.
--
-- +------------------+----------------------------------------+------------------+
-- | @instance@       | @name@ parameter                       | return value     |
-- | parameter        |                                        |                  |
-- +==================+========================================+==================+
-- | *                | @NULL@                                 | undefined        |
-- +------------------+----------------------------------------+------------------+
-- | invalid instance | *                                      | undefined        |
-- +------------------+----------------------------------------+------------------+
-- | @NULL@           | 'enumerateInstanceExtensionProperties' | fp               |
-- +------------------+----------------------------------------+------------------+
-- | @NULL@           | 'enumerateApiLayerProperties'          | fp               |
-- +------------------+----------------------------------------+------------------+
-- | @NULL@           | 'createInstance'                       | fp               |
-- +------------------+----------------------------------------+------------------+
-- | @NULL@           | * (any @name@ not covered above)       | @NULL@           |
-- +------------------+----------------------------------------+------------------+
-- | instance         | core OpenXR function                   | fp1              |
-- +------------------+----------------------------------------+------------------+
-- | instance         | enabled extension function for         | fp1              |
-- |                  | @instance@                             |                  |
-- +------------------+----------------------------------------+------------------+
-- | instance         | * (any @name@ not covered above)       | @NULL@           |
-- +------------------+----------------------------------------+------------------+
--
-- xrGetInstanceProcAddr behavior
--
-- [1]
--     The returned function pointer /must/ only be called with a handle
--     (the first parameter) that is @instance@ or a child of @instance@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrGetInstanceProcAddr-instance-parameter# If @instance@ is not
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_NULL_HANDLE XR_NULL_HANDLE>,
--     @instance@ /must/ be a valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrGetInstanceProcAddr-name-parameter# @name@ /must/ be a
--     null-terminated UTF-8 string
--
-- -   #VUID-xrGetInstanceProcAddr-function-parameter# @function@ /must/ be
--     a pointer to a 'OpenXR.Core10.FuncPointers.PFN_xrVoidFunction' value
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_OUT_OF_MEMORY'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- = See Also
--
-- 'OpenXR.Core10.FuncPointers.PFN_xrVoidFunction',
-- 'OpenXR.Core10.Handles.Instance'
getInstanceProcAddr :: forall io
                     . (MonadIO io)
                    => -- | @instance@ is the instance that the function pointer will be compatible
                       -- with, or @NULL@ for functions not dependent on any instance.
                       Instance
                    -> -- | @name@ is the name of the function to obtain.
                       ("name" ::: ByteString)
                    -> io (PFN_xrVoidFunction)
getInstanceProcAddr instance' name = liftIO . evalContT $ do
  let xrGetInstanceProcAddrPtr = pXrGetInstanceProcAddr (instanceCmds (instance' :: Instance))
  lift $ unless (xrGetInstanceProcAddrPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetInstanceProcAddr is null" Nothing Nothing
  let xrGetInstanceProcAddr' = mkXrGetInstanceProcAddr xrGetInstanceProcAddrPtr
  name' <- ContT $ useAsCString (name)
  pFunction <- ContT $ bracket (callocBytes @PFN_xrVoidFunction 8) free
  r <- lift $ traceAroundEvent "xrGetInstanceProcAddr" (xrGetInstanceProcAddr' (instanceHandle (instance')) name' (pFunction))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  function <- lift $ peek @PFN_xrVoidFunction pFunction
  pure $ (function)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrEnumerateApiLayerProperties
  :: FunPtr (Word32 -> Ptr Word32 -> Ptr ApiLayerProperties -> IO Result) -> Word32 -> Ptr Word32 -> Ptr ApiLayerProperties -> IO Result

-- | xrEnumerateApiLayerProperties - Returns up to requested number of global
-- layer properties
--
-- == Parameter Descriptions
--
-- -   @propertyCapacityInput@ is the capacity of the properties array, or
--     0 to indicate a request to retrieve the required capacity.
--
-- -   @propertyCountOutput@ is a pointer to the count of properties
--     written, or a pointer to the required capacity in the case that
--     propertyCapacityInput is 0.
--
-- -   @properties@ is a pointer to an array of 'ApiLayerProperties'
--     structures, but /can/ be @NULL@ if propertyCapacityInput is 0.
--
-- -   See the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#buffer-size-parameters Buffer Size Parameters>
--     section for a detailed description of retrieving the required
--     @properties@ size.
--
-- = Description
--
-- The list of available layers may change at any time due to actions
-- outside of the OpenXR runtime, so two calls to
-- 'enumerateApiLayerProperties' with the same parameters /may/ return
-- different results, or retrieve different @propertyCountOutput@ values or
-- @properties@ contents.
--
-- Once an instance has been created, the layers enabled for that instance
-- will continue to be enabled and valid for the lifetime of that instance,
-- even if some of them become unavailable for future instances.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrEnumerateApiLayerProperties-propertyCountOutput-parameter#
--     @propertyCountOutput@ /must/ be a pointer to a @uint32_t@ value
--
-- -   #VUID-xrEnumerateApiLayerProperties-properties-parameter# If
--     @propertyCapacityInput@ is not @0@, @properties@ /must/ be a pointer
--     to an array of @propertyCapacityInput@ 'ApiLayerProperties'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_OUT_OF_MEMORY'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT'
--
-- = See Also
--
-- 'ApiLayerProperties'
enumerateApiLayerProperties :: forall io
                             . (MonadIO io)
                            => io (("properties" ::: Vector ApiLayerProperties))
enumerateApiLayerProperties  = liftIO . evalContT $ do
  xrEnumerateApiLayerPropertiesPtr <- lift $ castFunPtr @_ @(("propertyCapacityInput" ::: Word32) -> ("propertyCountOutput" ::: Ptr Word32) -> Ptr ApiLayerProperties -> IO Result) <$> getInstanceProcAddr' nullPtr (Ptr "xrEnumerateApiLayerProperties"#)
  lift $ unless (xrEnumerateApiLayerPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrEnumerateApiLayerProperties is null" Nothing Nothing
  let xrEnumerateApiLayerProperties' = mkXrEnumerateApiLayerProperties xrEnumerateApiLayerPropertiesPtr
  pPropertyCountOutput <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "xrEnumerateApiLayerProperties" (xrEnumerateApiLayerProperties' (0) (pPropertyCountOutput) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  propertyCountOutput <- lift $ peek @Word32 pPropertyCountOutput
  pProperties <- ContT $ bracket (callocBytes @ApiLayerProperties ((fromIntegral (propertyCountOutput)) * 544)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pProperties `advancePtrBytes` (i * 544) :: Ptr ApiLayerProperties) . ($ ())) [0..(fromIntegral (propertyCountOutput)) - 1]
  r' <- lift $ traceAroundEvent "xrEnumerateApiLayerProperties" (xrEnumerateApiLayerProperties' ((propertyCountOutput)) (pPropertyCountOutput) ((pProperties)))
  lift $ when (r' < SUCCESS) (throwIO (OpenXrException r'))
  propertyCountOutput' <- lift $ peek @Word32 pPropertyCountOutput
  properties' <- lift $ generateM (fromIntegral (propertyCountOutput')) (\i -> peekCStruct @ApiLayerProperties (((pProperties) `advancePtrBytes` (544 * (i)) :: Ptr ApiLayerProperties)))
  pure $ (properties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrEnumerateInstanceExtensionProperties
  :: FunPtr (Ptr CChar -> Word32 -> Ptr Word32 -> Ptr ExtensionProperties -> IO Result) -> Ptr CChar -> Word32 -> Ptr Word32 -> Ptr ExtensionProperties -> IO Result

-- | xrEnumerateInstanceExtensionProperties - Returns properties of available
-- instance extensions
--
-- == Parameter Descriptions
--
-- -   @layerName@ is either @NULL@ or a pointer to a string naming the API
--     layer to retrieve extensions from, as returned by
--     'enumerateApiLayerProperties'.
--
-- -   @propertyCapacityInput@ is the capacity of the properties array, or
--     @0@ to indicate a request to retrieve the required capacity.
--
-- -   @propertyCountOutput@ is a pointer to the count of properties
--     written, or a pointer to the required capacity in the case that
--     @propertyCapacityInput@ is @0@.
--
-- -   @properties@ is a pointer to an array of 'ExtensionProperties'
--     structures, but /can/ be @NULL@ if @propertyCapacityInput@ is @0@.
--
-- -   See the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#buffer-size-parameters Buffer Size Parameters>
--     section for a detailed description of retrieving the required
--     @properties@ size.
--
-- = Description
--
-- If @properties@ is @NULL@, then the number of extensions properties
-- available is returned in @propertyCountOutput@. Otherwise,
-- @propertyCountInput@ must point to a variable set by the user to the
-- number of elements in the @properties@ array. If @propertyCountInput@ is
-- less than the number of extension properties available, the contents of
-- @properties@ will be undefined. If @propertyCountInput@ is smaller than
-- the number of extensions available, the runtime /must/ return the
-- failure code 'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT' and
-- the contents of @properties@ are undefined.
--
-- Because the list of available layers may change externally between calls
-- to 'enumerateInstanceExtensionProperties', two calls /may/ retrieve
-- different results if a @layerName@ is available in one call but not in
-- another. The extensions supported by a layer may also change between two
-- calls, e.g. if the layer implementation is replaced by a different
-- version between those calls.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrEnumerateInstanceExtensionProperties-layerName-parameter# If
--     @layerName@ is not @NULL@, @layerName@ /must/ be a null-terminated
--     UTF-8 string
--
-- -   #VUID-xrEnumerateInstanceExtensionProperties-propertyCountOutput-parameter#
--     @propertyCountOutput@ /must/ be a pointer to a @uint32_t@ value
--
-- -   #VUID-xrEnumerateInstanceExtensionProperties-properties-parameter#
--     If @propertyCapacityInput@ is not @0@, @properties@ /must/ be a
--     pointer to an array of @propertyCapacityInput@ 'ExtensionProperties'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_OUT_OF_MEMORY'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_API_LAYER_NOT_PRESENT'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT'
--
-- = See Also
--
-- 'ExtensionProperties'
enumerateInstanceExtensionProperties :: forall io
                                      . (MonadIO io)
                                     => -- No documentation found for Nested "xrEnumerateInstanceExtensionProperties" "layerName"
                                        ("layerName" ::: Maybe ByteString)
                                     -> io (("properties" ::: Vector ExtensionProperties))
enumerateInstanceExtensionProperties layerName = liftIO . evalContT $ do
  xrEnumerateInstanceExtensionPropertiesPtr <- lift $ castFunPtr @_ @(("layerName" ::: Ptr CChar) -> ("propertyCapacityInput" ::: Word32) -> ("propertyCountOutput" ::: Ptr Word32) -> Ptr ExtensionProperties -> IO Result) <$> getInstanceProcAddr' nullPtr (Ptr "xrEnumerateInstanceExtensionProperties"#)
  lift $ unless (xrEnumerateInstanceExtensionPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrEnumerateInstanceExtensionProperties is null" Nothing Nothing
  let xrEnumerateInstanceExtensionProperties' = mkXrEnumerateInstanceExtensionProperties xrEnumerateInstanceExtensionPropertiesPtr
  layerName' <- case (layerName) of
    Nothing -> pure nullPtr
    Just j -> ContT $ useAsCString (j)
  pPropertyCountOutput <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "xrEnumerateInstanceExtensionProperties" (xrEnumerateInstanceExtensionProperties' layerName' (0) (pPropertyCountOutput) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  propertyCountOutput <- lift $ peek @Word32 pPropertyCountOutput
  pProperties <- ContT $ bracket (callocBytes @ExtensionProperties ((fromIntegral (propertyCountOutput)) * 152)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pProperties `advancePtrBytes` (i * 152) :: Ptr ExtensionProperties) . ($ ())) [0..(fromIntegral (propertyCountOutput)) - 1]
  r' <- lift $ traceAroundEvent "xrEnumerateInstanceExtensionProperties" (xrEnumerateInstanceExtensionProperties' layerName' ((propertyCountOutput)) (pPropertyCountOutput) ((pProperties)))
  lift $ when (r' < SUCCESS) (throwIO (OpenXrException r'))
  propertyCountOutput' <- lift $ peek @Word32 pPropertyCountOutput
  properties' <- lift $ generateM (fromIntegral (propertyCountOutput')) (\i -> peekCStruct @ExtensionProperties (((pProperties) `advancePtrBytes` (152 * (i)) :: Ptr ExtensionProperties)))
  pure $ (properties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrCreateInstance
  :: FunPtr (Ptr (SomeStruct InstanceCreateInfo) -> Ptr (Ptr Instance_T) -> IO Result) -> Ptr (SomeStruct InstanceCreateInfo) -> Ptr (Ptr Instance_T) -> IO Result

-- | xrCreateInstance - Creates an OpenXR Instance
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'createInstance' creates the 'OpenXR.Core10.Handles.Instance', then
-- enables and initializes global API layers and extensions requested by
-- the application. If an extension is provided by an API layer, both the
-- API layer and extension /must/ be specified at 'createInstance' time. If
-- a specified API layer cannot be found, no
-- 'OpenXR.Core10.Handles.Instance' will be created and the function will
-- return 'OpenXR.Core10.Enums.Result.ERROR_API_LAYER_NOT_PRESENT'.
-- Likewise, if a specified extension cannot be found, the call /must/
-- return 'OpenXR.Core10.Enums.Result.ERROR_EXTENSION_NOT_PRESENT' and no
-- 'OpenXR.Core10.Handles.Instance' will be created. Additionally, some
-- runtimes /may/ limit the number of concurrent instances that may be in
-- use. If the application attempts to create more instances than a runtime
-- can simultaneously support, 'createInstance' /may/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_LIMIT_REACHED'.
--
-- If the 'ApplicationInfo'::@applicationName@ is the empty string the
-- runtime /must/ return 'OpenXR.Core10.Enums.Result.ERROR_NAME_INVALID'.
--
-- If the 'InstanceCreateInfo' structure contains a platform-specific
-- extension for a platform other than the target platform,
-- 'OpenXR.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED' /may/ be
-- returned. If a mandatory platform-specific extension is defined for the
-- target platform but no matching extension struct is provided in
-- 'InstanceCreateInfo' the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_OUT_OF_MEMORY'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_LIMIT_REACHED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_API_VERSION_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_API_LAYER_NOT_PRESENT'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_EXTENSION_NOT_PRESENT'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_NAME_INVALID'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Instance', 'InstanceCreateInfo'
createInstance :: forall a io
                . (Extendss InstanceCreateInfo a, PokeChain a, MonadIO io)
               => -- | @createInfo@ points to an instance of 'InstanceCreateInfo' controlling
                  -- creation of the instance.
                  --
                  -- #VUID-xrCreateInstance-createInfo-parameter# @createInfo@ /must/ be a
                  -- pointer to a valid 'InstanceCreateInfo' structure
                  (InstanceCreateInfo a)
               -> io (Instance)
createInstance createInfo = liftIO . evalContT $ do
  xrCreateInstancePtr <- lift $ castFunPtr @_ @(("createInfo" ::: Ptr (SomeStruct InstanceCreateInfo)) -> ("instance" ::: Ptr (Ptr Instance_T)) -> IO Result) <$> getInstanceProcAddr' nullPtr (Ptr "xrCreateInstance"#)
  lift $ unless (xrCreateInstancePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrCreateInstance is null" Nothing Nothing
  let xrCreateInstance' = mkXrCreateInstance xrCreateInstancePtr
  createInfo' <- ContT $ withCStruct (createInfo)
  pInstance <- ContT $ bracket (callocBytes @(Ptr Instance_T) 8) free
  r <- lift $ traceAroundEvent "xrCreateInstance" (xrCreateInstance' (forgetExtensions createInfo') (pInstance))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  instance' <- lift $ peek @(Ptr Instance_T) pInstance
  instance'' <- lift $ (\h -> Instance h <$> initInstanceCmds h) instance'
  pure $ (instance'')

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createInstance' and 'destroyInstance'
--
-- To ensure that 'destroyInstance' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withInstance :: forall a io r . (Extendss InstanceCreateInfo a, PokeChain a, MonadIO io) => InstanceCreateInfo a -> (io Instance -> (Instance -> io ()) -> r) -> r
withInstance createInfo b =
  b (createInstance createInfo)
    (\(o0) -> destroyInstance o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrDestroyInstance
  :: FunPtr (Ptr Instance_T -> IO Result) -> Ptr Instance_T -> IO Result

-- | xrDestroyInstance - Destroy an instance of OpenXR
--
-- = Parameters
--
-- The 'destroyInstance' function is used to destroy an
-- 'OpenXR.Core10.Handles.Instance'.
--
-- == Parameter Descriptions
--
-- -   @instance@ is the handle to the instance to destroy.
--
-- 'OpenXR.Core10.Handles.Instance' handles are destroyed using
-- 'destroyInstance'. When an 'OpenXR.Core10.Handles.Instance' is
-- destroyed, all handles that are children of that
-- 'OpenXR.Core10.Handles.Instance' are also destroyed.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrDestroyInstance-instance-parameter# @instance@ /must/ be a
--     valid 'OpenXR.Core10.Handles.Instance' handle
--
-- == Thread Safety
--
-- -   Access to @instance@, and any child handles, /must/ be externally
--     synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Instance'
destroyInstance :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "xrDestroyInstance" "instance"
                   Instance
                -> io ()
destroyInstance instance' = liftIO $ do
  let xrDestroyInstancePtr = pXrDestroyInstance (instanceCmds (instance' :: Instance))
  unless (xrDestroyInstancePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrDestroyInstance is null" Nothing Nothing
  let xrDestroyInstance' = mkXrDestroyInstance xrDestroyInstancePtr
  r <- traceAroundEvent "xrDestroyInstance" (xrDestroyInstance' (instanceHandle (instance')))
  when (r < SUCCESS) (throwIO (OpenXrException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrResultToString
  :: FunPtr (Ptr Instance_T -> Result -> Ptr (FixedArray MAX_RESULT_STRING_SIZE CChar) -> IO Result) -> Ptr Instance_T -> Result -> Ptr (FixedArray MAX_RESULT_STRING_SIZE CChar) -> IO Result

-- | xrResultToString - Converts an XrResult to a UTF-8 string
--
-- == Parameter Descriptions
--
-- = Description
--
-- Returns the text version of the provided
-- 'OpenXR.Core10.Enums.Result.Result' value as a UTF-8 string.
--
-- In all cases the returned string /must/ be one of:
--
-- == Result String Return Values
--
-- -   The literal string defined for the provide numeric value in the core
--     spec or extension. (e.g. the value 0 results in the string
--     'OpenXR.Core10.Enums.Result.SUCCESS')
--
-- -   @XR_UNKNOWN_SUCCESS_@ concatenated with the positive result number
--     expressed as a decimal number.
--
-- -   @XR_UNKNOWN_FAILURE_@ concatenated with the negative result number
--     expressed as a decimal number.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrResultToString-instance-parameter# @instance@ /must/ be a
--     valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrResultToString-value-parameter# @value@ /must/ be a valid
--     'OpenXR.Core10.Enums.Result.Result' value
--
-- -   #VUID-xrResultToString-buffer-parameter# @buffer@ /must/ be a
--     character array of length
--     'OpenXR.Core10.APIConstants.MAX_RESULT_STRING_SIZE'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Instance', 'OpenXR.Core10.Enums.Result.Result',
-- 'structureTypeToString'
resultToString :: forall io
                . (MonadIO io)
               => -- | @instance@ is the handle of the instance to ask for the string.
                  Instance
               -> -- | @value@ is the 'OpenXR.Core10.Enums.Result.Result' value to turn into a
                  -- string.
                  ("value" ::: Result)
               -> io (("buffer" ::: ByteString))
resultToString instance' value = liftIO . evalContT $ do
  let xrResultToStringPtr = pXrResultToString (instanceCmds (instance' :: Instance))
  lift $ unless (xrResultToStringPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrResultToString is null" Nothing Nothing
  let xrResultToString' = mkXrResultToString xrResultToStringPtr
  buffer <- ContT $ bracket (callocFixedArray @MAX_RESULT_STRING_SIZE @CChar) free
  r <- lift $ traceAroundEvent "xrResultToString" (xrResultToString' (instanceHandle (instance')) (value) (buffer))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  buffer' <- lift $ packCString . lowerArrayPtr @CChar @MAX_RESULT_STRING_SIZE $ buffer
  pure $ (buffer')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrStructureTypeToString
  :: FunPtr (Ptr Instance_T -> StructureType -> Ptr (FixedArray MAX_STRUCTURE_NAME_SIZE CChar) -> IO Result) -> Ptr Instance_T -> StructureType -> Ptr (FixedArray MAX_STRUCTURE_NAME_SIZE CChar) -> IO Result

-- | xrStructureTypeToString - Converts an XrStructureType to a UTF-8 string
--
-- == Parameter Descriptions
--
-- = Description
--
-- Returns the text version of the provided
-- 'OpenXR.Core10.Enums.StructureType.StructureType' value as a UTF-8
-- string.
--
-- In all cases the returned string /must/ be one of:
--
-- == Structure Type String Return Values
--
-- -   The literal string defined for the provide numeric value in the core
--     spec or extension. (e.g. the value of
--     'OpenXR.Core10.Enums.StructureType.TYPE_INSTANCE_CREATE_INFO'
--     results in the string
--     'OpenXR.Core10.Enums.StructureType.TYPE_INSTANCE_CREATE_INFO')
--
-- -   @XR_UNKNOWN_STRUCTURE_TYPE_@ concatenated with the structure type
--     number expressed as a decimal number.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrStructureTypeToString-instance-parameter# @instance@ /must/
--     be a valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrStructureTypeToString-value-parameter# @value@ /must/ be a
--     valid 'OpenXR.Core10.Enums.StructureType.StructureType' value
--
-- -   #VUID-xrStructureTypeToString-buffer-parameter# @buffer@ /must/ be a
--     character array of length
--     'OpenXR.Core10.APIConstants.MAX_STRUCTURE_NAME_SIZE'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Instance',
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'resultToString'
structureTypeToString :: forall io
                       . (MonadIO io)
                      => -- | @instance@ is the handle of the instance to ask for the string.
                         Instance
                      -> -- | @value@ is the 'OpenXR.Core10.Enums.StructureType.StructureType' value
                         -- to turn into a string.
                         ("value" ::: StructureType)
                      -> io (("buffer" ::: ByteString))
structureTypeToString instance' value = liftIO . evalContT $ do
  let xrStructureTypeToStringPtr = pXrStructureTypeToString (instanceCmds (instance' :: Instance))
  lift $ unless (xrStructureTypeToStringPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrStructureTypeToString is null" Nothing Nothing
  let xrStructureTypeToString' = mkXrStructureTypeToString xrStructureTypeToStringPtr
  buffer <- ContT $ bracket (callocFixedArray @MAX_STRUCTURE_NAME_SIZE @CChar) free
  r <- lift $ traceAroundEvent "xrStructureTypeToString" (xrStructureTypeToString' (instanceHandle (instance')) (value) (buffer))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  buffer' <- lift $ packCString . lowerArrayPtr @CChar @MAX_STRUCTURE_NAME_SIZE $ buffer
  pure $ (buffer')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetInstanceProperties
  :: FunPtr (Ptr Instance_T -> Ptr InstanceProperties -> IO Result) -> Ptr Instance_T -> Ptr InstanceProperties -> IO Result

-- | xrGetInstanceProperties - Gets information about the instance
--
-- == Parameter Descriptions
--
-- = Description
--
-- The @instanceProperties@ parameter /must/ be filled out by the runtime
-- in response to this call, with information as defined in
-- 'InstanceProperties'.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Instance', 'InstanceProperties'
getInstanceProperties :: forall io
                       . (MonadIO io)
                      => -- | @instance@ is a handle to an 'OpenXR.Core10.Handles.Instance' previously
                         -- created with 'createInstance'.
                         --
                         -- #VUID-xrGetInstanceProperties-instance-parameter# @instance@ /must/ be a
                         -- valid 'OpenXR.Core10.Handles.Instance' handle
                         Instance
                      -> io (InstanceProperties)
getInstanceProperties instance' = liftIO . evalContT $ do
  let xrGetInstancePropertiesPtr = pXrGetInstanceProperties (instanceCmds (instance' :: Instance))
  lift $ unless (xrGetInstancePropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetInstanceProperties is null" Nothing Nothing
  let xrGetInstanceProperties' = mkXrGetInstanceProperties xrGetInstancePropertiesPtr
  pInstanceProperties <- ContT (withZeroCStruct @InstanceProperties)
  r <- lift $ traceAroundEvent "xrGetInstanceProperties" (xrGetInstanceProperties' (instanceHandle (instance')) (pInstanceProperties))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  instanceProperties <- lift $ peekCStruct @InstanceProperties pInstanceProperties
  pure $ (instanceProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrPollEvent
  :: FunPtr (Ptr Instance_T -> Ptr EventDataBuffer -> IO Result) -> Ptr Instance_T -> Ptr EventDataBuffer -> IO Result

-- | xrPollEvent - Polls for events
--
-- = Parameters
--
-- 'pollEvent' polls for the next event and returns an event if one is
-- available. 'pollEvent' returns immediately regardless of whether an
-- event was available. The event (if present) is unilaterally removed from
-- the queue if a valid 'OpenXR.Core10.Handles.Instance' is provided. On
-- return the @eventData@ parameter is filled with the event’s data and the
-- type field is changed to the event’s type. Runtimes /may/ create valid
-- next chains depending on enabled extensions, but they /must/ guarantee
-- that any such chains point only to objects which fit completely within
-- the original 'EventDataBuffer' pointed to by @eventData@.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
--     -   'OpenXR.Core10.Enums.Result.EVENT_UNAVAILABLE'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- The runtime /must/ discard queued events which contain destroyed or
-- otherwise invalid handles.
--
-- +-----------------------------------------------------------------+-----------------------------------+
-- | Event                                                           | Description                       |
-- +=================================================================+===================================+
-- | 'OpenXR.Core10.OtherTypes.EventDataEventsLost'                  | event queue has overflowed and    |
-- |                                                                 | some events were lost             |
-- +-----------------------------------------------------------------+-----------------------------------+
-- | 'OpenXR.Core10.OtherTypes.EventDataInstanceLossPending'         | application is about to lose the  |
-- |                                                                 | instance                          |
-- +-----------------------------------------------------------------+-----------------------------------+
-- | 'OpenXR.Core10.OtherTypes.EventDataInteractionProfileChanged'   | active input form factor for one  |
-- |                                                                 | or more top level user paths has  |
-- |                                                                 | changed                           |
-- +-----------------------------------------------------------------+-----------------------------------+
-- | 'OpenXR.Core10.OtherTypes.EventDataReferenceSpaceChangePending' | runtime will begin operating with |
-- |                                                                 | updated space bounds              |
-- +-----------------------------------------------------------------+-----------------------------------+
-- | 'OpenXR.Core10.OtherTypes.EventDataSessionStateChanged'         | application has changed lifecycle |
-- |                                                                 | state                             |
-- +-----------------------------------------------------------------+-----------------------------------+
--
-- Event Descriptions
--
-- = See Also
--
-- 'EventDataBuffer', 'OpenXR.Core10.Handles.Instance'
pollEvent :: forall io
           . (MonadIO io)
          => -- | @instance@ is a valid 'OpenXR.Core10.Handles.Instance'.
             --
             -- #VUID-xrPollEvent-instance-parameter# @instance@ /must/ be a valid
             -- 'OpenXR.Core10.Handles.Instance' handle
             Instance
          -> io (Result, EventDataBuffer)
pollEvent instance' = liftIO . evalContT $ do
  let xrPollEventPtr = pXrPollEvent (instanceCmds (instance' :: Instance))
  lift $ unless (xrPollEventPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrPollEvent is null" Nothing Nothing
  let xrPollEvent' = mkXrPollEvent xrPollEventPtr
  pEventData <- ContT (withZeroCStruct @EventDataBuffer)
  r <- lift $ traceAroundEvent "xrPollEvent" (xrPollEvent' (instanceHandle (instance')) (pEventData))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  eventData <- lift $ peekCStruct @EventDataBuffer pEventData
  pure $ (r, eventData)


-- | XrApiLayerProperties - Structure specifying layer properties
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrVersion >,
-- 'enumerateApiLayerProperties'
data ApiLayerProperties = ApiLayerProperties
  { -- | @layerName@ is a string specifying the name of the API layer. Use this
    -- name in the 'InstanceCreateInfo'::@enabledApiLayerNames@ array to enable
    -- this API layer for an instance.
    layerName :: ByteString
  , -- | @specVersion@ is the API version the API layer was written to, encoded
    -- as described in the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#api-version-numbers-and-semantics API Version Numbers and Semantics>
    -- section.
    specVersion :: Version
  , -- | @layerVersion@ is the version of this API layer. It is an integer,
    -- increasing with backward compatible changes.
    layerVersion :: Word32
  , -- | @description@ is a string providing additional details that /can/ be
    -- used by the application to identify the API layer.
    description :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ApiLayerProperties)
#endif
deriving instance Show ApiLayerProperties

instance ToCStruct ApiLayerProperties where
  withCStruct x f = allocaBytesAligned 544 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ApiLayerProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_API_LAYER_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_API_LAYER_NAME_SIZE CChar))) (layerName)
    poke ((p `plusPtr` 272 :: Ptr Version)) (specVersion)
    poke ((p `plusPtr` 280 :: Ptr Word32)) (layerVersion)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 284 :: Ptr (FixedArray MAX_API_LAYER_DESCRIPTION_SIZE CChar))) (description)
    f
  cStructSize = 544
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_API_LAYER_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_API_LAYER_NAME_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 272 :: Ptr Version)) (zero)
    poke ((p `plusPtr` 280 :: Ptr Word32)) (zero)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 284 :: Ptr (FixedArray MAX_API_LAYER_DESCRIPTION_SIZE CChar))) (mempty)
    f

instance FromCStruct ApiLayerProperties where
  peekCStruct p = do
    layerName <- packCString (lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray MAX_API_LAYER_NAME_SIZE CChar))))
    specVersion <- peek @Version ((p `plusPtr` 272 :: Ptr Version))
    layerVersion <- peek @Word32 ((p `plusPtr` 280 :: Ptr Word32))
    description <- packCString (lowerArrayPtr ((p `plusPtr` 284 :: Ptr (FixedArray MAX_API_LAYER_DESCRIPTION_SIZE CChar))))
    pure $ ApiLayerProperties
             layerName specVersion layerVersion description

instance Storable ApiLayerProperties where
  sizeOf ~_ = 544
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ApiLayerProperties where
  zero = ApiLayerProperties
           mempty
           zero
           zero
           mempty


-- | XrExtensionProperties - Returns properties of available instance
-- extensions
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'enumerateInstanceExtensionProperties'
data ExtensionProperties = ExtensionProperties
  { -- | @extensionName@ is a @NULL@ terminated string specifying the name of the
    -- extension.
    extensionName :: ByteString
  , -- | @extensionVersion@ is the version of this extension. It is an integer,
    -- incremented with backward compatible changes.
    extensionVersion :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExtensionProperties)
#endif
deriving instance Show ExtensionProperties

instance ToCStruct ExtensionProperties where
  withCStruct x f = allocaBytesAligned 152 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExtensionProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EXTENSION_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))) (extensionName)
    poke ((p `plusPtr` 144 :: Ptr Word32)) (extensionVersion)
    f
  cStructSize = 152
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EXTENSION_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 144 :: Ptr Word32)) (zero)
    f

instance FromCStruct ExtensionProperties where
  peekCStruct p = do
    extensionName <- packCString (lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))))
    extensionVersion <- peek @Word32 ((p `plusPtr` 144 :: Ptr Word32))
    pure $ ExtensionProperties
             extensionName extensionVersion

instance Storable ExtensionProperties where
  sizeOf ~_ = 152
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExtensionProperties where
  zero = ExtensionProperties
           mempty
           zero


-- | XrApplicationInfo - Structure specifying application info
--
-- == Valid Usage (Implicit)
--
-- Note
--
-- When using the OpenXR API to implement a reusable engine that will be
-- used by many applications, @engineName@ /should/ be set to a unique
-- string that identifies the engine, and @engineVersion@ /should/ encode a
-- representation of the engine’s version. This way, all applications that
-- share this engine version will provide the same @engineName@ and
-- @engineVersion@ to the runtime. The engine /should/ then enable
-- individual applications to choose their specific @applicationName@ and
-- @applicationVersion@, enabling one application to be distinguished from
-- another application.
--
-- When using the OpenXR API to implement an individual application without
-- a shared engine, the input @engineName@ /should/ be left empty and
-- @engineVersion@ /should/ be set to 0. The @applicationName@ /should/
-- then be filled in with a unique string that identifies the app and the
-- @applicationVersion@ /should/ encode a representation of the
-- application’s version.
--
-- = See Also
--
-- 'InstanceCreateInfo',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrVersion >
data ApplicationInfo = ApplicationInfo
  { -- | @applicationName@ is a non-empty string containing the name of the
    -- application.
    --
    -- #VUID-XrApplicationInfo-applicationName-parameter# @applicationName@
    -- /must/ be a null-terminated UTF-8 string whose length is less than or
    -- equal to 'OpenXR.Core10.APIConstants.MAX_APPLICATION_NAME_SIZE'
    applicationName :: ByteString
  , -- | @applicationVersion@ is an unsigned integer variable containing the
    -- developer-supplied version number of the application.
    applicationVersion :: Word32
  , -- | @engineName@ is a string containing the name of the engine (if any) used
    -- to create the application. It may be empty to indicate no specified
    -- engine.
    --
    -- #VUID-XrApplicationInfo-engineName-parameter# @engineName@ /must/ be a
    -- null-terminated UTF-8 string whose length is less than or equal to
    -- 'OpenXR.Core10.APIConstants.MAX_ENGINE_NAME_SIZE'
    engineName :: ByteString
  , -- | @engineVersion@ is an unsigned integer variable containing the
    -- developer-supplied version number of the engine used to create the
    -- application. May be zero to indicate no specified engine.
    engineVersion :: Word32
  , -- | @apiVersion@ is the version of this API against which the application
    -- will run, encoded as described in the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#api-version-numbers-and-semantics API Version Numbers and Semantics>
    -- section. If the runtime does not support the requested @apiVersion@ it
    -- /must/ return
    -- 'OpenXR.Core10.Enums.Result.ERROR_API_VERSION_UNSUPPORTED'.
    apiVersion :: Version
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ApplicationInfo)
#endif
deriving instance Show ApplicationInfo

instance ToCStruct ApplicationInfo where
  withCStruct x f = allocaBytesAligned 272 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ApplicationInfo{..} f = do
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 0 :: Ptr (FixedArray MAX_APPLICATION_NAME_SIZE CChar))) (applicationName)
    poke ((p `plusPtr` 128 :: Ptr Word32)) (applicationVersion)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 132 :: Ptr (FixedArray MAX_ENGINE_NAME_SIZE CChar))) (engineName)
    poke ((p `plusPtr` 260 :: Ptr Word32)) (engineVersion)
    poke ((p `plusPtr` 264 :: Ptr Version)) (apiVersion)
    f
  cStructSize = 272
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 0 :: Ptr (FixedArray MAX_APPLICATION_NAME_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 128 :: Ptr Word32)) (zero)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 132 :: Ptr (FixedArray MAX_ENGINE_NAME_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 260 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 264 :: Ptr Version)) (zero)
    f

instance FromCStruct ApplicationInfo where
  peekCStruct p = do
    applicationName <- packCString (lowerArrayPtr ((p `plusPtr` 0 :: Ptr (FixedArray MAX_APPLICATION_NAME_SIZE CChar))))
    applicationVersion <- peek @Word32 ((p `plusPtr` 128 :: Ptr Word32))
    engineName <- packCString (lowerArrayPtr ((p `plusPtr` 132 :: Ptr (FixedArray MAX_ENGINE_NAME_SIZE CChar))))
    engineVersion <- peek @Word32 ((p `plusPtr` 260 :: Ptr Word32))
    apiVersion <- peek @Version ((p `plusPtr` 264 :: Ptr Version))
    pure $ ApplicationInfo
             applicationName applicationVersion engineName engineVersion apiVersion

instance Storable ApplicationInfo where
  sizeOf ~_ = 272
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ApplicationInfo where
  zero = ApplicationInfo
           mempty
           zero
           mempty
           zero
           zero


-- | XrInstanceCreateInfo - Structure specifying params of a newly created
-- instance
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrInstanceCreateInfo-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_INSTANCE_CREATE_INFO'
--
-- -   #VUID-XrInstanceCreateInfo-next-next# @next@ /must/ be @NULL@ or a
--     valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>.
--     See also:
--     'OpenXR.Extensions.XR_EXT_debug_utils.DebugUtilsMessengerCreateInfoEXT',
--     'OpenXR.Extensions.XR_KHR_android_create_instance.InstanceCreateInfoAndroidKHR'
--
-- -   #VUID-XrInstanceCreateInfo-createFlags-zerobitmask# @createFlags@
--     /must/ be @0@
--
-- -   #VUID-XrInstanceCreateInfo-applicationInfo-parameter#
--     @applicationInfo@ /must/ be a valid 'ApplicationInfo' structure
--
-- -   #VUID-XrInstanceCreateInfo-enabledApiLayerNames-parameter# If
--     @enabledApiLayerCount@ is not @0@, @enabledApiLayerNames@ /must/ be
--     a pointer to an array of @enabledApiLayerCount@ null-terminated
--     UTF-8 strings
--
-- -   #VUID-XrInstanceCreateInfo-enabledExtensionNames-parameter# If
--     @enabledExtensionCount@ is not @0@, @enabledExtensionNames@ /must/
--     be a pointer to an array of @enabledExtensionCount@ null-terminated
--     UTF-8 strings
--
-- = See Also
--
-- 'ApplicationInfo',
-- 'OpenXR.Core10.Enums.InstanceCreateFlagBits.InstanceCreateFlags',
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'createInstance'
data InstanceCreateInfo (es :: [Type]) = InstanceCreateInfo
  { -- | @next@ is @NULL@ or a pointer to the next structure in a structure
    -- chain. No such structures are defined in core OpenXR.
    next :: Chain es
  , -- | @createFlags@ is a bitmask of
    -- 'OpenXR.Core10.Enums.InstanceCreateFlagBits.InstanceCreateFlags' that
    -- identifies options that apply to the creation.
    createFlags :: InstanceCreateFlags
  , -- | @applicationInfo@ is an instance of 'ApplicationInfo'. This information
    -- helps runtimes recognize behavior inherent to classes of applications.
    -- 'ApplicationInfo' is defined in detail below.
    applicationInfo :: ApplicationInfo
  , -- | @enabledApiLayerNames@ is a pointer to an array of
    -- @enabledApiLayerCount@ strings containing the names of API layers to
    -- enable for the created instance. See the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#api-layers-and-extensions API Layers And Extensions>
    -- section for further details.
    enabledApiLayerNames :: Vector ByteString
  , -- | @enabledExtensionNames@ is a pointer to an array of
    -- @enabledExtensionCount@ strings containing the names of extensions to
    -- enable.
    enabledExtensionNames :: Vector ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (InstanceCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (InstanceCreateInfo es)

instance Extensible InstanceCreateInfo where
  extensibleTypeName = "InstanceCreateInfo"
  setNext x next = x{next = next}
  getNext InstanceCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends InstanceCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DebugUtilsMessengerCreateInfoEXT = Just f
    | Just Refl <- eqT @e @InstanceCreateInfoAndroidKHR = Just f
    | otherwise = Nothing

instance (Extendss InstanceCreateInfo es, PokeChain es) => ToCStruct (InstanceCreateInfo es) where
  withCStruct x f = allocaBytesAligned 328 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p InstanceCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_INSTANCE_CREATE_INFO)
    next'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) next''
    lift $ poke ((p `plusPtr` 16 :: Ptr InstanceCreateFlags)) (createFlags)
    lift $ poke ((p `plusPtr` 24 :: Ptr ApplicationInfo)) (applicationInfo)
    lift $ poke ((p `plusPtr` 296 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (enabledApiLayerNames)) :: Word32))
    pEnabledApiLayerNames' <- ContT $ allocaBytesAligned @(Ptr CChar) ((Data.Vector.length (enabledApiLayerNames)) * 8) 8
    Data.Vector.imapM_ (\i e -> do
      enabledApiLayerNames'' <- ContT $ useAsCString (e)
      lift $ poke (pEnabledApiLayerNames' `plusPtr` (8 * (i)) :: Ptr (Ptr CChar)) enabledApiLayerNames'') (enabledApiLayerNames)
    lift $ poke ((p `plusPtr` 304 :: Ptr (Ptr (Ptr CChar)))) (pEnabledApiLayerNames')
    lift $ poke ((p `plusPtr` 312 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (enabledExtensionNames)) :: Word32))
    pEnabledExtensionNames' <- ContT $ allocaBytesAligned @(Ptr CChar) ((Data.Vector.length (enabledExtensionNames)) * 8) 8
    Data.Vector.imapM_ (\i e -> do
      enabledExtensionNames'' <- ContT $ useAsCString (e)
      lift $ poke (pEnabledExtensionNames' `plusPtr` (8 * (i)) :: Ptr (Ptr CChar)) enabledExtensionNames'') (enabledExtensionNames)
    lift $ poke ((p `plusPtr` 320 :: Ptr (Ptr (Ptr CChar)))) (pEnabledExtensionNames')
    lift $ f
  cStructSize = 328
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_INSTANCE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr ApplicationInfo)) (zero)
    lift $ f

instance (Extendss InstanceCreateInfo es, PeekChain es) => FromCStruct (InstanceCreateInfo es) where
  peekCStruct p = do
    next <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next' <- peekChain (castPtr next)
    createFlags <- peek @InstanceCreateFlags ((p `plusPtr` 16 :: Ptr InstanceCreateFlags))
    applicationInfo <- peekCStruct @ApplicationInfo ((p `plusPtr` 24 :: Ptr ApplicationInfo))
    enabledApiLayerCount <- peek @Word32 ((p `plusPtr` 296 :: Ptr Word32))
    enabledApiLayerNames <- peek @(Ptr (Ptr CChar)) ((p `plusPtr` 304 :: Ptr (Ptr (Ptr CChar))))
    enabledApiLayerNames' <- generateM (fromIntegral enabledApiLayerCount) (\i -> packCString =<< peek ((enabledApiLayerNames `advancePtrBytes` (8 * (i)) :: Ptr (Ptr CChar))))
    enabledExtensionCount <- peek @Word32 ((p `plusPtr` 312 :: Ptr Word32))
    enabledExtensionNames <- peek @(Ptr (Ptr CChar)) ((p `plusPtr` 320 :: Ptr (Ptr (Ptr CChar))))
    enabledExtensionNames' <- generateM (fromIntegral enabledExtensionCount) (\i -> packCString =<< peek ((enabledExtensionNames `advancePtrBytes` (8 * (i)) :: Ptr (Ptr CChar))))
    pure $ InstanceCreateInfo
             next' createFlags applicationInfo enabledApiLayerNames' enabledExtensionNames'

instance es ~ '[] => Zero (InstanceCreateInfo es) where
  zero = InstanceCreateInfo
           ()
           zero
           zero
           mempty
           mempty


-- | XrInstanceProperties - Contains information about the instance
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrVersion >,
-- 'getInstanceProperties'
data InstanceProperties = InstanceProperties
  { -- | @runtimeVersion@ is the runtime’s version (not necessarily related to an
    -- OpenXR API version), expressed in the format of
    -- 'OpenXR.Version.MAKE_VERSION'.
    runtimeVersion :: Version
  , -- | @runtimeName@ is the name of the runtime.
    runtimeName :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (InstanceProperties)
#endif
deriving instance Show InstanceProperties

instance ToCStruct InstanceProperties where
  withCStruct x f = allocaBytesAligned 152 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p InstanceProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_INSTANCE_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Version)) (runtimeVersion)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 24 :: Ptr (FixedArray MAX_RUNTIME_NAME_SIZE CChar))) (runtimeName)
    f
  cStructSize = 152
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_INSTANCE_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Version)) (zero)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 24 :: Ptr (FixedArray MAX_RUNTIME_NAME_SIZE CChar))) (mempty)
    f

instance FromCStruct InstanceProperties where
  peekCStruct p = do
    runtimeVersion <- peek @Version ((p `plusPtr` 16 :: Ptr Version))
    runtimeName <- packCString (lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray MAX_RUNTIME_NAME_SIZE CChar))))
    pure $ InstanceProperties
             runtimeVersion runtimeName

instance Storable InstanceProperties where
  sizeOf ~_ = 152
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero InstanceProperties where
  zero = InstanceProperties
           zero
           mempty


-- | XrEventDataBuffer - Event buffer
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.OtherTypes.EventDataBaseHeader',
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'pollEvent'
data EventDataBuffer = EventDataBuffer
  { -- | @varying@ is a fixed sized output buffer big enough to hold returned
    -- data elements for all specified event data types.
    varying :: ByteString }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (EventDataBuffer)
#endif
deriving instance Show EventDataBuffer

instance ToCStruct EventDataBuffer where
  withCStruct x f = allocaBytesAligned 4016 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p EventDataBuffer{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EVENT_DATA_BUFFER)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (FixedArray 4000 Word8))) (varying)
    f
  cStructSize = 4016
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EVENT_DATA_BUFFER)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (FixedArray 4000 Word8))) (mempty)
    f

instance FromCStruct EventDataBuffer where
  peekCStruct p = do
    varying <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 16 :: Ptr (FixedArray 4000 Word8)))
    pure $ EventDataBuffer
             varying

instance Storable EventDataBuffer where
  sizeOf ~_ = 4016
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero EventDataBuffer where
  zero = EventDataBuffer
           mempty

