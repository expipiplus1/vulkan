{-# language CPP #-}
-- No documentation found for Chapter "VK_INTEL_performance_query"
module Vulkan.Extensions.VK_INTEL_performance_query  ( initializePerformanceApiINTEL
                                                     , uninitializePerformanceApiINTEL
                                                     , cmdSetPerformanceMarkerINTEL
                                                     , cmdSetPerformanceStreamMarkerINTEL
                                                     , cmdSetPerformanceOverrideINTEL
                                                     , acquirePerformanceConfigurationINTEL
                                                     , releasePerformanceConfigurationINTEL
                                                     , queueSetPerformanceConfigurationINTEL
                                                     , getPerformanceParameterINTEL
                                                     , pattern STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO_INTEL
                                                     , PerformanceValueINTEL(..)
                                                     , InitializePerformanceApiInfoINTEL(..)
                                                     , QueryPoolPerformanceQueryCreateInfoINTEL(..)
                                                     , PerformanceMarkerInfoINTEL(..)
                                                     , PerformanceStreamMarkerInfoINTEL(..)
                                                     , PerformanceOverrideInfoINTEL(..)
                                                     , PerformanceConfigurationAcquireInfoINTEL(..)
                                                     , PerformanceValueDataINTEL(..)
                                                     , peekPerformanceValueDataINTEL
                                                     , PerformanceConfigurationTypeINTEL( PERFORMANCE_CONFIGURATION_TYPE_COMMAND_QUEUE_METRICS_DISCOVERY_ACTIVATED_INTEL
                                                                                        , ..
                                                                                        )
                                                     , QueryPoolSamplingModeINTEL( QUERY_POOL_SAMPLING_MODE_MANUAL_INTEL
                                                                                 , ..
                                                                                 )
                                                     , PerformanceOverrideTypeINTEL( PERFORMANCE_OVERRIDE_TYPE_NULL_HARDWARE_INTEL
                                                                                   , PERFORMANCE_OVERRIDE_TYPE_FLUSH_GPU_CACHES_INTEL
                                                                                   , ..
                                                                                   )
                                                     , PerformanceParameterTypeINTEL( PERFORMANCE_PARAMETER_TYPE_HW_COUNTERS_SUPPORTED_INTEL
                                                                                    , PERFORMANCE_PARAMETER_TYPE_STREAM_MARKER_VALID_BITS_INTEL
                                                                                    , ..
                                                                                    )
                                                     , PerformanceValueTypeINTEL( PERFORMANCE_VALUE_TYPE_UINT32_INTEL
                                                                                , PERFORMANCE_VALUE_TYPE_UINT64_INTEL
                                                                                , PERFORMANCE_VALUE_TYPE_FLOAT_INTEL
                                                                                , PERFORMANCE_VALUE_TYPE_BOOL_INTEL
                                                                                , PERFORMANCE_VALUE_TYPE_STRING_INTEL
                                                                                , ..
                                                                                )
                                                     , QueryPoolCreateInfoINTEL
                                                     , INTEL_PERFORMANCE_QUERY_SPEC_VERSION
                                                     , pattern INTEL_PERFORMANCE_QUERY_SPEC_VERSION
                                                     , INTEL_PERFORMANCE_QUERY_EXTENSION_NAME
                                                     , pattern INTEL_PERFORMANCE_QUERY_EXTENSION_NAME
                                                     , PerformanceConfigurationINTEL(..)
                                                     ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Cont (runContT)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkAcquirePerformanceConfigurationINTEL))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetPerformanceMarkerINTEL))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetPerformanceOverrideINTEL))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetPerformanceStreamMarkerINTEL))
import Vulkan.Dynamic (DeviceCmds(pVkGetPerformanceParameterINTEL))
import Vulkan.Dynamic (DeviceCmds(pVkInitializePerformanceApiINTEL))
import Vulkan.Dynamic (DeviceCmds(pVkQueueSetPerformanceConfigurationINTEL))
import Vulkan.Dynamic (DeviceCmds(pVkReleasePerformanceConfigurationINTEL))
import Vulkan.Dynamic (DeviceCmds(pVkUninitializePerformanceApiINTEL))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Extensions.Handles (PerformanceConfigurationINTEL)
import Vulkan.Extensions.Handles (PerformanceConfigurationINTEL(..))
import Vulkan.Core10.Handles (Queue)
import Vulkan.Core10.Handles (Queue(..))
import Vulkan.Core10.Handles (Queue_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_INITIALIZE_PERFORMANCE_API_INFO_INTEL))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PERFORMANCE_CONFIGURATION_ACQUIRE_INFO_INTEL))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PERFORMANCE_MARKER_INFO_INTEL))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PERFORMANCE_OVERRIDE_INFO_INTEL))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PERFORMANCE_STREAM_MARKER_INFO_INTEL))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (PerformanceConfigurationINTEL(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkInitializePerformanceApiINTEL
  :: FunPtr (Ptr Device_T -> Ptr InitializePerformanceApiInfoINTEL -> IO Result) -> Ptr Device_T -> Ptr InitializePerformanceApiInfoINTEL -> IO Result

-- No documentation found for TopLevel "vkInitializePerformanceApiINTEL"
initializePerformanceApiINTEL :: forall io
                               . (MonadIO io)
                              => -- No documentation found for Nested "vkInitializePerformanceApiINTEL" "device"
                                 Device
                              -> -- No documentation found for Nested "vkInitializePerformanceApiINTEL" "pInitializeInfo"
                                 ("initializeInfo" ::: InitializePerformanceApiInfoINTEL)
                              -> io ()
initializePerformanceApiINTEL device initializeInfo = liftIO . evalContT $ do
  let vkInitializePerformanceApiINTELPtr = pVkInitializePerformanceApiINTEL (deviceCmds (device :: Device))
  lift $ unless (vkInitializePerformanceApiINTELPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkInitializePerformanceApiINTEL is null" Nothing Nothing
  let vkInitializePerformanceApiINTEL' = mkVkInitializePerformanceApiINTEL vkInitializePerformanceApiINTELPtr
  pInitializeInfo <- ContT $ withCStruct (initializeInfo)
  r <- lift $ vkInitializePerformanceApiINTEL' (deviceHandle (device)) pInitializeInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUninitializePerformanceApiINTEL
  :: FunPtr (Ptr Device_T -> IO ()) -> Ptr Device_T -> IO ()

-- No documentation found for TopLevel "vkUninitializePerformanceApiINTEL"
uninitializePerformanceApiINTEL :: forall io
                                 . (MonadIO io)
                                => -- No documentation found for Nested "vkUninitializePerformanceApiINTEL" "device"
                                   Device
                                -> io ()
uninitializePerformanceApiINTEL device = liftIO $ do
  let vkUninitializePerformanceApiINTELPtr = pVkUninitializePerformanceApiINTEL (deviceCmds (device :: Device))
  unless (vkUninitializePerformanceApiINTELPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkUninitializePerformanceApiINTEL is null" Nothing Nothing
  let vkUninitializePerformanceApiINTEL' = mkVkUninitializePerformanceApiINTEL vkUninitializePerformanceApiINTELPtr
  vkUninitializePerformanceApiINTEL' (deviceHandle (device))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetPerformanceMarkerINTEL
  :: FunPtr (Ptr CommandBuffer_T -> Ptr PerformanceMarkerInfoINTEL -> IO Result) -> Ptr CommandBuffer_T -> Ptr PerformanceMarkerInfoINTEL -> IO Result

-- No documentation found for TopLevel "vkCmdSetPerformanceMarkerINTEL"
cmdSetPerformanceMarkerINTEL :: forall io
                              . (MonadIO io)
                             => -- No documentation found for Nested "vkCmdSetPerformanceMarkerINTEL" "commandBuffer"
                                CommandBuffer
                             -> -- No documentation found for Nested "vkCmdSetPerformanceMarkerINTEL" "pMarkerInfo"
                                PerformanceMarkerInfoINTEL
                             -> io ()
cmdSetPerformanceMarkerINTEL commandBuffer markerInfo = liftIO . evalContT $ do
  let vkCmdSetPerformanceMarkerINTELPtr = pVkCmdSetPerformanceMarkerINTEL (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetPerformanceMarkerINTELPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetPerformanceMarkerINTEL is null" Nothing Nothing
  let vkCmdSetPerformanceMarkerINTEL' = mkVkCmdSetPerformanceMarkerINTEL vkCmdSetPerformanceMarkerINTELPtr
  pMarkerInfo <- ContT $ withCStruct (markerInfo)
  r <- lift $ vkCmdSetPerformanceMarkerINTEL' (commandBufferHandle (commandBuffer)) pMarkerInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetPerformanceStreamMarkerINTEL
  :: FunPtr (Ptr CommandBuffer_T -> Ptr PerformanceStreamMarkerInfoINTEL -> IO Result) -> Ptr CommandBuffer_T -> Ptr PerformanceStreamMarkerInfoINTEL -> IO Result

-- No documentation found for TopLevel "vkCmdSetPerformanceStreamMarkerINTEL"
cmdSetPerformanceStreamMarkerINTEL :: forall io
                                    . (MonadIO io)
                                   => -- No documentation found for Nested "vkCmdSetPerformanceStreamMarkerINTEL" "commandBuffer"
                                      CommandBuffer
                                   -> -- No documentation found for Nested "vkCmdSetPerformanceStreamMarkerINTEL" "pMarkerInfo"
                                      PerformanceStreamMarkerInfoINTEL
                                   -> io ()
cmdSetPerformanceStreamMarkerINTEL commandBuffer markerInfo = liftIO . evalContT $ do
  let vkCmdSetPerformanceStreamMarkerINTELPtr = pVkCmdSetPerformanceStreamMarkerINTEL (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetPerformanceStreamMarkerINTELPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetPerformanceStreamMarkerINTEL is null" Nothing Nothing
  let vkCmdSetPerformanceStreamMarkerINTEL' = mkVkCmdSetPerformanceStreamMarkerINTEL vkCmdSetPerformanceStreamMarkerINTELPtr
  pMarkerInfo <- ContT $ withCStruct (markerInfo)
  r <- lift $ vkCmdSetPerformanceStreamMarkerINTEL' (commandBufferHandle (commandBuffer)) pMarkerInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetPerformanceOverrideINTEL
  :: FunPtr (Ptr CommandBuffer_T -> Ptr PerformanceOverrideInfoINTEL -> IO Result) -> Ptr CommandBuffer_T -> Ptr PerformanceOverrideInfoINTEL -> IO Result

-- No documentation found for TopLevel "vkCmdSetPerformanceOverrideINTEL"
cmdSetPerformanceOverrideINTEL :: forall io
                                . (MonadIO io)
                               => -- No documentation found for Nested "vkCmdSetPerformanceOverrideINTEL" "commandBuffer"
                                  CommandBuffer
                               -> -- No documentation found for Nested "vkCmdSetPerformanceOverrideINTEL" "pOverrideInfo"
                                  PerformanceOverrideInfoINTEL
                               -> io ()
cmdSetPerformanceOverrideINTEL commandBuffer overrideInfo = liftIO . evalContT $ do
  let vkCmdSetPerformanceOverrideINTELPtr = pVkCmdSetPerformanceOverrideINTEL (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetPerformanceOverrideINTELPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetPerformanceOverrideINTEL is null" Nothing Nothing
  let vkCmdSetPerformanceOverrideINTEL' = mkVkCmdSetPerformanceOverrideINTEL vkCmdSetPerformanceOverrideINTELPtr
  pOverrideInfo <- ContT $ withCStruct (overrideInfo)
  r <- lift $ vkCmdSetPerformanceOverrideINTEL' (commandBufferHandle (commandBuffer)) pOverrideInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquirePerformanceConfigurationINTEL
  :: FunPtr (Ptr Device_T -> Ptr PerformanceConfigurationAcquireInfoINTEL -> Ptr PerformanceConfigurationINTEL -> IO Result) -> Ptr Device_T -> Ptr PerformanceConfigurationAcquireInfoINTEL -> Ptr PerformanceConfigurationINTEL -> IO Result

-- No documentation found for TopLevel "vkAcquirePerformanceConfigurationINTEL"
acquirePerformanceConfigurationINTEL :: forall io
                                      . (MonadIO io)
                                     => -- No documentation found for Nested "vkAcquirePerformanceConfigurationINTEL" "device"
                                        Device
                                     -> -- No documentation found for Nested "vkAcquirePerformanceConfigurationINTEL" "pAcquireInfo"
                                        PerformanceConfigurationAcquireInfoINTEL
                                     -> io (PerformanceConfigurationINTEL)
acquirePerformanceConfigurationINTEL device acquireInfo = liftIO . evalContT $ do
  let vkAcquirePerformanceConfigurationINTELPtr = pVkAcquirePerformanceConfigurationINTEL (deviceCmds (device :: Device))
  lift $ unless (vkAcquirePerformanceConfigurationINTELPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkAcquirePerformanceConfigurationINTEL is null" Nothing Nothing
  let vkAcquirePerformanceConfigurationINTEL' = mkVkAcquirePerformanceConfigurationINTEL vkAcquirePerformanceConfigurationINTELPtr
  pAcquireInfo <- ContT $ withCStruct (acquireInfo)
  pPConfiguration <- ContT $ bracket (callocBytes @PerformanceConfigurationINTEL 8) free
  r <- lift $ vkAcquirePerformanceConfigurationINTEL' (deviceHandle (device)) pAcquireInfo (pPConfiguration)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pConfiguration <- lift $ peek @PerformanceConfigurationINTEL pPConfiguration
  pure $ (pConfiguration)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkReleasePerformanceConfigurationINTEL
  :: FunPtr (Ptr Device_T -> PerformanceConfigurationINTEL -> IO Result) -> Ptr Device_T -> PerformanceConfigurationINTEL -> IO Result

-- No documentation found for TopLevel "vkReleasePerformanceConfigurationINTEL"
releasePerformanceConfigurationINTEL :: forall io
                                      . (MonadIO io)
                                     => -- No documentation found for Nested "vkReleasePerformanceConfigurationINTEL" "device"
                                        Device
                                     -> -- No documentation found for Nested "vkReleasePerformanceConfigurationINTEL" "configuration"
                                        PerformanceConfigurationINTEL
                                     -> io ()
releasePerformanceConfigurationINTEL device configuration = liftIO $ do
  let vkReleasePerformanceConfigurationINTELPtr = pVkReleasePerformanceConfigurationINTEL (deviceCmds (device :: Device))
  unless (vkReleasePerformanceConfigurationINTELPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkReleasePerformanceConfigurationINTEL is null" Nothing Nothing
  let vkReleasePerformanceConfigurationINTEL' = mkVkReleasePerformanceConfigurationINTEL vkReleasePerformanceConfigurationINTELPtr
  r <- vkReleasePerformanceConfigurationINTEL' (deviceHandle (device)) (configuration)
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueSetPerformanceConfigurationINTEL
  :: FunPtr (Ptr Queue_T -> PerformanceConfigurationINTEL -> IO Result) -> Ptr Queue_T -> PerformanceConfigurationINTEL -> IO Result

-- No documentation found for TopLevel "vkQueueSetPerformanceConfigurationINTEL"
queueSetPerformanceConfigurationINTEL :: forall io
                                       . (MonadIO io)
                                      => -- No documentation found for Nested "vkQueueSetPerformanceConfigurationINTEL" "queue"
                                         Queue
                                      -> -- No documentation found for Nested "vkQueueSetPerformanceConfigurationINTEL" "configuration"
                                         PerformanceConfigurationINTEL
                                      -> io ()
queueSetPerformanceConfigurationINTEL queue configuration = liftIO $ do
  let vkQueueSetPerformanceConfigurationINTELPtr = pVkQueueSetPerformanceConfigurationINTEL (deviceCmds (queue :: Queue))
  unless (vkQueueSetPerformanceConfigurationINTELPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkQueueSetPerformanceConfigurationINTEL is null" Nothing Nothing
  let vkQueueSetPerformanceConfigurationINTEL' = mkVkQueueSetPerformanceConfigurationINTEL vkQueueSetPerformanceConfigurationINTELPtr
  r <- vkQueueSetPerformanceConfigurationINTEL' (queueHandle (queue)) (configuration)
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPerformanceParameterINTEL
  :: FunPtr (Ptr Device_T -> PerformanceParameterTypeINTEL -> Ptr PerformanceValueINTEL -> IO Result) -> Ptr Device_T -> PerformanceParameterTypeINTEL -> Ptr PerformanceValueINTEL -> IO Result

-- No documentation found for TopLevel "vkGetPerformanceParameterINTEL"
getPerformanceParameterINTEL :: forall io
                              . (MonadIO io)
                             => -- No documentation found for Nested "vkGetPerformanceParameterINTEL" "device"
                                Device
                             -> -- No documentation found for Nested "vkGetPerformanceParameterINTEL" "parameter"
                                PerformanceParameterTypeINTEL
                             -> io (PerformanceValueINTEL)
getPerformanceParameterINTEL device parameter = liftIO . evalContT $ do
  let vkGetPerformanceParameterINTELPtr = pVkGetPerformanceParameterINTEL (deviceCmds (device :: Device))
  lift $ unless (vkGetPerformanceParameterINTELPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPerformanceParameterINTEL is null" Nothing Nothing
  let vkGetPerformanceParameterINTEL' = mkVkGetPerformanceParameterINTEL vkGetPerformanceParameterINTELPtr
  pPValue <- ContT (withZeroCStruct @PerformanceValueINTEL)
  r <- lift $ vkGetPerformanceParameterINTEL' (deviceHandle (device)) (parameter) (pPValue)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pValue <- lift $ peekCStruct @PerformanceValueINTEL pPValue
  pure $ (pValue)


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO_INTEL"
pattern STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO_INTEL = STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL



-- No documentation found for TopLevel "VkPerformanceValueINTEL"
data PerformanceValueINTEL = PerformanceValueINTEL
  { -- No documentation found for Nested "VkPerformanceValueINTEL" "type"
    type' :: PerformanceValueTypeINTEL
  , -- No documentation found for Nested "VkPerformanceValueINTEL" "data"
    data' :: PerformanceValueDataINTEL
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PerformanceValueINTEL)
#endif
deriving instance Show PerformanceValueINTEL

instance ToCStruct PerformanceValueINTEL where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PerformanceValueINTEL{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr PerformanceValueTypeINTEL)) (type')
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr PerformanceValueDataINTEL)) (data') . ($ ())
    lift $ f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr PerformanceValueTypeINTEL)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr PerformanceValueDataINTEL)) (zero) . ($ ())
    lift $ f

instance FromCStruct PerformanceValueINTEL where
  peekCStruct p = do
    type' <- peek @PerformanceValueTypeINTEL ((p `plusPtr` 0 :: Ptr PerformanceValueTypeINTEL))
    data' <- peekPerformanceValueDataINTEL type' ((p `plusPtr` 8 :: Ptr PerformanceValueDataINTEL))
    pure $ PerformanceValueINTEL
             type' data'

instance Zero PerformanceValueINTEL where
  zero = PerformanceValueINTEL
           zero
           zero



-- No documentation found for TopLevel "VkInitializePerformanceApiInfoINTEL"
data InitializePerformanceApiInfoINTEL = InitializePerformanceApiInfoINTEL
  { -- No documentation found for Nested "VkInitializePerformanceApiInfoINTEL" "pUserData"
    userData :: Ptr () }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (InitializePerformanceApiInfoINTEL)
#endif
deriving instance Show InitializePerformanceApiInfoINTEL

instance ToCStruct InitializePerformanceApiInfoINTEL where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p InitializePerformanceApiInfoINTEL{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INITIALIZE_PERFORMANCE_API_INFO_INTEL)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (userData)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INITIALIZE_PERFORMANCE_API_INFO_INTEL)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct InitializePerformanceApiInfoINTEL where
  peekCStruct p = do
    pUserData <- peek @(Ptr ()) ((p `plusPtr` 16 :: Ptr (Ptr ())))
    pure $ InitializePerformanceApiInfoINTEL
             pUserData


instance Storable InitializePerformanceApiInfoINTEL where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero InitializePerformanceApiInfoINTEL where
  zero = InitializePerformanceApiInfoINTEL
           zero



-- No documentation found for TopLevel "VkQueryPoolPerformanceQueryCreateInfoINTEL"
data QueryPoolPerformanceQueryCreateInfoINTEL = QueryPoolPerformanceQueryCreateInfoINTEL
  { -- No documentation found for Nested "VkQueryPoolPerformanceQueryCreateInfoINTEL" "performanceCountersSampling"
    performanceCountersSampling :: QueryPoolSamplingModeINTEL }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (QueryPoolPerformanceQueryCreateInfoINTEL)
#endif
deriving instance Show QueryPoolPerformanceQueryCreateInfoINTEL

instance ToCStruct QueryPoolPerformanceQueryCreateInfoINTEL where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p QueryPoolPerformanceQueryCreateInfoINTEL{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr QueryPoolSamplingModeINTEL)) (performanceCountersSampling)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr QueryPoolSamplingModeINTEL)) (zero)
    f

instance FromCStruct QueryPoolPerformanceQueryCreateInfoINTEL where
  peekCStruct p = do
    performanceCountersSampling <- peek @QueryPoolSamplingModeINTEL ((p `plusPtr` 16 :: Ptr QueryPoolSamplingModeINTEL))
    pure $ QueryPoolPerformanceQueryCreateInfoINTEL
             performanceCountersSampling


instance Storable QueryPoolPerformanceQueryCreateInfoINTEL where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero QueryPoolPerformanceQueryCreateInfoINTEL where
  zero = QueryPoolPerformanceQueryCreateInfoINTEL
           zero



-- No documentation found for TopLevel "VkPerformanceMarkerInfoINTEL"
data PerformanceMarkerInfoINTEL = PerformanceMarkerInfoINTEL
  { -- No documentation found for Nested "VkPerformanceMarkerInfoINTEL" "marker"
    marker :: Word64 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PerformanceMarkerInfoINTEL)
#endif
deriving instance Show PerformanceMarkerInfoINTEL

instance ToCStruct PerformanceMarkerInfoINTEL where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PerformanceMarkerInfoINTEL{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERFORMANCE_MARKER_INFO_INTEL)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (marker)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERFORMANCE_MARKER_INFO_INTEL)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    f

instance FromCStruct PerformanceMarkerInfoINTEL where
  peekCStruct p = do
    marker <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    pure $ PerformanceMarkerInfoINTEL
             marker


instance Storable PerformanceMarkerInfoINTEL where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PerformanceMarkerInfoINTEL where
  zero = PerformanceMarkerInfoINTEL
           zero



-- No documentation found for TopLevel "VkPerformanceStreamMarkerInfoINTEL"
data PerformanceStreamMarkerInfoINTEL = PerformanceStreamMarkerInfoINTEL
  { -- No documentation found for Nested "VkPerformanceStreamMarkerInfoINTEL" "marker"
    marker :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PerformanceStreamMarkerInfoINTEL)
#endif
deriving instance Show PerformanceStreamMarkerInfoINTEL

instance ToCStruct PerformanceStreamMarkerInfoINTEL where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PerformanceStreamMarkerInfoINTEL{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERFORMANCE_STREAM_MARKER_INFO_INTEL)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (marker)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERFORMANCE_STREAM_MARKER_INFO_INTEL)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PerformanceStreamMarkerInfoINTEL where
  peekCStruct p = do
    marker <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PerformanceStreamMarkerInfoINTEL
             marker


instance Storable PerformanceStreamMarkerInfoINTEL where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PerformanceStreamMarkerInfoINTEL where
  zero = PerformanceStreamMarkerInfoINTEL
           zero



-- No documentation found for TopLevel "VkPerformanceOverrideInfoINTEL"
data PerformanceOverrideInfoINTEL = PerformanceOverrideInfoINTEL
  { -- No documentation found for Nested "VkPerformanceOverrideInfoINTEL" "type"
    type' :: PerformanceOverrideTypeINTEL
  , -- No documentation found for Nested "VkPerformanceOverrideInfoINTEL" "enable"
    enable :: Bool
  , -- No documentation found for Nested "VkPerformanceOverrideInfoINTEL" "parameter"
    parameter :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PerformanceOverrideInfoINTEL)
#endif
deriving instance Show PerformanceOverrideInfoINTEL

instance ToCStruct PerformanceOverrideInfoINTEL where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PerformanceOverrideInfoINTEL{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERFORMANCE_OVERRIDE_INFO_INTEL)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PerformanceOverrideTypeINTEL)) (type')
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (enable))
    poke ((p `plusPtr` 24 :: Ptr Word64)) (parameter)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERFORMANCE_OVERRIDE_INFO_INTEL)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PerformanceOverrideTypeINTEL)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    f

instance FromCStruct PerformanceOverrideInfoINTEL where
  peekCStruct p = do
    type' <- peek @PerformanceOverrideTypeINTEL ((p `plusPtr` 16 :: Ptr PerformanceOverrideTypeINTEL))
    enable <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    parameter <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    pure $ PerformanceOverrideInfoINTEL
             type' (bool32ToBool enable) parameter


instance Storable PerformanceOverrideInfoINTEL where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PerformanceOverrideInfoINTEL where
  zero = PerformanceOverrideInfoINTEL
           zero
           zero
           zero



-- No documentation found for TopLevel "VkPerformanceConfigurationAcquireInfoINTEL"
data PerformanceConfigurationAcquireInfoINTEL = PerformanceConfigurationAcquireInfoINTEL
  { -- No documentation found for Nested "VkPerformanceConfigurationAcquireInfoINTEL" "type"
    type' :: PerformanceConfigurationTypeINTEL }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PerformanceConfigurationAcquireInfoINTEL)
#endif
deriving instance Show PerformanceConfigurationAcquireInfoINTEL

instance ToCStruct PerformanceConfigurationAcquireInfoINTEL where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PerformanceConfigurationAcquireInfoINTEL{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERFORMANCE_CONFIGURATION_ACQUIRE_INFO_INTEL)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PerformanceConfigurationTypeINTEL)) (type')
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERFORMANCE_CONFIGURATION_ACQUIRE_INFO_INTEL)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PerformanceConfigurationTypeINTEL)) (zero)
    f

instance FromCStruct PerformanceConfigurationAcquireInfoINTEL where
  peekCStruct p = do
    type' <- peek @PerformanceConfigurationTypeINTEL ((p `plusPtr` 16 :: Ptr PerformanceConfigurationTypeINTEL))
    pure $ PerformanceConfigurationAcquireInfoINTEL
             type'


instance Storable PerformanceConfigurationAcquireInfoINTEL where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PerformanceConfigurationAcquireInfoINTEL where
  zero = PerformanceConfigurationAcquireInfoINTEL
           zero


data PerformanceValueDataINTEL
  = Value32 Word32
  | Value64 Word64
  | ValueFloat Float
  | ValueBool Bool
  | ValueString ByteString
  deriving (Show)

instance ToCStruct PerformanceValueDataINTEL where
  withCStruct x f = allocaBytesAligned 8 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr PerformanceValueDataINTEL -> PerformanceValueDataINTEL -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    Value32 v -> lift $ poke (castPtr @_ @Word32 p) (v)
    Value64 v -> lift $ poke (castPtr @_ @Word64 p) (v)
    ValueFloat v -> lift $ poke (castPtr @_ @CFloat p) (CFloat (v))
    ValueBool v -> lift $ poke (castPtr @_ @Bool32 p) (boolToBool32 (v))
    ValueString v -> do
      valueString <- ContT $ useAsCString (v)
      lift $ poke (castPtr @_ @(Ptr CChar) p) valueString
  pokeZeroCStruct :: Ptr PerformanceValueDataINTEL -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 8
  cStructAlignment = 8

instance Zero PerformanceValueDataINTEL where
  zero = Value64 zero

peekPerformanceValueDataINTEL :: PerformanceValueTypeINTEL -> Ptr PerformanceValueDataINTEL -> IO PerformanceValueDataINTEL
peekPerformanceValueDataINTEL tag p = case tag of
  PERFORMANCE_VALUE_TYPE_UINT32_INTEL -> Value32 <$> (peek @Word32 (castPtr @_ @Word32 p))
  PERFORMANCE_VALUE_TYPE_UINT64_INTEL -> Value64 <$> (peek @Word64 (castPtr @_ @Word64 p))
  PERFORMANCE_VALUE_TYPE_FLOAT_INTEL -> ValueFloat <$> (do
    valueFloat <- peek @CFloat (castPtr @_ @CFloat p)
    pure $ (\(CFloat a) -> a) valueFloat)
  PERFORMANCE_VALUE_TYPE_BOOL_INTEL -> ValueBool <$> (do
    valueBool <- peek @Bool32 (castPtr @_ @Bool32 p)
    pure $ bool32ToBool valueBool)
  PERFORMANCE_VALUE_TYPE_STRING_INTEL -> ValueString <$> (packCString =<< peek (castPtr @_ @(Ptr CChar) p))


-- No documentation found for TopLevel "VkPerformanceConfigurationTypeINTEL"
newtype PerformanceConfigurationTypeINTEL = PerformanceConfigurationTypeINTEL Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkPerformanceConfigurationTypeINTEL" "VK_PERFORMANCE_CONFIGURATION_TYPE_COMMAND_QUEUE_METRICS_DISCOVERY_ACTIVATED_INTEL"
pattern PERFORMANCE_CONFIGURATION_TYPE_COMMAND_QUEUE_METRICS_DISCOVERY_ACTIVATED_INTEL =
  PerformanceConfigurationTypeINTEL 0
{-# complete PERFORMANCE_CONFIGURATION_TYPE_COMMAND_QUEUE_METRICS_DISCOVERY_ACTIVATED_INTEL :: PerformanceConfigurationTypeINTEL #-}

conNamePerformanceConfigurationTypeINTEL :: String
conNamePerformanceConfigurationTypeINTEL = "PerformanceConfigurationTypeINTEL"

enumPrefixPerformanceConfigurationTypeINTEL :: String
enumPrefixPerformanceConfigurationTypeINTEL =
  "PERFORMANCE_CONFIGURATION_TYPE_COMMAND_QUEUE_METRICS_DISCOVERY_ACTIVATED_INTEL"

showTablePerformanceConfigurationTypeINTEL :: [(PerformanceConfigurationTypeINTEL, String)]
showTablePerformanceConfigurationTypeINTEL =
  [(PERFORMANCE_CONFIGURATION_TYPE_COMMAND_QUEUE_METRICS_DISCOVERY_ACTIVATED_INTEL, "")]


instance Show PerformanceConfigurationTypeINTEL where
showsPrec = enumShowsPrec enumPrefixPerformanceConfigurationTypeINTEL
                          showTablePerformanceConfigurationTypeINTEL
                          conNamePerformanceConfigurationTypeINTEL
                          (\(PerformanceConfigurationTypeINTEL x) -> x)
                          (showsPrec 11)


instance Read PerformanceConfigurationTypeINTEL where
  readPrec = enumReadPrec enumPrefixPerformanceConfigurationTypeINTEL
                          showTablePerformanceConfigurationTypeINTEL
                          conNamePerformanceConfigurationTypeINTEL
                          PerformanceConfigurationTypeINTEL


-- No documentation found for TopLevel "VkQueryPoolSamplingModeINTEL"
newtype QueryPoolSamplingModeINTEL = QueryPoolSamplingModeINTEL Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkQueryPoolSamplingModeINTEL" "VK_QUERY_POOL_SAMPLING_MODE_MANUAL_INTEL"
pattern QUERY_POOL_SAMPLING_MODE_MANUAL_INTEL = QueryPoolSamplingModeINTEL 0
{-# complete QUERY_POOL_SAMPLING_MODE_MANUAL_INTEL :: QueryPoolSamplingModeINTEL #-}

conNameQueryPoolSamplingModeINTEL :: String
conNameQueryPoolSamplingModeINTEL = "QueryPoolSamplingModeINTEL"

enumPrefixQueryPoolSamplingModeINTEL :: String
enumPrefixQueryPoolSamplingModeINTEL = "QUERY_POOL_SAMPLING_MODE_MANUAL_INTEL"

showTableQueryPoolSamplingModeINTEL :: [(QueryPoolSamplingModeINTEL, String)]
showTableQueryPoolSamplingModeINTEL = [(QUERY_POOL_SAMPLING_MODE_MANUAL_INTEL, "")]


instance Show QueryPoolSamplingModeINTEL where
showsPrec = enumShowsPrec enumPrefixQueryPoolSamplingModeINTEL
                          showTableQueryPoolSamplingModeINTEL
                          conNameQueryPoolSamplingModeINTEL
                          (\(QueryPoolSamplingModeINTEL x) -> x)
                          (showsPrec 11)


instance Read QueryPoolSamplingModeINTEL where
  readPrec = enumReadPrec enumPrefixQueryPoolSamplingModeINTEL
                          showTableQueryPoolSamplingModeINTEL
                          conNameQueryPoolSamplingModeINTEL
                          QueryPoolSamplingModeINTEL


-- No documentation found for TopLevel "VkPerformanceOverrideTypeINTEL"
newtype PerformanceOverrideTypeINTEL = PerformanceOverrideTypeINTEL Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkPerformanceOverrideTypeINTEL" "VK_PERFORMANCE_OVERRIDE_TYPE_NULL_HARDWARE_INTEL"
pattern PERFORMANCE_OVERRIDE_TYPE_NULL_HARDWARE_INTEL    = PerformanceOverrideTypeINTEL 0
-- No documentation found for Nested "VkPerformanceOverrideTypeINTEL" "VK_PERFORMANCE_OVERRIDE_TYPE_FLUSH_GPU_CACHES_INTEL"
pattern PERFORMANCE_OVERRIDE_TYPE_FLUSH_GPU_CACHES_INTEL = PerformanceOverrideTypeINTEL 1
{-# complete PERFORMANCE_OVERRIDE_TYPE_NULL_HARDWARE_INTEL,
             PERFORMANCE_OVERRIDE_TYPE_FLUSH_GPU_CACHES_INTEL :: PerformanceOverrideTypeINTEL #-}

conNamePerformanceOverrideTypeINTEL :: String
conNamePerformanceOverrideTypeINTEL = "PerformanceOverrideTypeINTEL"

enumPrefixPerformanceOverrideTypeINTEL :: String
enumPrefixPerformanceOverrideTypeINTEL = "PERFORMANCE_OVERRIDE_TYPE_"

showTablePerformanceOverrideTypeINTEL :: [(PerformanceOverrideTypeINTEL, String)]
showTablePerformanceOverrideTypeINTEL =
  [ (PERFORMANCE_OVERRIDE_TYPE_NULL_HARDWARE_INTEL   , "NULL_HARDWARE_INTEL")
  , (PERFORMANCE_OVERRIDE_TYPE_FLUSH_GPU_CACHES_INTEL, "FLUSH_GPU_CACHES_INTEL")
  ]


instance Show PerformanceOverrideTypeINTEL where
showsPrec = enumShowsPrec enumPrefixPerformanceOverrideTypeINTEL
                          showTablePerformanceOverrideTypeINTEL
                          conNamePerformanceOverrideTypeINTEL
                          (\(PerformanceOverrideTypeINTEL x) -> x)
                          (showsPrec 11)


instance Read PerformanceOverrideTypeINTEL where
  readPrec = enumReadPrec enumPrefixPerformanceOverrideTypeINTEL
                          showTablePerformanceOverrideTypeINTEL
                          conNamePerformanceOverrideTypeINTEL
                          PerformanceOverrideTypeINTEL


-- No documentation found for TopLevel "VkPerformanceParameterTypeINTEL"
newtype PerformanceParameterTypeINTEL = PerformanceParameterTypeINTEL Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkPerformanceParameterTypeINTEL" "VK_PERFORMANCE_PARAMETER_TYPE_HW_COUNTERS_SUPPORTED_INTEL"
pattern PERFORMANCE_PARAMETER_TYPE_HW_COUNTERS_SUPPORTED_INTEL    = PerformanceParameterTypeINTEL 0
-- No documentation found for Nested "VkPerformanceParameterTypeINTEL" "VK_PERFORMANCE_PARAMETER_TYPE_STREAM_MARKER_VALID_BITS_INTEL"
pattern PERFORMANCE_PARAMETER_TYPE_STREAM_MARKER_VALID_BITS_INTEL = PerformanceParameterTypeINTEL 1
{-# complete PERFORMANCE_PARAMETER_TYPE_HW_COUNTERS_SUPPORTED_INTEL,
             PERFORMANCE_PARAMETER_TYPE_STREAM_MARKER_VALID_BITS_INTEL :: PerformanceParameterTypeINTEL #-}

conNamePerformanceParameterTypeINTEL :: String
conNamePerformanceParameterTypeINTEL = "PerformanceParameterTypeINTEL"

enumPrefixPerformanceParameterTypeINTEL :: String
enumPrefixPerformanceParameterTypeINTEL = "PERFORMANCE_PARAMETER_TYPE_"

showTablePerformanceParameterTypeINTEL :: [(PerformanceParameterTypeINTEL, String)]
showTablePerformanceParameterTypeINTEL =
  [ (PERFORMANCE_PARAMETER_TYPE_HW_COUNTERS_SUPPORTED_INTEL   , "HW_COUNTERS_SUPPORTED_INTEL")
  , (PERFORMANCE_PARAMETER_TYPE_STREAM_MARKER_VALID_BITS_INTEL, "STREAM_MARKER_VALID_BITS_INTEL")
  ]


instance Show PerformanceParameterTypeINTEL where
showsPrec = enumShowsPrec enumPrefixPerformanceParameterTypeINTEL
                          showTablePerformanceParameterTypeINTEL
                          conNamePerformanceParameterTypeINTEL
                          (\(PerformanceParameterTypeINTEL x) -> x)
                          (showsPrec 11)


instance Read PerformanceParameterTypeINTEL where
  readPrec = enumReadPrec enumPrefixPerformanceParameterTypeINTEL
                          showTablePerformanceParameterTypeINTEL
                          conNamePerformanceParameterTypeINTEL
                          PerformanceParameterTypeINTEL


-- No documentation found for TopLevel "VkPerformanceValueTypeINTEL"
newtype PerformanceValueTypeINTEL = PerformanceValueTypeINTEL Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkPerformanceValueTypeINTEL" "VK_PERFORMANCE_VALUE_TYPE_UINT32_INTEL"
pattern PERFORMANCE_VALUE_TYPE_UINT32_INTEL = PerformanceValueTypeINTEL 0
-- No documentation found for Nested "VkPerformanceValueTypeINTEL" "VK_PERFORMANCE_VALUE_TYPE_UINT64_INTEL"
pattern PERFORMANCE_VALUE_TYPE_UINT64_INTEL = PerformanceValueTypeINTEL 1
-- No documentation found for Nested "VkPerformanceValueTypeINTEL" "VK_PERFORMANCE_VALUE_TYPE_FLOAT_INTEL"
pattern PERFORMANCE_VALUE_TYPE_FLOAT_INTEL  = PerformanceValueTypeINTEL 2
-- No documentation found for Nested "VkPerformanceValueTypeINTEL" "VK_PERFORMANCE_VALUE_TYPE_BOOL_INTEL"
pattern PERFORMANCE_VALUE_TYPE_BOOL_INTEL   = PerformanceValueTypeINTEL 3
-- No documentation found for Nested "VkPerformanceValueTypeINTEL" "VK_PERFORMANCE_VALUE_TYPE_STRING_INTEL"
pattern PERFORMANCE_VALUE_TYPE_STRING_INTEL = PerformanceValueTypeINTEL 4
{-# complete PERFORMANCE_VALUE_TYPE_UINT32_INTEL,
             PERFORMANCE_VALUE_TYPE_UINT64_INTEL,
             PERFORMANCE_VALUE_TYPE_FLOAT_INTEL,
             PERFORMANCE_VALUE_TYPE_BOOL_INTEL,
             PERFORMANCE_VALUE_TYPE_STRING_INTEL :: PerformanceValueTypeINTEL #-}

conNamePerformanceValueTypeINTEL :: String
conNamePerformanceValueTypeINTEL = "PerformanceValueTypeINTEL"

enumPrefixPerformanceValueTypeINTEL :: String
enumPrefixPerformanceValueTypeINTEL = "PERFORMANCE_VALUE_TYPE_"

showTablePerformanceValueTypeINTEL :: [(PerformanceValueTypeINTEL, String)]
showTablePerformanceValueTypeINTEL =
  [ (PERFORMANCE_VALUE_TYPE_UINT32_INTEL, "UINT32_INTEL")
  , (PERFORMANCE_VALUE_TYPE_UINT64_INTEL, "UINT64_INTEL")
  , (PERFORMANCE_VALUE_TYPE_FLOAT_INTEL , "FLOAT_INTEL")
  , (PERFORMANCE_VALUE_TYPE_BOOL_INTEL  , "BOOL_INTEL")
  , (PERFORMANCE_VALUE_TYPE_STRING_INTEL, "STRING_INTEL")
  ]


instance Show PerformanceValueTypeINTEL where
showsPrec = enumShowsPrec enumPrefixPerformanceValueTypeINTEL
                          showTablePerformanceValueTypeINTEL
                          conNamePerformanceValueTypeINTEL
                          (\(PerformanceValueTypeINTEL x) -> x)
                          (showsPrec 11)


instance Read PerformanceValueTypeINTEL where
  readPrec = enumReadPrec enumPrefixPerformanceValueTypeINTEL
                          showTablePerformanceValueTypeINTEL
                          conNamePerformanceValueTypeINTEL
                          PerformanceValueTypeINTEL


-- No documentation found for TopLevel "VkQueryPoolCreateInfoINTEL"
type QueryPoolCreateInfoINTEL = QueryPoolPerformanceQueryCreateInfoINTEL


type INTEL_PERFORMANCE_QUERY_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_INTEL_PERFORMANCE_QUERY_SPEC_VERSION"
pattern INTEL_PERFORMANCE_QUERY_SPEC_VERSION :: forall a . Integral a => a
pattern INTEL_PERFORMANCE_QUERY_SPEC_VERSION = 2


type INTEL_PERFORMANCE_QUERY_EXTENSION_NAME = "VK_INTEL_performance_query"

-- No documentation found for TopLevel "VK_INTEL_PERFORMANCE_QUERY_EXTENSION_NAME"
pattern INTEL_PERFORMANCE_QUERY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern INTEL_PERFORMANCE_QUERY_EXTENSION_NAME = "VK_INTEL_performance_query"

