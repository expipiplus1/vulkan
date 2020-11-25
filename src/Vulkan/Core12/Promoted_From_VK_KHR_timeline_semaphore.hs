{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_timeline_semaphore"
module Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore  ( getSemaphoreCounterValue
                                                              , waitSemaphores
                                                              , waitSemaphoresSafe
                                                              , signalSemaphore
                                                              , PhysicalDeviceTimelineSemaphoreFeatures(..)
                                                              , PhysicalDeviceTimelineSemaphoreProperties(..)
                                                              , SemaphoreTypeCreateInfo(..)
                                                              , TimelineSemaphoreSubmitInfo(..)
                                                              , SemaphoreWaitInfo(..)
                                                              , SemaphoreSignalInfo(..)
                                                              , StructureType(..)
                                                              , SemaphoreType(..)
                                                              , SemaphoreWaitFlagBits(..)
                                                              , SemaphoreWaitFlags
                                                              ) where

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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Control.Monad.IO.Class (MonadIO)
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
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetSemaphoreCounterValue))
import Vulkan.Dynamic (DeviceCmds(pVkSignalSemaphore))
import Vulkan.Dynamic (DeviceCmds(pVkWaitSemaphores))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Handles (Semaphore)
import Vulkan.Core10.Handles (Semaphore(..))
import Vulkan.Core12.Enums.SemaphoreType (SemaphoreType)
import Vulkan.Core12.Enums.SemaphoreWaitFlagBits (SemaphoreWaitFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core12.Enums.SemaphoreType (SemaphoreType(..))
import Vulkan.Core12.Enums.SemaphoreWaitFlagBits (SemaphoreWaitFlagBits(..))
import Vulkan.Core12.Enums.SemaphoreWaitFlagBits (SemaphoreWaitFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSemaphoreCounterValue
  :: FunPtr (Ptr Device_T -> Semaphore -> Ptr Word64 -> IO Result) -> Ptr Device_T -> Semaphore -> Ptr Word64 -> IO Result

-- No documentation found for TopLevel "vkGetSemaphoreCounterValue"
getSemaphoreCounterValue :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkGetSemaphoreCounterValue" "device"
                            Device
                         -> -- No documentation found for Nested "vkGetSemaphoreCounterValue" "semaphore"
                            Semaphore
                         -> io (("value" ::: Word64))
getSemaphoreCounterValue device semaphore = liftIO . evalContT $ do
  let vkGetSemaphoreCounterValuePtr = pVkGetSemaphoreCounterValue (deviceCmds (device :: Device))
  lift $ unless (vkGetSemaphoreCounterValuePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetSemaphoreCounterValue is null" Nothing Nothing
  let vkGetSemaphoreCounterValue' = mkVkGetSemaphoreCounterValue vkGetSemaphoreCounterValuePtr
  pPValue <- ContT $ bracket (callocBytes @Word64 8) free
  r <- lift $ vkGetSemaphoreCounterValue' (deviceHandle (device)) (semaphore) (pPValue)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pValue <- lift $ peek @Word64 pPValue
  pure $ (pValue)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkWaitSemaphoresUnsafe
  :: FunPtr (Ptr Device_T -> Ptr SemaphoreWaitInfo -> Word64 -> IO Result) -> Ptr Device_T -> Ptr SemaphoreWaitInfo -> Word64 -> IO Result

foreign import ccall
  "dynamic" mkVkWaitSemaphoresSafe
  :: FunPtr (Ptr Device_T -> Ptr SemaphoreWaitInfo -> Word64 -> IO Result) -> Ptr Device_T -> Ptr SemaphoreWaitInfo -> Word64 -> IO Result

-- | waitSemaphores with selectable safeness
waitSemaphoresSafeOrUnsafe :: forall io
                            . (MonadIO io)
                           => -- No documentation found for TopLevel ""
                              (FunPtr (Ptr Device_T -> Ptr SemaphoreWaitInfo -> Word64 -> IO Result) -> Ptr Device_T -> Ptr SemaphoreWaitInfo -> Word64 -> IO Result)
                           -> -- No documentation found for Nested "vkWaitSemaphores" "device"
                              Device
                           -> -- No documentation found for Nested "vkWaitSemaphores" "pWaitInfo"
                              SemaphoreWaitInfo
                           -> -- No documentation found for Nested "vkWaitSemaphores" "timeout"
                              ("timeout" ::: Word64)
                           -> io (Result)
waitSemaphoresSafeOrUnsafe mkVkWaitSemaphores device waitInfo timeout = liftIO . evalContT $ do
  let vkWaitSemaphoresPtr = pVkWaitSemaphores (deviceCmds (device :: Device))
  lift $ unless (vkWaitSemaphoresPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkWaitSemaphores is null" Nothing Nothing
  let vkWaitSemaphores' = mkVkWaitSemaphores vkWaitSemaphoresPtr
  pWaitInfo <- ContT $ withCStruct (waitInfo)
  r <- lift $ vkWaitSemaphores' (deviceHandle (device)) pWaitInfo (timeout)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)

-- No documentation found for TopLevel "vkWaitSemaphores"
waitSemaphores :: forall io
                . (MonadIO io)
               => -- No documentation found for Nested "vkWaitSemaphores" "device"
                  Device
               -> -- No documentation found for Nested "vkWaitSemaphores" "pWaitInfo"
                  SemaphoreWaitInfo
               -> -- No documentation found for Nested "vkWaitSemaphores" "timeout"
                  ("timeout" ::: Word64)
               -> io (Result)
waitSemaphores = waitSemaphoresSafeOrUnsafe mkVkWaitSemaphoresUnsafe

-- | A variant of 'waitSemaphores' which makes a *safe* FFI call
waitSemaphoresSafe :: forall io
                    . (MonadIO io)
                   => -- No documentation found for Nested "vkWaitSemaphores" "device"
                      Device
                   -> -- No documentation found for Nested "vkWaitSemaphores" "pWaitInfo"
                      SemaphoreWaitInfo
                   -> -- No documentation found for Nested "vkWaitSemaphores" "timeout"
                      ("timeout" ::: Word64)
                   -> io (Result)
waitSemaphoresSafe = waitSemaphoresSafeOrUnsafe mkVkWaitSemaphoresSafe


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSignalSemaphore
  :: FunPtr (Ptr Device_T -> Ptr SemaphoreSignalInfo -> IO Result) -> Ptr Device_T -> Ptr SemaphoreSignalInfo -> IO Result

-- No documentation found for TopLevel "vkSignalSemaphore"
signalSemaphore :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vkSignalSemaphore" "device"
                   Device
                -> -- No documentation found for Nested "vkSignalSemaphore" "pSignalInfo"
                   SemaphoreSignalInfo
                -> io ()
signalSemaphore device signalInfo = liftIO . evalContT $ do
  let vkSignalSemaphorePtr = pVkSignalSemaphore (deviceCmds (device :: Device))
  lift $ unless (vkSignalSemaphorePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSignalSemaphore is null" Nothing Nothing
  let vkSignalSemaphore' = mkVkSignalSemaphore vkSignalSemaphorePtr
  pSignalInfo <- ContT $ withCStruct (signalInfo)
  r <- lift $ vkSignalSemaphore' (deviceHandle (device)) pSignalInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))



-- No documentation found for TopLevel "VkPhysicalDeviceTimelineSemaphoreFeatures"
data PhysicalDeviceTimelineSemaphoreFeatures = PhysicalDeviceTimelineSemaphoreFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceTimelineSemaphoreFeatures" "timelineSemaphore"
    timelineSemaphore :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTimelineSemaphoreFeatures)
#endif
deriving instance Show PhysicalDeviceTimelineSemaphoreFeatures

instance ToCStruct PhysicalDeviceTimelineSemaphoreFeatures where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTimelineSemaphoreFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (timelineSemaphore))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTimelineSemaphoreFeatures where
  peekCStruct p = do
    timelineSemaphore <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceTimelineSemaphoreFeatures
             (bool32ToBool timelineSemaphore)


instance Storable PhysicalDeviceTimelineSemaphoreFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTimelineSemaphoreFeatures where
  zero = PhysicalDeviceTimelineSemaphoreFeatures
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceTimelineSemaphoreProperties"
data PhysicalDeviceTimelineSemaphoreProperties = PhysicalDeviceTimelineSemaphoreProperties
  { -- No documentation found for Nested "VkPhysicalDeviceTimelineSemaphoreProperties" "maxTimelineSemaphoreValueDifference"
    maxTimelineSemaphoreValueDifference :: Word64 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTimelineSemaphoreProperties)
#endif
deriving instance Show PhysicalDeviceTimelineSemaphoreProperties

instance ToCStruct PhysicalDeviceTimelineSemaphoreProperties where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTimelineSemaphoreProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (maxTimelineSemaphoreValueDifference)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    f

instance FromCStruct PhysicalDeviceTimelineSemaphoreProperties where
  peekCStruct p = do
    maxTimelineSemaphoreValueDifference <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    pure $ PhysicalDeviceTimelineSemaphoreProperties
             maxTimelineSemaphoreValueDifference


instance Storable PhysicalDeviceTimelineSemaphoreProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTimelineSemaphoreProperties where
  zero = PhysicalDeviceTimelineSemaphoreProperties
           zero



-- No documentation found for TopLevel "VkSemaphoreTypeCreateInfo"
data SemaphoreTypeCreateInfo = SemaphoreTypeCreateInfo
  { -- No documentation found for Nested "VkSemaphoreTypeCreateInfo" "semaphoreType"
    semaphoreType :: SemaphoreType
  , -- No documentation found for Nested "VkSemaphoreTypeCreateInfo" "initialValue"
    initialValue :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SemaphoreTypeCreateInfo)
#endif
deriving instance Show SemaphoreTypeCreateInfo

instance ToCStruct SemaphoreTypeCreateInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SemaphoreTypeCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SemaphoreType)) (semaphoreType)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (initialValue)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SemaphoreType)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    f

instance FromCStruct SemaphoreTypeCreateInfo where
  peekCStruct p = do
    semaphoreType <- peek @SemaphoreType ((p `plusPtr` 16 :: Ptr SemaphoreType))
    initialValue <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    pure $ SemaphoreTypeCreateInfo
             semaphoreType initialValue


instance Storable SemaphoreTypeCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SemaphoreTypeCreateInfo where
  zero = SemaphoreTypeCreateInfo
           zero
           zero



-- No documentation found for TopLevel "VkTimelineSemaphoreSubmitInfo"
data TimelineSemaphoreSubmitInfo = TimelineSemaphoreSubmitInfo
  { -- No documentation found for Nested "VkTimelineSemaphoreSubmitInfo" "waitSemaphoreValueCount"
    waitSemaphoreValueCount :: Word32
  , -- No documentation found for Nested "VkTimelineSemaphoreSubmitInfo" "pWaitSemaphoreValues"
    waitSemaphoreValues :: Vector Word64
  , -- No documentation found for Nested "VkTimelineSemaphoreSubmitInfo" "signalSemaphoreValueCount"
    signalSemaphoreValueCount :: Word32
  , -- No documentation found for Nested "VkTimelineSemaphoreSubmitInfo" "pSignalSemaphoreValues"
    signalSemaphoreValues :: Vector Word64
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TimelineSemaphoreSubmitInfo)
#endif
deriving instance Show TimelineSemaphoreSubmitInfo

instance ToCStruct TimelineSemaphoreSubmitInfo where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TimelineSemaphoreSubmitInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    let pWaitSemaphoreValuesLength = Data.Vector.length $ (waitSemaphoreValues)
    waitSemaphoreValueCount'' <- lift $ if (waitSemaphoreValueCount) == 0
      then pure $ fromIntegral pWaitSemaphoreValuesLength
      else do
        unless (fromIntegral pWaitSemaphoreValuesLength == (waitSemaphoreValueCount) || pWaitSemaphoreValuesLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pWaitSemaphoreValues must be empty or have 'waitSemaphoreValueCount' elements" Nothing Nothing
        pure (waitSemaphoreValueCount)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (waitSemaphoreValueCount'')
    pWaitSemaphoreValues'' <- if Data.Vector.null (waitSemaphoreValues)
      then pure nullPtr
      else do
        pPWaitSemaphoreValues <- ContT $ allocaBytesAligned @Word64 (((Data.Vector.length (waitSemaphoreValues))) * 8) 8
        lift $ Data.Vector.imapM_ (\i e -> poke (pPWaitSemaphoreValues `plusPtr` (8 * (i)) :: Ptr Word64) (e)) ((waitSemaphoreValues))
        pure $ pPWaitSemaphoreValues
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word64))) pWaitSemaphoreValues''
    let pSignalSemaphoreValuesLength = Data.Vector.length $ (signalSemaphoreValues)
    signalSemaphoreValueCount'' <- lift $ if (signalSemaphoreValueCount) == 0
      then pure $ fromIntegral pSignalSemaphoreValuesLength
      else do
        unless (fromIntegral pSignalSemaphoreValuesLength == (signalSemaphoreValueCount) || pSignalSemaphoreValuesLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pSignalSemaphoreValues must be empty or have 'signalSemaphoreValueCount' elements" Nothing Nothing
        pure (signalSemaphoreValueCount)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (signalSemaphoreValueCount'')
    pSignalSemaphoreValues'' <- if Data.Vector.null (signalSemaphoreValues)
      then pure nullPtr
      else do
        pPSignalSemaphoreValues <- ContT $ allocaBytesAligned @Word64 (((Data.Vector.length (signalSemaphoreValues))) * 8) 8
        lift $ Data.Vector.imapM_ (\i e -> poke (pPSignalSemaphoreValues `plusPtr` (8 * (i)) :: Ptr Word64) (e)) ((signalSemaphoreValues))
        pure $ pPSignalSemaphoreValues
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Word64))) pSignalSemaphoreValues''
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct TimelineSemaphoreSubmitInfo where
  peekCStruct p = do
    waitSemaphoreValueCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pWaitSemaphoreValues <- peek @(Ptr Word64) ((p `plusPtr` 24 :: Ptr (Ptr Word64)))
    let pWaitSemaphoreValuesLength = if pWaitSemaphoreValues == nullPtr then 0 else (fromIntegral waitSemaphoreValueCount)
    pWaitSemaphoreValues' <- generateM pWaitSemaphoreValuesLength (\i -> peek @Word64 ((pWaitSemaphoreValues `advancePtrBytes` (8 * (i)) :: Ptr Word64)))
    signalSemaphoreValueCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pSignalSemaphoreValues <- peek @(Ptr Word64) ((p `plusPtr` 40 :: Ptr (Ptr Word64)))
    let pSignalSemaphoreValuesLength = if pSignalSemaphoreValues == nullPtr then 0 else (fromIntegral signalSemaphoreValueCount)
    pSignalSemaphoreValues' <- generateM pSignalSemaphoreValuesLength (\i -> peek @Word64 ((pSignalSemaphoreValues `advancePtrBytes` (8 * (i)) :: Ptr Word64)))
    pure $ TimelineSemaphoreSubmitInfo
             waitSemaphoreValueCount pWaitSemaphoreValues' signalSemaphoreValueCount pSignalSemaphoreValues'

instance Zero TimelineSemaphoreSubmitInfo where
  zero = TimelineSemaphoreSubmitInfo
           zero
           mempty
           zero
           mempty



-- No documentation found for TopLevel "VkSemaphoreWaitInfo"
data SemaphoreWaitInfo = SemaphoreWaitInfo
  { -- No documentation found for Nested "VkSemaphoreWaitInfo" "flags"
    flags :: SemaphoreWaitFlags
  , -- No documentation found for Nested "VkSemaphoreWaitInfo" "pSemaphores"
    semaphores :: Vector Semaphore
  , -- No documentation found for Nested "VkSemaphoreWaitInfo" "pValues"
    values :: Vector Word64
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SemaphoreWaitInfo)
#endif
deriving instance Show SemaphoreWaitInfo

instance ToCStruct SemaphoreWaitInfo where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SemaphoreWaitInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr SemaphoreWaitFlags)) (flags)
    let pSemaphoresLength = Data.Vector.length $ (semaphores)
    lift $ unless ((Data.Vector.length $ (values)) == pSemaphoresLength) $
      throwIO $ IOError Nothing InvalidArgument "" "pValues and pSemaphores must have the same length" Nothing Nothing
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral pSemaphoresLength :: Word32))
    pPSemaphores' <- ContT $ allocaBytesAligned @Semaphore ((Data.Vector.length (semaphores)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSemaphores' `plusPtr` (8 * (i)) :: Ptr Semaphore) (e)) (semaphores)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Semaphore))) (pPSemaphores')
    pPValues' <- ContT $ allocaBytesAligned @Word64 ((Data.Vector.length (values)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPValues' `plusPtr` (8 * (i)) :: Ptr Word64) (e)) (values)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Word64))) (pPValues')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPSemaphores' <- ContT $ allocaBytesAligned @Semaphore ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSemaphores' `plusPtr` (8 * (i)) :: Ptr Semaphore) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Semaphore))) (pPSemaphores')
    pPValues' <- ContT $ allocaBytesAligned @Word64 ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPValues' `plusPtr` (8 * (i)) :: Ptr Word64) (e)) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Word64))) (pPValues')
    lift $ f

instance FromCStruct SemaphoreWaitInfo where
  peekCStruct p = do
    flags <- peek @SemaphoreWaitFlags ((p `plusPtr` 16 :: Ptr SemaphoreWaitFlags))
    semaphoreCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pSemaphores <- peek @(Ptr Semaphore) ((p `plusPtr` 24 :: Ptr (Ptr Semaphore)))
    pSemaphores' <- generateM (fromIntegral semaphoreCount) (\i -> peek @Semaphore ((pSemaphores `advancePtrBytes` (8 * (i)) :: Ptr Semaphore)))
    pValues <- peek @(Ptr Word64) ((p `plusPtr` 32 :: Ptr (Ptr Word64)))
    pValues' <- generateM (fromIntegral semaphoreCount) (\i -> peek @Word64 ((pValues `advancePtrBytes` (8 * (i)) :: Ptr Word64)))
    pure $ SemaphoreWaitInfo
             flags pSemaphores' pValues'

instance Zero SemaphoreWaitInfo where
  zero = SemaphoreWaitInfo
           zero
           mempty
           mempty



-- No documentation found for TopLevel "VkSemaphoreSignalInfo"
data SemaphoreSignalInfo = SemaphoreSignalInfo
  { -- No documentation found for Nested "VkSemaphoreSignalInfo" "semaphore"
    semaphore :: Semaphore
  , -- No documentation found for Nested "VkSemaphoreSignalInfo" "value"
    value :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SemaphoreSignalInfo)
#endif
deriving instance Show SemaphoreSignalInfo

instance ToCStruct SemaphoreSignalInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SemaphoreSignalInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (semaphore)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (value)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    f

instance FromCStruct SemaphoreSignalInfo where
  peekCStruct p = do
    semaphore <- peek @Semaphore ((p `plusPtr` 16 :: Ptr Semaphore))
    value <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    pure $ SemaphoreSignalInfo
             semaphore value


instance Storable SemaphoreSignalInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SemaphoreSignalInfo where
  zero = SemaphoreSignalInfo
           zero
           zero

