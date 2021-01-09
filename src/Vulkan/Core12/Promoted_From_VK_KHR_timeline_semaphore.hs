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

import Vulkan.Internal.Utils (traceAroundEvent)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Handles (Semaphore)
import Vulkan.Core10.Handles (Semaphore(..))
import Vulkan.Core12.Enums.SemaphoreType (SemaphoreType)
import Vulkan.Core12.Enums.SemaphoreWaitFlagBits (SemaphoreWaitFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
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

-- | vkGetSemaphoreCounterValue - Query the current state of a timeline
-- semaphore
--
-- = Description
--
-- Note
--
-- If a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-submission queue submission>
-- command is pending execution, then the value returned by this command
-- /may/ immediately be out of date.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Semaphore'
getSemaphoreCounterValue :: forall io
                          . (MonadIO io)
                         => -- | @device@ is the logical device that owns the semaphore.
                            --
                            -- #VUID-vkGetSemaphoreCounterValue-device-parameter# @device@ /must/ be a
                            -- valid 'Vulkan.Core10.Handles.Device' handle
                            Device
                         -> -- | @semaphore@ is the handle of the semaphore to query.
                            --
                            -- #VUID-vkGetSemaphoreCounterValue-semaphore-03255# @semaphore@ /must/
                            -- have been created with a
                            -- 'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
                            -- 'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE'
                            --
                            -- #VUID-vkGetSemaphoreCounterValue-semaphore-parameter# @semaphore@ /must/
                            -- be a valid 'Vulkan.Core10.Handles.Semaphore' handle
                            --
                            -- #VUID-vkGetSemaphoreCounterValue-semaphore-parent# @semaphore@ /must/
                            -- have been created, allocated, or retrieved from @device@
                            Semaphore
                         -> io (("value" ::: Word64))
getSemaphoreCounterValue device semaphore = liftIO . evalContT $ do
  let vkGetSemaphoreCounterValuePtr = pVkGetSemaphoreCounterValue (deviceCmds (device :: Device))
  lift $ unless (vkGetSemaphoreCounterValuePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetSemaphoreCounterValue is null" Nothing Nothing
  let vkGetSemaphoreCounterValue' = mkVkGetSemaphoreCounterValue vkGetSemaphoreCounterValuePtr
  pPValue <- ContT $ bracket (callocBytes @Word64 8) free
  r <- lift $ traceAroundEvent "vkGetSemaphoreCounterValue" (vkGetSemaphoreCounterValue' (deviceHandle (device)) (semaphore) (pPValue))
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
                           => (FunPtr (Ptr Device_T -> Ptr SemaphoreWaitInfo -> Word64 -> IO Result) -> Ptr Device_T -> Ptr SemaphoreWaitInfo -> Word64 -> IO Result)
                           -> -- | @device@ is the logical device that owns the semaphore.
                              --
                              -- #VUID-vkWaitSemaphores-device-parameter# @device@ /must/ be a valid
                              -- 'Vulkan.Core10.Handles.Device' handle
                              Device
                           -> -- | @pWaitInfo@ is a pointer to a 'SemaphoreWaitInfo' structure containing
                              -- information about the wait condition.
                              --
                              -- #VUID-vkWaitSemaphores-pWaitInfo-parameter# @pWaitInfo@ /must/ be a
                              -- valid pointer to a valid 'SemaphoreWaitInfo' structure
                              SemaphoreWaitInfo
                           -> -- | @timeout@ is the timeout period in units of nanoseconds. @timeout@ is
                              -- adjusted to the closest value allowed by the implementation-dependent
                              -- timeout accuracy, which /may/ be substantially longer than one
                              -- nanosecond, and /may/ be longer than the requested period.
                              ("timeout" ::: Word64)
                           -> io (Result)
waitSemaphoresSafeOrUnsafe mkVkWaitSemaphores device waitInfo timeout = liftIO . evalContT $ do
  let vkWaitSemaphoresPtr = pVkWaitSemaphores (deviceCmds (device :: Device))
  lift $ unless (vkWaitSemaphoresPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkWaitSemaphores is null" Nothing Nothing
  let vkWaitSemaphores' = mkVkWaitSemaphores vkWaitSemaphoresPtr
  pWaitInfo <- ContT $ withCStruct (waitInfo)
  r <- lift $ traceAroundEvent "vkWaitSemaphores" (vkWaitSemaphores' (deviceHandle (device)) pWaitInfo (timeout))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)

-- | vkWaitSemaphores - Wait for timeline semaphores on the host
--
-- = Description
--
-- If the condition is satisfied when 'waitSemaphores' is called, then
-- 'waitSemaphores' returns immediately. If the condition is not satisfied
-- at the time 'waitSemaphores' is called, then 'waitSemaphores' will block
-- and wait until the condition is satisfied or the @timeout@ has expired,
-- whichever is sooner.
--
-- If @timeout@ is zero, then 'waitSemaphores' does not wait, but simply
-- returns information about the current state of the semaphore.
-- 'Vulkan.Core10.Enums.Result.TIMEOUT' will be returned in this case if
-- the condition is not satisfied, even though no actual wait was
-- performed.
--
-- If the condition is satisfied before the @timeout@ has expired,
-- 'waitSemaphores' returns 'Vulkan.Core10.Enums.Result.SUCCESS'.
-- Otherwise, 'waitSemaphores' returns 'Vulkan.Core10.Enums.Result.TIMEOUT'
-- after the @timeout@ has expired.
--
-- If device loss occurs (see
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-lost-device Lost Device>)
-- before the timeout has expired, 'waitSemaphores' /must/ return in finite
-- time with either 'Vulkan.Core10.Enums.Result.SUCCESS' or
-- 'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.TIMEOUT'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'SemaphoreWaitInfo'
waitSemaphores :: forall io
                . (MonadIO io)
               => -- | @device@ is the logical device that owns the semaphore.
                  --
                  -- #VUID-vkWaitSemaphores-device-parameter# @device@ /must/ be a valid
                  -- 'Vulkan.Core10.Handles.Device' handle
                  Device
               -> -- | @pWaitInfo@ is a pointer to a 'SemaphoreWaitInfo' structure containing
                  -- information about the wait condition.
                  --
                  -- #VUID-vkWaitSemaphores-pWaitInfo-parameter# @pWaitInfo@ /must/ be a
                  -- valid pointer to a valid 'SemaphoreWaitInfo' structure
                  SemaphoreWaitInfo
               -> -- | @timeout@ is the timeout period in units of nanoseconds. @timeout@ is
                  -- adjusted to the closest value allowed by the implementation-dependent
                  -- timeout accuracy, which /may/ be substantially longer than one
                  -- nanosecond, and /may/ be longer than the requested period.
                  ("timeout" ::: Word64)
               -> io (Result)
waitSemaphores = waitSemaphoresSafeOrUnsafe mkVkWaitSemaphoresUnsafe

-- | A variant of 'waitSemaphores' which makes a *safe* FFI call
waitSemaphoresSafe :: forall io
                    . (MonadIO io)
                   => -- | @device@ is the logical device that owns the semaphore.
                      --
                      -- #VUID-vkWaitSemaphores-device-parameter# @device@ /must/ be a valid
                      -- 'Vulkan.Core10.Handles.Device' handle
                      Device
                   -> -- | @pWaitInfo@ is a pointer to a 'SemaphoreWaitInfo' structure containing
                      -- information about the wait condition.
                      --
                      -- #VUID-vkWaitSemaphores-pWaitInfo-parameter# @pWaitInfo@ /must/ be a
                      -- valid pointer to a valid 'SemaphoreWaitInfo' structure
                      SemaphoreWaitInfo
                   -> -- | @timeout@ is the timeout period in units of nanoseconds. @timeout@ is
                      -- adjusted to the closest value allowed by the implementation-dependent
                      -- timeout accuracy, which /may/ be substantially longer than one
                      -- nanosecond, and /may/ be longer than the requested period.
                      ("timeout" ::: Word64)
                   -> io (Result)
waitSemaphoresSafe = waitSemaphoresSafeOrUnsafe mkVkWaitSemaphoresSafe


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSignalSemaphore
  :: FunPtr (Ptr Device_T -> Ptr SemaphoreSignalInfo -> IO Result) -> Ptr Device_T -> Ptr SemaphoreSignalInfo -> IO Result

-- | vkSignalSemaphore - Signal a timeline semaphore on the host
--
-- = Description
--
-- When 'signalSemaphore' is executed on the host, it defines and
-- immediately executes a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operation>
-- which sets the timeline semaphore to the given value.
--
-- The first synchronization scope is defined by the host execution model,
-- but includes execution of 'signalSemaphore' on the host and anything
-- that happened-before it.
--
-- The second synchronization scope is empty.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'SemaphoreSignalInfo'
signalSemaphore :: forall io
                 . (MonadIO io)
                => -- | @device@ is the logical device that owns the semaphore.
                   --
                   -- #VUID-vkSignalSemaphore-device-parameter# @device@ /must/ be a valid
                   -- 'Vulkan.Core10.Handles.Device' handle
                   Device
                -> -- | @pSignalInfo@ is a pointer to a 'SemaphoreSignalInfo' structure
                   -- containing information about the signal operation.
                   --
                   -- #VUID-vkSignalSemaphore-pSignalInfo-parameter# @pSignalInfo@ /must/ be a
                   -- valid pointer to a valid 'SemaphoreSignalInfo' structure
                   SemaphoreSignalInfo
                -> io ()
signalSemaphore device signalInfo = liftIO . evalContT $ do
  let vkSignalSemaphorePtr = pVkSignalSemaphore (deviceCmds (device :: Device))
  lift $ unless (vkSignalSemaphorePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSignalSemaphore is null" Nothing Nothing
  let vkSignalSemaphore' = mkVkSignalSemaphore vkSignalSemaphorePtr
  pSignalInfo <- ContT $ withCStruct (signalInfo)
  r <- lift $ traceAroundEvent "vkSignalSemaphore" (vkSignalSemaphore' (deviceHandle (device)) pSignalInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkPhysicalDeviceTimelineSemaphoreFeatures - Structure describing
-- timeline semaphore features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceTimelineSemaphoreFeatures' structure
-- describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceTimelineSemaphoreFeatures' structure is included
-- in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceTimelineSemaphoreFeatures' /can/ also be included in the
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceTimelineSemaphoreFeatures = PhysicalDeviceTimelineSemaphoreFeatures
  { -- | #extension-features-timelineSemaphore# @timelineSemaphore@ indicates
    -- whether semaphores created with a
    -- 'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
    -- 'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE' are
    -- supported.
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


-- | VkPhysicalDeviceTimelineSemaphoreProperties - Structure describing
-- timeline semaphore properties that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceTimelineSemaphoreProperties' structure
-- describe the following implementation-dependent limits:
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceTimelineSemaphoreProperties = PhysicalDeviceTimelineSemaphoreProperties
  { -- | #extension-limits-maxTimelineSemaphoreValueDifference#
    -- @maxTimelineSemaphoreValueDifference@ indicates the maximum difference
    -- allowed by the implementation between the current value of a timeline
    -- semaphore and any pending signal or wait operations.
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


-- | VkSemaphoreTypeCreateInfo - Structure specifying the type of a newly
-- created semaphore
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSemaphoreTypeCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO'
--
-- -   #VUID-VkSemaphoreTypeCreateInfo-semaphoreType-parameter#
--     @semaphoreType@ /must/ be a valid
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' value
--
-- == Valid Usage
--
-- -   #VUID-VkSemaphoreTypeCreateInfo-timelineSemaphore-03252# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-timelineSemaphore timelineSemaphore>
--     feature is not enabled, @semaphoreType@ /must/ not equal
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE'
--
-- -   #VUID-VkSemaphoreTypeCreateInfo-semaphoreType-03279# If
--     @semaphoreType@ is
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_BINARY',
--     @initialValue@ /must/ be zero
--
-- If no 'SemaphoreTypeCreateInfo' structure is included in the @pNext@
-- chain of 'Vulkan.Core10.QueueSemaphore.SemaphoreCreateInfo', then the
-- created semaphore will have a default
-- 'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
-- 'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_BINARY'.
--
-- = See Also
--
-- 'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SemaphoreTypeCreateInfo = SemaphoreTypeCreateInfo
  { -- | @semaphoreType@ is a 'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType'
    -- value specifying the type of the semaphore.
    semaphoreType :: SemaphoreType
  , -- | @initialValue@ is the initial payload value if @semaphoreType@ is
    -- 'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE'.
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


-- | VkTimelineSemaphoreSubmitInfo - Structure specifying signal and wait
-- values for timeline semaphores
--
-- = Description
--
-- If the semaphore in 'Vulkan.Core10.Queue.SubmitInfo'::@pWaitSemaphores@
-- or 'Vulkan.Core10.Queue.SubmitInfo'::@pSignalSemaphores@ corresponding
-- to an entry in @pWaitSemaphoreValues@ or @pSignalSemaphoreValues@
-- respectively was not created with a
-- 'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
-- 'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE', the
-- implementation /must/ ignore the value in the @pWaitSemaphoreValues@ or
-- @pSignalSemaphoreValues@ entry.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkTimelineSemaphoreSubmitInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO'
--
-- -   #VUID-VkTimelineSemaphoreSubmitInfo-pWaitSemaphoreValues-parameter#
--     If @waitSemaphoreValueCount@ is not @0@, and @pWaitSemaphoreValues@
--     is not @NULL@, @pWaitSemaphoreValues@ /must/ be a valid pointer to
--     an array of @waitSemaphoreValueCount@ @uint64_t@ values
--
-- -   #VUID-VkTimelineSemaphoreSubmitInfo-pSignalSemaphoreValues-parameter#
--     If @signalSemaphoreValueCount@ is not @0@, and
--     @pSignalSemaphoreValues@ is not @NULL@, @pSignalSemaphoreValues@
--     /must/ be a valid pointer to an array of @signalSemaphoreValueCount@
--     @uint64_t@ values
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data TimelineSemaphoreSubmitInfo = TimelineSemaphoreSubmitInfo
  { -- | @waitSemaphoreValueCount@ is the number of semaphore wait values
    -- specified in @pWaitSemaphoreValues@.
    waitSemaphoreValueCount :: Word32
  , -- | @pWaitSemaphoreValues@ is an array of length @waitSemaphoreValueCount@
    -- containing values for the corresponding semaphores in
    -- 'Vulkan.Core10.Queue.SubmitInfo'::@pWaitSemaphores@ to wait for.
    waitSemaphoreValues :: Vector Word64
  , -- | @signalSemaphoreValueCount@ is the number of semaphore signal values
    -- specified in @pSignalSemaphoreValues@.
    signalSemaphoreValueCount :: Word32
  , -- | @pSignalSemaphoreValues@ is an array of length
    -- @signalSemaphoreValueCount@ containing values for the corresponding
    -- semaphores in 'Vulkan.Core10.Queue.SubmitInfo'::@pSignalSemaphores@ to
    -- set when signaled.
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


-- | VkSemaphoreWaitInfo - Structure containing information about the
-- semaphore wait condition
--
-- == Valid Usage
--
-- -   #VUID-VkSemaphoreWaitInfo-pSemaphores-03256# All of the elements of
--     @pSemaphores@ /must/ reference a semaphore that was created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSemaphoreWaitInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO'
--
-- -   #VUID-VkSemaphoreWaitInfo-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkSemaphoreWaitInfo-flags-parameter# @flags@ /must/ be a valid
--     combination of
--     'Vulkan.Core12.Enums.SemaphoreWaitFlagBits.SemaphoreWaitFlagBits'
--     values
--
-- -   #VUID-VkSemaphoreWaitInfo-pSemaphores-parameter# @pSemaphores@
--     /must/ be a valid pointer to an array of @semaphoreCount@ valid
--     'Vulkan.Core10.Handles.Semaphore' handles
--
-- -   #VUID-VkSemaphoreWaitInfo-pValues-parameter# @pValues@ /must/ be a
--     valid pointer to an array of @semaphoreCount@ @uint64_t@ values
--
-- -   #VUID-VkSemaphoreWaitInfo-semaphoreCount-arraylength#
--     @semaphoreCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Semaphore',
-- 'Vulkan.Core12.Enums.SemaphoreWaitFlagBits.SemaphoreWaitFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'waitSemaphores',
-- 'Vulkan.Extensions.VK_KHR_timeline_semaphore.waitSemaphoresKHR'
data SemaphoreWaitInfo = SemaphoreWaitInfo
  { -- | @flags@ is a bitmask of
    -- 'Vulkan.Core12.Enums.SemaphoreWaitFlagBits.SemaphoreWaitFlagBits'
    -- specifying additional parameters for the semaphore wait operation.
    flags :: SemaphoreWaitFlags
  , -- | @pSemaphores@ is a pointer to an array of @semaphoreCount@ semaphore
    -- handles to wait on.
    semaphores :: Vector Semaphore
  , -- | @pValues@ is a pointer to an array of @semaphoreCount@ timeline
    -- semaphore values.
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
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

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


-- | VkSemaphoreSignalInfo - Structure containing information about a
-- semaphore signal operation
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Semaphore',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'signalSemaphore',
-- 'Vulkan.Extensions.VK_KHR_timeline_semaphore.signalSemaphoreKHR'
data SemaphoreSignalInfo = SemaphoreSignalInfo
  { -- | @semaphore@ is the handle of the semaphore to signal.
    --
    -- #VUID-VkSemaphoreSignalInfo-semaphore-03257# @semaphore@ /must/ have
    -- been created with a 'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
    -- 'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE'
    --
    -- #VUID-VkSemaphoreSignalInfo-semaphore-parameter# @semaphore@ /must/ be a
    -- valid 'Vulkan.Core10.Handles.Semaphore' handle
    semaphore :: Semaphore
  , -- | @value@ is the value to signal.
    --
    -- #VUID-VkSemaphoreSignalInfo-value-03258# @value@ /must/ have a value
    -- greater than the current value of the semaphore
    --
    -- #VUID-VkSemaphoreSignalInfo-value-03259# @value@ /must/ be less than the
    -- value of any pending semaphore signal operations
    --
    -- #VUID-VkSemaphoreSignalInfo-value-03260# @value@ /must/ have a value
    -- which does not differ from the current value of the semaphore or the
    -- value of any outstanding semaphore wait or signal operation on
    -- @semaphore@ by more than
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxTimelineSemaphoreValueDifference maxTimelineSemaphoreValueDifference>
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

