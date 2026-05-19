{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_queue_perf_hint - device extension
--
-- = VK_QCOM_queue_perf_hint
--
-- [__Name String__]
--     @VK_QCOM_queue_perf_hint@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     303
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Matthew Netsch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_queue_perf_hint] @mnetsch%0A*Here describe the issue or question you have about the VK_QCOM_queue_perf_hint extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_QCOM_queue_perf_hint.adoc VK_QCOM_queue_perf_hint>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-02-26
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with @VK_KHR_internally_synchronized_queues@
--
-- [__Contributors__]
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc
--
-- == Description
--
-- This extension exposes power constraints to the application, allowing it
-- to provide hints for influencing the device’s clock frequency.
--
-- These hints are 'Vulkan.Core10.Handles.Queue' state and are persistent
-- across the life of the queue until the app updates or removes the
-- constraint. The kernel combines the constraints across the active queues
-- from all processes to determine the actual clock frequency levels.
--
-- == New Commands
--
-- -   'queueSetPerfHintQCOM'
--
-- == New Structures
--
-- -   'PerfHintInfoQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceQueuePerfHintFeaturesQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceQueuePerfHintPropertiesQCOM'
--
-- == New Enums
--
-- -   'PerfHintTypeQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_QUEUE_PERF_HINT_EXTENSION_NAME'
--
-- -   'QCOM_QUEUE_PERF_HINT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PERF_HINT_INFO_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_QUEUE_PERF_HINT_FEATURES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_QUEUE_PERF_HINT_PROPERTIES_QCOM'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2026-02-26 (Matthew Netsch)
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_QCOM_queue_perf_hint Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_queue_perf_hint  ( queueSetPerfHintQCOM
                                                  , PerfHintInfoQCOM(..)
                                                  , PhysicalDeviceQueuePerfHintFeaturesQCOM(..)
                                                  , PhysicalDeviceQueuePerfHintPropertiesQCOM(..)
                                                  , PerfHintTypeQCOM( PERF_HINT_TYPE_DEFAULT_QCOM
                                                                    , PERF_HINT_TYPE_FREQUENCY_MIN_QCOM
                                                                    , PERF_HINT_TYPE_FREQUENCY_MAX_QCOM
                                                                    , PERF_HINT_TYPE_FREQUENCY_SCALED_QCOM
                                                                    , ..
                                                                    )
                                                  , QCOM_QUEUE_PERF_HINT_SPEC_VERSION
                                                  , pattern QCOM_QUEUE_PERF_HINT_SPEC_VERSION
                                                  , QCOM_QUEUE_PERF_HINT_EXTENSION_NAME
                                                  , pattern QCOM_QUEUE_PERF_HINT_EXTENSION_NAME
                                                  ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
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
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Dynamic (DeviceCmds(pVkQueueSetPerfHintQCOM))
import Vulkan.Core10.Handles (Queue)
import Vulkan.Core10.Handles (Queue(..))
import Vulkan.Core10.Handles (Queue(Queue))
import Vulkan.Core10.Enums.QueueFlagBits (QueueFlags)
import Vulkan.Core10.Handles (Queue_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PERF_HINT_INFO_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_QUEUE_PERF_HINT_FEATURES_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_QUEUE_PERF_HINT_PROPERTIES_QCOM))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueSetPerfHintQCOM
  :: FunPtr (Ptr Queue_T -> Ptr PerfHintInfoQCOM -> IO Result) -> Ptr Queue_T -> Ptr PerfHintInfoQCOM -> IO Result

-- | vkQueueSetPerfHintQCOM - Set a performance hint on a queue
--
-- == Valid Usage
--
-- -   #VUID-vkQueueSetPerfHintQCOM-queuePerfHint-12387# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-queuePerfHint queuePerfHint>
--     feature /must/ be enabled
--
-- -   #VUID-vkQueueSetPerfHintQCOM-queue-12388# @queue@ /must/ support at
--     least one of the queue types specified in
--     'PhysicalDeviceQueuePerfHintPropertiesQCOM'::@supportedQueues@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkQueueSetPerfHintQCOM-queue-parameter# @queue@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Queue' handle
--
-- -   #VUID-vkQueueSetPerfHintQCOM-pPerfHintInfo-parameter#
--     @pPerfHintInfo@ /must/ be a valid pointer to a valid
--     'PerfHintInfoQCOM' structure
--
-- == Host Synchronization
--
-- -   Host access to @queue@ /must/ be externally synchronized if it was
--     not created with
--     'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DEVICE_QUEUE_CREATE_INTERNALLY_SYNCHRONIZED_BIT_KHR'
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | -                                                                                                                          | -                                                                                                                      | -                                                                                                                           | Any                                                                                                                   | -                                                                                                                                      |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_queue_perf_hint VK_QCOM_queue_perf_hint>,
-- 'PerfHintInfoQCOM', 'Vulkan.Core10.Handles.Queue'
queueSetPerfHintQCOM :: forall io
                      . (MonadIO io)
                     => -- | @queue@ is the queue to set the performance hint on.
                        Queue
                     -> -- | @pPerfHintInfo@ is a pointer to a 'PerfHintInfoQCOM' structure,
                        -- describing the performance hint to set.
                        PerfHintInfoQCOM
                     -> io ()
queueSetPerfHintQCOM queue perfHintInfo = liftIO . evalContT $ do
  let vkQueueSetPerfHintQCOMPtr = pVkQueueSetPerfHintQCOM (case queue of Queue{deviceCmds} -> deviceCmds)
  lift $ unless (vkQueueSetPerfHintQCOMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkQueueSetPerfHintQCOM is null" Nothing Nothing
  let vkQueueSetPerfHintQCOM' = mkVkQueueSetPerfHintQCOM vkQueueSetPerfHintQCOMPtr
  pPerfHintInfo <- ContT $ withCStruct (perfHintInfo)
  r <- lift $ traceAroundEvent "vkQueueSetPerfHintQCOM" (vkQueueSetPerfHintQCOM'
                                                           (queueHandle (queue))
                                                           pPerfHintInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkPerfHintInfoQCOM - Structure specifying the parameters used for
-- setting a performance hint on a queue
--
-- = Description
--
-- The device has two global frequency constraints, @fmin@ and @fmax@, that
-- can be influenced by the 'PERF_HINT_TYPE_FREQUENCY_MIN_QCOM',
-- 'PERF_HINT_TYPE_FREQUENCY_MAX_QCOM', and
-- 'PERF_HINT_TYPE_FREQUENCY_SCALED_QCOM' performance hints. These
-- constraints determine the range of clock frequencies that the platform
-- performance algorithms /may/ select from. If no constraints are set,
-- @fmin@ and @fmax@ are set to the minimum and maximum frequencies the
-- device can support, @Fmin@ and @Fmax@ respectively.
--
-- The @fmin@ constraint applied by 'PERF_HINT_TYPE_FREQUENCY_SCALED_QCOM'
-- is calculated with the following:
--
-- -   \(f_{min} = \lfloor\frac{scale}{100} \times
--     F_{max}\rfloor\)
--
-- The implementation rounds @fmin@ down to the next available lower
-- frequency the device supports, clamped to @Fmin@.
--
-- The global frequency constraints are determined by combining the
-- performance hints from all of the device’s active queues of all
-- processes on the host. Performance hints that influence device frequency
-- are ranked, where a higher ranked hint supersedes all lower ranked
-- hints. These are listed in order of highest rank to lowest:
--
-- -   'PERF_HINT_TYPE_FREQUENCY_MAX_QCOM'
--
-- -   'PERF_HINT_TYPE_FREQUENCY_SCALED_QCOM' and @scale@ equal to @100@
--
-- -   'PERF_HINT_TYPE_FREQUENCY_SCALED_QCOM' and @scale@ equal to @99@
--
-- -   'PERF_HINT_TYPE_FREQUENCY_SCALED_QCOM' and @scale@ equal to @98@
--
-- -   …​
--
-- -   'PERF_HINT_TYPE_FREQUENCY_SCALED_QCOM' and @scale@ equal to @0@
--
-- -   'PERF_HINT_TYPE_DEFAULT_QCOM'
--
-- -   'PERF_HINT_TYPE_FREQUENCY_MIN_QCOM'
--
-- This means that 'PERF_HINT_TYPE_FREQUENCY_MIN_QCOM' only takes effect if
-- all of the device’s other active queues running on the host also have
-- the 'PERF_HINT_TYPE_FREQUENCY_MIN_QCOM' hint applied.
--
-- If any queue is active that never had a performance hint applied and
-- therefore is in the 'PERF_HINT_TYPE_DEFAULT_QCOM' state, it will
-- supersede in setting the constraints over
-- 'PERF_HINT_TYPE_FREQUENCY_MIN_QCOM'. This is necessary to avoid
-- negatively impacting performance for normal queues while a low power
-- queue is active.
--
-- == Valid Usage
--
-- -   #VUID-VkPerfHintInfoQCOM-type-12389# If @type@ is not
--     'PERF_HINT_TYPE_FREQUENCY_SCALED_QCOM', @scale@ /must/ equal @0@
--
-- -   #VUID-VkPerfHintInfoQCOM-scale-12390# @scale@ /must/ be less than or
--     equal to @100@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPerfHintInfoQCOM-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PERF_HINT_INFO_QCOM'
--
-- -   #VUID-VkPerfHintInfoQCOM-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkPerfHintInfoQCOM-type-parameter# @type@ /must/ be a valid
--     'PerfHintTypeQCOM' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_queue_perf_hint VK_QCOM_queue_perf_hint>,
-- 'PerfHintTypeQCOM', 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'queueSetPerfHintQCOM'
data PerfHintInfoQCOM = PerfHintInfoQCOM
  { -- | @type@ is a 'PerfHintTypeQCOM' value indicating the type of performance
    -- hint to apply.
    type' :: PerfHintTypeQCOM
  , -- | @scale@ is a normalized fixed-point scale factor.
    scale :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PerfHintInfoQCOM)
#endif
deriving instance Show PerfHintInfoQCOM

instance ToCStruct PerfHintInfoQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PerfHintInfoQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERF_HINT_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PerfHintTypeQCOM)) (type')
    poke ((p `plusPtr` 20 :: Ptr Word32)) (scale)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERF_HINT_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PerfHintTypeQCOM)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct PerfHintInfoQCOM where
  peekCStruct p = do
    type' <- peek @PerfHintTypeQCOM ((p `plusPtr` 16 :: Ptr PerfHintTypeQCOM))
    scale <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ PerfHintInfoQCOM
             type' scale

instance Storable PerfHintInfoQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PerfHintInfoQCOM where
  zero = PerfHintInfoQCOM
           zero
           zero


-- | VkPhysicalDeviceQueuePerfHintFeaturesQCOM - Structure describing queue
-- perf hint features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceQueuePerfHintFeaturesQCOM' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceQueuePerfHintFeaturesQCOM', it /must/ add an instance of
-- the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structures>]
--
--     -   'Vulkan.Core10.Device.DeviceCreateInfo'
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_queue_perf_hint VK_QCOM_queue_perf_hint>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceQueuePerfHintFeaturesQCOM = PhysicalDeviceQueuePerfHintFeaturesQCOM
  { -- | #features-queuePerfHint# @queuePerfHint@ specifies whether the
    -- implementation supports setting
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#devsandqueues-perfhint queue perf hints>.
    queuePerfHint :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceQueuePerfHintFeaturesQCOM)
#endif
deriving instance Show PhysicalDeviceQueuePerfHintFeaturesQCOM

instance ToCStruct PhysicalDeviceQueuePerfHintFeaturesQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceQueuePerfHintFeaturesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_QUEUE_PERF_HINT_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (queuePerfHint))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_QUEUE_PERF_HINT_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceQueuePerfHintFeaturesQCOM where
  peekCStruct p = do
    queuePerfHint <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceQueuePerfHintFeaturesQCOM
             (bool32ToBool queuePerfHint)

instance Storable PhysicalDeviceQueuePerfHintFeaturesQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceQueuePerfHintFeaturesQCOM where
  zero = PhysicalDeviceQueuePerfHintFeaturesQCOM
           zero


-- | VkPhysicalDeviceQueuePerfHintPropertiesQCOM - Structure describing queue
-- perf hint properties for a physical device
--
-- = Members
--
-- The members of the 'PhysicalDeviceQueuePerfHintPropertiesQCOM' structure
-- describe the following:
--
-- = Description
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-queuePerfHint queuePerfHint>
-- feature is supported by @physicalDevice@, @supportedQueues@ /must/
-- return at least one supported queue type.
--
-- If the 'PhysicalDeviceQueuePerfHintPropertiesQCOM' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structure>]
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_queue_perf_hint VK_QCOM_queue_perf_hint>,
-- 'Vulkan.Core10.Enums.QueueFlagBits.QueueFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceQueuePerfHintPropertiesQCOM = PhysicalDeviceQueuePerfHintPropertiesQCOM
  { -- | @supportedQueues@ is a bitmask of
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QueueFlagBits' indicating the types
    -- of queues on which
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#devsandqueues-perfhint setting perf hints>
    -- are supported.
    supportedQueues :: QueueFlags }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceQueuePerfHintPropertiesQCOM)
#endif
deriving instance Show PhysicalDeviceQueuePerfHintPropertiesQCOM

instance ToCStruct PhysicalDeviceQueuePerfHintPropertiesQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceQueuePerfHintPropertiesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_QUEUE_PERF_HINT_PROPERTIES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr QueueFlags)) (supportedQueues)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_QUEUE_PERF_HINT_PROPERTIES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr QueueFlags)) (zero)
    f

instance FromCStruct PhysicalDeviceQueuePerfHintPropertiesQCOM where
  peekCStruct p = do
    supportedQueues <- peek @QueueFlags ((p `plusPtr` 16 :: Ptr QueueFlags))
    pure $ PhysicalDeviceQueuePerfHintPropertiesQCOM
             supportedQueues

instance Storable PhysicalDeviceQueuePerfHintPropertiesQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceQueuePerfHintPropertiesQCOM where
  zero = PhysicalDeviceQueuePerfHintPropertiesQCOM
           zero


-- | VkPerfHintTypeQCOM - Specifies the performance hint type
--
-- = Description
--
-- -   'PERF_HINT_TYPE_DEFAULT_QCOM' resets the performance hint state back
--     to default for the queue.
--
-- -   'PERF_HINT_TYPE_FREQUENCY_MIN_QCOM' specifies the queue /should/
--     prioritize power and sets the frequency constraints @fmin@ and
--     @fmax@ to @Fmin@.
--
-- -   'PERF_HINT_TYPE_FREQUENCY_MAX_QCOM' specifies the queue /should/
--     prioritize performance and sets the frequency constraints @fmin@ and
--     @fmax@ to @Fmax@.
--
-- -   'PERF_HINT_TYPE_FREQUENCY_SCALED_QCOM' specifies the queue /should/
--     be balanced between performance and power and sets the frequency
--     constraint @fmin@ by applying a scale factor to @Fmax@. No
--     constraint is applied to @fmax@.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_queue_perf_hint VK_QCOM_queue_perf_hint>,
-- 'PerfHintInfoQCOM'
newtype PerfHintTypeQCOM = PerfHintTypeQCOM Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkPerfHintTypeQCOM" "VK_PERF_HINT_TYPE_DEFAULT_QCOM"
pattern PERF_HINT_TYPE_DEFAULT_QCOM = PerfHintTypeQCOM 0

-- No documentation found for Nested "VkPerfHintTypeQCOM" "VK_PERF_HINT_TYPE_FREQUENCY_MIN_QCOM"
pattern PERF_HINT_TYPE_FREQUENCY_MIN_QCOM = PerfHintTypeQCOM 1

-- No documentation found for Nested "VkPerfHintTypeQCOM" "VK_PERF_HINT_TYPE_FREQUENCY_MAX_QCOM"
pattern PERF_HINT_TYPE_FREQUENCY_MAX_QCOM = PerfHintTypeQCOM 2

-- No documentation found for Nested "VkPerfHintTypeQCOM" "VK_PERF_HINT_TYPE_FREQUENCY_SCALED_QCOM"
pattern PERF_HINT_TYPE_FREQUENCY_SCALED_QCOM = PerfHintTypeQCOM 3

{-# COMPLETE
  PERF_HINT_TYPE_DEFAULT_QCOM
  , PERF_HINT_TYPE_FREQUENCY_MIN_QCOM
  , PERF_HINT_TYPE_FREQUENCY_MAX_QCOM
  , PERF_HINT_TYPE_FREQUENCY_SCALED_QCOM ::
    PerfHintTypeQCOM
  #-}

conNamePerfHintTypeQCOM :: String
conNamePerfHintTypeQCOM = "PerfHintTypeQCOM"

enumPrefixPerfHintTypeQCOM :: String
enumPrefixPerfHintTypeQCOM = "PERF_HINT_TYPE_"

showTablePerfHintTypeQCOM :: [(PerfHintTypeQCOM, String)]
showTablePerfHintTypeQCOM =
  [ (PERF_HINT_TYPE_DEFAULT_QCOM, "DEFAULT_QCOM")
  ,
    ( PERF_HINT_TYPE_FREQUENCY_MIN_QCOM
    , "FREQUENCY_MIN_QCOM"
    )
  ,
    ( PERF_HINT_TYPE_FREQUENCY_MAX_QCOM
    , "FREQUENCY_MAX_QCOM"
    )
  ,
    ( PERF_HINT_TYPE_FREQUENCY_SCALED_QCOM
    , "FREQUENCY_SCALED_QCOM"
    )
  ]

instance Show PerfHintTypeQCOM where
  showsPrec =
    enumShowsPrec
      enumPrefixPerfHintTypeQCOM
      showTablePerfHintTypeQCOM
      conNamePerfHintTypeQCOM
      (\(PerfHintTypeQCOM x) -> x)
      (showsPrec 11)

instance Read PerfHintTypeQCOM where
  readPrec =
    enumReadPrec
      enumPrefixPerfHintTypeQCOM
      showTablePerfHintTypeQCOM
      conNamePerfHintTypeQCOM
      PerfHintTypeQCOM

type QCOM_QUEUE_PERF_HINT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_QCOM_QUEUE_PERF_HINT_SPEC_VERSION"
pattern QCOM_QUEUE_PERF_HINT_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_QUEUE_PERF_HINT_SPEC_VERSION = 1


type QCOM_QUEUE_PERF_HINT_EXTENSION_NAME = "VK_QCOM_queue_perf_hint"

-- No documentation found for TopLevel "VK_QCOM_QUEUE_PERF_HINT_EXTENSION_NAME"
pattern QCOM_QUEUE_PERF_HINT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_QUEUE_PERF_HINT_EXTENSION_NAME = "VK_QCOM_queue_perf_hint"

