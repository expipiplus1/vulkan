{-# language CPP #-}
-- | = Name
--
-- VK_NV_compute_occupancy_priority - device extension
--
-- = VK_NV_compute_occupancy_priority
--
-- [__Name String__]
--     @VK_NV_compute_occupancy_priority@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     646
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
--     -   Chris Lentini
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_compute_occupancy_priority] @clentini%0A*Here describe the issue or question you have about the VK_NV_compute_occupancy_priority extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_NV_compute_occupancy_priority.adoc VK_NV_compute_occupancy_priority>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-12-01
--
-- [__Contributors__]
--
--     -   Chris Lentini, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   Lionel Duc, NVIDIA
--
--     -   Peter Deayton, NVIDIA
--
-- == Description
--
-- This extension provides applications with control over how their compute
-- workloads utilize GPU compute resources, specifically allowing
-- prioritization relative to other simultaneously executing workloads.
-- Applications can specify the priority with which compute workloads
-- should occupy GPU compute resources, allowing for a fine-grained
-- distinction between workloads that may want to execute at a background
-- priority over a long period of time versus workloads with harder latency
-- requirements.
--
-- The extension introduces a new command
-- 'cmdSetComputeOccupancyPriorityNV' that allows applications to set the
-- occupancy priority for subsequent compute dispatches. The occupancy
-- priority affects how compute workloads utilize GPU compute resources
-- relative to other simultaneously executing workloads.
--
-- The occupancy priority is stateful on a command buffer. All commands
-- listed in the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#dispatch Dispatching Commands>
-- chapter issued subsequent to a 'cmdSetComputeOccupancyPriorityNV' call
-- will be executed with the specified priority parameters until another
-- 'cmdSetComputeOccupancyPriorityNV' call is made.
--
-- For convenience, three named occupancy priority values are defined:
--
-- -   __VK_COMPUTE_OCCUPANCY_PRIORITY_LOW_NV__ - a constant value that can
--     be used for
--     'ComputeOccupancyPriorityParametersNV'::@occupancyPriority@ to
--     specify a low priority level.
--
-- -   __VK_COMPUTE_OCCUPANCY_PRIORITY_NORMAL_NV__ - a constant value that
--     can be used for
--     'ComputeOccupancyPriorityParametersNV'::@occupancyPriority@ to
--     specify a normal priority level. This represents the default
--     priority level.
--
-- -   __VK_COMPUTE_OCCUPANCY_PRIORITY_HIGH_NV__ - a constant value that
--     can be used for
--     'ComputeOccupancyPriorityParametersNV'::@occupancyPriority@ to
--     specify a high priority level.
--
-- All command buffers (primary and secondary) start with a priority level
-- equal to the VK_COMPUTE_OCCUPANCY_PRIORITY_NORMAL_NV value. The priority
-- state is not inherited by secondary command buffers - each command
-- buffer maintains its own independent priority state.
--
-- == New Commands
--
-- -   'cmdSetComputeOccupancyPriorityNV'
--
-- == New Structures
--
-- -   'ComputeOccupancyPriorityParametersNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceComputeOccupancyPriorityFeaturesNV'
--
-- == New Enum Constants
--
-- -   'Vulkan.Core10.APIConstants.COMPUTE_OCCUPANCY_PRIORITY_HIGH_NV'
--
-- -   'Vulkan.Core10.APIConstants.COMPUTE_OCCUPANCY_PRIORITY_LOW_NV'
--
-- -   'Vulkan.Core10.APIConstants.COMPUTE_OCCUPANCY_PRIORITY_NORMAL_NV'
--
-- -   'NV_COMPUTE_OCCUPANCY_PRIORITY_EXTENSION_NAME'
--
-- -   'NV_COMPUTE_OCCUPANCY_PRIORITY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMPUTE_OCCUPANCY_PRIORITY_PARAMETERS_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_OCCUPANCY_PRIORITY_FEATURES_NV'
--
-- The extension only allows specification of occupancy priority for
-- compute workloads, however, the priorities will also impact the
-- prioritization of compute workloads relative to simultaneously executing
-- graphics workloads. In such a scenario, the graphics workload may be
-- thought of as executing at VK_COMPUTE_OCCUPANCY_PRIORITY_NORMAL_NV
-- priority, and so a simultaneously executing compute workload with
-- VK_COMPUTE_OCCUPANCY_PRIORITY_HIGH_NV occupancy priority will
-- preferentially utilize available compute resources.
--
-- Workloads specified with a higher priority may begin execution after
-- workloads specified with a lower priority, at which point they may find
-- GPU compute resources already occupied. So, while they will from that
-- point forward preferentially occupy available compute resources, they
-- may not ramp up to full occupancy until the already present lower
-- priority work has reached a point where it can relinquish compute
-- resources.
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2025-08-06 (Chris Lentini)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_compute_occupancy_priority Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_compute_occupancy_priority  ( cmdSetComputeOccupancyPriorityNV
                                                           , ComputeOccupancyPriorityParametersNV(..)
                                                           , PhysicalDeviceComputeOccupancyPriorityFeaturesNV(..)
                                                           , NV_COMPUTE_OCCUPANCY_PRIORITY_SPEC_VERSION
                                                           , pattern NV_COMPUTE_OCCUPANCY_PRIORITY_SPEC_VERSION
                                                           , NV_COMPUTE_OCCUPANCY_PRIORITY_EXTENSION_NAME
                                                           , pattern NV_COMPUTE_OCCUPANCY_PRIORITY_EXTENSION_NAME
                                                           , pattern COMPUTE_OCCUPANCY_PRIORITY_LOW_NV
                                                           , pattern COMPUTE_OCCUPANCY_PRIORITY_NORMAL_NV
                                                           , pattern COMPUTE_OCCUPANCY_PRIORITY_HIGH_NV
                                                           ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetComputeOccupancyPriorityNV))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMPUTE_OCCUPANCY_PRIORITY_PARAMETERS_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_OCCUPANCY_PRIORITY_FEATURES_NV))
import Vulkan.Core10.APIConstants (pattern COMPUTE_OCCUPANCY_PRIORITY_HIGH_NV)
import Vulkan.Core10.APIConstants (pattern COMPUTE_OCCUPANCY_PRIORITY_LOW_NV)
import Vulkan.Core10.APIConstants (pattern COMPUTE_OCCUPANCY_PRIORITY_NORMAL_NV)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetComputeOccupancyPriorityNV
  :: FunPtr (Ptr CommandBuffer_T -> Ptr ComputeOccupancyPriorityParametersNV -> IO ()) -> Ptr CommandBuffer_T -> Ptr ComputeOccupancyPriorityParametersNV -> IO ()

-- | vkCmdSetComputeOccupancyPriorityNV - Set the compute occupancy priority
-- for subsequent compute dispatches
--
-- = Description
--
-- The occupancy priority affects how compute workloads utilize GPU compute
-- resources relative to other simultaneously executing workloads. The
-- priority is stateful on a command buffer. All compute dispatch commands
-- issued subsequent to a 'cmdSetComputeOccupancyPriorityNV' call will be
-- executed with the specified priority parameters until another
-- 'cmdSetComputeOccupancyPriorityNV' call is made.
--
-- All command buffers (primary and secondary) start with a priority level
-- equal to the
-- 'Vulkan.Core10.APIConstants.COMPUTE_OCCUPANCY_PRIORITY_NORMAL_NV' value.
-- The priority state is not inherited by secondary command buffers - each
-- command buffer maintains its own independent priority state.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetComputeOccupancyPriorityNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetComputeOccupancyPriorityNV-pParameters-parameter#
--     @pParameters@ /must/ be a valid pointer to a valid
--     'ComputeOccupancyPriorityParametersNV' structure
--
-- -   #VUID-vkCmdSetComputeOccupancyPriorityNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetComputeOccupancyPriorityNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT' operations
--
-- -   #VUID-vkCmdSetComputeOccupancyPriorityNV-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdSetComputeOccupancyPriorityNV is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_compute_occupancy_priority VK_NV_compute_occupancy_priority>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'ComputeOccupancyPriorityParametersNV'
cmdSetComputeOccupancyPriorityNV :: forall io
                                  . (MonadIO io)
                                 => -- | @commandBuffer@ is the command buffer into which the command will be
                                    -- recorded.
                                    CommandBuffer
                                 -> -- | @pParameters@ is a pointer to a 'ComputeOccupancyPriorityParametersNV'
                                    -- structure specifying the occupancy priority parameters.
                                    ComputeOccupancyPriorityParametersNV
                                 -> io ()
cmdSetComputeOccupancyPriorityNV commandBuffer
                                   parameters = liftIO . evalContT $ do
  let vkCmdSetComputeOccupancyPriorityNVPtr = pVkCmdSetComputeOccupancyPriorityNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetComputeOccupancyPriorityNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetComputeOccupancyPriorityNV is null" Nothing Nothing
  let vkCmdSetComputeOccupancyPriorityNV' = mkVkCmdSetComputeOccupancyPriorityNV vkCmdSetComputeOccupancyPriorityNVPtr
  pParameters <- ContT $ withCStruct (parameters)
  lift $ traceAroundEvent "vkCmdSetComputeOccupancyPriorityNV" (vkCmdSetComputeOccupancyPriorityNV'
                                                                  (commandBufferHandle (commandBuffer))
                                                                  pParameters)
  pure $ ()


-- | VkComputeOccupancyPriorityParametersNV - Structure specifying compute
-- occupancy priority parameters
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_compute_occupancy_priority VK_NV_compute_occupancy_priority>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdSetComputeOccupancyPriorityNV'
data ComputeOccupancyPriorityParametersNV = ComputeOccupancyPriorityParametersNV
  { -- | @occupancyPriority@ is a value specifying the occupancy priority for
    -- subsequent compute workloads, with a valid range of [0.0, 1.0]. A value
    -- of 0.0 represents the lowest priority, while a value of 1.0 is the
    -- maximum priority. Default priority is specified by a value of 0.5.
    --
    -- #VUID-VkComputeOccupancyPriorityParametersNV-occupancyPriority-12298#
    -- @occupancyPriority@ /must/ be between @0@ and @1@, inclusive
    occupancyPriority :: Float
  , -- | @occupancyThrottling@ is a value specifying the level of occupancy
    -- throttling applied to subsequent workloads, with a valid range of [0.0,
    -- 1.0]. A value of 0.0 (the default) means no throttling is applied,
    -- allowing workloads to use the full available compute capacity. Non-zero
    -- values represent increasing levels of throttling, with higher values
    -- resulting in more restrictive occupancy limits. A value of 1.0
    -- represents the maximum level of throttling supported by the
    -- implementation.
    --
    -- #VUID-VkComputeOccupancyPriorityParametersNV-occupancyThrottling-12299#
    -- @occupancyThrottling@ /must/ be between @0@ and @1@, inclusive
    occupancyThrottling :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ComputeOccupancyPriorityParametersNV)
#endif
deriving instance Show ComputeOccupancyPriorityParametersNV

instance ToCStruct ComputeOccupancyPriorityParametersNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ComputeOccupancyPriorityParametersNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMPUTE_OCCUPANCY_PRIORITY_PARAMETERS_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (occupancyPriority))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (occupancyThrottling))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMPUTE_OCCUPANCY_PRIORITY_PARAMETERS_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct ComputeOccupancyPriorityParametersNV where
  peekCStruct p = do
    occupancyPriority <- peek @CFloat ((p `plusPtr` 16 :: Ptr CFloat))
    occupancyThrottling <- peek @CFloat ((p `plusPtr` 20 :: Ptr CFloat))
    pure $ ComputeOccupancyPriorityParametersNV
             (coerce @CFloat @Float occupancyPriority)
             (coerce @CFloat @Float occupancyThrottling)

instance Storable ComputeOccupancyPriorityParametersNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ComputeOccupancyPriorityParametersNV where
  zero = ComputeOccupancyPriorityParametersNV
           zero
           zero


-- | VkPhysicalDeviceComputeOccupancyPriorityFeaturesNV - Structure
-- describing whether compute occupancy priority is supported by the
-- implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceComputeOccupancyPriorityFeaturesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceComputeOccupancyPriorityFeaturesNV', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_compute_occupancy_priority VK_NV_compute_occupancy_priority>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceComputeOccupancyPriorityFeaturesNV = PhysicalDeviceComputeOccupancyPriorityFeaturesNV
  { -- | #features-computeOccupancyPriority# @computeOccupancyPriority@ is true
    -- if compute occupancy priority is supported.
    computeOccupancyPriority :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceComputeOccupancyPriorityFeaturesNV)
#endif
deriving instance Show PhysicalDeviceComputeOccupancyPriorityFeaturesNV

instance ToCStruct PhysicalDeviceComputeOccupancyPriorityFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceComputeOccupancyPriorityFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_OCCUPANCY_PRIORITY_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (computeOccupancyPriority))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_OCCUPANCY_PRIORITY_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceComputeOccupancyPriorityFeaturesNV where
  peekCStruct p = do
    computeOccupancyPriority <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceComputeOccupancyPriorityFeaturesNV
             (bool32ToBool computeOccupancyPriority)

instance Storable PhysicalDeviceComputeOccupancyPriorityFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceComputeOccupancyPriorityFeaturesNV where
  zero = PhysicalDeviceComputeOccupancyPriorityFeaturesNV
           zero


type NV_COMPUTE_OCCUPANCY_PRIORITY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_COMPUTE_OCCUPANCY_PRIORITY_SPEC_VERSION"
pattern NV_COMPUTE_OCCUPANCY_PRIORITY_SPEC_VERSION :: forall a . Integral a => a
pattern NV_COMPUTE_OCCUPANCY_PRIORITY_SPEC_VERSION = 1


type NV_COMPUTE_OCCUPANCY_PRIORITY_EXTENSION_NAME = "VK_NV_compute_occupancy_priority"

-- No documentation found for TopLevel "VK_NV_COMPUTE_OCCUPANCY_PRIORITY_EXTENSION_NAME"
pattern NV_COMPUTE_OCCUPANCY_PRIORITY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_COMPUTE_OCCUPANCY_PRIORITY_EXTENSION_NAME = "VK_NV_compute_occupancy_priority"

