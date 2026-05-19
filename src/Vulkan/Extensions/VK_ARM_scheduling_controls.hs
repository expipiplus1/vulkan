{-# language CPP #-}
-- | = Name
--
-- VK_ARM_scheduling_controls - device extension
--
-- = VK_ARM_scheduling_controls
--
-- [__Name String__]
--     @VK_ARM_scheduling_controls@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     418
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_shader_core_builtins VK_ARM_shader_core_builtins>
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_scheduling_controls] @kpet%0A*Here describe the issue or question you have about the VK_ARM_scheduling_controls extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-09-05
--
-- [__Interactions and External Dependencies__]
--     None
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Kévin Petit, Arm Ltd.
--
--     -   Jan-Harald Fredriksen, Arm Ltd.
--
--     -   Mikel Garai, Arm Ltd.
--
-- == Description
--
-- This extension exposes a collection of controls to modify the scheduling
-- behavior of Arm Mali devices.
--
-- == New Commands
--
-- -   'cmdSetDispatchParametersARM'
--
-- == New Structures
--
-- -   'DispatchParametersARM'
--
-- -   Extending 'Vulkan.Core10.Device.DeviceQueueCreateInfo',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'DeviceQueueShaderCoreControlCreateInfoARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceSchedulingControlsFeaturesARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM'
--
--     -   'PhysicalDeviceSchedulingControlsPropertiesARM'
--
-- == New Enums
--
-- -   'PhysicalDeviceSchedulingControlsFlagBitsARM'
--
-- == New Bitmasks
--
-- -   'PhysicalDeviceSchedulingControlsFlagsARM'
--
-- == New Enum Constants
--
-- -   'ARM_SCHEDULING_CONTROLS_EXTENSION_NAME'
--
-- -   'ARM_SCHEDULING_CONTROLS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_QUEUE_SHADER_CORE_CONTROL_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPATCH_PARAMETERS_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_DISPATCH_PARAMETERS_PROPERTIES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_FEATURES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_PROPERTIES_ARM'
--
-- == New SPIR-V Capabilities
--
-- None.
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 2, 2025-09-05 (Kévin Petit)
--
--     -   Add dispatch parameters controls
--
--         -   'PHYSICAL_DEVICE_SCHEDULING_CONTROLS_DISPATCH_PARAMETERS_ARM'
--
--         -   'cmdSetDispatchParametersARM'
--
--         -   'DispatchParametersARM'
--
--         -   'PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM'
--
-- -   Revision 1, 2023-08-23 (Kévin Petit)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_ARM_scheduling_controls Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_scheduling_controls  ( cmdSetDispatchParametersARM
                                                     , DeviceQueueShaderCoreControlCreateInfoARM(..)
                                                     , PhysicalDeviceSchedulingControlsFeaturesARM(..)
                                                     , PhysicalDeviceSchedulingControlsPropertiesARM(..)
                                                     , PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM(..)
                                                     , DispatchParametersARM(..)
                                                     , PhysicalDeviceSchedulingControlsFlagsARM
                                                     , PhysicalDeviceSchedulingControlsFlagBitsARM( PHYSICAL_DEVICE_SCHEDULING_CONTROLS_SHADER_CORE_COUNT_ARM
                                                                                                  , PHYSICAL_DEVICE_SCHEDULING_CONTROLS_DISPATCH_PARAMETERS_ARM
                                                                                                  , ..
                                                                                                  )
                                                     , ARM_SCHEDULING_CONTROLS_SPEC_VERSION
                                                     , pattern ARM_SCHEDULING_CONTROLS_SPEC_VERSION
                                                     , ARM_SCHEDULING_CONTROLS_EXTENSION_NAME
                                                     , pattern ARM_SCHEDULING_CONTROLS_EXTENSION_NAME
                                                     ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
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
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDispatchParametersARM))
import Vulkan.Core10.FundamentalTypes (Flags64)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_QUEUE_SHADER_CORE_CONTROL_CREATE_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPATCH_PARAMETERS_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_DISPATCH_PARAMETERS_PROPERTIES_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_FEATURES_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_PROPERTIES_ARM))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDispatchParametersARM
  :: FunPtr (Ptr CommandBuffer_T -> Ptr DispatchParametersARM -> IO ()) -> Ptr CommandBuffer_T -> Ptr DispatchParametersARM -> IO ()

-- | vkCmdSetDispatchParametersARM - Set parameters that affect dispatch
-- commands
--
-- = Description
--
-- Parameters set using 'cmdSetDispatchParametersARM' affect the following
-- dispatch commands:
--
-- -   'Vulkan.Core10.CommandBufferBuilding.cmdDispatch'
--
-- -   'Vulkan.Core11.Promoted_From_VK_KHR_device_group.cmdDispatchBase'
--
-- -   'Vulkan.Core10.CommandBufferBuilding.cmdDispatchIndirect'
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetDispatchParametersARM-schedulingControlsFlags-12391#
--     'PhysicalDeviceSchedulingControlsPropertiesARM'::@schedulingControlsFlags@
--     /must/ contain
--     'PHYSICAL_DEVICE_SCHEDULING_CONTROLS_DISPATCH_PARAMETERS_ARM'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDispatchParametersARM-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDispatchParametersARM-pDispatchParameters-parameter#
--     @pDispatchParameters@ /must/ be a valid pointer to a valid
--     'DispatchParametersARM' structure
--
-- -   #VUID-vkCmdSetDispatchParametersARM-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDispatchParametersARM-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT' operations
--
-- -   #VUID-vkCmdSetDispatchParametersARM-renderpass# This command /must/
--     only be called outside of a render pass instance
--
-- -   #VUID-vkCmdSetDispatchParametersARM-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdSetDispatchParametersARM is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_scheduling_controls VK_ARM_scheduling_controls>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'DispatchParametersARM'
cmdSetDispatchParametersARM :: forall io
                             . (MonadIO io)
                            => -- | @commandBuffer@ is the command buffer into which the command will be
                               -- recorded.
                               CommandBuffer
                            -> -- | @pDispatchParameters@ is a pointer to a 'DispatchParametersARM'
                               -- structure specifying the dispatch parameters to be set.
                               DispatchParametersARM
                            -> io ()
cmdSetDispatchParametersARM commandBuffer
                              dispatchParameters = liftIO . evalContT $ do
  let vkCmdSetDispatchParametersARMPtr = pVkCmdSetDispatchParametersARM (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetDispatchParametersARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDispatchParametersARM is null" Nothing Nothing
  let vkCmdSetDispatchParametersARM' = mkVkCmdSetDispatchParametersARM vkCmdSetDispatchParametersARMPtr
  pDispatchParameters <- ContT $ withCStruct (dispatchParameters)
  lift $ traceAroundEvent "vkCmdSetDispatchParametersARM" (vkCmdSetDispatchParametersARM'
                                                             (commandBufferHandle (commandBuffer))
                                                             pDispatchParameters)
  pure $ ()


-- | VkDeviceQueueShaderCoreControlCreateInfoARM - Control the number of
-- shader cores used by queues
--
-- = Description
--
-- Queues created without specifying
-- 'DeviceQueueShaderCoreControlCreateInfoARM' will default to using all
-- the shader cores available.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structures>]
--
--     -   'Vulkan.Core10.Device.DeviceCreateInfo'
--
--     -   'Vulkan.Core10.Device.DeviceQueueCreateInfo'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_scheduling_controls VK_ARM_scheduling_controls>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceQueueShaderCoreControlCreateInfoARM = DeviceQueueShaderCoreControlCreateInfoARM
  { -- | @shaderCoreCount@ is the number of shader cores this queue uses.
    --
    -- #VUID-VkDeviceQueueShaderCoreControlCreateInfoARM-shaderCoreCount-09399#
    -- @shaderCoreCount@ /must/ be greater than 0 and less than or equal to the
    -- total number of shader cores as reported via
    -- 'Vulkan.Extensions.VK_ARM_shader_core_builtins.PhysicalDeviceShaderCoreBuiltinsPropertiesARM'::@shaderCoreCount@
    shaderCoreCount :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceQueueShaderCoreControlCreateInfoARM)
#endif
deriving instance Show DeviceQueueShaderCoreControlCreateInfoARM

instance ToCStruct DeviceQueueShaderCoreControlCreateInfoARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceQueueShaderCoreControlCreateInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_QUEUE_SHADER_CORE_CONTROL_CREATE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (shaderCoreCount)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_QUEUE_SHADER_CORE_CONTROL_CREATE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct DeviceQueueShaderCoreControlCreateInfoARM where
  peekCStruct p = do
    shaderCoreCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ DeviceQueueShaderCoreControlCreateInfoARM
             shaderCoreCount

instance Storable DeviceQueueShaderCoreControlCreateInfoARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceQueueShaderCoreControlCreateInfoARM where
  zero = DeviceQueueShaderCoreControlCreateInfoARM
           zero


-- | VkPhysicalDeviceSchedulingControlsFeaturesARM - Structure describing
-- scheduling controls features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceSchedulingControlsFeaturesARM' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceSchedulingControlsFeaturesARM', it /must/ add an instance
-- of the structure, with the desired feature members set to
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_scheduling_controls VK_ARM_scheduling_controls>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSchedulingControlsFeaturesARM = PhysicalDeviceSchedulingControlsFeaturesARM
  { -- | #features-schedulingControls# @schedulingControls@ indicates that the
    -- implementation supports scheduling controls.
    schedulingControls :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSchedulingControlsFeaturesARM)
#endif
deriving instance Show PhysicalDeviceSchedulingControlsFeaturesARM

instance ToCStruct PhysicalDeviceSchedulingControlsFeaturesARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSchedulingControlsFeaturesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (schedulingControls))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceSchedulingControlsFeaturesARM where
  peekCStruct p = do
    schedulingControls <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceSchedulingControlsFeaturesARM
             (bool32ToBool schedulingControls)

instance Storable PhysicalDeviceSchedulingControlsFeaturesARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSchedulingControlsFeaturesARM where
  zero = PhysicalDeviceSchedulingControlsFeaturesARM
           zero


-- | VkPhysicalDeviceSchedulingControlsPropertiesARM - Structure containing
-- scheduling control properties of a physical device
--
-- = Members
--
-- -   #limits-schedulingControlsFlags#@schedulingControlsFlags@ specifies
--     the specific scheduling controls that a physical device supports.
--
-- = Description
--
-- If the 'PhysicalDeviceSchedulingControlsPropertiesARM' structure is
-- included in the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_scheduling_controls VK_ARM_scheduling_controls>,
-- 'PhysicalDeviceSchedulingControlsFlagsARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSchedulingControlsPropertiesARM = PhysicalDeviceSchedulingControlsPropertiesARM
  { -- No documentation found for Nested "VkPhysicalDeviceSchedulingControlsPropertiesARM" "schedulingControlsFlags"
    schedulingControlsFlags :: PhysicalDeviceSchedulingControlsFlagsARM }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSchedulingControlsPropertiesARM)
#endif
deriving instance Show PhysicalDeviceSchedulingControlsPropertiesARM

instance ToCStruct PhysicalDeviceSchedulingControlsPropertiesARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSchedulingControlsPropertiesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PhysicalDeviceSchedulingControlsFlagsARM)) (schedulingControlsFlags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PhysicalDeviceSchedulingControlsFlagsARM)) (zero)
    f

instance FromCStruct PhysicalDeviceSchedulingControlsPropertiesARM where
  peekCStruct p = do
    schedulingControlsFlags <- peek @PhysicalDeviceSchedulingControlsFlagsARM ((p `plusPtr` 16 :: Ptr PhysicalDeviceSchedulingControlsFlagsARM))
    pure $ PhysicalDeviceSchedulingControlsPropertiesARM
             schedulingControlsFlags

instance Storable PhysicalDeviceSchedulingControlsPropertiesARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSchedulingControlsPropertiesARM where
  zero = PhysicalDeviceSchedulingControlsPropertiesARM
           zero


-- | VkPhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM -
-- Structure containing scheduling control dispatch parameters properties
-- of a physical device
--
-- = Members
--
-- -   #limits-schedulingControlsMaxWarpsCount#@schedulingControlsMaxWarpsCount@
--     specifies the maximum number of warps that a shader core /can/ run
--     concurrently.
--
-- -   #limits-schedulingControlsMaxQueuedWorkgroupBatchesCount#@schedulingControlsMaxQueuedWorkgroupBatchesCount@
--     specifies the maximum number of workgroup batches that a shader core
--     /can/ queue.
--
-- -   #limits-schedulingControlsMaxWorkGroupBatchSize#@schedulingControlsMaxWorkGroupBatchSize@
--     specifies the maximum size of workgroup batches that /can/ be
--     requested using 'DispatchParametersARM'::@workGroupBatchSize@.
--
-- = Description
--
-- If the 'PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM'
-- structure is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- If
-- 'PhysicalDeviceSchedulingControlsPropertiesARM'::@schedulingControlsFlags@
-- does not contain
-- 'PHYSICAL_DEVICE_SCHEDULING_CONTROLS_DISPATCH_PARAMETERS_ARM' then
-- @schedulingControlsMaxWarpCount@ and
-- @schedulingControlsMaxQueuedBatchesCount@ are undefined.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structure>]
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_scheduling_controls VK_ARM_scheduling_controls>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM = PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM
  { -- No documentation found for Nested "VkPhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM" "schedulingControlsMaxWarpsCount"
    schedulingControlsMaxWarpsCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM" "schedulingControlsMaxQueuedBatchesCount"
    schedulingControlsMaxQueuedBatchesCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM" "schedulingControlsMaxWorkGroupBatchSize"
    schedulingControlsMaxWorkGroupBatchSize :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM)
#endif
deriving instance Show PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM

instance ToCStruct PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_DISPATCH_PARAMETERS_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (schedulingControlsMaxWarpsCount)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (schedulingControlsMaxQueuedBatchesCount)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (schedulingControlsMaxWorkGroupBatchSize)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_DISPATCH_PARAMETERS_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM where
  peekCStruct p = do
    schedulingControlsMaxWarpsCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    schedulingControlsMaxQueuedBatchesCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    schedulingControlsMaxWorkGroupBatchSize <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM
             schedulingControlsMaxWarpsCount
             schedulingControlsMaxQueuedBatchesCount
             schedulingControlsMaxWorkGroupBatchSize

instance Storable PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM where
  zero = PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM
           zero
           zero
           zero


-- | VkDispatchParametersARM - Structure specifying parameters that affect
-- dispatch commands
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_scheduling_controls VK_ARM_scheduling_controls>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdSetDispatchParametersARM'
data DispatchParametersARM = DispatchParametersARM
  { -- | @workGroupBatchSize@, if it is not 0, is the number of workgroups in
    -- each batch distributed to shader cores. Otherwise, the implementation
    -- selects the number of workgroups in each batch.
    --
    -- #VUID-VkDispatchParametersARM-workGroupBatchSize-12394#
    -- @workGroupBatchSize@ /must/ be less than or equal to
    -- 'PhysicalDeviceSchedulingControlsPropertiesARM'::@schedulingControlsMaxWorkGroupBatchSize@
    workGroupBatchSize :: Word32
  , -- | @maxQueuedWorkGroupBatches@, if it is not 0, is the maximum number of
    -- workgroup batches that shader cores /may/ queue. Otherwise, the
    -- implementation selects the maximum number of workgroup batches that
    -- shader cores /may/ queue.
    --
    -- #VUID-VkDispatchParametersARM-maxQueuedWorkGroupBatches-12393#
    -- @maxQueuedWorkGroupBatches@ /must/ be less than or equal to
    -- 'PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM'::@schedulingControlsMaxQueuedBatchesCount@
    maxQueuedWorkGroupBatches :: Word32
  , -- | @maxWarpsPerShaderCore@, if it is not 0, is the maximum number of warps
    -- that /may/ run concurrently on individual shader cores. Otherwise, the
    -- implementation selects the maximum number of warps that /may/ run
    -- concurrently on individual shader cores.
    --
    -- #VUID-VkDispatchParametersARM-maxWarpsPerShaderCore-12392#
    -- @maxWarpsPerShaderCore@ /must/ be less than or equal to
    -- 'PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM'::@schedulingControlsMaxWarpCount@
    maxWarpsPerShaderCore :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DispatchParametersARM)
#endif
deriving instance Show DispatchParametersARM

instance ToCStruct DispatchParametersARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DispatchParametersARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPATCH_PARAMETERS_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (workGroupBatchSize)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxQueuedWorkGroupBatches)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxWarpsPerShaderCore)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPATCH_PARAMETERS_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct DispatchParametersARM where
  peekCStruct p = do
    workGroupBatchSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxQueuedWorkGroupBatches <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxWarpsPerShaderCore <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ DispatchParametersARM
             workGroupBatchSize maxQueuedWorkGroupBatches maxWarpsPerShaderCore

instance Storable DispatchParametersARM where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DispatchParametersARM where
  zero = DispatchParametersARM
           zero
           zero
           zero


type PhysicalDeviceSchedulingControlsFlagsARM = PhysicalDeviceSchedulingControlsFlagBitsARM

-- | VkPhysicalDeviceSchedulingControlsFlagBitsARM - Bitmask specifying
-- scheduling controls supported by a physical device
--
-- = Description
--
-- -   'PHYSICAL_DEVICE_SCHEDULING_CONTROLS_SHADER_CORE_COUNT_ARM'
--     specifies that a 'DeviceQueueShaderCoreControlCreateInfoARM'
--     structure /may/ be included in the @pNext@ chain of a
--     'Vulkan.Core10.Device.DeviceQueueCreateInfo' or
--     'Vulkan.Core10.Device.DeviceCreateInfo' structure.
--
-- -   'PHYSICAL_DEVICE_SCHEDULING_CONTROLS_DISPATCH_PARAMETERS_ARM'
--     specifies that a 'cmdSetDispatchParametersARM' command /may/ be
--     recorded in a command buffer and that properties returned in
--     'PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM'
--     are valid.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_scheduling_controls VK_ARM_scheduling_controls>,
-- 'PhysicalDeviceSchedulingControlsFlagsARM'
newtype PhysicalDeviceSchedulingControlsFlagBitsARM = PhysicalDeviceSchedulingControlsFlagBitsARM Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPhysicalDeviceSchedulingControlsFlagBitsARM" "VK_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_SHADER_CORE_COUNT_ARM"
pattern PHYSICAL_DEVICE_SCHEDULING_CONTROLS_SHADER_CORE_COUNT_ARM = PhysicalDeviceSchedulingControlsFlagBitsARM 0x0000000000000001

-- No documentation found for Nested "VkPhysicalDeviceSchedulingControlsFlagBitsARM" "VK_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_DISPATCH_PARAMETERS_ARM"
pattern PHYSICAL_DEVICE_SCHEDULING_CONTROLS_DISPATCH_PARAMETERS_ARM = PhysicalDeviceSchedulingControlsFlagBitsARM 0x0000000000000002

conNamePhysicalDeviceSchedulingControlsFlagBitsARM :: String
conNamePhysicalDeviceSchedulingControlsFlagBitsARM = "PhysicalDeviceSchedulingControlsFlagBitsARM"

enumPrefixPhysicalDeviceSchedulingControlsFlagBitsARM :: String
enumPrefixPhysicalDeviceSchedulingControlsFlagBitsARM = "PHYSICAL_DEVICE_SCHEDULING_CONTROLS_"

showTablePhysicalDeviceSchedulingControlsFlagBitsARM :: [(PhysicalDeviceSchedulingControlsFlagBitsARM, String)]
showTablePhysicalDeviceSchedulingControlsFlagBitsARM =
  [
    ( PHYSICAL_DEVICE_SCHEDULING_CONTROLS_SHADER_CORE_COUNT_ARM
    , "SHADER_CORE_COUNT_ARM"
    )
  ,
    ( PHYSICAL_DEVICE_SCHEDULING_CONTROLS_DISPATCH_PARAMETERS_ARM
    , "DISPATCH_PARAMETERS_ARM"
    )
  ]

instance Show PhysicalDeviceSchedulingControlsFlagBitsARM where
  showsPrec =
    enumShowsPrec
      enumPrefixPhysicalDeviceSchedulingControlsFlagBitsARM
      showTablePhysicalDeviceSchedulingControlsFlagBitsARM
      conNamePhysicalDeviceSchedulingControlsFlagBitsARM
      (\(PhysicalDeviceSchedulingControlsFlagBitsARM x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PhysicalDeviceSchedulingControlsFlagBitsARM where
  readPrec =
    enumReadPrec
      enumPrefixPhysicalDeviceSchedulingControlsFlagBitsARM
      showTablePhysicalDeviceSchedulingControlsFlagBitsARM
      conNamePhysicalDeviceSchedulingControlsFlagBitsARM
      PhysicalDeviceSchedulingControlsFlagBitsARM

type ARM_SCHEDULING_CONTROLS_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_ARM_SCHEDULING_CONTROLS_SPEC_VERSION"
pattern ARM_SCHEDULING_CONTROLS_SPEC_VERSION :: forall a . Integral a => a
pattern ARM_SCHEDULING_CONTROLS_SPEC_VERSION = 2


type ARM_SCHEDULING_CONTROLS_EXTENSION_NAME = "VK_ARM_scheduling_controls"

-- No documentation found for TopLevel "VK_ARM_SCHEDULING_CONTROLS_EXTENSION_NAME"
pattern ARM_SCHEDULING_CONTROLS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern ARM_SCHEDULING_CONTROLS_EXTENSION_NAME = "VK_ARM_scheduling_controls"

