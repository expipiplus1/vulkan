{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands  ( cmdProcessCommandsNVX
                                                                    , cmdReserveSpaceForCommandsNVX
                                                                    , createIndirectCommandsLayoutNVX
                                                                    , withIndirectCommandsLayoutNVX
                                                                    , destroyIndirectCommandsLayoutNVX
                                                                    , createObjectTableNVX
                                                                    , withObjectTableNVX
                                                                    , destroyObjectTableNVX
                                                                    , registerObjectsNVX
                                                                    , withRegisteredObjectsNVX
                                                                    , unregisterObjectsNVX
                                                                    , getPhysicalDeviceGeneratedCommandsPropertiesNVX
                                                                    , DeviceGeneratedCommandsFeaturesNVX(..)
                                                                    , DeviceGeneratedCommandsLimitsNVX(..)
                                                                    , IndirectCommandsTokenNVX(..)
                                                                    , IndirectCommandsLayoutTokenNVX(..)
                                                                    , IndirectCommandsLayoutCreateInfoNVX(..)
                                                                    , CmdProcessCommandsInfoNVX(..)
                                                                    , CmdReserveSpaceForCommandsInfoNVX(..)
                                                                    , ObjectTableCreateInfoNVX(..)
                                                                    , ObjectTableEntryNVX(..)
                                                                    , ObjectTablePipelineEntryNVX(..)
                                                                    , ObjectTableDescriptorSetEntryNVX(..)
                                                                    , ObjectTableVertexBufferEntryNVX(..)
                                                                    , ObjectTableIndexBufferEntryNVX(..)
                                                                    , ObjectTablePushConstantEntryNVX(..)
                                                                    , IndirectCommandsLayoutUsageFlagBitsNVX( INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX
                                                                                                            , INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX
                                                                                                            , INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX
                                                                                                            , INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX
                                                                                                            , ..
                                                                                                            )
                                                                    , IndirectCommandsLayoutUsageFlagsNVX
                                                                    , ObjectEntryUsageFlagBitsNVX( OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX
                                                                                                 , OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX
                                                                                                 , ..
                                                                                                 )
                                                                    , ObjectEntryUsageFlagsNVX
                                                                    , IndirectCommandsTokenTypeNVX( INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX
                                                                                                  , INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX
                                                                                                  , INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX
                                                                                                  , INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX
                                                                                                  , INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX
                                                                                                  , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX
                                                                                                  , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX
                                                                                                  , INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX
                                                                                                  , ..
                                                                                                  )
                                                                    , ObjectEntryTypeNVX( OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX
                                                                                        , OBJECT_ENTRY_TYPE_PIPELINE_NVX
                                                                                        , OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX
                                                                                        , OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX
                                                                                        , OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX
                                                                                        , ..
                                                                                        )
                                                                    , NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION
                                                                    , pattern NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION
                                                                    , NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
                                                                    , pattern NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
                                                                    , ObjectTableNVX(..)
                                                                    , IndirectCommandsLayoutNVX(..)
                                                                    ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.Core10.Handles (Buffer)
import Graphics.Vulkan.Core10.Handles (CommandBuffer)
import Graphics.Vulkan.Core10.Handles (CommandBuffer(..))
import Graphics.Vulkan.Core10.Handles (CommandBuffer_T)
import Graphics.Vulkan.Core10.Handles (DescriptorSet)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdProcessCommandsNVX))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdReserveSpaceForCommandsNVX))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCreateIndirectCommandsLayoutNVX))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCreateObjectTableNVX))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkDestroyIndirectCommandsLayoutNVX))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkDestroyObjectTableNVX))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkRegisterObjectsNVX))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkUnregisterObjectsNVX))
import Graphics.Vulkan.Core10.BaseType (DeviceSize)
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.IndexType (IndexType)
import Graphics.Vulkan.Extensions.Handles (IndirectCommandsLayoutNVX)
import Graphics.Vulkan.Extensions.Handles (IndirectCommandsLayoutNVX(..))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceGeneratedCommandsPropertiesNVX))
import Graphics.Vulkan.Extensions.Handles (ObjectTableNVX)
import Graphics.Vulkan.Extensions.Handles (ObjectTableNVX(..))
import Graphics.Vulkan.Core10.Handles (PhysicalDevice)
import Graphics.Vulkan.Core10.Handles (PhysicalDevice(..))
import Graphics.Vulkan.Core10.Handles (PhysicalDevice_T)
import Graphics.Vulkan.Core10.Handles (Pipeline)
import Graphics.Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint)
import Graphics.Vulkan.Core10.Handles (PipelineLayout)
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero)
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Graphics.Vulkan.Extensions.Handles (IndirectCommandsLayoutNVX(..))
import Graphics.Vulkan.Extensions.Handles (ObjectTableNVX(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdProcessCommandsNVX
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CmdProcessCommandsInfoNVX -> IO ()) -> Ptr CommandBuffer_T -> Ptr CmdProcessCommandsInfoNVX -> IO ()

-- | vkCmdProcessCommandsNVX - Performs the generation of commands on the
-- device
--
-- = Parameters
--
-- -   @commandBuffer@ is the primary command buffer in which the
--     generation process takes space.
--
-- -   @pProcessCommandsInfo@ is a pointer to a 'CmdProcessCommandsInfoNVX'
--     structure containing parameters affecting the processing of
--     commands.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pProcessCommandsInfo@ /must/ be a valid pointer to a valid
--     'CmdProcessCommandsInfoNVX' structure
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'CmdProcessCommandsInfoNVX',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
cmdProcessCommandsNVX :: forall io . MonadIO io => CommandBuffer -> CmdProcessCommandsInfoNVX -> io ()
cmdProcessCommandsNVX commandBuffer processCommandsInfo = liftIO . evalContT $ do
  let vkCmdProcessCommandsNVX' = mkVkCmdProcessCommandsNVX (pVkCmdProcessCommandsNVX (deviceCmds (commandBuffer :: CommandBuffer)))
  pProcessCommandsInfo <- ContT $ withCStruct (processCommandsInfo)
  lift $ vkCmdProcessCommandsNVX' (commandBufferHandle (commandBuffer)) pProcessCommandsInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdReserveSpaceForCommandsNVX
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CmdReserveSpaceForCommandsInfoNVX -> IO ()) -> Ptr CommandBuffer_T -> Ptr CmdReserveSpaceForCommandsInfoNVX -> IO ()

-- | vkCmdReserveSpaceForCommandsNVX - Perform a reservation of command
-- buffer space
--
-- = Parameters
--
-- -   @commandBuffer@ is the secondary command buffer in which the space
--     for device-generated commands is reserved.
--
-- -   @pProcessCommandsInfo@ is a pointer to a
--     'CmdReserveSpaceForCommandsInfoNVX' structure containing parameters
--     affecting the reservation of command buffer space.
--
-- == Valid Usage
--
-- -   The provided @commandBuffer@ /must/ not have had a prior space
--     reservation since its creation or the last reset.
--
-- -   The state of the @commandBuffer@ /must/ be legal to execute all
--     commands within the sequence provided by the
--     @indirectCommandsLayout@ member of @pProcessCommandsInfo@.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pReserveSpaceInfo@ /must/ be a valid pointer to a valid
--     'CmdReserveSpaceForCommandsInfoNVX' structure
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   @commandBuffer@ /must/ be a secondary
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Secondary                                                                                                                  | Inside                                                                                                                 | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'CmdReserveSpaceForCommandsInfoNVX',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
cmdReserveSpaceForCommandsNVX :: forall io . MonadIO io => CommandBuffer -> ("reserveSpaceInfo" ::: CmdReserveSpaceForCommandsInfoNVX) -> io ()
cmdReserveSpaceForCommandsNVX commandBuffer reserveSpaceInfo = liftIO . evalContT $ do
  let vkCmdReserveSpaceForCommandsNVX' = mkVkCmdReserveSpaceForCommandsNVX (pVkCmdReserveSpaceForCommandsNVX (deviceCmds (commandBuffer :: CommandBuffer)))
  pReserveSpaceInfo <- ContT $ withCStruct (reserveSpaceInfo)
  lift $ vkCmdReserveSpaceForCommandsNVX' (commandBufferHandle (commandBuffer)) pReserveSpaceInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateIndirectCommandsLayoutNVX
  :: FunPtr (Ptr Device_T -> Ptr IndirectCommandsLayoutCreateInfoNVX -> Ptr AllocationCallbacks -> Ptr IndirectCommandsLayoutNVX -> IO Result) -> Ptr Device_T -> Ptr IndirectCommandsLayoutCreateInfoNVX -> Ptr AllocationCallbacks -> Ptr IndirectCommandsLayoutNVX -> IO Result

-- | vkCreateIndirectCommandsLayoutNVX - Create an indirect command layout
-- object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the indirect command
--     layout.
--
-- -   @pCreateInfo@ is a pointer to a
--     'IndirectCommandsLayoutCreateInfoNVX' structure containing
--     parameters affecting creation of the indirect command layout.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pIndirectCommandsLayout@ is a pointer to a
--     'Graphics.Vulkan.Extensions.Handles.IndirectCommandsLayoutNVX'
--     handle in which the resulting indirect command layout is returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'IndirectCommandsLayoutCreateInfoNVX' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @pIndirectCommandsLayout@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.Extensions.Handles.IndirectCommandsLayoutNVX'
--     handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'IndirectCommandsLayoutCreateInfoNVX',
-- 'Graphics.Vulkan.Extensions.Handles.IndirectCommandsLayoutNVX'
createIndirectCommandsLayoutNVX :: forall io . MonadIO io => Device -> IndirectCommandsLayoutCreateInfoNVX -> ("allocator" ::: Maybe AllocationCallbacks) -> io (IndirectCommandsLayoutNVX)
createIndirectCommandsLayoutNVX device createInfo allocator = liftIO . evalContT $ do
  let vkCreateIndirectCommandsLayoutNVX' = mkVkCreateIndirectCommandsLayoutNVX (pVkCreateIndirectCommandsLayoutNVX (deviceCmds (device :: Device)))
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPIndirectCommandsLayout <- ContT $ bracket (callocBytes @IndirectCommandsLayoutNVX 8) free
  r <- lift $ vkCreateIndirectCommandsLayoutNVX' (deviceHandle (device)) pCreateInfo pAllocator (pPIndirectCommandsLayout)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pIndirectCommandsLayout <- lift $ peek @IndirectCommandsLayoutNVX pPIndirectCommandsLayout
  pure $ (pIndirectCommandsLayout)

-- | A convenience wrapper to make a compatible pair of
-- 'createIndirectCommandsLayoutNVX' and 'destroyIndirectCommandsLayoutNVX'
--
-- To ensure that 'destroyIndirectCommandsLayoutNVX' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the first argument.
-- To just extract the pair pass '(,)' as the first argument.
--
withIndirectCommandsLayoutNVX :: forall io r . MonadIO io => (io (IndirectCommandsLayoutNVX) -> ((IndirectCommandsLayoutNVX) -> io ()) -> r) -> Device -> IndirectCommandsLayoutCreateInfoNVX -> Maybe AllocationCallbacks -> r
withIndirectCommandsLayoutNVX b device pCreateInfo pAllocator =
  b (createIndirectCommandsLayoutNVX device pCreateInfo pAllocator)
    (\(o0) -> destroyIndirectCommandsLayoutNVX device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyIndirectCommandsLayoutNVX
  :: FunPtr (Ptr Device_T -> IndirectCommandsLayoutNVX -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> IndirectCommandsLayoutNVX -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyIndirectCommandsLayoutNVX - Destroy an object table
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the layout.
--
-- -   @indirectCommandsLayout@ is the table to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @indirectCommandsLayout@ /must/
--     have completed execution
--
-- -   If 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @objectTable@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @objectTable@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @indirectCommandsLayout@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.IndirectCommandsLayoutNVX'
--     handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @indirectCommandsLayout@ /must/ have been created, allocated, or
--     retrieved from @device@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Extensions.Handles.IndirectCommandsLayoutNVX'
destroyIndirectCommandsLayoutNVX :: forall io . MonadIO io => Device -> IndirectCommandsLayoutNVX -> ("allocator" ::: Maybe AllocationCallbacks) -> io ()
destroyIndirectCommandsLayoutNVX device indirectCommandsLayout allocator = liftIO . evalContT $ do
  let vkDestroyIndirectCommandsLayoutNVX' = mkVkDestroyIndirectCommandsLayoutNVX (pVkDestroyIndirectCommandsLayoutNVX (deviceCmds (device :: Device)))
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyIndirectCommandsLayoutNVX' (deviceHandle (device)) (indirectCommandsLayout) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateObjectTableNVX
  :: FunPtr (Ptr Device_T -> Ptr ObjectTableCreateInfoNVX -> Ptr AllocationCallbacks -> Ptr ObjectTableNVX -> IO Result) -> Ptr Device_T -> Ptr ObjectTableCreateInfoNVX -> Ptr AllocationCallbacks -> Ptr ObjectTableNVX -> IO Result

-- | vkCreateObjectTableNVX - Create an object table
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the object table.
--
-- -   @pCreateInfo@ is a pointer to a 'ObjectTableCreateInfoNVX' structure
--     containing parameters affecting creation of the table.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pObjectTable@ is a pointer to a
--     'Graphics.Vulkan.Extensions.Handles.ObjectTableNVX' handle in which
--     the resulting object table is returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'ObjectTableCreateInfoNVX' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @pObjectTable@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.Extensions.Handles.ObjectTableNVX' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device', 'ObjectTableCreateInfoNVX',
-- 'Graphics.Vulkan.Extensions.Handles.ObjectTableNVX'
createObjectTableNVX :: forall io . MonadIO io => Device -> ObjectTableCreateInfoNVX -> ("allocator" ::: Maybe AllocationCallbacks) -> io (ObjectTableNVX)
createObjectTableNVX device createInfo allocator = liftIO . evalContT $ do
  let vkCreateObjectTableNVX' = mkVkCreateObjectTableNVX (pVkCreateObjectTableNVX (deviceCmds (device :: Device)))
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPObjectTable <- ContT $ bracket (callocBytes @ObjectTableNVX 8) free
  r <- lift $ vkCreateObjectTableNVX' (deviceHandle (device)) pCreateInfo pAllocator (pPObjectTable)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pObjectTable <- lift $ peek @ObjectTableNVX pPObjectTable
  pure $ (pObjectTable)

-- | A convenience wrapper to make a compatible pair of
-- 'createObjectTableNVX' and 'destroyObjectTableNVX'
--
-- To ensure that 'destroyObjectTableNVX' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the first argument.
-- To just extract the pair pass '(,)' as the first argument.
--
withObjectTableNVX :: forall io r . MonadIO io => (io (ObjectTableNVX) -> ((ObjectTableNVX) -> io ()) -> r) -> Device -> ObjectTableCreateInfoNVX -> Maybe AllocationCallbacks -> r
withObjectTableNVX b device pCreateInfo pAllocator =
  b (createObjectTableNVX device pCreateInfo pAllocator)
    (\(o0) -> destroyObjectTableNVX device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyObjectTableNVX
  :: FunPtr (Ptr Device_T -> ObjectTableNVX -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> ObjectTableNVX -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyObjectTableNVX - Destroy an object table
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the table.
--
-- -   @objectTable@ is the table to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @objectTable@ /must/ have
--     completed execution.
--
-- -   If 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @objectTable@ was created, a compatible set of
--     callbacks /must/ be provided here.
--
-- -   If no
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @objectTable@ was created, @pAllocator@ /must/ be
--     @NULL@.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @objectTable@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.ObjectTableNVX' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @objectTable@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @objectTable@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Extensions.Handles.ObjectTableNVX'
destroyObjectTableNVX :: forall io . MonadIO io => Device -> ObjectTableNVX -> ("allocator" ::: Maybe AllocationCallbacks) -> io ()
destroyObjectTableNVX device objectTable allocator = liftIO . evalContT $ do
  let vkDestroyObjectTableNVX' = mkVkDestroyObjectTableNVX (pVkDestroyObjectTableNVX (deviceCmds (device :: Device)))
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyObjectTableNVX' (deviceHandle (device)) (objectTable) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkRegisterObjectsNVX
  :: FunPtr (Ptr Device_T -> ObjectTableNVX -> Word32 -> Ptr (Ptr ObjectTableEntryNVX) -> Ptr Word32 -> IO Result) -> Ptr Device_T -> ObjectTableNVX -> Word32 -> Ptr (Ptr ObjectTableEntryNVX) -> Ptr Word32 -> IO Result

-- | vkRegisterObjectsNVX - Register resource bindings in an object table
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the object table.
--
-- -   @objectTable@ is the table for which the resources are registered.
--
-- -   @objectCount@ is the number of resources to register.
--
-- -   @ppObjectTableEntries@ provides an array for detailed binding
--     informations. Each array element is a pointer to a structure of type
--     'ObjectTablePipelineEntryNVX', 'ObjectTableDescriptorSetEntryNVX',
--     'ObjectTableVertexBufferEntryNVX', 'ObjectTableIndexBufferEntryNVX'
--     or 'ObjectTablePushConstantEntryNVX' (see below for details).
--
-- -   @pObjectIndices@ are the indices at which each resource is
--     registered.
--
-- == Valid Usage
--
-- -   The contents of @pObjectTableEntry@ /must/ yield plausible bindings
--     supported by the device.
--
-- -   At any @pObjectIndices@ there /must/ not be a registered resource
--     already.
--
-- -   Any value inside @pObjectIndices@ /must/ be below the appropriate
--     'ObjectTableCreateInfoNVX'::@pObjectEntryCounts@ limits provided at
--     @objectTable@ creation time.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @objectTable@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.ObjectTableNVX' handle
--
-- -   @ppObjectTableEntries@ /must/ be a valid pointer to an array of
--     @objectCount@ valid 'ObjectTableEntryNVX' structures
--
-- -   @pObjectIndices@ /must/ be a valid pointer to an array of
--     @objectCount@ @uint32_t@ values
--
-- -   @objectCount@ /must/ be greater than @0@
--
-- -   @objectTable@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @objectTable@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device', 'ObjectTableEntryNVX',
-- 'Graphics.Vulkan.Extensions.Handles.ObjectTableNVX'
registerObjectsNVX :: forall io . MonadIO io => Device -> ObjectTableNVX -> ("objectTableEntries" ::: Vector ObjectTableEntryNVX) -> ("objectIndices" ::: Vector Word32) -> io ()
registerObjectsNVX device objectTable objectTableEntries objectIndices = liftIO . evalContT $ do
  let vkRegisterObjectsNVX' = mkVkRegisterObjectsNVX (pVkRegisterObjectsNVX (deviceCmds (device :: Device)))
  let ppObjectTableEntriesLength = Data.Vector.length $ (objectTableEntries)
  let pObjectIndicesLength = Data.Vector.length $ (objectIndices)
  lift $ unless (pObjectIndicesLength == ppObjectTableEntriesLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pObjectIndices and ppObjectTableEntries must have the same length" Nothing Nothing
  pPpObjectTableEntries <- ContT $ allocaBytesAligned @(Ptr ObjectTableEntryNVX) ((Data.Vector.length (objectTableEntries)) * 8) 8
  Data.Vector.imapM_ (\i e -> do
    ppObjectTableEntries <- ContT $ withCStruct (e)
    lift $ poke (pPpObjectTableEntries `plusPtr` (8 * (i)) :: Ptr (Ptr ObjectTableEntryNVX)) ppObjectTableEntries) (objectTableEntries)
  pPObjectIndices <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (objectIndices)) * 4) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPObjectIndices `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (objectIndices)
  r <- lift $ vkRegisterObjectsNVX' (deviceHandle (device)) (objectTable) ((fromIntegral ppObjectTableEntriesLength :: Word32)) (pPpObjectTableEntries) (pPObjectIndices)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))

-- | A convenience wrapper to make a compatible pair of 'registerObjectsNVX'
-- and 'unregisterObjectsNVX'
--
-- To ensure that 'unregisterObjectsNVX' is always called: pass
-- 'Control.Exception.bracket_' (or the allocate function from your
-- favourite resource management library) as the first argument.
-- To just extract the pair pass '(,)' as the first argument.
--
-- Note that there is no inner resource
withRegisteredObjectsNVX :: forall io r . MonadIO io => (io () -> io () -> r) -> Device -> ObjectTableNVX -> Vector ObjectTableEntryNVX -> Vector Word32 -> Vector ObjectEntryTypeNVX -> r
withRegisteredObjectsNVX b device objectTable ppObjectTableEntries pObjectIndices pObjectEntryTypes =
  b (registerObjectsNVX device objectTable ppObjectTableEntries pObjectIndices)
    (unregisterObjectsNVX device objectTable pObjectEntryTypes pObjectIndices)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUnregisterObjectsNVX
  :: FunPtr (Ptr Device_T -> ObjectTableNVX -> Word32 -> Ptr ObjectEntryTypeNVX -> Ptr Word32 -> IO Result) -> Ptr Device_T -> ObjectTableNVX -> Word32 -> Ptr ObjectEntryTypeNVX -> Ptr Word32 -> IO Result

-- | vkUnregisterObjectsNVX - Unregister resource bindings in an object table
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the object table.
--
-- -   @objectTable@ is the table from which the resources are
--     unregistered.
--
-- -   @objectCount@ is the number of resources being removed from the
--     object table.
--
-- -   @pObjectEntryType@ provides an array of 'ObjectEntryTypeNVX' for the
--     resources being removed.
--
-- -   @pObjectIndices@ provides the array of object indices to be removed.
--
-- == Valid Usage
--
-- -   At any @pObjectIndices@ there /must/ be a registered resource
--     already.
--
-- -   The @pObjectEntryTypes@ of the resource at @pObjectIndices@ /must/
--     match.
--
-- -   All operations on the device using the registered resource /must/
--     have been completed.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @objectTable@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.ObjectTableNVX' handle
--
-- -   @pObjectEntryTypes@ /must/ be a valid pointer to an array of
--     @objectCount@ valid 'ObjectEntryTypeNVX' values
--
-- -   @pObjectIndices@ /must/ be a valid pointer to an array of
--     @objectCount@ @uint32_t@ values
--
-- -   @objectCount@ /must/ be greater than @0@
--
-- -   @objectTable@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @objectTable@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device', 'ObjectEntryTypeNVX',
-- 'Graphics.Vulkan.Extensions.Handles.ObjectTableNVX'
unregisterObjectsNVX :: forall io . MonadIO io => Device -> ObjectTableNVX -> ("objectEntryTypes" ::: Vector ObjectEntryTypeNVX) -> ("objectIndices" ::: Vector Word32) -> io ()
unregisterObjectsNVX device objectTable objectEntryTypes objectIndices = liftIO . evalContT $ do
  let vkUnregisterObjectsNVX' = mkVkUnregisterObjectsNVX (pVkUnregisterObjectsNVX (deviceCmds (device :: Device)))
  let pObjectEntryTypesLength = Data.Vector.length $ (objectEntryTypes)
  let pObjectIndicesLength = Data.Vector.length $ (objectIndices)
  lift $ unless (pObjectIndicesLength == pObjectEntryTypesLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pObjectIndices and pObjectEntryTypes must have the same length" Nothing Nothing
  pPObjectEntryTypes <- ContT $ allocaBytesAligned @ObjectEntryTypeNVX ((Data.Vector.length (objectEntryTypes)) * 4) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPObjectEntryTypes `plusPtr` (4 * (i)) :: Ptr ObjectEntryTypeNVX) (e)) (objectEntryTypes)
  pPObjectIndices <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (objectIndices)) * 4) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPObjectIndices `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (objectIndices)
  r <- lift $ vkUnregisterObjectsNVX' (deviceHandle (device)) (objectTable) ((fromIntegral pObjectEntryTypesLength :: Word32)) (pPObjectEntryTypes) (pPObjectIndices)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr DeviceGeneratedCommandsFeaturesNVX -> Ptr DeviceGeneratedCommandsLimitsNVX -> IO ()) -> Ptr PhysicalDevice_T -> Ptr DeviceGeneratedCommandsFeaturesNVX -> Ptr DeviceGeneratedCommandsLimitsNVX -> IO ()

-- | vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX - Returns
-- device-generated commands related properties of a physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the handle to the physical device whose
--     properties will be queried.
--
-- -   @pFeatures@ is a pointer to a 'DeviceGeneratedCommandsFeaturesNVX'
--     structure in which features are returned.
--
-- -   @pLimits@ is a pointer to a 'DeviceGeneratedCommandsLimitsNVX'
--     structure in which limitations are returned.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'DeviceGeneratedCommandsFeaturesNVX',
-- 'DeviceGeneratedCommandsLimitsNVX',
-- 'Graphics.Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceGeneratedCommandsPropertiesNVX :: forall io . MonadIO io => PhysicalDevice -> io (DeviceGeneratedCommandsFeaturesNVX, DeviceGeneratedCommandsLimitsNVX)
getPhysicalDeviceGeneratedCommandsPropertiesNVX physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX' = mkVkGetPhysicalDeviceGeneratedCommandsPropertiesNVX (pVkGetPhysicalDeviceGeneratedCommandsPropertiesNVX (instanceCmds (physicalDevice :: PhysicalDevice)))
  pPFeatures <- ContT (withZeroCStruct @DeviceGeneratedCommandsFeaturesNVX)
  pPLimits <- ContT (withZeroCStruct @DeviceGeneratedCommandsLimitsNVX)
  lift $ vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX' (physicalDeviceHandle (physicalDevice)) (pPFeatures) (pPLimits)
  pFeatures <- lift $ peekCStruct @DeviceGeneratedCommandsFeaturesNVX pPFeatures
  pLimits <- lift $ peekCStruct @DeviceGeneratedCommandsLimitsNVX pPLimits
  pure $ (pFeatures, pLimits)


-- | VkDeviceGeneratedCommandsFeaturesNVX - Structure specifying physical
-- device support
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceGeneratedCommandsPropertiesNVX'
data DeviceGeneratedCommandsFeaturesNVX = DeviceGeneratedCommandsFeaturesNVX
  { -- | @computeBindingPointSupport@ specifies whether the
    -- 'Graphics.Vulkan.Extensions.Handles.ObjectTableNVX' supports entries
    -- with 'OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX' bit set and
    -- 'Graphics.Vulkan.Extensions.Handles.IndirectCommandsLayoutNVX' supports
    -- 'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE'.
    computeBindingPointSupport :: Bool }
  deriving (Typeable)
deriving instance Show DeviceGeneratedCommandsFeaturesNVX

instance ToCStruct DeviceGeneratedCommandsFeaturesNVX where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGeneratedCommandsFeaturesNVX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (computeBindingPointSupport))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct DeviceGeneratedCommandsFeaturesNVX where
  peekCStruct p = do
    computeBindingPointSupport <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ DeviceGeneratedCommandsFeaturesNVX
             (bool32ToBool computeBindingPointSupport)

instance Storable DeviceGeneratedCommandsFeaturesNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceGeneratedCommandsFeaturesNVX where
  zero = DeviceGeneratedCommandsFeaturesNVX
           zero


-- | VkDeviceGeneratedCommandsLimitsNVX - Structure specifying physical
-- device limits
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceGeneratedCommandsPropertiesNVX'
data DeviceGeneratedCommandsLimitsNVX = DeviceGeneratedCommandsLimitsNVX
  { -- | @maxIndirectCommandsLayoutTokenCount@ the maximum number of tokens in
    -- 'Graphics.Vulkan.Extensions.Handles.IndirectCommandsLayoutNVX'.
    maxIndirectCommandsLayoutTokenCount :: Word32
  , -- | @maxObjectEntryCounts@ the maximum number of entries per resource type
    -- in 'Graphics.Vulkan.Extensions.Handles.ObjectTableNVX'.
    maxObjectEntryCounts :: Word32
  , -- | @minSequenceCountBufferOffsetAlignment@ the minimum alignment for memory
    -- addresses optionally used in 'cmdProcessCommandsNVX'.
    minSequenceCountBufferOffsetAlignment :: Word32
  , -- | @minSequenceIndexBufferOffsetAlignment@ the minimum alignment for memory
    -- addresses optionally used in 'cmdProcessCommandsNVX'.
    minSequenceIndexBufferOffsetAlignment :: Word32
  , -- | @minCommandsTokenBufferOffsetAlignment@ the minimum alignment for memory
    -- addresses optionally used in 'cmdProcessCommandsNVX'.
    minCommandsTokenBufferOffsetAlignment :: Word32
  }
  deriving (Typeable)
deriving instance Show DeviceGeneratedCommandsLimitsNVX

instance ToCStruct DeviceGeneratedCommandsLimitsNVX where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGeneratedCommandsLimitsNVX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxIndirectCommandsLayoutTokenCount)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxObjectEntryCounts)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (minSequenceCountBufferOffsetAlignment)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (minSequenceIndexBufferOffsetAlignment)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (minCommandsTokenBufferOffsetAlignment)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    f

instance FromCStruct DeviceGeneratedCommandsLimitsNVX where
  peekCStruct p = do
    maxIndirectCommandsLayoutTokenCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxObjectEntryCounts <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    minSequenceCountBufferOffsetAlignment <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    minSequenceIndexBufferOffsetAlignment <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    minCommandsTokenBufferOffsetAlignment <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pure $ DeviceGeneratedCommandsLimitsNVX
             maxIndirectCommandsLayoutTokenCount maxObjectEntryCounts minSequenceCountBufferOffsetAlignment minSequenceIndexBufferOffsetAlignment minCommandsTokenBufferOffsetAlignment

instance Storable DeviceGeneratedCommandsLimitsNVX where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceGeneratedCommandsLimitsNVX where
  zero = DeviceGeneratedCommandsLimitsNVX
           zero
           zero
           zero
           zero
           zero


-- | VkIndirectCommandsTokenNVX - Structure specifying parameters for the
-- reservation of command buffer space
--
-- == Valid Usage
--
-- -   The @buffer@’s usage flag /must/ have the
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set.
--
-- -   The @offset@ /must/ be aligned to
--     'DeviceGeneratedCommandsLimitsNVX'::@minCommandsTokenBufferOffsetAlignment@.
--
-- == Valid Usage (Implicit)
--
-- -   @tokenType@ /must/ be a valid 'IndirectCommandsTokenTypeNVX' value
--
-- -   @buffer@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Buffer'
--     handle
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer', 'CmdProcessCommandsInfoNVX',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'IndirectCommandsTokenTypeNVX'
data IndirectCommandsTokenNVX = IndirectCommandsTokenNVX
  { -- | @tokenType@ specifies the token command type.
    tokenType :: IndirectCommandsTokenTypeNVX
  , -- | @buffer@ specifies the 'Graphics.Vulkan.Core10.Handles.Buffer' storing
    -- the functional arguments for each squence. These argumetns can be
    -- written by the device.
    buffer :: Buffer
  , -- | @offset@ specified an offset into @buffer@ where the arguments start.
    offset :: DeviceSize
  }
  deriving (Typeable)
deriving instance Show IndirectCommandsTokenNVX

instance ToCStruct IndirectCommandsTokenNVX where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsTokenNVX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr IndirectCommandsTokenTypeNVX)) (tokenType)
    poke ((p `plusPtr` 8 :: Ptr Buffer)) (buffer)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (offset)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr IndirectCommandsTokenTypeNVX)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct IndirectCommandsTokenNVX where
  peekCStruct p = do
    tokenType <- peek @IndirectCommandsTokenTypeNVX ((p `plusPtr` 0 :: Ptr IndirectCommandsTokenTypeNVX))
    buffer <- peek @Buffer ((p `plusPtr` 8 :: Ptr Buffer))
    offset <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ IndirectCommandsTokenNVX
             tokenType buffer offset

instance Storable IndirectCommandsTokenNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero IndirectCommandsTokenNVX where
  zero = IndirectCommandsTokenNVX
           zero
           zero
           zero


-- | VkIndirectCommandsLayoutTokenNVX - Struct specifying the details of an
-- indirect command layout token
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'IndirectCommandsLayoutCreateInfoNVX', 'IndirectCommandsTokenTypeNVX'
data IndirectCommandsLayoutTokenNVX = IndirectCommandsLayoutTokenNVX
  { -- | @tokenType@ /must/ be a valid 'IndirectCommandsTokenTypeNVX' value
    tokenType :: IndirectCommandsTokenTypeNVX
  , -- | @bindingUnit@ /must/ stay within device supported limits for the
    -- appropriate commands.
    bindingUnit :: Word32
  , -- | @dynamicCount@ /must/ stay within device supported limits for the
    -- appropriate commands.
    dynamicCount :: Word32
  , -- | @divisor@ /must/ be greater than @0@ and a power of two.
    divisor :: Word32
  }
  deriving (Typeable)
deriving instance Show IndirectCommandsLayoutTokenNVX

instance ToCStruct IndirectCommandsLayoutTokenNVX where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsLayoutTokenNVX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr IndirectCommandsTokenTypeNVX)) (tokenType)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (bindingUnit)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (dynamicCount)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (divisor)
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr IndirectCommandsTokenTypeNVX)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    f

instance FromCStruct IndirectCommandsLayoutTokenNVX where
  peekCStruct p = do
    tokenType <- peek @IndirectCommandsTokenTypeNVX ((p `plusPtr` 0 :: Ptr IndirectCommandsTokenTypeNVX))
    bindingUnit <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    dynamicCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    divisor <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pure $ IndirectCommandsLayoutTokenNVX
             tokenType bindingUnit dynamicCount divisor

instance Storable IndirectCommandsLayoutTokenNVX where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero IndirectCommandsLayoutTokenNVX where
  zero = IndirectCommandsLayoutTokenNVX
           zero
           zero
           zero
           zero


-- | VkIndirectCommandsLayoutCreateInfoNVX - Structure specifying the
-- parameters of a newly created indirect commands layout object
--
-- = Description
--
-- The following code illustrates some of the key flags:
--
-- > void cmdProcessAllSequences(cmd, objectTable, indirectCommandsLayout, pIndirectCommandsTokens, sequencesCount, indexbuffer, indexbufferoffset)
-- > {
-- >   for (s = 0; s < sequencesCount; s++)
-- >   {
-- >     sequence = s;
-- >
-- >     if (indirectCommandsLayout.flags & VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX) {
-- >       sequence = incoherent_implementation_dependent_permutation[ sequence ];
-- >     }
-- >     if (indirectCommandsLayout.flags & VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX) {
-- >       sequence = indexbuffer.load_uint32( sequence * sizeof(uint32_t) + indexbufferoffset);
-- >     }
-- >
-- >     cmdProcessSequence( cmd, objectTable, indirectCommandsLayout, pIndirectCommandsTokens, sequence );
-- >   }
-- > }
--
-- == Valid Usage
--
-- -   @tokenCount@ /must/ be greater than @0@ and below
--     'DeviceGeneratedCommandsLimitsNVX'::@maxIndirectCommandsLayoutTokenCount@
--
-- -   If the
--     'DeviceGeneratedCommandsFeaturesNVX'::@computeBindingPointSupport@
--     feature is not enabled, then @pipelineBindPoint@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE'
--
-- -   If @pTokens@ contains an entry of
--     'INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX' it /must/ be the first
--     element of the array and there /must/ be only a single element of
--     such token type.
--
-- -   All state binding tokens in @pTokens@ /must/ occur prior work
--     provoking tokens ('INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX').
--
-- -   The content of @pTokens@ /must/ include one single work provoking
--     token that is compatible with the @pipelineBindPoint@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @pipelineBindPoint@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
--     value
--
-- -   @flags@ /must/ be a valid combination of
--     'IndirectCommandsLayoutUsageFlagBitsNVX' values
--
-- -   @flags@ /must/ not be @0@
--
-- -   @pTokens@ /must/ be a valid pointer to an array of @tokenCount@
--     valid 'IndirectCommandsLayoutTokenNVX' structures
--
-- -   @tokenCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'IndirectCommandsLayoutTokenNVX', 'IndirectCommandsLayoutUsageFlagsNVX',
-- 'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createIndirectCommandsLayoutNVX'
data IndirectCommandsLayoutCreateInfoNVX = IndirectCommandsLayoutCreateInfoNVX
  { -- | @pipelineBindPoint@ is the
    -- 'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' that
    -- this layout targets.
    pipelineBindPoint :: PipelineBindPoint
  , -- | @flags@ is a bitmask of 'IndirectCommandsLayoutUsageFlagBitsNVX'
    -- specifying usage hints of this layout.
    flags :: IndirectCommandsLayoutUsageFlagsNVX
  , -- | @pTokens@ is an array describing each command token in detail. See
    -- 'IndirectCommandsTokenTypeNVX' and 'IndirectCommandsLayoutTokenNVX'
    -- below for details.
    tokens :: Vector IndirectCommandsLayoutTokenNVX
  }
  deriving (Typeable)
deriving instance Show IndirectCommandsLayoutCreateInfoNVX

instance ToCStruct IndirectCommandsLayoutCreateInfoNVX where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsLayoutCreateInfoNVX{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineBindPoint)) (pipelineBindPoint)
    lift $ poke ((p `plusPtr` 20 :: Ptr IndirectCommandsLayoutUsageFlagsNVX)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (tokens)) :: Word32))
    pPTokens' <- ContT $ allocaBytesAligned @IndirectCommandsLayoutTokenNVX ((Data.Vector.length (tokens)) * 16) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPTokens' `plusPtr` (16 * (i)) :: Ptr IndirectCommandsLayoutTokenNVX) (e) . ($ ())) (tokens)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr IndirectCommandsLayoutTokenNVX))) (pPTokens')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineBindPoint)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr IndirectCommandsLayoutUsageFlagsNVX)) (zero)
    pPTokens' <- ContT $ allocaBytesAligned @IndirectCommandsLayoutTokenNVX ((Data.Vector.length (mempty)) * 16) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPTokens' `plusPtr` (16 * (i)) :: Ptr IndirectCommandsLayoutTokenNVX) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr IndirectCommandsLayoutTokenNVX))) (pPTokens')
    lift $ f

instance FromCStruct IndirectCommandsLayoutCreateInfoNVX where
  peekCStruct p = do
    pipelineBindPoint <- peek @PipelineBindPoint ((p `plusPtr` 16 :: Ptr PipelineBindPoint))
    flags <- peek @IndirectCommandsLayoutUsageFlagsNVX ((p `plusPtr` 20 :: Ptr IndirectCommandsLayoutUsageFlagsNVX))
    tokenCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pTokens <- peek @(Ptr IndirectCommandsLayoutTokenNVX) ((p `plusPtr` 32 :: Ptr (Ptr IndirectCommandsLayoutTokenNVX)))
    pTokens' <- generateM (fromIntegral tokenCount) (\i -> peekCStruct @IndirectCommandsLayoutTokenNVX ((pTokens `advancePtrBytes` (16 * (i)) :: Ptr IndirectCommandsLayoutTokenNVX)))
    pure $ IndirectCommandsLayoutCreateInfoNVX
             pipelineBindPoint flags pTokens'

instance Zero IndirectCommandsLayoutCreateInfoNVX where
  zero = IndirectCommandsLayoutCreateInfoNVX
           zero
           zero
           mempty


-- | VkCmdProcessCommandsInfoNVX - Structure specifying parameters for the
-- generation of commands
--
-- == Valid Usage
--
-- -   The provided @objectTable@ /must/ include all objects referenced by
--     the generation process
--
-- -   @indirectCommandsTokenCount@ /must/ match the
--     @indirectCommandsLayout@’s @tokenCount@
--
-- -   The @tokenType@ member of each entry in the
--     @pIndirectCommandsTokens@ array /must/ match the values used at
--     creation time of @indirectCommandsLayout@
--
-- -   If @targetCommandBuffer@ is provided, it /must/ have reserved
--     command space
--
-- -   If @targetCommandBuffer@ is provided, the @objectTable@ /must/ match
--     the reservation’s @objectTable@ and /must/ have had all referenced
--     objects registered at reservation time
--
-- -   If @targetCommandBuffer@ is provided, the @indirectCommandsLayout@
--     /must/ match the reservation’s @indirectCommandsLayout@
--
-- -   If @targetCommandBuffer@ is provided, the @maxSequencesCount@ /must/
--     not exceed the reservation’s @maxSequencesCount@
--
-- -   If @sequencesCountBuffer@ is used, its usage flag /must/ have the
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   If @sequencesCountBuffer@ is used, @sequencesCountOffset@ /must/ be
--     aligned to
--     'DeviceGeneratedCommandsLimitsNVX'::@minSequenceCountBufferOffsetAlignment@
--
-- -   If @sequencesIndexBuffer@ is used, its usage flag /must/ have the
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   If @sequencesIndexBuffer@ is used, @sequencesIndexOffset@ /must/ be
--     aligned to
--     'DeviceGeneratedCommandsLimitsNVX'::@minSequenceIndexBufferOffsetAlignment@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @objectTable@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.ObjectTableNVX' handle
--
-- -   @indirectCommandsLayout@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.IndirectCommandsLayoutNVX'
--     handle
--
-- -   @pIndirectCommandsTokens@ /must/ be a valid pointer to an array of
--     @indirectCommandsTokenCount@ valid 'IndirectCommandsTokenNVX'
--     structures
--
-- -   If @targetCommandBuffer@ is not @NULL@, @targetCommandBuffer@ /must/
--     be a valid 'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   If @sequencesCountBuffer@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @sequencesCountBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   If @sequencesIndexBuffer@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @sequencesIndexBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   @indirectCommandsTokenCount@ /must/ be greater than @0@
--
-- -   Each of @indirectCommandsLayout@, @objectTable@,
--     @sequencesCountBuffer@, @sequencesIndexBuffer@, and
--     @targetCommandBuffer@ that are valid handles of non-ignored
--     parameters /must/ have been created, allocated, or retrieved from
--     the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @objectTable@ /must/ be externally synchronized
--
-- -   Host access to @targetCommandBuffer@ /must/ be externally
--     synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'Graphics.Vulkan.Extensions.Handles.IndirectCommandsLayoutNVX',
-- 'IndirectCommandsTokenNVX',
-- 'Graphics.Vulkan.Extensions.Handles.ObjectTableNVX',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdProcessCommandsNVX'
data CmdProcessCommandsInfoNVX = CmdProcessCommandsInfoNVX
  { -- | @objectTable@ is the 'Graphics.Vulkan.Extensions.Handles.ObjectTableNVX'
    -- to be used for the generation process. Only registered objects at the
    -- time 'cmdReserveSpaceForCommandsNVX' is called, will be taken into
    -- account for the reservation.
    objectTable :: ObjectTableNVX
  , -- | @indirectCommandsLayout@ is the
    -- 'Graphics.Vulkan.Extensions.Handles.IndirectCommandsLayoutNVX' that
    -- provides the command sequence to generate.
    indirectCommandsLayout :: IndirectCommandsLayoutNVX
  , -- | @pIndirectCommandsTokens@ provides an array of
    -- 'IndirectCommandsTokenNVX' that reference the input data for each token
    -- command.
    indirectCommandsTokens :: Vector IndirectCommandsTokenNVX
  , -- | @maxSequencesCount@ is the maximum number of sequences for which command
    -- buffer space will be reserved. If @sequencesCountBuffer@ is
    -- 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', this is also the
    -- actual number of sequences generated.
    maxSequencesCount :: Word32
  , -- | @targetCommandBuffer@ /can/ be the secondary
    -- 'Graphics.Vulkan.Core10.Handles.CommandBuffer' in which the commands
    -- should be recorded. If @targetCommandBuffer@ is @NULL@ an implicit
    -- reservation as well as execution takes place on the processing
    -- 'Graphics.Vulkan.Core10.Handles.CommandBuffer'.
    targetCommandBuffer :: Ptr CommandBuffer_T
  , -- | @sequencesCountBuffer@ /can/ be 'Graphics.Vulkan.Core10.Handles.Buffer'
    -- from which the actual amount of sequences is sourced from as @uint32_t@
    -- value.
    sequencesCountBuffer :: Buffer
  , -- | @sequencesCountOffset@ is the byte offset into @sequencesCountBuffer@
    -- where the count value is stored.
    sequencesCountOffset :: DeviceSize
  , -- | @sequencesIndexBuffer@ /must/ be set if @indirectCommandsLayout@’s
    -- 'INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX' is set and
    -- provides the used sequence indices as @uint32_t@ array. Otherwise it
    -- /must/ be 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE'.
    sequencesIndexBuffer :: Buffer
  , -- | @sequencesIndexOffset@ is the byte offset into @sequencesIndexBuffer@
    -- where the index values start.
    sequencesIndexOffset :: DeviceSize
  }
  deriving (Typeable)
deriving instance Show CmdProcessCommandsInfoNVX

instance ToCStruct CmdProcessCommandsInfoNVX where
  withCStruct x f = allocaBytesAligned 96 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CmdProcessCommandsInfoNVX{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr ObjectTableNVX)) (objectTable)
    lift $ poke ((p `plusPtr` 24 :: Ptr IndirectCommandsLayoutNVX)) (indirectCommandsLayout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (indirectCommandsTokens)) :: Word32))
    pPIndirectCommandsTokens' <- ContT $ allocaBytesAligned @IndirectCommandsTokenNVX ((Data.Vector.length (indirectCommandsTokens)) * 24) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPIndirectCommandsTokens' `plusPtr` (24 * (i)) :: Ptr IndirectCommandsTokenNVX) (e) . ($ ())) (indirectCommandsTokens)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr IndirectCommandsTokenNVX))) (pPIndirectCommandsTokens')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (maxSequencesCount)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr CommandBuffer_T))) (targetCommandBuffer)
    lift $ poke ((p `plusPtr` 64 :: Ptr Buffer)) (sequencesCountBuffer)
    lift $ poke ((p `plusPtr` 72 :: Ptr DeviceSize)) (sequencesCountOffset)
    lift $ poke ((p `plusPtr` 80 :: Ptr Buffer)) (sequencesIndexBuffer)
    lift $ poke ((p `plusPtr` 88 :: Ptr DeviceSize)) (sequencesIndexOffset)
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr ObjectTableNVX)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr IndirectCommandsLayoutNVX)) (zero)
    pPIndirectCommandsTokens' <- ContT $ allocaBytesAligned @IndirectCommandsTokenNVX ((Data.Vector.length (mempty)) * 24) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPIndirectCommandsTokens' `plusPtr` (24 * (i)) :: Ptr IndirectCommandsTokenNVX) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr IndirectCommandsTokenNVX))) (pPIndirectCommandsTokens')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    lift $ f

instance FromCStruct CmdProcessCommandsInfoNVX where
  peekCStruct p = do
    objectTable <- peek @ObjectTableNVX ((p `plusPtr` 16 :: Ptr ObjectTableNVX))
    indirectCommandsLayout <- peek @IndirectCommandsLayoutNVX ((p `plusPtr` 24 :: Ptr IndirectCommandsLayoutNVX))
    indirectCommandsTokenCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pIndirectCommandsTokens <- peek @(Ptr IndirectCommandsTokenNVX) ((p `plusPtr` 40 :: Ptr (Ptr IndirectCommandsTokenNVX)))
    pIndirectCommandsTokens' <- generateM (fromIntegral indirectCommandsTokenCount) (\i -> peekCStruct @IndirectCommandsTokenNVX ((pIndirectCommandsTokens `advancePtrBytes` (24 * (i)) :: Ptr IndirectCommandsTokenNVX)))
    maxSequencesCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    targetCommandBuffer <- peek @(Ptr CommandBuffer_T) ((p `plusPtr` 56 :: Ptr (Ptr CommandBuffer_T)))
    sequencesCountBuffer <- peek @Buffer ((p `plusPtr` 64 :: Ptr Buffer))
    sequencesCountOffset <- peek @DeviceSize ((p `plusPtr` 72 :: Ptr DeviceSize))
    sequencesIndexBuffer <- peek @Buffer ((p `plusPtr` 80 :: Ptr Buffer))
    sequencesIndexOffset <- peek @DeviceSize ((p `plusPtr` 88 :: Ptr DeviceSize))
    pure $ CmdProcessCommandsInfoNVX
             objectTable indirectCommandsLayout pIndirectCommandsTokens' maxSequencesCount targetCommandBuffer sequencesCountBuffer sequencesCountOffset sequencesIndexBuffer sequencesIndexOffset

instance Zero CmdProcessCommandsInfoNVX where
  zero = CmdProcessCommandsInfoNVX
           zero
           zero
           mempty
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkCmdReserveSpaceForCommandsInfoNVX - Structure specifying parameters
-- for the reservation of command buffer space
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @objectTable@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.ObjectTableNVX' handle
--
-- -   @indirectCommandsLayout@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.IndirectCommandsLayoutNVX'
--     handle
--
-- -   Both of @indirectCommandsLayout@, and @objectTable@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @objectTable@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.Handles.IndirectCommandsLayoutNVX',
-- 'Graphics.Vulkan.Extensions.Handles.ObjectTableNVX',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdReserveSpaceForCommandsNVX'
data CmdReserveSpaceForCommandsInfoNVX = CmdReserveSpaceForCommandsInfoNVX
  { -- | @objectTable@ is the 'Graphics.Vulkan.Extensions.Handles.ObjectTableNVX'
    -- to be used for the generation process. Only registered objects at the
    -- time 'cmdReserveSpaceForCommandsNVX' is called, will be taken into
    -- account for the reservation.
    objectTable :: ObjectTableNVX
  , -- | @indirectCommandsLayout@ is the
    -- 'Graphics.Vulkan.Extensions.Handles.IndirectCommandsLayoutNVX' that
    -- /must/ also be used at generation time.
    indirectCommandsLayout :: IndirectCommandsLayoutNVX
  , -- | @maxSequencesCount@ is the maximum number of sequences for which command
    -- buffer space will be reserved.
    maxSequencesCount :: Word32
  }
  deriving (Typeable)
deriving instance Show CmdReserveSpaceForCommandsInfoNVX

instance ToCStruct CmdReserveSpaceForCommandsInfoNVX where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CmdReserveSpaceForCommandsInfoNVX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ObjectTableNVX)) (objectTable)
    poke ((p `plusPtr` 24 :: Ptr IndirectCommandsLayoutNVX)) (indirectCommandsLayout)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (maxSequencesCount)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ObjectTableNVX)) (zero)
    poke ((p `plusPtr` 24 :: Ptr IndirectCommandsLayoutNVX)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    f

instance FromCStruct CmdReserveSpaceForCommandsInfoNVX where
  peekCStruct p = do
    objectTable <- peek @ObjectTableNVX ((p `plusPtr` 16 :: Ptr ObjectTableNVX))
    indirectCommandsLayout <- peek @IndirectCommandsLayoutNVX ((p `plusPtr` 24 :: Ptr IndirectCommandsLayoutNVX))
    maxSequencesCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pure $ CmdReserveSpaceForCommandsInfoNVX
             objectTable indirectCommandsLayout maxSequencesCount

instance Storable CmdReserveSpaceForCommandsInfoNVX where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CmdReserveSpaceForCommandsInfoNVX where
  zero = CmdReserveSpaceForCommandsInfoNVX
           zero
           zero
           zero


-- | VkObjectTableCreateInfoNVX - Structure specifying the parameters of a
-- newly created object table
--
-- == Valid Usage
--
-- -   If the
--     'DeviceGeneratedCommandsFeaturesNVX'::@computeBindingPointSupport@
--     feature is not enabled, @pObjectEntryUsageFlags@ /must/ not contain
--     'OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX'
--
-- -   Any value within @pObjectEntryCounts@ /must/ not exceed
--     'DeviceGeneratedCommandsLimitsNVX'::@maxObjectEntryCounts@
--
-- -   @maxUniformBuffersPerDescriptor@ /must/ be within the limits
--     supported by the device.
--
-- -   @maxStorageBuffersPerDescriptor@ /must/ be within the limits
--     supported by the device.
--
-- -   @maxStorageImagesPerDescriptor@ /must/ be within the limits
--     supported by the device.
--
-- -   @maxSampledImagesPerDescriptor@ /must/ be within the limits
--     supported by the device.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @pObjectEntryTypes@ /must/ be a valid pointer to an array of
--     @objectCount@ valid 'ObjectEntryTypeNVX' values
--
-- -   @pObjectEntryCounts@ /must/ be a valid pointer to an array of
--     @objectCount@ @uint32_t@ values
--
-- -   @pObjectEntryUsageFlags@ /must/ be a valid pointer to an array of
--     @objectCount@ valid combinations of 'ObjectEntryUsageFlagBitsNVX'
--     values
--
-- -   Each element of @pObjectEntryUsageFlags@ /must/ not be @0@
--
-- -   @objectCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'ObjectEntryTypeNVX', 'ObjectEntryUsageFlagsNVX',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createObjectTableNVX'
data ObjectTableCreateInfoNVX = ObjectTableCreateInfoNVX
  { -- | @pObjectEntryTypes@ is a pointer to an array of 'ObjectEntryTypeNVX'
    -- values providing the entry type of a given configuration.
    objectEntryTypes :: Vector ObjectEntryTypeNVX
  , -- | @pObjectEntryCounts@ is a pointer to an array of counts of how many
    -- objects can be registered in the table.
    objectEntryCounts :: Vector Word32
  , -- | @pObjectEntryUsageFlags@ is a pointer to an array of bitmasks of
    -- 'ObjectEntryUsageFlagBitsNVX' specifying the binding usage of the entry.
    objectEntryUsageFlags :: Vector ObjectEntryUsageFlagsNVX
  , -- | @maxUniformBuffersPerDescriptor@ is the maximum number of
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'
    -- or
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
    -- used by any single registered
    -- 'Graphics.Vulkan.Core10.Handles.DescriptorSet' in this table.
    maxUniformBuffersPerDescriptor :: Word32
  , -- | @maxStorageBuffersPerDescriptor@ is the maximum number of
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'
    -- or
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
    -- used by any single registered
    -- 'Graphics.Vulkan.Core10.Handles.DescriptorSet' in this table.
    maxStorageBuffersPerDescriptor :: Word32
  , -- | @maxStorageImagesPerDescriptor@ is the maximum number of
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'
    -- or
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
    -- used by any single registered
    -- 'Graphics.Vulkan.Core10.Handles.DescriptorSet' in this table.
    maxStorageImagesPerDescriptor :: Word32
  , -- | @maxSampledImagesPerDescriptor@ is the maximum number of
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER',
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
    -- or
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
    -- used by any single registered
    -- 'Graphics.Vulkan.Core10.Handles.DescriptorSet' in this table.
    maxSampledImagesPerDescriptor :: Word32
  , -- | @maxPipelineLayouts@ is the maximum number of unique
    -- 'Graphics.Vulkan.Core10.Handles.PipelineLayout' used by any registered
    -- 'Graphics.Vulkan.Core10.Handles.DescriptorSet' or
    -- 'Graphics.Vulkan.Core10.Handles.Pipeline' in this table.
    maxPipelineLayouts :: Word32
  }
  deriving (Typeable)
deriving instance Show ObjectTableCreateInfoNVX

instance ToCStruct ObjectTableCreateInfoNVX where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ObjectTableCreateInfoNVX{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    let pObjectEntryTypesLength = Data.Vector.length $ (objectEntryTypes)
    let pObjectEntryCountsLength = Data.Vector.length $ (objectEntryCounts)
    lift $ unless (pObjectEntryCountsLength == pObjectEntryTypesLength) $
      throwIO $ IOError Nothing InvalidArgument "" "pObjectEntryCounts and pObjectEntryTypes must have the same length" Nothing Nothing
    let pObjectEntryUsageFlagsLength = Data.Vector.length $ (objectEntryUsageFlags)
    lift $ unless (pObjectEntryUsageFlagsLength == pObjectEntryTypesLength) $
      throwIO $ IOError Nothing InvalidArgument "" "pObjectEntryUsageFlags and pObjectEntryTypes must have the same length" Nothing Nothing
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral pObjectEntryTypesLength :: Word32))
    pPObjectEntryTypes' <- ContT $ allocaBytesAligned @ObjectEntryTypeNVX ((Data.Vector.length (objectEntryTypes)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPObjectEntryTypes' `plusPtr` (4 * (i)) :: Ptr ObjectEntryTypeNVX) (e)) (objectEntryTypes)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ObjectEntryTypeNVX))) (pPObjectEntryTypes')
    pPObjectEntryCounts' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (objectEntryCounts)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPObjectEntryCounts' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (objectEntryCounts)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Word32))) (pPObjectEntryCounts')
    pPObjectEntryUsageFlags' <- ContT $ allocaBytesAligned @ObjectEntryUsageFlagsNVX ((Data.Vector.length (objectEntryUsageFlags)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPObjectEntryUsageFlags' `plusPtr` (4 * (i)) :: Ptr ObjectEntryUsageFlagsNVX) (e)) (objectEntryUsageFlags)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr ObjectEntryUsageFlagsNVX))) (pPObjectEntryUsageFlags')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (maxUniformBuffersPerDescriptor)
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (maxStorageBuffersPerDescriptor)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (maxStorageImagesPerDescriptor)
    lift $ poke ((p `plusPtr` 60 :: Ptr Word32)) (maxSampledImagesPerDescriptor)
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) (maxPipelineLayouts)
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPObjectEntryTypes' <- ContT $ allocaBytesAligned @ObjectEntryTypeNVX ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPObjectEntryTypes' `plusPtr` (4 * (i)) :: Ptr ObjectEntryTypeNVX) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ObjectEntryTypeNVX))) (pPObjectEntryTypes')
    pPObjectEntryCounts' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPObjectEntryCounts' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Word32))) (pPObjectEntryCounts')
    pPObjectEntryUsageFlags' <- ContT $ allocaBytesAligned @ObjectEntryUsageFlagsNVX ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPObjectEntryUsageFlags' `plusPtr` (4 * (i)) :: Ptr ObjectEntryUsageFlagsNVX) (e)) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr ObjectEntryUsageFlagsNVX))) (pPObjectEntryUsageFlags')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 60 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) (zero)
    lift $ f

instance FromCStruct ObjectTableCreateInfoNVX where
  peekCStruct p = do
    objectCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pObjectEntryTypes <- peek @(Ptr ObjectEntryTypeNVX) ((p `plusPtr` 24 :: Ptr (Ptr ObjectEntryTypeNVX)))
    pObjectEntryTypes' <- generateM (fromIntegral objectCount) (\i -> peek @ObjectEntryTypeNVX ((pObjectEntryTypes `advancePtrBytes` (4 * (i)) :: Ptr ObjectEntryTypeNVX)))
    pObjectEntryCounts <- peek @(Ptr Word32) ((p `plusPtr` 32 :: Ptr (Ptr Word32)))
    pObjectEntryCounts' <- generateM (fromIntegral objectCount) (\i -> peek @Word32 ((pObjectEntryCounts `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pObjectEntryUsageFlags <- peek @(Ptr ObjectEntryUsageFlagsNVX) ((p `plusPtr` 40 :: Ptr (Ptr ObjectEntryUsageFlagsNVX)))
    pObjectEntryUsageFlags' <- generateM (fromIntegral objectCount) (\i -> peek @ObjectEntryUsageFlagsNVX ((pObjectEntryUsageFlags `advancePtrBytes` (4 * (i)) :: Ptr ObjectEntryUsageFlagsNVX)))
    maxUniformBuffersPerDescriptor <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    maxStorageBuffersPerDescriptor <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    maxStorageImagesPerDescriptor <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    maxSampledImagesPerDescriptor <- peek @Word32 ((p `plusPtr` 60 :: Ptr Word32))
    maxPipelineLayouts <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    pure $ ObjectTableCreateInfoNVX
             pObjectEntryTypes' pObjectEntryCounts' pObjectEntryUsageFlags' maxUniformBuffersPerDescriptor maxStorageBuffersPerDescriptor maxStorageImagesPerDescriptor maxSampledImagesPerDescriptor maxPipelineLayouts

instance Zero ObjectTableCreateInfoNVX where
  zero = ObjectTableCreateInfoNVX
           mempty
           mempty
           mempty
           zero
           zero
           zero
           zero
           zero


-- | VkObjectTableEntryNVX - Common parameters of an object table resource
-- entry
--
-- == Valid Usage
--
-- -   If the
--     'DeviceGeneratedCommandsFeaturesNVX'::@computeBindingPointSupport@
--     feature is not enabled, @flags@ /must/ not contain
--     'OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX'
--
-- == Valid Usage (Implicit)
--
-- -   @type@ /must/ be a valid 'ObjectEntryTypeNVX' value
--
-- -   @flags@ /must/ be a valid combination of
--     'ObjectEntryUsageFlagBitsNVX' values
--
-- -   @flags@ /must/ not be @0@
--
-- = See Also
--
-- 'ObjectEntryTypeNVX', 'ObjectEntryUsageFlagsNVX', 'registerObjectsNVX'
data ObjectTableEntryNVX = ObjectTableEntryNVX
  { -- | @type@ defines the entry type
    type' :: ObjectEntryTypeNVX
  , -- | @flags@ defines which
    -- 'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' the
    -- resource can be used with. Some entry types allow only a single flag to
    -- be set.
    flags :: ObjectEntryUsageFlagsNVX
  }
  deriving (Typeable)
deriving instance Show ObjectTableEntryNVX

instance ToCStruct ObjectTableEntryNVX where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ObjectTableEntryNVX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ObjectEntryTypeNVX)) (type')
    poke ((p `plusPtr` 4 :: Ptr ObjectEntryUsageFlagsNVX)) (flags)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ObjectEntryTypeNVX)) (zero)
    poke ((p `plusPtr` 4 :: Ptr ObjectEntryUsageFlagsNVX)) (zero)
    f

instance FromCStruct ObjectTableEntryNVX where
  peekCStruct p = do
    type' <- peek @ObjectEntryTypeNVX ((p `plusPtr` 0 :: Ptr ObjectEntryTypeNVX))
    flags <- peek @ObjectEntryUsageFlagsNVX ((p `plusPtr` 4 :: Ptr ObjectEntryUsageFlagsNVX))
    pure $ ObjectTableEntryNVX
             type' flags

instance Storable ObjectTableEntryNVX where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ObjectTableEntryNVX where
  zero = ObjectTableEntryNVX
           zero
           zero


-- | VkObjectTablePipelineEntryNVX - Parameters of an object table pipeline
-- entry
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'ObjectEntryTypeNVX', 'ObjectEntryUsageFlagsNVX',
-- 'Graphics.Vulkan.Core10.Handles.Pipeline'
data ObjectTablePipelineEntryNVX = ObjectTablePipelineEntryNVX
  { -- | @type@ /must/ be a valid 'ObjectEntryTypeNVX' value
    type' :: ObjectEntryTypeNVX
  , -- | @flags@ /must/ not be @0@
    flags :: ObjectEntryUsageFlagsNVX
  , -- | @pipeline@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Pipeline'
    -- handle
    pipeline :: Pipeline
  }
  deriving (Typeable)
deriving instance Show ObjectTablePipelineEntryNVX

instance ToCStruct ObjectTablePipelineEntryNVX where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ObjectTablePipelineEntryNVX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ObjectEntryTypeNVX)) (type')
    poke ((p `plusPtr` 4 :: Ptr ObjectEntryUsageFlagsNVX)) (flags)
    poke ((p `plusPtr` 8 :: Ptr Pipeline)) (pipeline)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ObjectEntryTypeNVX)) (zero)
    poke ((p `plusPtr` 4 :: Ptr ObjectEntryUsageFlagsNVX)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Pipeline)) (zero)
    f

instance FromCStruct ObjectTablePipelineEntryNVX where
  peekCStruct p = do
    type' <- peek @ObjectEntryTypeNVX ((p `plusPtr` 0 :: Ptr ObjectEntryTypeNVX))
    flags <- peek @ObjectEntryUsageFlagsNVX ((p `plusPtr` 4 :: Ptr ObjectEntryUsageFlagsNVX))
    pipeline <- peek @Pipeline ((p `plusPtr` 8 :: Ptr Pipeline))
    pure $ ObjectTablePipelineEntryNVX
             type' flags pipeline

instance Storable ObjectTablePipelineEntryNVX where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ObjectTablePipelineEntryNVX where
  zero = ObjectTablePipelineEntryNVX
           zero
           zero
           zero


-- | VkObjectTableDescriptorSetEntryNVX - Parameters of an object table
-- descriptor set entry
--
-- == Valid Usage
--
-- -   @type@ /must/ be 'OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX'
--
-- == Valid Usage (Implicit)
--
-- -   @type@ /must/ be a valid 'ObjectEntryTypeNVX' value
--
-- -   @flags@ /must/ be a valid combination of
--     'ObjectEntryUsageFlagBitsNVX' values
--
-- -   @flags@ /must/ not be @0@
--
-- -   @pipelineLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   @descriptorSet@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.DescriptorSet' handle
--
-- -   Both of @descriptorSet@, and @pipelineLayout@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.DescriptorSet', 'ObjectEntryTypeNVX',
-- 'ObjectEntryUsageFlagsNVX',
-- 'Graphics.Vulkan.Core10.Handles.PipelineLayout'
data ObjectTableDescriptorSetEntryNVX = ObjectTableDescriptorSetEntryNVX
  { -- No documentation found for Nested "VkObjectTableDescriptorSetEntryNVX" "type"
    type' :: ObjectEntryTypeNVX
  , -- No documentation found for Nested "VkObjectTableDescriptorSetEntryNVX" "flags"
    flags :: ObjectEntryUsageFlagsNVX
  , -- | @pipelineLayout@ specifies the
    -- 'Graphics.Vulkan.Core10.Handles.PipelineLayout' that the @descriptorSet@
    -- is used with.
    pipelineLayout :: PipelineLayout
  , -- | @descriptorSet@ specifies the
    -- 'Graphics.Vulkan.Core10.Handles.DescriptorSet' that can be bound with
    -- this entry.
    descriptorSet :: DescriptorSet
  }
  deriving (Typeable)
deriving instance Show ObjectTableDescriptorSetEntryNVX

instance ToCStruct ObjectTableDescriptorSetEntryNVX where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ObjectTableDescriptorSetEntryNVX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ObjectEntryTypeNVX)) (type')
    poke ((p `plusPtr` 4 :: Ptr ObjectEntryUsageFlagsNVX)) (flags)
    poke ((p `plusPtr` 8 :: Ptr PipelineLayout)) (pipelineLayout)
    poke ((p `plusPtr` 16 :: Ptr DescriptorSet)) (descriptorSet)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ObjectEntryTypeNVX)) (zero)
    poke ((p `plusPtr` 4 :: Ptr ObjectEntryUsageFlagsNVX)) (zero)
    poke ((p `plusPtr` 8 :: Ptr PipelineLayout)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DescriptorSet)) (zero)
    f

instance FromCStruct ObjectTableDescriptorSetEntryNVX where
  peekCStruct p = do
    type' <- peek @ObjectEntryTypeNVX ((p `plusPtr` 0 :: Ptr ObjectEntryTypeNVX))
    flags <- peek @ObjectEntryUsageFlagsNVX ((p `plusPtr` 4 :: Ptr ObjectEntryUsageFlagsNVX))
    pipelineLayout <- peek @PipelineLayout ((p `plusPtr` 8 :: Ptr PipelineLayout))
    descriptorSet <- peek @DescriptorSet ((p `plusPtr` 16 :: Ptr DescriptorSet))
    pure $ ObjectTableDescriptorSetEntryNVX
             type' flags pipelineLayout descriptorSet

instance Storable ObjectTableDescriptorSetEntryNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ObjectTableDescriptorSetEntryNVX where
  zero = ObjectTableDescriptorSetEntryNVX
           zero
           zero
           zero
           zero


-- | VkObjectTableVertexBufferEntryNVX - Parameters of an object table vertex
-- buffer entry
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer', 'ObjectEntryTypeNVX',
-- 'ObjectEntryUsageFlagsNVX'
data ObjectTableVertexBufferEntryNVX = ObjectTableVertexBufferEntryNVX
  { -- | @type@ /must/ be a valid 'ObjectEntryTypeNVX' value
    type' :: ObjectEntryTypeNVX
  , -- | @flags@ /must/ not be @0@
    flags :: ObjectEntryUsageFlagsNVX
  , -- | @buffer@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Buffer'
    -- handle
    buffer :: Buffer
  }
  deriving (Typeable)
deriving instance Show ObjectTableVertexBufferEntryNVX

instance ToCStruct ObjectTableVertexBufferEntryNVX where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ObjectTableVertexBufferEntryNVX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ObjectEntryTypeNVX)) (type')
    poke ((p `plusPtr` 4 :: Ptr ObjectEntryUsageFlagsNVX)) (flags)
    poke ((p `plusPtr` 8 :: Ptr Buffer)) (buffer)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ObjectEntryTypeNVX)) (zero)
    poke ((p `plusPtr` 4 :: Ptr ObjectEntryUsageFlagsNVX)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Buffer)) (zero)
    f

instance FromCStruct ObjectTableVertexBufferEntryNVX where
  peekCStruct p = do
    type' <- peek @ObjectEntryTypeNVX ((p `plusPtr` 0 :: Ptr ObjectEntryTypeNVX))
    flags <- peek @ObjectEntryUsageFlagsNVX ((p `plusPtr` 4 :: Ptr ObjectEntryUsageFlagsNVX))
    buffer <- peek @Buffer ((p `plusPtr` 8 :: Ptr Buffer))
    pure $ ObjectTableVertexBufferEntryNVX
             type' flags buffer

instance Storable ObjectTableVertexBufferEntryNVX where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ObjectTableVertexBufferEntryNVX where
  zero = ObjectTableVertexBufferEntryNVX
           zero
           zero
           zero


-- | VkObjectTableIndexBufferEntryNVX - Parameters of an object table index
-- buffer entry
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Enums.IndexType.IndexType',
-- 'ObjectEntryTypeNVX', 'ObjectEntryUsageFlagsNVX'
data ObjectTableIndexBufferEntryNVX = ObjectTableIndexBufferEntryNVX
  { -- | @type@ /must/ be a valid 'ObjectEntryTypeNVX' value
    type' :: ObjectEntryTypeNVX
  , -- | @flags@ /must/ not be @0@
    flags :: ObjectEntryUsageFlagsNVX
  , -- | @buffer@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Buffer'
    -- handle
    buffer :: Buffer
  , -- | @indexType@ /must/ be a valid
    -- 'Graphics.Vulkan.Core10.Enums.IndexType.IndexType' value
    indexType :: IndexType
  }
  deriving (Typeable)
deriving instance Show ObjectTableIndexBufferEntryNVX

instance ToCStruct ObjectTableIndexBufferEntryNVX where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ObjectTableIndexBufferEntryNVX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ObjectEntryTypeNVX)) (type')
    poke ((p `plusPtr` 4 :: Ptr ObjectEntryUsageFlagsNVX)) (flags)
    poke ((p `plusPtr` 8 :: Ptr Buffer)) (buffer)
    poke ((p `plusPtr` 16 :: Ptr IndexType)) (indexType)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ObjectEntryTypeNVX)) (zero)
    poke ((p `plusPtr` 4 :: Ptr ObjectEntryUsageFlagsNVX)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 16 :: Ptr IndexType)) (zero)
    f

instance FromCStruct ObjectTableIndexBufferEntryNVX where
  peekCStruct p = do
    type' <- peek @ObjectEntryTypeNVX ((p `plusPtr` 0 :: Ptr ObjectEntryTypeNVX))
    flags <- peek @ObjectEntryUsageFlagsNVX ((p `plusPtr` 4 :: Ptr ObjectEntryUsageFlagsNVX))
    buffer <- peek @Buffer ((p `plusPtr` 8 :: Ptr Buffer))
    indexType <- peek @IndexType ((p `plusPtr` 16 :: Ptr IndexType))
    pure $ ObjectTableIndexBufferEntryNVX
             type' flags buffer indexType

instance Storable ObjectTableIndexBufferEntryNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ObjectTableIndexBufferEntryNVX where
  zero = ObjectTableIndexBufferEntryNVX
           zero
           zero
           zero
           zero


-- | VkObjectTablePushConstantEntryNVX - Parameters of an object table push
-- constant entry
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'ObjectEntryTypeNVX', 'ObjectEntryUsageFlagsNVX',
-- 'Graphics.Vulkan.Core10.Handles.PipelineLayout',
-- 'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags'
data ObjectTablePushConstantEntryNVX = ObjectTablePushConstantEntryNVX
  { -- | @type@ /must/ be a valid 'ObjectEntryTypeNVX' value
    type' :: ObjectEntryTypeNVX
  , -- | @flags@ /must/ not be @0@
    flags :: ObjectEntryUsageFlagsNVX
  , -- | @pipelineLayout@ /must/ be a valid
    -- 'Graphics.Vulkan.Core10.Handles.PipelineLayout' handle
    pipelineLayout :: PipelineLayout
  , -- | @stageFlags@ /must/ not be @0@
    stageFlags :: ShaderStageFlags
  }
  deriving (Typeable)
deriving instance Show ObjectTablePushConstantEntryNVX

instance ToCStruct ObjectTablePushConstantEntryNVX where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ObjectTablePushConstantEntryNVX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ObjectEntryTypeNVX)) (type')
    poke ((p `plusPtr` 4 :: Ptr ObjectEntryUsageFlagsNVX)) (flags)
    poke ((p `plusPtr` 8 :: Ptr PipelineLayout)) (pipelineLayout)
    poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (stageFlags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ObjectEntryTypeNVX)) (zero)
    poke ((p `plusPtr` 4 :: Ptr ObjectEntryUsageFlagsNVX)) (zero)
    poke ((p `plusPtr` 8 :: Ptr PipelineLayout)) (zero)
    poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (zero)
    f

instance FromCStruct ObjectTablePushConstantEntryNVX where
  peekCStruct p = do
    type' <- peek @ObjectEntryTypeNVX ((p `plusPtr` 0 :: Ptr ObjectEntryTypeNVX))
    flags <- peek @ObjectEntryUsageFlagsNVX ((p `plusPtr` 4 :: Ptr ObjectEntryUsageFlagsNVX))
    pipelineLayout <- peek @PipelineLayout ((p `plusPtr` 8 :: Ptr PipelineLayout))
    stageFlags <- peek @ShaderStageFlags ((p `plusPtr` 16 :: Ptr ShaderStageFlags))
    pure $ ObjectTablePushConstantEntryNVX
             type' flags pipelineLayout stageFlags

instance Storable ObjectTablePushConstantEntryNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ObjectTablePushConstantEntryNVX where
  zero = ObjectTablePushConstantEntryNVX
           zero
           zero
           zero
           zero


-- | VkIndirectCommandsLayoutUsageFlagBitsNVX - Bitmask specifying allowed
-- usage of an indirect commands layout
--
-- = See Also
--
-- 'IndirectCommandsLayoutUsageFlagsNVX'
newtype IndirectCommandsLayoutUsageFlagBitsNVX = IndirectCommandsLayoutUsageFlagBitsNVX Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX' specifies
-- that the processing of sequences /can/ happen at an
-- implementation-dependent order, which is not guaranteed to be coherent
-- across multiple invocations.
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX = IndirectCommandsLayoutUsageFlagBitsNVX 0x00000001
-- | 'INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX' specifies that
-- there is likely a high difference between allocated number of sequences
-- and actually used.
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX = IndirectCommandsLayoutUsageFlagBitsNVX 0x00000002
-- | 'INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX' specifies that
-- there are likely many draw or dispatch calls that are zero-sized (zero
-- grid dimension, no primitives to render).
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX = IndirectCommandsLayoutUsageFlagBitsNVX 0x00000004
-- | 'INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX' specifies
-- that the input data for the sequences is not implicitly indexed from
-- 0..sequencesUsed but a user provided
-- 'Graphics.Vulkan.Core10.Handles.Buffer' encoding the index is provided.
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX = IndirectCommandsLayoutUsageFlagBitsNVX 0x00000008

type IndirectCommandsLayoutUsageFlagsNVX = IndirectCommandsLayoutUsageFlagBitsNVX

instance Show IndirectCommandsLayoutUsageFlagBitsNVX where
  showsPrec p = \case
    INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX -> showString "INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX"
    INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX -> showString "INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX"
    INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX -> showString "INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX"
    INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX -> showString "INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX"
    IndirectCommandsLayoutUsageFlagBitsNVX x -> showParen (p >= 11) (showString "IndirectCommandsLayoutUsageFlagBitsNVX 0x" . showHex x)

instance Read IndirectCommandsLayoutUsageFlagBitsNVX where
  readPrec = parens (choose [("INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX", pure INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX)
                            , ("INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX", pure INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX)
                            , ("INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX", pure INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX)
                            , ("INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX", pure INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX)]
                     +++
                     prec 10 (do
                       expectP (Ident "IndirectCommandsLayoutUsageFlagBitsNVX")
                       v <- step readPrec
                       pure (IndirectCommandsLayoutUsageFlagBitsNVX v)))


-- | VkObjectEntryUsageFlagBitsNVX - Bitmask specifying allowed usage of an
-- object entry
--
-- = See Also
--
-- 'ObjectEntryUsageFlagsNVX'
newtype ObjectEntryUsageFlagBitsNVX = ObjectEntryUsageFlagBitsNVX Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX' specifies that the resource is
-- bound to
-- 'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
pattern OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX = ObjectEntryUsageFlagBitsNVX 0x00000001
-- | 'OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX' specifies that the resource is
-- bound to
-- 'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE'
pattern OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX = ObjectEntryUsageFlagBitsNVX 0x00000002

type ObjectEntryUsageFlagsNVX = ObjectEntryUsageFlagBitsNVX

instance Show ObjectEntryUsageFlagBitsNVX where
  showsPrec p = \case
    OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX -> showString "OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX"
    OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX -> showString "OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX"
    ObjectEntryUsageFlagBitsNVX x -> showParen (p >= 11) (showString "ObjectEntryUsageFlagBitsNVX 0x" . showHex x)

instance Read ObjectEntryUsageFlagBitsNVX where
  readPrec = parens (choose [("OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX", pure OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX)
                            , ("OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX", pure OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX)]
                     +++
                     prec 10 (do
                       expectP (Ident "ObjectEntryUsageFlagBitsNVX")
                       v <- step readPrec
                       pure (ObjectEntryUsageFlagBitsNVX v)))


-- | VkIndirectCommandsTokenTypeNVX - Enum specifying
--
-- = Description
--
-- \'
--
-- +---------------------------------------------------+-----------------------------------------------------------------------+
-- | Token type                                        | Equivalent command                                                    |
-- +===================================================+=======================================================================+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX'       | 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindPipeline'        |
-- +---------------------------------------------------+-----------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX' | 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets'  |
-- +---------------------------------------------------+-----------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX'   | 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'     |
-- +---------------------------------------------------+-----------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX'  | 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers'   |
-- +---------------------------------------------------+-----------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX'  | 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdPushConstants'       |
-- +---------------------------------------------------+-----------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX'   | 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect' |
-- +---------------------------------------------------+-----------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX'           | 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect'        |
-- +---------------------------------------------------+-----------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX'       | 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDispatchIndirect'    |
-- +---------------------------------------------------+-----------------------------------------------------------------------+
--
-- Supported indirect command tokens
--
-- = See Also
--
-- 'IndirectCommandsLayoutTokenNVX', 'IndirectCommandsTokenNVX'
newtype IndirectCommandsTokenTypeNVX = IndirectCommandsTokenTypeNVX Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX = IndirectCommandsTokenTypeNVX 0
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX = IndirectCommandsTokenTypeNVX 1
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX = IndirectCommandsTokenTypeNVX 2
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX = IndirectCommandsTokenTypeNVX 3
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX = IndirectCommandsTokenTypeNVX 4
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX = IndirectCommandsTokenTypeNVX 5
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX = IndirectCommandsTokenTypeNVX 6
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX = IndirectCommandsTokenTypeNVX 7
{-# complete INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX,
             INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX,
             INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX,
             INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX,
             INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX,
             INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX,
             INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX,
             INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX :: IndirectCommandsTokenTypeNVX #-}

instance Show IndirectCommandsTokenTypeNVX where
  showsPrec p = \case
    INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX -> showString "INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX"
    INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX -> showString "INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX"
    INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX -> showString "INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX"
    INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX -> showString "INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX"
    INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX -> showString "INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX"
    INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX -> showString "INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX"
    INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX -> showString "INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX"
    INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX -> showString "INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX"
    IndirectCommandsTokenTypeNVX x -> showParen (p >= 11) (showString "IndirectCommandsTokenTypeNVX " . showsPrec 11 x)

instance Read IndirectCommandsTokenTypeNVX where
  readPrec = parens (choose [("INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX", pure INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX)
                            , ("INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX", pure INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX)
                            , ("INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX", pure INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX)
                            , ("INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX", pure INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX)
                            , ("INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX", pure INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX)
                            , ("INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX", pure INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX)
                            , ("INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX", pure INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX)
                            , ("INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX", pure INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX)]
                     +++
                     prec 10 (do
                       expectP (Ident "IndirectCommandsTokenTypeNVX")
                       v <- step readPrec
                       pure (IndirectCommandsTokenTypeNVX v)))


-- | VkObjectEntryTypeNVX - Enum specifying object table entry type
--
-- = See Also
--
-- 'ObjectTableCreateInfoNVX', 'ObjectTableDescriptorSetEntryNVX',
-- 'ObjectTableEntryNVX', 'ObjectTableIndexBufferEntryNVX',
-- 'ObjectTablePipelineEntryNVX', 'ObjectTablePushConstantEntryNVX',
-- 'ObjectTableVertexBufferEntryNVX', 'unregisterObjectsNVX'
newtype ObjectEntryTypeNVX = ObjectEntryTypeNVX Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX' specifies a
-- 'Graphics.Vulkan.Core10.Handles.DescriptorSet' resource entry that is
-- registered via 'ObjectTableDescriptorSetEntryNVX'.
pattern OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX = ObjectEntryTypeNVX 0
-- | 'OBJECT_ENTRY_TYPE_PIPELINE_NVX' specifies a
-- 'Graphics.Vulkan.Core10.Handles.Pipeline' resource entry that is
-- registered via 'ObjectTablePipelineEntryNVX'.
pattern OBJECT_ENTRY_TYPE_PIPELINE_NVX = ObjectEntryTypeNVX 1
-- | 'OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX' specifies a
-- 'Graphics.Vulkan.Core10.Handles.Buffer' resource entry that is
-- registered via 'ObjectTableIndexBufferEntryNVX'.
pattern OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX = ObjectEntryTypeNVX 2
-- | 'OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX' specifies a
-- 'Graphics.Vulkan.Core10.Handles.Buffer' resource entry that is
-- registered via 'ObjectTableVertexBufferEntryNVX'.
pattern OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX = ObjectEntryTypeNVX 3
-- | 'OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX' specifies the resource entry is
-- registered via 'ObjectTablePushConstantEntryNVX'.
pattern OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX = ObjectEntryTypeNVX 4
{-# complete OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX,
             OBJECT_ENTRY_TYPE_PIPELINE_NVX,
             OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX,
             OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX,
             OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX :: ObjectEntryTypeNVX #-}

instance Show ObjectEntryTypeNVX where
  showsPrec p = \case
    OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX -> showString "OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX"
    OBJECT_ENTRY_TYPE_PIPELINE_NVX -> showString "OBJECT_ENTRY_TYPE_PIPELINE_NVX"
    OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX -> showString "OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX"
    OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX -> showString "OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX"
    OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX -> showString "OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX"
    ObjectEntryTypeNVX x -> showParen (p >= 11) (showString "ObjectEntryTypeNVX " . showsPrec 11 x)

instance Read ObjectEntryTypeNVX where
  readPrec = parens (choose [("OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX", pure OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX)
                            , ("OBJECT_ENTRY_TYPE_PIPELINE_NVX", pure OBJECT_ENTRY_TYPE_PIPELINE_NVX)
                            , ("OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX", pure OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX)
                            , ("OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX", pure OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX)
                            , ("OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX", pure OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX)]
                     +++
                     prec 10 (do
                       expectP (Ident "ObjectEntryTypeNVX")
                       v <- step readPrec
                       pure (ObjectEntryTypeNVX v)))


type NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION"
pattern NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION :: forall a . Integral a => a
pattern NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 3


type NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME = "VK_NVX_device_generated_commands"

-- No documentation found for TopLevel "VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME"
pattern NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME = "VK_NVX_device_generated_commands"

