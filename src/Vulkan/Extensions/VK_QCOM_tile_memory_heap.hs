{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_tile_memory_heap - device extension
--
-- = VK_QCOM_tile_memory_heap
--
-- [__Name String__]
--     @VK_QCOM_tile_memory_heap@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     548
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_memory_requirements2 VK_KHR_get_memory_requirements2>
--          and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_QCOM_tile_properties
--
-- [__Contact__]
--
--     -   Patrick Boyle
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_tile_memory_heap] @pboyleQCOM%0A*Here describe the issue or question you have about the VK_QCOM_tile_memory_heap extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_QCOM_tile_memory_heap.adoc VK_QCOM_tile_memory_heap>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-05-05
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with @VK_QCOM_tile_properties@
--
--     -   Interacts with @VK_QCOM_tile_shading@
--
-- [__Contributors__]
--
--     -   Patrick Boyle, Qualcomm Technologies, Inc.
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc.
--
--     -   Srihari Babu Alla, Qualcomm Technologies, Inc.
--
--     -   Kevin Matlage, Qualcomm Technologies, Inc.
--
-- == Description
--
-- This extension adds a new memory heap which allows applications to
-- allocate and manage tile memory. A tile memory heap is denoted by the
-- new
-- 'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_TILE_MEMORY_BIT_QCOM'
-- property. Memory contents within this heap behave differently than other
-- heaps and only persist its memory contents within a command buffer
-- submission batch boundary. This boundary may be extended to a queue
-- submit boundary by querying @queueSubmitBoundary@ in the new
-- 'PhysicalDeviceTileMemoryHeapPropertiesQCOM' structure.
--
-- Tile memory from this heap can be bound to VkImages or VkBuffers. The
-- following new usage flags
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TILE_MEMORY_BIT_QCOM',
-- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TILE_MEMORY_BIT_QCOM',
-- 'Vulkan.Core14.Enums.BufferUsageFlags2.BUFFER_USAGE_2_TILE_MEMORY_BIT_QCOM'
-- were added for this. A new extended structure is added to get memory
-- requirements for tile memory 'TileMemoryRequirementsQCOM'.
--
-- A new command is added to bind tile memory to a command buffer in order
-- to access and persist tile memory contents while executing commands
-- 'cmdBindTileMemoryQCOM'.
--
-- This extension can be used in combination with
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_properties VK_QCOM_tile_properties>
-- with the new structure 'TileMemorySizeInfoQCOM'.
--
-- == Issues
--
-- None.
--
-- == New Commands
--
-- -   'cmdBindTileMemoryQCOM'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo':
--
--     -   'TileMemoryBindInfoQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2':
--
--     -   'TileMemoryRequirementsQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceTileMemoryHeapFeaturesQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceTileMemoryHeapPropertiesQCOM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_properties VK_QCOM_tile_properties>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Pass.RenderPassCreateInfo',
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.RenderPassCreateInfo2',
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo':
--
--     -   'TileMemorySizeInfoQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_TILE_MEMORY_HEAP_EXTENSION_NAME'
--
-- -   'QCOM_TILE_MEMORY_HEAP_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TILE_MEMORY_BIT_QCOM'
--
-- -   Extending
--     'Vulkan.Core14.Enums.BufferUsageFlags2.BufferUsageFlagBits2':
--
--     -   'Vulkan.Core14.Enums.BufferUsageFlags2.BUFFER_USAGE_2_TILE_MEMORY_BIT_QCOM'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TILE_MEMORY_BIT_QCOM'
--
-- -   Extending
--     'Vulkan.Core10.Enums.MemoryHeapFlagBits.MemoryHeapFlagBits':
--
--     -   'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_TILE_MEMORY_BIT_QCOM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_MEMORY_HEAP_FEATURES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_MEMORY_HEAP_PROPERTIES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TILE_MEMORY_BIND_INFO_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TILE_MEMORY_REQUIREMENTS_QCOM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_properties VK_QCOM_tile_properties>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TILE_MEMORY_SIZE_INFO_QCOM'
--
-- == Version History
--
-- -   Revision 1, 2025-03-26 (Patrick Boyle)
--
--     -   Initial version
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_QCOM_tile_memory_heap Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_tile_memory_heap  ( cmdBindTileMemoryQCOM
                                                   , TileMemoryBindInfoQCOM(..)
                                                   , PhysicalDeviceTileMemoryHeapFeaturesQCOM(..)
                                                   , PhysicalDeviceTileMemoryHeapPropertiesQCOM(..)
                                                   , TileMemorySizeInfoQCOM(..)
                                                   , TileMemoryRequirementsQCOM(..)
                                                   , QCOM_TILE_MEMORY_HEAP_SPEC_VERSION
                                                   , pattern QCOM_TILE_MEMORY_HEAP_SPEC_VERSION
                                                   , QCOM_TILE_MEMORY_HEAP_EXTENSION_NAME
                                                   , pattern QCOM_TILE_MEMORY_HEAP_EXTENSION_NAME
                                                   ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindTileMemoryQCOM))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_MEMORY_HEAP_FEATURES_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_MEMORY_HEAP_PROPERTIES_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_TILE_MEMORY_BIND_INFO_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_TILE_MEMORY_REQUIREMENTS_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_TILE_MEMORY_SIZE_INFO_QCOM))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindTileMemoryQCOM
  :: FunPtr (Ptr CommandBuffer_T -> Ptr TileMemoryBindInfoQCOM -> IO ()) -> Ptr CommandBuffer_T -> Ptr TileMemoryBindInfoQCOM -> IO ()

-- | vkCmdBindTileMemoryQCOM - Bind tile memory to a command buffer
--
-- = Description
--
-- Calling 'cmdBindTileMemoryQCOM' when @pTileMemoryBindInfo@ is @NULL@ is
-- equivalent to binding no tile memory to the command buffer.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindTileMemoryQCOM-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindTileMemoryQCOM-pTileMemoryBindInfo-parameter# If
--     @pTileMemoryBindInfo@ is not @NULL@, @pTileMemoryBindInfo@ /must/ be
--     a valid pointer to a valid 'TileMemoryBindInfoQCOM' structure
--
-- -   #VUID-vkCmdBindTileMemoryQCOM-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindTileMemoryQCOM-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdBindTileMemoryQCOM-renderpass# This command /must/ only
--     be called outside of a render pass instance
--
-- -   #VUID-vkCmdBindTileMemoryQCOM-videocoding# This command /must/ only
--     be called outside of a video coding scope
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
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdBindTileMemoryQCOM is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_memory_heap VK_QCOM_tile_memory_heap>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'TileMemoryBindInfoQCOM'
cmdBindTileMemoryQCOM :: forall io
                       . (MonadIO io)
                      => -- | @commandBuffer@ is the command buffer that the tile memory will be bound
                         -- to.
                         CommandBuffer
                      -> -- | @pTileMemoryBindInfo@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a
                         -- pointer to a 'TileMemoryBindInfoQCOM' structure defining how tile memory
                         -- is bound.
                         ("tileMemoryBindInfo" ::: Maybe TileMemoryBindInfoQCOM)
                      -> io ()
cmdBindTileMemoryQCOM commandBuffer tileMemoryBindInfo = liftIO . evalContT $ do
  let vkCmdBindTileMemoryQCOMPtr = pVkCmdBindTileMemoryQCOM (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBindTileMemoryQCOMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindTileMemoryQCOM is null" Nothing Nothing
  let vkCmdBindTileMemoryQCOM' = mkVkCmdBindTileMemoryQCOM vkCmdBindTileMemoryQCOMPtr
  pTileMemoryBindInfo <- case (tileMemoryBindInfo) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkCmdBindTileMemoryQCOM" (vkCmdBindTileMemoryQCOM'
                                                       (commandBufferHandle (commandBuffer))
                                                       pTileMemoryBindInfo)
  pure $ ()


-- | VkTileMemoryBindInfoQCOM - Structure specifying tile memory to bind
--
-- = Description
--
-- @memory@ is used to bind this memory object to tile memory for all
-- subsequent commands in the @commandBuffer@. Tile memory contents for
-- ranges in the heap outside the bound @memory@ are discarded and become
-- undefined for the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-tile-heaps active tile memory scope>
-- if an action command is executed.
--
-- For secondary command buffers executing within a render pass instance,
-- the active bound tile memory object is provided with this structure
-- included in the @pNext@ chain of
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'.
--
-- If this structure was not specified since recording started for
-- @commandBuffer@, no tile memory is bound to the command buffer and all
-- contents become undefined for the /tile memory scope/ if an action
-- command is executed.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_memory_heap VK_QCOM_tile_memory_heap>,
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdBindTileMemoryQCOM'
data TileMemoryBindInfoQCOM = TileMemoryBindInfoQCOM
  { -- | @memory@ is the tile memory object to be bound.
    --
    -- #VUID-VkTileMemoryBindInfoQCOM-memory-10726# @memory@ /must/ have been
    -- allocated from a 'Vulkan.Core10.DeviceInitialization.MemoryHeap' with
    -- the
    -- 'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_TILE_MEMORY_BIT_QCOM'
    -- property
    --
    -- #VUID-VkTileMemoryBindInfoQCOM-memory-parameter# @memory@ /must/ be a
    -- valid 'Vulkan.Core10.Handles.DeviceMemory' handle
    memory :: DeviceMemory }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TileMemoryBindInfoQCOM)
#endif
deriving instance Show TileMemoryBindInfoQCOM

instance ToCStruct TileMemoryBindInfoQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TileMemoryBindInfoQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TILE_MEMORY_BIND_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (memory)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TILE_MEMORY_BIND_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (zero)
    f

instance FromCStruct TileMemoryBindInfoQCOM where
  peekCStruct p = do
    memory <- peek @DeviceMemory ((p `plusPtr` 16 :: Ptr DeviceMemory))
    pure $ TileMemoryBindInfoQCOM
             memory

instance Storable TileMemoryBindInfoQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero TileMemoryBindInfoQCOM where
  zero = TileMemoryBindInfoQCOM
           zero


-- | VkPhysicalDeviceTileMemoryHeapFeaturesQCOM - Structure describing
-- whether the tile memory heap features can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceTileMemoryHeapFeaturesQCOM' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceTileMemoryHeapFeaturesQCOM', it /must/ add an instance of
-- the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_memory_heap VK_QCOM_tile_memory_heap>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceTileMemoryHeapFeaturesQCOM = PhysicalDeviceTileMemoryHeapFeaturesQCOM
  { -- | #features-tileMemoryHeap# @tileMemoryHeap@ indicates whether the
    -- implementation supports tile memory heap functionality.
    tileMemoryHeap :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTileMemoryHeapFeaturesQCOM)
#endif
deriving instance Show PhysicalDeviceTileMemoryHeapFeaturesQCOM

instance ToCStruct PhysicalDeviceTileMemoryHeapFeaturesQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTileMemoryHeapFeaturesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_MEMORY_HEAP_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (tileMemoryHeap))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_MEMORY_HEAP_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTileMemoryHeapFeaturesQCOM where
  peekCStruct p = do
    tileMemoryHeap <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceTileMemoryHeapFeaturesQCOM
             (bool32ToBool tileMemoryHeap)

instance Storable PhysicalDeviceTileMemoryHeapFeaturesQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTileMemoryHeapFeaturesQCOM where
  zero = PhysicalDeviceTileMemoryHeapFeaturesQCOM
           zero


-- | VkPhysicalDeviceTileMemoryHeapPropertiesQCOM - Structure describing tile
-- memory heap properties that can be supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceTileMemoryHeapPropertiesQCOM' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_memory_heap VK_QCOM_tile_memory_heap>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceTileMemoryHeapPropertiesQCOM = PhysicalDeviceTileMemoryHeapPropertiesQCOM
  { -- | #limits-queueSubmitBoundary# @queueSubmitBoundary@ is a boolean
    -- describing if tile memory becomes undefined at a queue submit boundary
    -- instead of the default command buffer submission batch boundary.
    queueSubmitBoundary :: Bool
  , -- | #limits-tileBufferTransfers# @tileBufferTransfers@ is a boolean
    -- describing if buffers bound to tile memory support transfer operations.
    tileBufferTransfers :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTileMemoryHeapPropertiesQCOM)
#endif
deriving instance Show PhysicalDeviceTileMemoryHeapPropertiesQCOM

instance ToCStruct PhysicalDeviceTileMemoryHeapPropertiesQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTileMemoryHeapPropertiesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_MEMORY_HEAP_PROPERTIES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (queueSubmitBoundary))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (tileBufferTransfers))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_MEMORY_HEAP_PROPERTIES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTileMemoryHeapPropertiesQCOM where
  peekCStruct p = do
    queueSubmitBoundary <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    tileBufferTransfers <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceTileMemoryHeapPropertiesQCOM
             (bool32ToBool queueSubmitBoundary)
             (bool32ToBool tileBufferTransfers)

instance Storable PhysicalDeviceTileMemoryHeapPropertiesQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTileMemoryHeapPropertiesQCOM where
  zero = PhysicalDeviceTileMemoryHeapPropertiesQCOM
           zero
           zero


-- | VkTileMemorySizeInfoQCOM - Structure describing tile memory size in use
-- in a render pass instance
--
-- = Description
--
-- The returned tile properties are invalid if the @size@ is not equal to
-- the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-bind-tile-memory bound tile memory’s>
-- allocation size when the render pass is executed.
--
-- If this structure is not provided, the @size@ of the reserved region
-- defaults to @0@.
--
-- Tile memory is reserved for application use by binding tile memory
-- objects to the command buffer.
--
-- The size provided by this command is informational only for use when
-- evaluating tile properties. If the application does not need to query
-- the tile properties, then this size /can/ be safely omitted.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_memory_heap VK_QCOM_tile_memory_heap>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_properties VK_QCOM_tile_properties>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data TileMemorySizeInfoQCOM = TileMemorySizeInfoQCOM
  { -- | @size@ is the size in bytes of tile memory used by the render pass or
    -- preserved for later use.
    --
    -- #VUID-VkTileMemorySizeInfoQCOM-size-10729# @size@ must be less than or
    -- equal to the largest size memory heap with the
    -- 'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_TILE_MEMORY_BIT_QCOM'
    -- property
    size :: DeviceSize }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TileMemorySizeInfoQCOM)
#endif
deriving instance Show TileMemorySizeInfoQCOM

instance ToCStruct TileMemorySizeInfoQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TileMemorySizeInfoQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TILE_MEMORY_SIZE_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (size)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TILE_MEMORY_SIZE_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct TileMemorySizeInfoQCOM where
  peekCStruct p = do
    size <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ TileMemorySizeInfoQCOM
             size

instance Storable TileMemorySizeInfoQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero TileMemorySizeInfoQCOM where
  zero = TileMemorySizeInfoQCOM
           zero


-- | VkTileMemoryRequirementsQCOM - Structure specifying tile memory
-- requirements
--
-- = Description
--
-- The @size@ and @alignment@ /must/ be used when the resource is bound to
-- a 'Vulkan.Core10.Handles.DeviceMemory' object that was allocated from a
-- 'Vulkan.Core10.DeviceInitialization.MemoryType' that has a @heapIndex@
-- that corresponds to a 'Vulkan.Core10.DeviceInitialization.MemoryHeap'
-- with the
-- 'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_TILE_MEMORY_BIT_QCOM'
-- property.
--
-- If the resource cannot be bound to tile memory, then @size@ and
-- @alignment@ is filled with zero by the implementation.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_memory_heap VK_QCOM_tile_memory_heap>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data TileMemoryRequirementsQCOM = TileMemoryRequirementsQCOM
  { -- | @size@ is the size, in bytes, of the tile memory allocation required for
    -- the resource.
    size :: DeviceSize
  , -- | @alignment@ is the alignment, in bytes, of the offset within the tile
    -- memory allocation required for the resource.
    alignment :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TileMemoryRequirementsQCOM)
#endif
deriving instance Show TileMemoryRequirementsQCOM

instance ToCStruct TileMemoryRequirementsQCOM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TileMemoryRequirementsQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TILE_MEMORY_REQUIREMENTS_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (size)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (alignment)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TILE_MEMORY_REQUIREMENTS_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct TileMemoryRequirementsQCOM where
  peekCStruct p = do
    size <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    alignment <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    pure $ TileMemoryRequirementsQCOM
             size alignment

instance Storable TileMemoryRequirementsQCOM where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero TileMemoryRequirementsQCOM where
  zero = TileMemoryRequirementsQCOM
           zero
           zero


type QCOM_TILE_MEMORY_HEAP_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_QCOM_TILE_MEMORY_HEAP_SPEC_VERSION"
pattern QCOM_TILE_MEMORY_HEAP_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_TILE_MEMORY_HEAP_SPEC_VERSION = 1


type QCOM_TILE_MEMORY_HEAP_EXTENSION_NAME = "VK_QCOM_tile_memory_heap"

-- No documentation found for TopLevel "VK_QCOM_TILE_MEMORY_HEAP_EXTENSION_NAME"
pattern QCOM_TILE_MEMORY_HEAP_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_TILE_MEMORY_HEAP_EXTENSION_NAME = "VK_QCOM_tile_memory_heap"

