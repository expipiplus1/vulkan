{-# language CPP #-}
-- | = Name
--
-- VK_NV_memory_decompression - device extension
--
-- = VK_NV_memory_decompression
--
-- [__Name String__]
--     @VK_NV_memory_decompression@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     428
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--              or
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--          and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Vulkan Version 1.2>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_memory_decompression VK_EXT_memory_decompression>
--         extension
--
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_memory_decompression] @vkushwaha-nv%0A*Here describe the issue or question you have about the VK_NV_memory_decompression extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-01-31
--
-- [__Contributors__]
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
-- == Description
--
-- This extension adds support for performing memory to memory
-- decompression.
--
-- == New Commands
--
-- -   'cmdDecompressMemoryIndirectCountNV'
--
-- -   'cmdDecompressMemoryNV'
--
-- == New Structures
--
-- -   'DecompressMemoryRegionNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMemoryDecompressionFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMemoryDecompressionPropertiesNV'
--
-- == New Enums
--
-- -   'MemoryDecompressionMethodFlagBitsNV'
--
-- == New Bitmasks
--
-- -   'MemoryDecompressionMethodFlagsNV'
--
-- == New Enum Constants
--
-- -   'NV_MEMORY_DECOMPRESSION_EXTENSION_NAME'
--
-- -   'NV_MEMORY_DECOMPRESSION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_FEATURES_NV'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_PROPERTIES_NV'
--
-- == Version History
--
-- -   Revision 1, 2022-01-31 (Vikram Kushwaha)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_memory_decompression Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_memory_decompression  ( cmdDecompressMemoryNV
                                                     , cmdDecompressMemoryIndirectCountNV
                                                     , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_FEATURES_NV
                                                     , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_PROPERTIES_NV
                                                     , DecompressMemoryRegionNV(..)
                                                     , MemoryDecompressionMethodFlagsNV
                                                     , MemoryDecompressionMethodFlagBitsNV
                                                     , PhysicalDeviceMemoryDecompressionFeaturesNV
                                                     , PhysicalDeviceMemoryDecompressionPropertiesNV
                                                     , NV_MEMORY_DECOMPRESSION_SPEC_VERSION
                                                     , pattern NV_MEMORY_DECOMPRESSION_SPEC_VERSION
                                                     , NV_MEMORY_DECOMPRESSION_EXTENSION_NAME
                                                     , pattern NV_MEMORY_DECOMPRESSION_EXTENSION_NAME
                                                     , PhysicalDeviceMemoryDecompressionFeaturesEXT(..)
                                                     , PhysicalDeviceMemoryDecompressionPropertiesEXT(..)
                                                     , MemoryDecompressionMethodFlagBitsEXT(..)
                                                     , MemoryDecompressionMethodFlagsEXT
                                                     ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkCmdDecompressMemoryIndirectCountNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDecompressMemoryNV))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Extensions.VK_EXT_memory_decompression (MemoryDecompressionMethodFlagBitsEXT)
import Vulkan.Extensions.VK_EXT_memory_decompression (MemoryDecompressionMethodFlagsEXT)
import Vulkan.Extensions.VK_EXT_memory_decompression (PhysicalDeviceMemoryDecompressionFeaturesEXT)
import Vulkan.Extensions.VK_EXT_memory_decompression (PhysicalDeviceMemoryDecompressionPropertiesEXT)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_PROPERTIES_EXT))
import Vulkan.Extensions.VK_EXT_memory_decompression (MemoryDecompressionMethodFlagBitsEXT(..))
import Vulkan.Extensions.VK_EXT_memory_decompression (MemoryDecompressionMethodFlagsEXT)
import Vulkan.Extensions.VK_EXT_memory_decompression (PhysicalDeviceMemoryDecompressionFeaturesEXT(..))
import Vulkan.Extensions.VK_EXT_memory_decompression (PhysicalDeviceMemoryDecompressionPropertiesEXT(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDecompressMemoryNV
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr DecompressMemoryRegionNV -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr DecompressMemoryRegionNV -> IO ()

-- | vkCmdDecompressMemoryNV - Decompress memory containing compressed data
--
-- = Description
--
-- Each region specified in @pDecompressMemoryRegions@ is decompressed from
-- the compressed to decompressed region based on the decompression method
-- specified in 'DecompressMemoryRegionNV'::@decompressionMethod@. If the
-- regions containing compressed and decompressed data overlap, the
-- decompression behavior is undefined.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDecompressMemoryNV-None-07684# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-memoryDecompression memoryDecompression>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDecompressMemoryNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDecompressMemoryNV-pDecompressMemoryRegions-parameter#
--     @pDecompressMemoryRegions@ /must/ be a valid pointer to an array of
--     @decompressRegionCount@ valid 'DecompressMemoryRegionNV' structures
--
-- -   #VUID-vkCmdDecompressMemoryNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDecompressMemoryNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdDecompressMemoryNV-renderpass# This command /must/ only
--     be called outside of a render pass instance
--
-- -   #VUID-vkCmdDecompressMemoryNV-suspended# This command /must/ not be
--     called between suspended render pass instances
--
-- -   #VUID-vkCmdDecompressMemoryNV-videocoding# This command /must/ only
--     be called outside of a video coding scope
--
-- -   #VUID-vkCmdDecompressMemoryNV-decompressRegionCount-arraylength#
--     @decompressRegionCount@ /must/ be greater than @0@
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdDecompressMemoryNV is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_memory_decompression VK_NV_memory_decompression>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'DecompressMemoryRegionNV'
cmdDecompressMemoryNV :: forall io
                       . (MonadIO io)
                      => -- | @commandBuffer@ is the command buffer into which the command will be
                         -- recorded.
                         CommandBuffer
                      -> -- | @pDecompressMemoryRegions@ is a pointer to an array of
                         -- @decompressRegionCount@ 'DecompressMemoryRegionNV' structures specifying
                         -- decompression parameters.
                         ("decompressMemoryRegions" ::: Vector DecompressMemoryRegionNV)
                      -> io ()
cmdDecompressMemoryNV commandBuffer
                        decompressMemoryRegions = liftIO . evalContT $ do
  let vkCmdDecompressMemoryNVPtr = pVkCmdDecompressMemoryNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdDecompressMemoryNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDecompressMemoryNV is null" Nothing Nothing
  let vkCmdDecompressMemoryNV' = mkVkCmdDecompressMemoryNV vkCmdDecompressMemoryNVPtr
  pPDecompressMemoryRegions <- ContT $ allocaBytes @DecompressMemoryRegionNV ((Data.Vector.length (decompressMemoryRegions)) * 40)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPDecompressMemoryRegions `plusPtr` (40 * (i)) :: Ptr DecompressMemoryRegionNV) (e)) (decompressMemoryRegions)
  lift $ traceAroundEvent "vkCmdDecompressMemoryNV" (vkCmdDecompressMemoryNV'
                                                       (commandBufferHandle (commandBuffer))
                                                       ((fromIntegral (Data.Vector.length $ (decompressMemoryRegions)) :: Word32))
                                                       (pPDecompressMemoryRegions))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDecompressMemoryIndirectCountNV
  :: FunPtr (Ptr CommandBuffer_T -> DeviceAddress -> DeviceAddress -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> DeviceAddress -> DeviceAddress -> Word32 -> IO ()

-- | vkCmdDecompressMemoryIndirectCountNV - Indirect decompress data between
-- memory regions
--
-- = Description
--
-- Each region specified in @indirectCommandsAddress@ is decompressed from
-- the source to destination region based on the specified
-- @decompressionMethod@.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-None-07692# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-memoryDecompression memoryDecompression>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-indirectCommandsAddress-07694#
--     @indirectCommandsAddress@ /must/ be a device address allocated to
--     the application from a buffer created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     usage flag set
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-indirectCommandsAddress-07695#
--     @indirectCommandsAddress@ /must/ be a multiple of @4@
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-indirectCommandsCountAddress-07697#
--     @indirectCommandsCountAddress@ /must/ be a device address allocated
--     to the application from a buffer created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     usage flag set
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-indirectCommandsCountAddress-07698#
--     @indirectCommandsCountAddress@ /must/ be a multiple of @4@
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-indirectCommandsCountAddress-07699#
--     The count stored in @indirectCommandsCountAddress@ /must/ be less
--     than or equal to
--     'Vulkan.Extensions.VK_EXT_memory_decompression.PhysicalDeviceMemoryDecompressionPropertiesEXT'::@maxDecompressionIndirectCount@
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-indirectCommandsAddress-11794#
--     All device addresses between @indirectCommandsAddress@ and
--     @indirectCommandsAddress@ + (@stride@ × (count stored in
--     @indirectCommandsCountAddress@)) - 1 /must/ be in the buffer device
--     address range of the same buffer
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-stride-11770# @stride@
--     /must/ be a multiple of @4@ and /must/ be greater than or equal to
--     sizeof('DecompressMemoryRegionNV')
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-indirectCommandsAddress-parameter#
--     @indirectCommandsAddress@ /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress' value
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-indirectCommandsCountAddress-parameter#
--     @indirectCommandsCountAddress@ /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress' value
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-renderpass# This command
--     /must/ only be called outside of a render pass instance
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-suspended# This command
--     /must/ not be called between suspended render pass instances
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-videocoding# This command
--     /must/ only be called outside of a video coding scope
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdDecompressMemoryIndirectCountNV is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_memory_decompression VK_NV_memory_decompression>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress'
cmdDecompressMemoryIndirectCountNV :: forall io
                                    . (MonadIO io)
                                   => -- | @commandBuffer@ is the command buffer into which the command will be
                                      -- recorded.
                                      CommandBuffer
                                   -> -- | @indirectCommandsAddress@ is the device address containing decompression
                                      -- parameters laid out as an array of 'DecompressMemoryRegionNV'
                                      -- structures.
                                      ("indirectCommandsAddress" ::: DeviceAddress)
                                   -> -- | @indirectCommandsCountAddress@ is the device address containing a 32-bit
                                      -- integer value specifying the decompression count.
                                      ("indirectCommandsCountAddress" ::: DeviceAddress)
                                   -> -- | @stride@ is the byte stride between successive sets of decompression
                                      -- parameters located starting from @indirectCommandsAddress@.
                                      ("stride" ::: Word32)
                                   -> io ()
cmdDecompressMemoryIndirectCountNV commandBuffer
                                     indirectCommandsAddress
                                     indirectCommandsCountAddress
                                     stride = liftIO $ do
  let vkCmdDecompressMemoryIndirectCountNVPtr = pVkCmdDecompressMemoryIndirectCountNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdDecompressMemoryIndirectCountNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDecompressMemoryIndirectCountNV is null" Nothing Nothing
  let vkCmdDecompressMemoryIndirectCountNV' = mkVkCmdDecompressMemoryIndirectCountNV vkCmdDecompressMemoryIndirectCountNVPtr
  traceAroundEvent "vkCmdDecompressMemoryIndirectCountNV" (vkCmdDecompressMemoryIndirectCountNV'
                                                             (commandBufferHandle (commandBuffer))
                                                             (indirectCommandsAddress)
                                                             (indirectCommandsCountAddress)
                                                             (stride))
  pure $ ()


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_FEATURES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_FEATURES_NV = STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_FEATURES_EXT


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_PROPERTIES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_PROPERTIES_NV = STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_PROPERTIES_EXT


-- | VkDecompressMemoryRegionNV - Structure specifying decompression region
-- parameters
--
-- == Valid Usage
--
-- -   #VUID-VkDecompressMemoryRegionNV-decompressionMethod-07690# The
--     @decompressionMethod@ /must/ have a single bit set
--
-- -   #VUID-VkDecompressMemoryRegionNV-srcAddress-07685# @srcAddress@
--     /must/ be 4 byte aligned
--
-- -   #VUID-VkDecompressMemoryRegionNV-srcAddress-07686# The memory range
--     defined by @srcAddress@ and @compressedSize@ /must/ be contained
--     within the size of the buffer bound to @srcAddress@, minus the
--     offset of @srcAddress@ from the base address of that buffer
--
-- -   #VUID-VkDecompressMemoryRegionNV-dstAddress-07687# @dstAddress@
--     /must/ be 4 byte aligned
--
-- -   #VUID-VkDecompressMemoryRegionNV-dstAddress-07688# The memory range
--     defined by @dstAddress@ and @decompressedSize@ /must/ be contained
--     within the size of the buffer bound to @dstAddress@, minus the
--     offset of @dstAddress@ from the base address of that buffer
--
-- -   #VUID-VkDecompressMemoryRegionNV-decompressedSize-07689#
--     @decompressedSize@ /must/ be large enough to hold the decompressed
--     data based on the @decompressionMethod@
--
-- -   #VUID-VkDecompressMemoryRegionNV-compressedSize-11795#
--     @compressedSize@ /must/ not be zero
--
-- -   #VUID-VkDecompressMemoryRegionNV-decompressedSize-11796#
--     @decompressedSize@ /must/ not be zero
--
-- -   #VUID-VkDecompressMemoryRegionNV-srcAddress-07691# The memory range
--     defined by @srcAddress@ and @compressedSize@ /must/ not overlap the
--     memory range defined by @dstAddress@ and @decompressedSize@
--
-- -   #VUID-VkDecompressMemoryRegionNV-decompressionMethod-09395# If
--     @decompressionMethod@ is
--     'Vulkan.Extensions.VK_EXT_memory_decompression.MEMORY_DECOMPRESSION_METHOD_GDEFLATE_1_0_BIT_EXT',
--     then @decompressedSize@ /must/ be less than or equal to 65536 bytes
--
-- -   #VUID-vkCmdDecompressMemoryNV-memoryDecompression-11766# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-memoryDecompression memoryDecompression>
--     feature /must/ be enabled
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_memory_decompression VK_NV_memory_decompression>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Extensions.VK_EXT_memory_decompression.MemoryDecompressionMethodFlagsEXT',
-- 'cmdDecompressMemoryNV'
data DecompressMemoryRegionNV = DecompressMemoryRegionNV
  { -- | @srcAddress@ is the address where compressed data is stored.
    srcAddress :: DeviceAddress
  , -- | @dstAddress@ is the destination address where decompressed data will be
    -- written.
    dstAddress :: DeviceAddress
  , -- | @compressedSize@ is the size of compressed data in bytes.
    compressedSize :: DeviceSize
  , -- | @decompressedSize@ is the size of decompressed data in bytes.
    decompressedSize :: DeviceSize
  , -- | @decompressionMethod@ is a bitmask of
    -- 'MemoryDecompressionMethodFlagBitsNV' with a single bit set specifying
    -- the method used to decompress data.
    decompressionMethod :: MemoryDecompressionMethodFlagsNV
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DecompressMemoryRegionNV)
#endif
deriving instance Show DecompressMemoryRegionNV

instance ToCStruct DecompressMemoryRegionNV where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DecompressMemoryRegionNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (srcAddress)
    poke ((p `plusPtr` 8 :: Ptr DeviceAddress)) (dstAddress)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (compressedSize)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (decompressedSize)
    poke ((p `plusPtr` 32 :: Ptr MemoryDecompressionMethodFlagsNV)) (decompressionMethod)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr MemoryDecompressionMethodFlagsNV)) (zero)
    f

instance FromCStruct DecompressMemoryRegionNV where
  peekCStruct p = do
    srcAddress <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    dstAddress <- peek @DeviceAddress ((p `plusPtr` 8 :: Ptr DeviceAddress))
    compressedSize <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    decompressedSize <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    decompressionMethod <- peek @MemoryDecompressionMethodFlagsNV ((p `plusPtr` 32 :: Ptr MemoryDecompressionMethodFlagsNV))
    pure $ DecompressMemoryRegionNV
             srcAddress
             dstAddress
             compressedSize
             decompressedSize
             decompressionMethod

instance Storable DecompressMemoryRegionNV where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DecompressMemoryRegionNV where
  zero = DecompressMemoryRegionNV
           zero
           zero
           zero
           zero
           zero


-- No documentation found for TopLevel "VkMemoryDecompressionMethodFlagsNV"
type MemoryDecompressionMethodFlagsNV = MemoryDecompressionMethodFlagsEXT


-- No documentation found for TopLevel "VkMemoryDecompressionMethodFlagBitsNV"
type MemoryDecompressionMethodFlagBitsNV = MemoryDecompressionMethodFlagBitsEXT


-- No documentation found for TopLevel "VkPhysicalDeviceMemoryDecompressionFeaturesNV"
type PhysicalDeviceMemoryDecompressionFeaturesNV = PhysicalDeviceMemoryDecompressionFeaturesEXT


-- No documentation found for TopLevel "VkPhysicalDeviceMemoryDecompressionPropertiesNV"
type PhysicalDeviceMemoryDecompressionPropertiesNV = PhysicalDeviceMemoryDecompressionPropertiesEXT


type NV_MEMORY_DECOMPRESSION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_MEMORY_DECOMPRESSION_SPEC_VERSION"
pattern NV_MEMORY_DECOMPRESSION_SPEC_VERSION :: forall a . Integral a => a
pattern NV_MEMORY_DECOMPRESSION_SPEC_VERSION = 1


type NV_MEMORY_DECOMPRESSION_EXTENSION_NAME = "VK_NV_memory_decompression"

-- No documentation found for TopLevel "VK_NV_MEMORY_DECOMPRESSION_EXTENSION_NAME"
pattern NV_MEMORY_DECOMPRESSION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_MEMORY_DECOMPRESSION_EXTENSION_NAME = "VK_NV_memory_decompression"

