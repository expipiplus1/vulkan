{-# language CPP #-}
-- | = Name
--
-- VK_EXT_memory_decompression - device extension
--
-- = VK_EXT_memory_decompression
--
-- [__Name String__]
--     @VK_EXT_memory_decompression@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     551
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>
--
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_memory_decompression] @vkushwaha-nv%0A*Here describe the issue or question you have about the VK_EXT_memory_decompression extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_memory_decompression.adoc VK_EXT_memory_decompression>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-01-23
--
-- [__Contributors__]
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Spencer Fricke, LunarG
--
-- == Description
--
-- This extension adds support for performing memory to memory
-- decompression.
--
-- == New Commands
--
-- -   'cmdDecompressMemoryEXT'
--
-- -   'cmdDecompressMemoryIndirectCountEXT'
--
-- == New Structures
--
-- -   'DecompressMemoryInfoEXT'
--
-- -   'DecompressMemoryRegionEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMemoryDecompressionFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMemoryDecompressionPropertiesEXT'
--
-- == New Enums
--
-- -   'MemoryDecompressionMethodFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'MemoryDecompressionMethodFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_MEMORY_DECOMPRESSION_EXTENSION_NAME'
--
-- -   'EXT_MEMORY_DECOMPRESSION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MEMORY_DECOMPRESSION_READ_BIT_EXT'
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MEMORY_DECOMPRESSION_WRITE_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core14.Enums.BufferUsageFlags2.BufferUsageFlagBits2':
--
--     -   'Vulkan.Core14.Enums.BufferUsageFlags2.BUFFER_USAGE_2_MEMORY_DECOMPRESSION_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MEMORY_DECOMPRESSION_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DECOMPRESS_MEMORY_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_PROPERTIES_EXT'
--
-- == Issues
--
-- 1) How does an app know the minimum size that @decompressedSize@ should
-- be set to?
--
-- __RESOLVED__: When decompressing, data is typically processed in chunks.
-- For example, with GDeflate 1.0, data is streamed in 64 KB blocks, but
-- the final block may be smaller. The exact size of this last block
-- depends on the compression method and original data size and so it must
-- be stored in the compressed bitstream so that the decompressor can set
-- @decompressedSize@ correctly. It is still ok for the last block to take
-- up all 64 KB, but setting it too low will cause issues and is undefined
-- behavior. It is a known limitation that the validation layers will not
-- be able to detect the minimum size of @decompressedSize@ unless it
-- decides to implement each decompression method specification.
--
-- == Version History
--
-- -   Revision 1, 2025-01-23 (Daniel Koch)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_memory_decompression Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_memory_decompression  ( cmdDecompressMemoryEXT
                                                      , cmdDecompressMemoryIndirectCountEXT
                                                      , pattern MEMORY_DECOMPRESSION_METHOD_GDEFLATE_1_0_BIT_NV
                                                      , PhysicalDeviceMemoryDecompressionFeaturesEXT(..)
                                                      , PhysicalDeviceMemoryDecompressionPropertiesEXT(..)
                                                      , DecompressMemoryRegionEXT(..)
                                                      , DecompressMemoryInfoEXT(..)
                                                      , MemoryDecompressionMethodFlagsEXT
                                                      , MemoryDecompressionMethodFlagBitsEXT( MEMORY_DECOMPRESSION_METHOD_GDEFLATE_1_0_BIT_EXT
                                                                                            , ..
                                                                                            )
                                                      , EXT_MEMORY_DECOMPRESSION_SPEC_VERSION
                                                      , pattern EXT_MEMORY_DECOMPRESSION_SPEC_VERSION
                                                      , EXT_MEMORY_DECOMPRESSION_EXTENSION_NAME
                                                      , pattern EXT_MEMORY_DECOMPRESSION_EXTENSION_NAME
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
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkCmdDecompressMemoryEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDecompressMemoryIndirectCountEXT))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.FundamentalTypes (Flags64)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DECOMPRESS_MEMORY_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_PROPERTIES_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDecompressMemoryEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr DecompressMemoryInfoEXT -> IO ()) -> Ptr CommandBuffer_T -> Ptr DecompressMemoryInfoEXT -> IO ()

-- | vkCmdDecompressMemoryEXT - Decompress data between memory regions
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDecompressMemoryEXT-memoryDecompression-11761# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-memoryDecompression memoryDecompression>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDecompressMemoryEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDecompressMemoryEXT-pDecompressMemoryInfoEXT-parameter#
--     @pDecompressMemoryInfoEXT@ /must/ be a valid pointer to a valid
--     'DecompressMemoryInfoEXT' structure
--
-- -   #VUID-vkCmdDecompressMemoryEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDecompressMemoryEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdDecompressMemoryEXT-renderpass# This command /must/ only
--     be called outside of a render pass instance
--
-- -   #VUID-vkCmdDecompressMemoryEXT-suspended# This command /must/ not be
--     called between suspended render pass instances
--
-- -   #VUID-vkCmdDecompressMemoryEXT-videocoding# This command /must/ only
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdDecompressMemoryEXT is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_memory_decompression VK_EXT_memory_decompression>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'DecompressMemoryInfoEXT'
cmdDecompressMemoryEXT :: forall io
                        . (MonadIO io)
                       => -- | @commandBuffer@ is the command buffer into which the command will be
                          -- recorded.
                          CommandBuffer
                       -> -- | @pDecompressMemoryInfoEXT@ is a pointer to a 'DecompressMemoryInfoEXT'
                          -- structure describing the decompression parameters.
                          DecompressMemoryInfoEXT
                       -> io ()
cmdDecompressMemoryEXT commandBuffer
                         decompressMemoryInfoEXT = liftIO . evalContT $ do
  let vkCmdDecompressMemoryEXTPtr = pVkCmdDecompressMemoryEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdDecompressMemoryEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDecompressMemoryEXT is null" Nothing Nothing
  let vkCmdDecompressMemoryEXT' = mkVkCmdDecompressMemoryEXT vkCmdDecompressMemoryEXTPtr
  pDecompressMemoryInfoEXT <- ContT $ withCStruct (decompressMemoryInfoEXT)
  lift $ traceAroundEvent "vkCmdDecompressMemoryEXT" (vkCmdDecompressMemoryEXT'
                                                        (commandBufferHandle (commandBuffer))
                                                        pDecompressMemoryInfoEXT)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDecompressMemoryIndirectCountEXT
  :: FunPtr (Ptr CommandBuffer_T -> MemoryDecompressionMethodFlagsEXT -> DeviceAddress -> DeviceAddress -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> MemoryDecompressionMethodFlagsEXT -> DeviceAddress -> DeviceAddress -> Word32 -> Word32 -> IO ()

-- | vkCmdDecompressMemoryIndirectCountEXT - Indirect decompress data between
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
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-None-07692# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-memoryDecompression memoryDecompression>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-indirectCommandsAddress-07694#
--     @indirectCommandsAddress@ /must/ be a device address allocated to
--     the application from a buffer created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     usage flag set
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-indirectCommandsAddress-07695#
--     @indirectCommandsAddress@ /must/ be a multiple of @4@
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-indirectCommandsCountAddress-07697#
--     @indirectCommandsCountAddress@ /must/ be a device address allocated
--     to the application from a buffer created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     usage flag set
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-indirectCommandsCountAddress-07698#
--     @indirectCommandsCountAddress@ /must/ be a multiple of @4@
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-indirectCommandsCountAddress-07699#
--     The count stored in @indirectCommandsCountAddress@ /must/ be less
--     than or equal to
--     'PhysicalDeviceMemoryDecompressionPropertiesEXT'::@maxDecompressionIndirectCount@
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-indirectCommandsAddress-11794#
--     All device addresses between @indirectCommandsAddress@ and
--     @indirectCommandsAddress@ + (@stride@ × (count stored in
--     @indirectCommandsCountAddress@)) - 1 /must/ be in the buffer device
--     address range of the same buffer
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-decompressionMethod-07690#
--     The @decompressionMethod@ /must/ have a single bit set
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-stride-11767# @stride@
--     /must/ be a multiple of @4@ and /must/ be greater than or equal to
--     sizeof('DecompressMemoryRegionEXT')
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-maxDecompressionCount-11768#
--     @maxDecompressionCount@ /must/ be less than or equal to
--     'PhysicalDeviceMemoryDecompressionPropertiesEXT'::@maxDecompressionIndirectCount@
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-decompressionMethod-11769#
--     If @decompressionMethod@ is
--     'MEMORY_DECOMPRESSION_METHOD_GDEFLATE_1_0_BIT_EXT', then all values
--     in 'DecompressMemoryRegionEXT'::@decompressedSize@ /must/ be less
--     than or equal to 65536 bytes
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-decompressionMethod-11810#
--     @decompressionMethod@ /must/ be a valid bit specified in
--     'PhysicalDeviceMemoryDecompressionPropertiesEXT'::@decompressionMethods@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-decompressionMethod-parameter#
--     @decompressionMethod@ /must/ be a valid combination of
--     'MemoryDecompressionMethodFlagBitsEXT' values
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-decompressionMethod-requiredbitmask#
--     @decompressionMethod@ /must/ not be @0@
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-indirectCommandsAddress-parameter#
--     @indirectCommandsAddress@ /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress' value
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-indirectCommandsCountAddress-parameter#
--     @indirectCommandsCountAddress@ /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress' value
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-renderpass# This command
--     /must/ only be called outside of a render pass instance
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-suspended# This command
--     /must/ not be called between suspended render pass instances
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountEXT-videocoding# This
--     command /must/ only be called outside of a video coding scope
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
-- vkCmdDecompressMemoryIndirectCountEXT is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_memory_decompression VK_EXT_memory_decompression>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'MemoryDecompressionMethodFlagsEXT'
cmdDecompressMemoryIndirectCountEXT :: forall io
                                     . (MonadIO io)
                                    => -- | @commandBuffer@ is the command buffer into which the command will be
                                       -- recorded.
                                       CommandBuffer
                                    -> -- | @decompressionMethod@ is a bitmask of
                                       -- 'MemoryDecompressionMethodFlagBitsEXT' with a single bit set specifying
                                       -- the method used to decompress data.
                                       MemoryDecompressionMethodFlagsEXT
                                    -> -- | @indirectCommandsAddress@ is the device address containing decompression
                                       -- parameters laid out as an array of 'DecompressMemoryRegionEXT'
                                       -- structures.
                                       ("indirectCommandsAddress" ::: DeviceAddress)
                                    -> -- | @indirectCommandsCountAddress@ is the device address containing a 32-bit
                                       -- integer value specifying the decompression count.
                                       ("indirectCommandsCountAddress" ::: DeviceAddress)
                                    -> -- | @maxDecompressionCount@ is maximum number of decompressions that will be
                                       -- executed. The actual number of executed decompressions is the minimum of
                                       -- the count specified in @indirectCommandsCountAddress@ and
                                       -- @maxDecompressionCount@.
                                       ("maxDecompressionCount" ::: Word32)
                                    -> -- | @stride@ is the byte stride between successive sets of decompression
                                       -- parameters located starting from @indirectCommandsAddress@.
                                       ("stride" ::: Word32)
                                    -> io ()
cmdDecompressMemoryIndirectCountEXT commandBuffer
                                      decompressionMethod
                                      indirectCommandsAddress
                                      indirectCommandsCountAddress
                                      maxDecompressionCount
                                      stride = liftIO $ do
  let vkCmdDecompressMemoryIndirectCountEXTPtr = pVkCmdDecompressMemoryIndirectCountEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdDecompressMemoryIndirectCountEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDecompressMemoryIndirectCountEXT is null" Nothing Nothing
  let vkCmdDecompressMemoryIndirectCountEXT' = mkVkCmdDecompressMemoryIndirectCountEXT vkCmdDecompressMemoryIndirectCountEXTPtr
  traceAroundEvent "vkCmdDecompressMemoryIndirectCountEXT" (vkCmdDecompressMemoryIndirectCountEXT'
                                                              (commandBufferHandle (commandBuffer))
                                                              (decompressionMethod)
                                                              (indirectCommandsAddress)
                                                              (indirectCommandsCountAddress)
                                                              (maxDecompressionCount)
                                                              (stride))
  pure $ ()


-- No documentation found for TopLevel "VK_MEMORY_DECOMPRESSION_METHOD_GDEFLATE_1_0_BIT_NV"
pattern MEMORY_DECOMPRESSION_METHOD_GDEFLATE_1_0_BIT_NV = MEMORY_DECOMPRESSION_METHOD_GDEFLATE_1_0_BIT_EXT


-- | VkPhysicalDeviceMemoryDecompressionFeaturesEXT - Structure describing if
-- memory decompression is supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceMemoryDecompressionFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceMemoryDecompressionFeaturesEXT', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_memory_decompression VK_EXT_memory_decompression>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_memory_decompression VK_NV_memory_decompression>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMemoryDecompressionFeaturesEXT = PhysicalDeviceMemoryDecompressionFeaturesEXT
  { -- | #features-memoryDecompression# @memoryDecompression@ indicates whether
    -- memory decompression is supported.
    memoryDecompression :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMemoryDecompressionFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceMemoryDecompressionFeaturesEXT

instance ToCStruct PhysicalDeviceMemoryDecompressionFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMemoryDecompressionFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (memoryDecompression))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMemoryDecompressionFeaturesEXT where
  peekCStruct p = do
    memoryDecompression <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMemoryDecompressionFeaturesEXT
             (bool32ToBool memoryDecompression)

instance Storable PhysicalDeviceMemoryDecompressionFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMemoryDecompressionFeaturesEXT where
  zero = PhysicalDeviceMemoryDecompressionFeaturesEXT
           zero


-- | VkPhysicalDeviceMemoryDecompressionPropertiesEXT - Structure describing
-- supported memory decompression methods by an implementation
--
-- = Description
--
-- If
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-memoryDecompression memoryDecompression>
-- feature is supported, @decompressionMethods@ /must/ have at least one
-- bit set.
--
-- If the 'PhysicalDeviceMemoryDecompressionPropertiesEXT' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_memory_decompression VK_EXT_memory_decompression>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_memory_decompression VK_NV_memory_decompression>,
-- 'MemoryDecompressionMethodFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMemoryDecompressionPropertiesEXT = PhysicalDeviceMemoryDecompressionPropertiesEXT
  { -- | @decompressionMethods@ is a bitmask of
    -- 'MemoryDecompressionMethodFlagBitsEXT' specifying memory decompression
    -- methods supported by the implementation.
    decompressionMethods :: MemoryDecompressionMethodFlagsEXT
  , -- | @maxDecompressionIndirectCount@ specifies the maximum supported count
    -- value identified by either
    -- 'cmdDecompressMemoryIndirectCountEXT'::@maxDecompressionCount@ or the
    -- value specified in
    -- 'cmdDecompressMemoryIndirectCountEXT'::@indirectCommandsCountAddress@
    maxDecompressionIndirectCount :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMemoryDecompressionPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceMemoryDecompressionPropertiesEXT

instance ToCStruct PhysicalDeviceMemoryDecompressionPropertiesEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMemoryDecompressionPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MemoryDecompressionMethodFlagsEXT)) (decompressionMethods)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (maxDecompressionIndirectCount)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MemoryDecompressionMethodFlagsEXT)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    f

instance FromCStruct PhysicalDeviceMemoryDecompressionPropertiesEXT where
  peekCStruct p = do
    decompressionMethods <- peek @MemoryDecompressionMethodFlagsEXT ((p `plusPtr` 16 :: Ptr MemoryDecompressionMethodFlagsEXT))
    maxDecompressionIndirectCount <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    pure $ PhysicalDeviceMemoryDecompressionPropertiesEXT
             decompressionMethods maxDecompressionIndirectCount

instance Storable PhysicalDeviceMemoryDecompressionPropertiesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMemoryDecompressionPropertiesEXT where
  zero = PhysicalDeviceMemoryDecompressionPropertiesEXT
           zero
           zero


-- | VkDecompressMemoryRegionEXT - Structure specifying decompression region
--
-- = Description
--
-- Accesses to compressed and decompressed data specified in @srcAddress@
-- and @dstAddress@ /must/ be
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies synchronized>
-- with the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MEMORY_DECOMPRESSION_BIT_EXT'
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-pipeline-stages pipeline stage>
-- with
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-access-types access type>
-- of
-- 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MEMORY_DECOMPRESSION_READ_BIT_EXT'
-- or
-- 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MEMORY_DECOMPRESSION_WRITE_BIT_EXT'.
--
-- == Valid Usage
--
-- -   #VUID-VkDecompressMemoryRegionEXT-srcAddress-07685# @srcAddress@
--     /must/ be 4 byte aligned
--
-- -   #VUID-VkDecompressMemoryRegionEXT-srcAddress-07686# The memory range
--     defined by @srcAddress@ and @compressedSize@ /must/ be contained
--     within the size of the buffer bound to @srcAddress@, minus the
--     offset of @srcAddress@ from the base address of that buffer
--
-- -   #VUID-VkDecompressMemoryRegionEXT-dstAddress-07687# @dstAddress@
--     /must/ be 4 byte aligned
--
-- -   #VUID-VkDecompressMemoryRegionEXT-dstAddress-07688# The memory range
--     defined by @dstAddress@ and @decompressedSize@ /must/ be contained
--     within the size of the buffer bound to @dstAddress@, minus the
--     offset of @dstAddress@ from the base address of that buffer
--
-- -   #VUID-VkDecompressMemoryRegionEXT-decompressedSize-07689#
--     @decompressedSize@ /must/ be large enough to hold the decompressed
--     data based on the @decompressionMethod@
--
-- -   #VUID-VkDecompressMemoryRegionEXT-compressedSize-11795#
--     @compressedSize@ /must/ not be zero
--
-- -   #VUID-VkDecompressMemoryRegionEXT-decompressedSize-11796#
--     @decompressedSize@ /must/ not be zero
--
-- -   #VUID-VkDecompressMemoryRegionEXT-srcAddress-07691# The memory range
--     defined by @srcAddress@ and @compressedSize@ /must/ not overlap the
--     memory range defined by @dstAddress@ and @decompressedSize@
--
-- -   #VUID-VkDecompressMemoryRegionEXT-srcAddress-11764# @srcAddress@
--     /must/ be a device address allocated to the application from a
--     buffer created with the
--     'Vulkan.Core14.Enums.BufferUsageFlags2.BUFFER_USAGE_2_MEMORY_DECOMPRESSION_BIT_EXT'
--     usage flag set
--
-- -   #VUID-VkDecompressMemoryRegionEXT-dstAddress-11765# @dstAddress@
--     /must/ be a device address allocated to the application from a
--     buffer created with the
--     'Vulkan.Core14.Enums.BufferUsageFlags2.BUFFER_USAGE_2_MEMORY_DECOMPRESSION_BIT_EXT'
--     usage flag set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDecompressMemoryRegionEXT-srcAddress-parameter# @srcAddress@
--     /must/ be a valid 'Vulkan.Core10.FundamentalTypes.DeviceAddress'
--     value
--
-- -   #VUID-VkDecompressMemoryRegionEXT-dstAddress-parameter# @dstAddress@
--     /must/ be a valid 'Vulkan.Core10.FundamentalTypes.DeviceAddress'
--     value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_memory_decompression VK_EXT_memory_decompression>,
-- 'DecompressMemoryInfoEXT',
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
data DecompressMemoryRegionEXT = DecompressMemoryRegionEXT
  { -- | @srcAddress@ is the address where compressed data is stored.
    srcAddress :: DeviceAddress
  , -- | @dstAddress@ is the destination address where decompressed data will be
    -- written.
    dstAddress :: DeviceAddress
  , -- | @compressedSize@ is the size of compressed data in bytes.
    compressedSize :: DeviceSize
  , -- | @decompressedSize@ is the size of decompressed data in bytes.
    decompressedSize :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DecompressMemoryRegionEXT)
#endif
deriving instance Show DecompressMemoryRegionEXT

instance ToCStruct DecompressMemoryRegionEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DecompressMemoryRegionEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (srcAddress)
    poke ((p `plusPtr` 8 :: Ptr DeviceAddress)) (dstAddress)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (compressedSize)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (decompressedSize)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct DecompressMemoryRegionEXT where
  peekCStruct p = do
    srcAddress <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    dstAddress <- peek @DeviceAddress ((p `plusPtr` 8 :: Ptr DeviceAddress))
    compressedSize <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    decompressedSize <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    pure $ DecompressMemoryRegionEXT
             srcAddress dstAddress compressedSize decompressedSize

instance Storable DecompressMemoryRegionEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DecompressMemoryRegionEXT where
  zero = DecompressMemoryRegionEXT
           zero
           zero
           zero
           zero


-- | VkDecompressMemoryInfoEXT - Structure specifying decompression memory
-- info
--
-- = Description
--
-- Each memory region specified in @pRegions@ is decompressed from the
-- source to the destination address based on the decompression method
-- specified in @decompressionMethod@. If any of the specified source and
-- destination regions overlap in memory, then the results of decompression
-- are undefined.
--
-- == Valid Usage
--
-- -   #VUID-VkDecompressMemoryInfoEXT-decompressionMethod-07690# The
--     @decompressionMethod@ /must/ have a single bit set
--
-- -   #VUID-VkDecompressMemoryInfoEXT-decompressionMethod-11762# If
--     @decompressionMethod@ is
--     'MEMORY_DECOMPRESSION_METHOD_GDEFLATE_1_0_BIT_EXT', then for each
--     element of @pRegions@, @decompressedSize@ /must/ be less than or
--     equal to 65536 bytes
--
-- -   #VUID-VkDecompressMemoryInfoEXT-decompressionMethod-11763#
--     @decompressionMethod@ /must/ be a valid bit specified in
--     'PhysicalDeviceMemoryDecompressionPropertiesEXT'::@decompressionMethods@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDecompressMemoryInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DECOMPRESS_MEMORY_INFO_EXT'
--
-- -   #VUID-VkDecompressMemoryInfoEXT-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkDecompressMemoryInfoEXT-decompressionMethod-parameter#
--     @decompressionMethod@ /must/ be a valid combination of
--     'MemoryDecompressionMethodFlagBitsEXT' values
--
-- -   #VUID-VkDecompressMemoryInfoEXT-decompressionMethod-requiredbitmask#
--     @decompressionMethod@ /must/ not be @0@
--
-- -   #VUID-VkDecompressMemoryInfoEXT-pRegions-parameter# @pRegions@
--     /must/ be a valid pointer to an array of @regionCount@
--     'DecompressMemoryRegionEXT' structures
--
-- -   #VUID-VkDecompressMemoryInfoEXT-regionCount-arraylength#
--     @regionCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_memory_decompression VK_EXT_memory_decompression>,
-- 'DecompressMemoryRegionEXT', 'MemoryDecompressionMethodFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdDecompressMemoryEXT'
data DecompressMemoryInfoEXT = DecompressMemoryInfoEXT
  { -- | @decompressionMethod@ is a bitmask of
    -- 'MemoryDecompressionMethodFlagBitsEXT' with a single bit set specifying
    -- the method used to decompress data.
    decompressionMethod :: MemoryDecompressionMethodFlagsEXT
  , -- | @pRegions@ is a pointer to an array of 'DecompressMemoryRegionEXT'
    -- structures specifying the regions to decompress.
    regions :: Vector DecompressMemoryRegionEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DecompressMemoryInfoEXT)
#endif
deriving instance Show DecompressMemoryInfoEXT

instance ToCStruct DecompressMemoryInfoEXT where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DecompressMemoryInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DECOMPRESS_MEMORY_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr MemoryDecompressionMethodFlagsEXT)) (decompressionMethod)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytes @DecompressMemoryRegionEXT ((Data.Vector.length (regions)) * 32)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions' `plusPtr` (32 * (i)) :: Ptr DecompressMemoryRegionEXT) (e)) (regions)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr DecompressMemoryRegionEXT))) (pPRegions')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DECOMPRESS_MEMORY_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MemoryDecompressionMethodFlagsEXT)) (zero)
    f

instance FromCStruct DecompressMemoryInfoEXT where
  peekCStruct p = do
    decompressionMethod <- peek @MemoryDecompressionMethodFlagsEXT ((p `plusPtr` 16 :: Ptr MemoryDecompressionMethodFlagsEXT))
    regionCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pRegions <- peek @(Ptr DecompressMemoryRegionEXT) ((p `plusPtr` 32 :: Ptr (Ptr DecompressMemoryRegionEXT)))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekCStruct @DecompressMemoryRegionEXT ((pRegions `advancePtrBytes` (32 * (i)) :: Ptr DecompressMemoryRegionEXT)))
    pure $ DecompressMemoryInfoEXT
             decompressionMethod pRegions'

instance Zero DecompressMemoryInfoEXT where
  zero = DecompressMemoryInfoEXT
           zero
           mempty


type MemoryDecompressionMethodFlagsEXT = MemoryDecompressionMethodFlagBitsEXT

-- | VkMemoryDecompressionMethodFlagBitsEXT - List the supported memory
-- decompression methods
--
-- = Description
--
-- -   'MEMORY_DECOMPRESSION_METHOD_GDEFLATE_1_0_BIT_EXT' specifies that
--     the GDeflate 1.0 algorithm is used to decompress data.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_memory_decompression VK_EXT_memory_decompression>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_memory_decompression VK_NV_memory_decompression>,
-- 'MemoryDecompressionMethodFlagsEXT'
newtype MemoryDecompressionMethodFlagBitsEXT = MemoryDecompressionMethodFlagBitsEXT Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkMemoryDecompressionMethodFlagBitsEXT" "VK_MEMORY_DECOMPRESSION_METHOD_GDEFLATE_1_0_BIT_EXT"
pattern MEMORY_DECOMPRESSION_METHOD_GDEFLATE_1_0_BIT_EXT = MemoryDecompressionMethodFlagBitsEXT 0x0000000000000001

conNameMemoryDecompressionMethodFlagBitsEXT :: String
conNameMemoryDecompressionMethodFlagBitsEXT = "MemoryDecompressionMethodFlagBitsEXT"

enumPrefixMemoryDecompressionMethodFlagBitsEXT :: String
enumPrefixMemoryDecompressionMethodFlagBitsEXT = "MEMORY_DECOMPRESSION_METHOD_GDEFLATE_1_0_BIT_EXT"

showTableMemoryDecompressionMethodFlagBitsEXT :: [(MemoryDecompressionMethodFlagBitsEXT, String)]
showTableMemoryDecompressionMethodFlagBitsEXT =
  [
    ( MEMORY_DECOMPRESSION_METHOD_GDEFLATE_1_0_BIT_EXT
    , ""
    )
  ]

instance Show MemoryDecompressionMethodFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixMemoryDecompressionMethodFlagBitsEXT
      showTableMemoryDecompressionMethodFlagBitsEXT
      conNameMemoryDecompressionMethodFlagBitsEXT
      (\(MemoryDecompressionMethodFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read MemoryDecompressionMethodFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixMemoryDecompressionMethodFlagBitsEXT
      showTableMemoryDecompressionMethodFlagBitsEXT
      conNameMemoryDecompressionMethodFlagBitsEXT
      MemoryDecompressionMethodFlagBitsEXT

type EXT_MEMORY_DECOMPRESSION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_MEMORY_DECOMPRESSION_SPEC_VERSION"
pattern EXT_MEMORY_DECOMPRESSION_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_MEMORY_DECOMPRESSION_SPEC_VERSION = 1


type EXT_MEMORY_DECOMPRESSION_EXTENSION_NAME = "VK_EXT_memory_decompression"

-- No documentation found for TopLevel "VK_EXT_MEMORY_DECOMPRESSION_EXTENSION_NAME"
pattern EXT_MEMORY_DECOMPRESSION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_MEMORY_DECOMPRESSION_EXTENSION_NAME = "VK_EXT_memory_decompression"

