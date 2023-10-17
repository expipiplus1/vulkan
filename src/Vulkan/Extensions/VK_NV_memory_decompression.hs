{-# language CPP #-}
-- | = Name
--
-- VK_NV_memory_decompression - device extension
--
-- == VK_NV_memory_decompression
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
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>
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
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_PROPERTIES_NV'
--
-- == Version History
--
-- -   Revision 1, 2022-01-31 (Vikram Kushwaha)
--
--     -   Initial draft
--
-- == See Also
--
-- 'DecompressMemoryRegionNV', 'MemoryDecompressionMethodFlagBitsNV',
-- 'MemoryDecompressionMethodFlagsNV',
-- 'PhysicalDeviceMemoryDecompressionFeaturesNV',
-- 'PhysicalDeviceMemoryDecompressionPropertiesNV',
-- 'cmdDecompressMemoryIndirectCountNV', 'cmdDecompressMemoryNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_memory_decompression Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_memory_decompression  ( cmdDecompressMemoryNV
                                                     , cmdDecompressMemoryIndirectCountNV
                                                     , PhysicalDeviceMemoryDecompressionFeaturesNV(..)
                                                     , PhysicalDeviceMemoryDecompressionPropertiesNV(..)
                                                     , DecompressMemoryRegionNV(..)
                                                     , MemoryDecompressionMethodFlagsNV
                                                     , MemoryDecompressionMethodFlagBitsNV( MEMORY_DECOMPRESSION_METHOD_GDEFLATE_1_0_BIT_NV
                                                                                          , ..
                                                                                          )
                                                     , NV_MEMORY_DECOMPRESSION_SPEC_VERSION
                                                     , pattern NV_MEMORY_DECOMPRESSION_SPEC_VERSION
                                                     , NV_MEMORY_DECOMPRESSION_EXTENSION_NAME
                                                     , pattern NV_MEMORY_DECOMPRESSION_EXTENSION_NAME
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkCmdDecompressMemoryIndirectCountNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDecompressMemoryNV))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.FundamentalTypes (Flags64)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_PROPERTIES_NV))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDecompressMemoryNV
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr DecompressMemoryRegionNV -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr DecompressMemoryRegionNV -> IO ()

-- | vkCmdDecompressMemoryNV - Decompress data between memory regions
--
-- = Description
--
-- Each region specified in @pDecompressMemoryRegions@ is decompressed from
-- the source to destination region based on the specified decompression
-- method.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDecompressMemoryNV-None-07684# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-memoryDecompression memoryDecompression>
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
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdDecompressMemoryNV-renderpass# This command /must/ only
--     be called outside of a render pass instance
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
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
-- the source to destination region based on the specified decompression
-- method.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-None-07692# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-memoryDecompression memoryDecompression>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-indirectCommandsAddress-07693#
--     If @indirectCommandsAddress@ comes from a non-sparse buffer then it
--     /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-indirectCommandsAddress-07694#
--     The 'Vulkan.Core10.Handles.Buffer' that @indirectCommandsAddress@
--     comes from /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-offset-07695# @offset@
--     /must/ be a multiple of @4@
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-indirectCommandsCountAddress-07696#
--     If @indirectCommandsCountAddress@ comes from a non-sparse buffer
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-indirectCommandsCountAddress-07697#
--     The 'Vulkan.Core10.Handles.Buffer' that
--     @indirectCommandsCountAddress@ comes from /must/ have been created
--     with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-indirectCommandsCountAddress-07698#
--     @indirectCommandsCountAddress@ /must/ be a multiple of @4@
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-indirectCommandsCountAddress-07699#
--     The count stored in @indirectCommandsCountAddress@ /must/ be less
--     than or equal to
--     'PhysicalDeviceMemoryDecompressionPropertiesNV'::@maxDecompressionIndirectCount@
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-stride-07700# @stride@
--     /must/ be a multiple of @4@ and /must/ be greater than or equal to
--     sizeof('DecompressMemoryRegionNV')
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-indirectCommandsCountAddress-07701#
--     If the count stored in @indirectCommandsCountAddress@ is equal to
--     @1@, (@offset@ + sizeof('DecompressMemoryRegionNV')) /must/ be less
--     than or equal to the size of the 'Vulkan.Core10.Handles.Buffer' that
--     @indirectCommandsAddress@ comes from
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-indirectCommandsCountAddress-07702#
--     If the count stored in @indirectCommandsCountAddress@ is greater
--     than @1@, @indirectCommandsAddress@ +
--     sizeof('DecompressMemoryRegionNV') + (@stride@ × (count stored in
--     @countBuffer@ - 1)) /must/ be less than or equal to the last valid
--     address in the 'Vulkan.Core10.Handles.Buffer' that
--     @indirectCommandsAddress@ was created from
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdDecompressMemoryIndirectCountNV-renderpass# This command
--     /must/ only be called outside of a render pass instance
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
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
                                   -> -- | @indirectCommandsCountAddress@ is the device address containing the
                                      -- decompression count.
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


-- | VkPhysicalDeviceMemoryDecompressionFeaturesNV - Structure describing if
-- memory decompression is supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceMemoryDecompressionFeaturesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceMemoryDecompressionFeaturesNV' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_memory_decompression VK_NV_memory_decompression>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMemoryDecompressionFeaturesNV = PhysicalDeviceMemoryDecompressionFeaturesNV
  { -- | #features-memoryDecompression# @memoryDecompression@ indicates whether
    -- memory decompression is supported.
    memoryDecompression :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMemoryDecompressionFeaturesNV)
#endif
deriving instance Show PhysicalDeviceMemoryDecompressionFeaturesNV

instance ToCStruct PhysicalDeviceMemoryDecompressionFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMemoryDecompressionFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (memoryDecompression))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMemoryDecompressionFeaturesNV where
  peekCStruct p = do
    memoryDecompression <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMemoryDecompressionFeaturesNV
             (bool32ToBool memoryDecompression)

instance Storable PhysicalDeviceMemoryDecompressionFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMemoryDecompressionFeaturesNV where
  zero = PhysicalDeviceMemoryDecompressionFeaturesNV
           zero


-- | VkPhysicalDeviceMemoryDecompressionPropertiesNV - Structure describing
-- supported memory decompression methods by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceMemoryDecompressionPropertiesNV' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_memory_decompression VK_NV_memory_decompression>,
-- 'MemoryDecompressionMethodFlagsNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMemoryDecompressionPropertiesNV = PhysicalDeviceMemoryDecompressionPropertiesNV
  { -- | @decompressionMethods@ is a bitmask of
    -- 'MemoryDecompressionMethodFlagBitsNV' specifying memory decompression
    -- methods supported by the implementation.
    decompressionMethods :: MemoryDecompressionMethodFlagsNV
  , -- | @maxDecompressionIndirectCount@ specifies the maximum supported count
    -- value in the @countBuffer@ of 'cmdDecompressMemoryIndirectCountNV'
    maxDecompressionIndirectCount :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMemoryDecompressionPropertiesNV)
#endif
deriving instance Show PhysicalDeviceMemoryDecompressionPropertiesNV

instance ToCStruct PhysicalDeviceMemoryDecompressionPropertiesNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMemoryDecompressionPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MemoryDecompressionMethodFlagsNV)) (decompressionMethods)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (maxDecompressionIndirectCount)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MemoryDecompressionMethodFlagsNV)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    f

instance FromCStruct PhysicalDeviceMemoryDecompressionPropertiesNV where
  peekCStruct p = do
    decompressionMethods <- peek @MemoryDecompressionMethodFlagsNV ((p `plusPtr` 16 :: Ptr MemoryDecompressionMethodFlagsNV))
    maxDecompressionIndirectCount <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    pure $ PhysicalDeviceMemoryDecompressionPropertiesNV
             decompressionMethods maxDecompressionIndirectCount

instance Storable PhysicalDeviceMemoryDecompressionPropertiesNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMemoryDecompressionPropertiesNV where
  zero = PhysicalDeviceMemoryDecompressionPropertiesNV
           zero
           zero


-- | VkDecompressMemoryRegionNV - Structure specifying decompression
-- parameters
--
-- == Valid Usage
--
-- -   #VUID-VkDecompressMemoryRegionNV-srcAddress-07685# The @srcAddress@
--     /must/ be 4 byte aligned
--
-- -   #VUID-VkDecompressMemoryRegionNV-srcAddress-07686# The memory in
--     range @srcAddress@ and @srcAddress@ + @compressedSize@ /must/ be
--     valid and bound to a 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkDecompressMemoryRegionNV-dstAddress-07687# The @dstAddress@
--     /must/ be 4 byte aligned
--
-- -   #VUID-VkDecompressMemoryRegionNV-dstAddress-07688# The memory in
--     range @dstAddress@ and @dstAddress@ + @decompressedSize@ /must/ be
--     valid and bound to a 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkDecompressMemoryRegionNV-decompressedSize-07689# The
--     @decompressedSize@ /must/ be large enough to hold the decompressed
--     data based on the @decompressionMethod@
--
-- -   #VUID-VkDecompressMemoryRegionNV-decompressionMethod-07690# The
--     @decompressionMethod@ /must/ have a single bit set
--
-- -   #VUID-VkDecompressMemoryRegionNV-srcAddress-07691# The @srcAddress@
--     to @srcAddress@ + @compressedSize@ region /must/ not overlap with
--     the @dstAddress@ and @dstAddress@ + @decompressedSize@ region
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDecompressMemoryRegionNV-decompressionMethod-parameter#
--     @decompressionMethod@ /must/ be a valid combination of
--     'MemoryDecompressionMethodFlagBitsNV' values
--
-- -   #VUID-VkDecompressMemoryRegionNV-decompressionMethod-requiredbitmask#
--     @decompressionMethod@ /must/ not be @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_memory_decompression VK_NV_memory_decompression>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'MemoryDecompressionMethodFlagsNV', 'cmdDecompressMemoryNV'
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


type MemoryDecompressionMethodFlagsNV = MemoryDecompressionMethodFlagBitsNV

-- | VkMemoryDecompressionMethodFlagBitsNV - List the supported memory
-- decompression methods
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_memory_decompression VK_NV_memory_decompression>
newtype MemoryDecompressionMethodFlagBitsNV = MemoryDecompressionMethodFlagBitsNV Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'MEMORY_DECOMPRESSION_METHOD_GDEFLATE_1_0_BIT_NV' specifies that the
-- GDeflate 1.0 algorithm is used to decompress data.
pattern MEMORY_DECOMPRESSION_METHOD_GDEFLATE_1_0_BIT_NV = MemoryDecompressionMethodFlagBitsNV 0x0000000000000001

conNameMemoryDecompressionMethodFlagBitsNV :: String
conNameMemoryDecompressionMethodFlagBitsNV = "MemoryDecompressionMethodFlagBitsNV"

enumPrefixMemoryDecompressionMethodFlagBitsNV :: String
enumPrefixMemoryDecompressionMethodFlagBitsNV = "MEMORY_DECOMPRESSION_METHOD_GDEFLATE_1_0_BIT_NV"

showTableMemoryDecompressionMethodFlagBitsNV :: [(MemoryDecompressionMethodFlagBitsNV, String)]
showTableMemoryDecompressionMethodFlagBitsNV =
  [
    ( MEMORY_DECOMPRESSION_METHOD_GDEFLATE_1_0_BIT_NV
    , ""
    )
  ]

instance Show MemoryDecompressionMethodFlagBitsNV where
  showsPrec =
    enumShowsPrec
      enumPrefixMemoryDecompressionMethodFlagBitsNV
      showTableMemoryDecompressionMethodFlagBitsNV
      conNameMemoryDecompressionMethodFlagBitsNV
      (\(MemoryDecompressionMethodFlagBitsNV x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read MemoryDecompressionMethodFlagBitsNV where
  readPrec =
    enumReadPrec
      enumPrefixMemoryDecompressionMethodFlagBitsNV
      showTableMemoryDecompressionMethodFlagBitsNV
      conNameMemoryDecompressionMethodFlagBitsNV
      MemoryDecompressionMethodFlagBitsNV

type NV_MEMORY_DECOMPRESSION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_MEMORY_DECOMPRESSION_SPEC_VERSION"
pattern NV_MEMORY_DECOMPRESSION_SPEC_VERSION :: forall a . Integral a => a
pattern NV_MEMORY_DECOMPRESSION_SPEC_VERSION = 1


type NV_MEMORY_DECOMPRESSION_EXTENSION_NAME = "VK_NV_memory_decompression"

-- No documentation found for TopLevel "VK_NV_MEMORY_DECOMPRESSION_EXTENSION_NAME"
pattern NV_MEMORY_DECOMPRESSION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_MEMORY_DECOMPRESSION_EXTENSION_NAME = "VK_NV_memory_decompression"

