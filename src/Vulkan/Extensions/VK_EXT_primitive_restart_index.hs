{-# language CPP #-}
-- | = Name
--
-- VK_EXT_primitive_restart_index - device extension
--
-- = VK_EXT_primitive_restart_index
--
-- [__Name String__]
--     @VK_EXT_primitive_restart_index@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     679
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
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_primitive_restart_index] @zmike%0A*Here describe the issue or question you have about the VK_EXT_primitive_restart_index extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_primitive_restart_index.adoc VK_EXT_primitive_restart_index>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-03-10
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Spencer Fricke, LunarG
--
--     -   Ricardo Garcia, Igalia
--
--     -   Piers Daniell, NVIDIA
--
-- == Description
--
-- This extension allows setting a custom primitive restart index. It is
-- primarily intended to support GL emulation.
--
-- == New Commands
--
-- -   'cmdSetPrimitiveRestartIndexEXT'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePrimitiveRestartIndexFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PRIMITIVE_RESTART_INDEX_EXTENSION_NAME'
--
-- -   'EXT_PRIMITIVE_RESTART_INDEX_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIMITIVE_RESTART_INDEX_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2026-03-10 (Mike Blumenkrantz)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_primitive_restart_index Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_primitive_restart_index  ( cmdSetPrimitiveRestartIndexEXT
                                                         , PhysicalDevicePrimitiveRestartIndexFeaturesEXT(..)
                                                         , EXT_PRIMITIVE_RESTART_INDEX_SPEC_VERSION
                                                         , pattern EXT_PRIMITIVE_RESTART_INDEX_SPEC_VERSION
                                                         , EXT_PRIMITIVE_RESTART_INDEX_EXTENSION_NAME
                                                         , pattern EXT_PRIMITIVE_RESTART_INDEX_EXTENSION_NAME
                                                         ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetPrimitiveRestartIndexEXT))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIMITIVE_RESTART_INDEX_FEATURES_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetPrimitiveRestartIndexEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> IO ()

-- | vkCmdSetPrimitiveRestartIndexEXT - Set primitive assembly restart index
-- dynamically for a command buffer
--
-- = Description
--
-- This command sets a custom primitive restart index for subsequent
-- drawing commands. Binding an index buffer invalidates the custom index
-- value.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetPrimitiveRestartIndexEXT-primitiveRestartIndex-12395#
--     The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-primitiveRestartIndex primitiveRestartIndex>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetPrimitiveRestartIndexEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetPrimitiveRestartIndexEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetPrimitiveRestartIndexEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdSetPrimitiveRestartIndexEXT-videocoding# This command
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_GRAPHICS_BIT                                                                                                 | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdSetPrimitiveRestartIndexEXT is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_primitive_restart_index VK_EXT_primitive_restart_index>,
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetPrimitiveRestartIndexEXT :: forall io
                                . (MonadIO io)
                               => -- | @commandBuffer@ is the command buffer into which the command will be
                                  -- recorded.
                                  CommandBuffer
                               -> -- | @primitiveRestartIndex@ controls which special vertex index value is
                                  -- treated as restarting the assembly of primitives. This overrides the
                                  -- default values specified in
                                  -- 'Vulkan.Core10.GraphicsPipeline.PipelineInputAssemblyStateCreateInfo'::@primitiveRestartEnable@.
                                  ("primitiveRestartIndex" ::: Word32)
                               -> io ()
cmdSetPrimitiveRestartIndexEXT commandBuffer primitiveRestartIndex = liftIO $ do
  let vkCmdSetPrimitiveRestartIndexEXTPtr = pVkCmdSetPrimitiveRestartIndexEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetPrimitiveRestartIndexEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetPrimitiveRestartIndexEXT is null" Nothing Nothing
  let vkCmdSetPrimitiveRestartIndexEXT' = mkVkCmdSetPrimitiveRestartIndexEXT vkCmdSetPrimitiveRestartIndexEXTPtr
  traceAroundEvent "vkCmdSetPrimitiveRestartIndexEXT" (vkCmdSetPrimitiveRestartIndexEXT'
                                                         (commandBufferHandle (commandBuffer))
                                                         (primitiveRestartIndex))
  pure $ ()


-- | VkPhysicalDevicePrimitiveRestartIndexFeaturesEXT - Structure describing
-- features to configure primitive restart index
--
-- = Description
--
-- If the 'PhysicalDevicePrimitiveRestartIndexFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDevicePrimitiveRestartIndexFeaturesEXT', it /must/ add an
-- instance of the structure, with the desired feature members set to
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_primitive_restart_index VK_EXT_primitive_restart_index>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePrimitiveRestartIndexFeaturesEXT = PhysicalDevicePrimitiveRestartIndexFeaturesEXT
  { -- | #features-primitiveRestartIndex# @primitiveRestartIndex@ specifies
    -- whether the index for primitive restart can be set
    primitiveRestartIndex :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePrimitiveRestartIndexFeaturesEXT)
#endif
deriving instance Show PhysicalDevicePrimitiveRestartIndexFeaturesEXT

instance ToCStruct PhysicalDevicePrimitiveRestartIndexFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePrimitiveRestartIndexFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIMITIVE_RESTART_INDEX_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (primitiveRestartIndex))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIMITIVE_RESTART_INDEX_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePrimitiveRestartIndexFeaturesEXT where
  peekCStruct p = do
    primitiveRestartIndex <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePrimitiveRestartIndexFeaturesEXT
             (bool32ToBool primitiveRestartIndex)

instance Storable PhysicalDevicePrimitiveRestartIndexFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePrimitiveRestartIndexFeaturesEXT where
  zero = PhysicalDevicePrimitiveRestartIndexFeaturesEXT
           zero


type EXT_PRIMITIVE_RESTART_INDEX_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_PRIMITIVE_RESTART_INDEX_SPEC_VERSION"
pattern EXT_PRIMITIVE_RESTART_INDEX_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PRIMITIVE_RESTART_INDEX_SPEC_VERSION = 1


type EXT_PRIMITIVE_RESTART_INDEX_EXTENSION_NAME = "VK_EXT_primitive_restart_index"

-- No documentation found for TopLevel "VK_EXT_PRIMITIVE_RESTART_INDEX_EXTENSION_NAME"
pattern EXT_PRIMITIVE_RESTART_INDEX_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PRIMITIVE_RESTART_INDEX_EXTENSION_NAME = "VK_EXT_primitive_restart_index"

