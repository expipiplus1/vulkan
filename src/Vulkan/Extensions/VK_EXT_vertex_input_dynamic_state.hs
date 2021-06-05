{-# language CPP #-}
-- | = Name
--
-- VK_EXT_vertex_input_dynamic_state - device extension
--
-- == VK_EXT_vertex_input_dynamic_state
--
-- [__Name String__]
--     @VK_EXT_vertex_input_dynamic_state@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     353
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_vertex_input_dynamic_state:%20&body=@pdaniell-nv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-08-21
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Spencer Fricke, Samsung
--
--     -   Stu Smith, AMD
--
-- == Description
--
-- One of the states that contributes to the combinatorial explosion of
-- pipeline state objects that need to be created, is the vertex input
-- binding and attribute descriptions. By allowing them to be dynamic
-- applications may reduce the number of pipeline objects they need to
-- create.
--
-- This extension adds dynamic state support for what is normally static
-- state in 'Vulkan.Core10.Pipeline.PipelineVertexInputStateCreateInfo'.
--
-- == New Commands
--
-- -   'cmdSetVertexInputEXT'
--
-- == New Structures
--
-- -   'VertexInputAttributeDescription2EXT'
--
-- -   'VertexInputBindingDescription2EXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceVertexInputDynamicStateFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_VERTEX_INPUT_DYNAMIC_STATE_EXTENSION_NAME'
--
-- -   'EXT_VERTEX_INPUT_DYNAMIC_STATE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_INPUT_DYNAMIC_STATE_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_VERTEX_INPUT_ATTRIBUTE_DESCRIPTION_2_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_VERTEX_INPUT_BINDING_DESCRIPTION_2_EXT'
--
-- == Version History
--
-- -   Revision 2, 2020-11-05 (Piers Daniell)
--
--     -   Make 'VertexInputBindingDescription2EXT' extensible
--
--     -   Add new 'VertexInputAttributeDescription2EXT' struct for the
--         @pVertexAttributeDescriptions@ parameter to
--         'cmdSetVertexInputEXT' so it is also extensible
--
-- -   Revision 1, 2020-08-21 (Piers Daniell)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'PhysicalDeviceVertexInputDynamicStateFeaturesEXT',
-- 'VertexInputAttributeDescription2EXT',
-- 'VertexInputBindingDescription2EXT', 'cmdSetVertexInputEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_vertex_input_dynamic_state Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state  ( cmdSetVertexInputEXT
                                                            , PhysicalDeviceVertexInputDynamicStateFeaturesEXT(..)
                                                            , VertexInputBindingDescription2EXT(..)
                                                            , VertexInputAttributeDescription2EXT(..)
                                                            , EXT_VERTEX_INPUT_DYNAMIC_STATE_SPEC_VERSION
                                                            , pattern EXT_VERTEX_INPUT_DYNAMIC_STATE_SPEC_VERSION
                                                            , EXT_VERTEX_INPUT_DYNAMIC_STATE_EXTENSION_NAME
                                                            , pattern EXT_VERTEX_INPUT_DYNAMIC_STATE_EXTENSION_NAME
                                                            ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetVertexInputEXT))
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.VertexInputRate (VertexInputRate)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_INPUT_DYNAMIC_STATE_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_VERTEX_INPUT_ATTRIBUTE_DESCRIPTION_2_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_VERTEX_INPUT_BINDING_DESCRIPTION_2_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetVertexInputEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr VertexInputBindingDescription2EXT -> Word32 -> Ptr VertexInputAttributeDescription2EXT -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr VertexInputBindingDescription2EXT -> Word32 -> Ptr VertexInputAttributeDescription2EXT -> IO ()

-- | vkCmdSetVertexInputEXT - Set the dynamic vertex input state
--
-- = Description
--
-- This command sets the vertex input attribute and vertex input binding
-- descriptions state for subsequent drawing commands.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetVertexInputEXT-None-04790# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-vertexInputDynamicState vertexInputDynamicState>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdSetVertexInputEXT-vertexBindingDescriptionCount-04791#
--     @vertexBindingDescriptionCount@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   #VUID-vkCmdSetVertexInputEXT-vertexAttributeDescriptionCount-04792#
--     @vertexAttributeDescriptionCount@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputAttributes@
--
-- -   #VUID-vkCmdSetVertexInputEXT-binding-04793# For every @binding@
--     specified by each element of @pVertexAttributeDescriptions@, a
--     'VertexInputBindingDescription2EXT' /must/ exist in
--     @pVertexBindingDescriptions@ with the same value of @binding@
--
-- -   #VUID-vkCmdSetVertexInputEXT-pVertexBindingDescriptions-04794# All
--     elements of @pVertexBindingDescriptions@ /must/ describe distinct
--     binding numbers
--
-- -   #VUID-vkCmdSetVertexInputEXT-pVertexAttributeDescriptions-04795# All
--     elements of @pVertexAttributeDescriptions@ /must/ describe distinct
--     attribute locations
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetVertexInputEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetVertexInputEXT-pVertexBindingDescriptions-parameter#
--     If @vertexBindingDescriptionCount@ is not @0@,
--     @pVertexBindingDescriptions@ /must/ be a valid pointer to an array
--     of @vertexBindingDescriptionCount@ valid
--     'VertexInputBindingDescription2EXT' structures
--
-- -   #VUID-vkCmdSetVertexInputEXT-pVertexAttributeDescriptions-parameter#
--     If @vertexAttributeDescriptionCount@ is not @0@,
--     @pVertexAttributeDescriptions@ /must/ be a valid pointer to an array
--     of @vertexAttributeDescriptionCount@ valid
--     'VertexInputAttributeDescription2EXT' structures
--
-- -   #VUID-vkCmdSetVertexInputEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetVertexInputEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'VertexInputAttributeDescription2EXT',
-- 'VertexInputBindingDescription2EXT'
cmdSetVertexInputEXT :: forall io
                      . (MonadIO io)
                     => -- | @commandBuffer@ is the command buffer into which the command will be
                        -- recorded.
                        CommandBuffer
                     -> -- | @pVertexBindingDescriptions@ is a pointer to an array of
                        -- 'VertexInputBindingDescription2EXT' structures.
                        ("vertexBindingDescriptions" ::: Vector VertexInputBindingDescription2EXT)
                     -> -- | @pVertexAttributeDescriptions@ is a pointer to an array of
                        -- 'VertexInputAttributeDescription2EXT' structures.
                        ("vertexAttributeDescriptions" ::: Vector VertexInputAttributeDescription2EXT)
                     -> io ()
cmdSetVertexInputEXT commandBuffer vertexBindingDescriptions vertexAttributeDescriptions = liftIO . evalContT $ do
  let vkCmdSetVertexInputEXTPtr = pVkCmdSetVertexInputEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetVertexInputEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetVertexInputEXT is null" Nothing Nothing
  let vkCmdSetVertexInputEXT' = mkVkCmdSetVertexInputEXT vkCmdSetVertexInputEXTPtr
  pPVertexBindingDescriptions <- ContT $ allocaBytesAligned @VertexInputBindingDescription2EXT ((Data.Vector.length (vertexBindingDescriptions)) * 32) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPVertexBindingDescriptions `plusPtr` (32 * (i)) :: Ptr VertexInputBindingDescription2EXT) (e)) (vertexBindingDescriptions)
  pPVertexAttributeDescriptions <- ContT $ allocaBytesAligned @VertexInputAttributeDescription2EXT ((Data.Vector.length (vertexAttributeDescriptions)) * 32) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPVertexAttributeDescriptions `plusPtr` (32 * (i)) :: Ptr VertexInputAttributeDescription2EXT) (e)) (vertexAttributeDescriptions)
  lift $ traceAroundEvent "vkCmdSetVertexInputEXT" (vkCmdSetVertexInputEXT' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (vertexBindingDescriptions)) :: Word32)) (pPVertexBindingDescriptions) ((fromIntegral (Data.Vector.length $ (vertexAttributeDescriptions)) :: Word32)) (pPVertexAttributeDescriptions))
  pure $ ()


-- | VkPhysicalDeviceVertexInputDynamicStateFeaturesEXT - Structure
-- describing whether the dynamic vertex input state can be used
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceVertexInputDynamicStateFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceVertexInputDynamicStateFeaturesEXT' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVertexInputDynamicStateFeaturesEXT = PhysicalDeviceVertexInputDynamicStateFeaturesEXT
  { -- | #features-vertexInputDynamicState# @vertexInputDynamicState@ indicates
    -- that the implementation supports the following dynamic states:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
    vertexInputDynamicState :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVertexInputDynamicStateFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceVertexInputDynamicStateFeaturesEXT

instance ToCStruct PhysicalDeviceVertexInputDynamicStateFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVertexInputDynamicStateFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_INPUT_DYNAMIC_STATE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (vertexInputDynamicState))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_INPUT_DYNAMIC_STATE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceVertexInputDynamicStateFeaturesEXT where
  peekCStruct p = do
    vertexInputDynamicState <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceVertexInputDynamicStateFeaturesEXT
             (bool32ToBool vertexInputDynamicState)

instance Storable PhysicalDeviceVertexInputDynamicStateFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVertexInputDynamicStateFeaturesEXT where
  zero = PhysicalDeviceVertexInputDynamicStateFeaturesEXT
           zero


-- | VkVertexInputBindingDescription2EXT - Structure specifying the extended
-- vertex input binding description
--
-- == Valid Usage
--
-- -   #VUID-VkVertexInputBindingDescription2EXT-binding-04796# @binding@
--     /must/ be less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   #VUID-VkVertexInputBindingDescription2EXT-stride-04797# @stride@
--     /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindingStride@
--
-- -   #VUID-VkVertexInputBindingDescription2EXT-divisor-04798# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-vertexAttributeInstanceRateZeroDivisor vertexAttributeInstanceRateZeroDivisor>
--     feature is not enabled, @divisor@ /must/ not be @0@
--
-- -   #VUID-VkVertexInputBindingDescription2EXT-divisor-04799# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-vertexAttributeInstanceRateDivisor vertexAttributeInstanceRateDivisor>
--     feature is not enabled, @divisor@ /must/ be @1@
--
-- -   #VUID-VkVertexInputBindingDescription2EXT-divisor-04800# @divisor@
--     /must/ be a value between @0@ and
--     'Vulkan.Extensions.VK_EXT_vertex_attribute_divisor.PhysicalDeviceVertexAttributeDivisorPropertiesEXT'::@maxVertexAttribDivisor@,
--     inclusive
--
-- -   #VUID-VkVertexInputBindingDescription2EXT-divisor-04801# If
--     @divisor@ is not @1@ then @inputRate@ /must/ be of type
--     'Vulkan.Core10.Enums.VertexInputRate.VERTEX_INPUT_RATE_INSTANCE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkVertexInputBindingDescription2EXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_VERTEX_INPUT_BINDING_DESCRIPTION_2_EXT'
--
-- -   #VUID-VkVertexInputBindingDescription2EXT-inputRate-parameter#
--     @inputRate@ /must/ be a valid
--     'Vulkan.Core10.Enums.VertexInputRate.VertexInputRate' value
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core10.Enums.VertexInputRate.VertexInputRate',
-- 'cmdSetVertexInputEXT'
data VertexInputBindingDescription2EXT = VertexInputBindingDescription2EXT
  { -- | @binding@ is the binding number that this structure describes.
    binding :: Word32
  , -- | @stride@ is the distance in bytes between two consecutive elements
    -- within the buffer.
    stride :: Word32
  , -- | @inputRate@ is a 'Vulkan.Core10.Enums.VertexInputRate.VertexInputRate'
    -- value specifying whether vertex attribute addressing is a function of
    -- the vertex index or of the instance index.
    inputRate :: VertexInputRate
  , -- | @divisor@ is the number of successive instances that will use the same
    -- value of the vertex attribute when instanced rendering is enabled. This
    -- member /can/ be set to a value other than @1@ if the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-vertexAttributeInstanceRateDivisor vertexAttributeInstanceRateDivisor>
    -- feature is enabled. For example, if the divisor is N, the same vertex
    -- attribute will be applied to N successive instances before moving on to
    -- the next vertex attribute. The maximum value of divisor is
    -- implementation dependent and can be queried using
    -- 'Vulkan.Extensions.VK_EXT_vertex_attribute_divisor.PhysicalDeviceVertexAttributeDivisorPropertiesEXT'::@maxVertexAttribDivisor@.
    -- A value of @0@ /can/ be used for the divisor if the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-vertexAttributeInstanceRateZeroDivisor vertexAttributeInstanceRateZeroDivisor>
    -- feature is enabled. In this case, the same vertex attribute will be
    -- applied to all instances.
    divisor :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (VertexInputBindingDescription2EXT)
#endif
deriving instance Show VertexInputBindingDescription2EXT

instance ToCStruct VertexInputBindingDescription2EXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p VertexInputBindingDescription2EXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_VERTEX_INPUT_BINDING_DESCRIPTION_2_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (binding)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (stride)
    poke ((p `plusPtr` 24 :: Ptr VertexInputRate)) (inputRate)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (divisor)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_VERTEX_INPUT_BINDING_DESCRIPTION_2_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr VertexInputRate)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    f

instance FromCStruct VertexInputBindingDescription2EXT where
  peekCStruct p = do
    binding <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    stride <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    inputRate <- peek @VertexInputRate ((p `plusPtr` 24 :: Ptr VertexInputRate))
    divisor <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pure $ VertexInputBindingDescription2EXT
             binding stride inputRate divisor

instance Storable VertexInputBindingDescription2EXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero VertexInputBindingDescription2EXT where
  zero = VertexInputBindingDescription2EXT
           zero
           zero
           zero
           zero


-- | VkVertexInputAttributeDescription2EXT - Structure specifying the
-- extended vertex input attribute description
--
-- == Valid Usage
--
-- -   #VUID-VkVertexInputAttributeDescription2EXT-location-04802#
--     @location@ /must/ be less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputAttributes@
--
-- -   #VUID-VkVertexInputAttributeDescription2EXT-binding-04803# @binding@
--     /must/ be less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   #VUID-VkVertexInputAttributeDescription2EXT-offset-04804# @offset@
--     /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputAttributeOffset@
--
-- -   #VUID-VkVertexInputAttributeDescription2EXT-format-04805# @format@
--     /must/ be allowed as a vertex buffer format, as specified by the
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_VERTEX_BUFFER_BIT'
--     flag in
--     'Vulkan.Core10.DeviceInitialization.FormatProperties'::@bufferFeatures@
--     returned by
--     'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFormatProperties'
--
-- -   #VUID-VkVertexInputAttributeDescription2EXT-vertexAttributeAccessBeyondStride-04806#
--     If the @VK_KHR_portability_subset@ extension is enabled, and
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR'::@vertexAttributeAccessBeyondStride@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', the sum of @offset@ plus
--     the size of the vertex attribute data described by @format@ /must/
--     not be greater than @stride@ in the
--     'VertexInputBindingDescription2EXT' referenced in @binding@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkVertexInputAttributeDescription2EXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_VERTEX_INPUT_ATTRIBUTE_DESCRIPTION_2_EXT'
--
-- -   #VUID-VkVertexInputAttributeDescription2EXT-format-parameter#
--     @format@ /must/ be a valid 'Vulkan.Core10.Enums.Format.Format' value
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdSetVertexInputEXT'
data VertexInputAttributeDescription2EXT = VertexInputAttributeDescription2EXT
  { -- | @location@ is the shader input location number for this attribute.
    location :: Word32
  , -- | @binding@ is the binding number which this attribute takes its data
    -- from.
    binding :: Word32
  , -- | @format@ is the size and type of the vertex attribute data.
    format :: Format
  , -- | @offset@ is a byte offset of this attribute relative to the start of an
    -- element in the vertex input binding.
    offset :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (VertexInputAttributeDescription2EXT)
#endif
deriving instance Show VertexInputAttributeDescription2EXT

instance ToCStruct VertexInputAttributeDescription2EXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p VertexInputAttributeDescription2EXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_VERTEX_INPUT_ATTRIBUTE_DESCRIPTION_2_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (location)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (binding)
    poke ((p `plusPtr` 24 :: Ptr Format)) (format)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (offset)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_VERTEX_INPUT_ATTRIBUTE_DESCRIPTION_2_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    f

instance FromCStruct VertexInputAttributeDescription2EXT where
  peekCStruct p = do
    location <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    binding <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    format <- peek @Format ((p `plusPtr` 24 :: Ptr Format))
    offset <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pure $ VertexInputAttributeDescription2EXT
             location binding format offset

instance Storable VertexInputAttributeDescription2EXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero VertexInputAttributeDescription2EXT where
  zero = VertexInputAttributeDescription2EXT
           zero
           zero
           zero
           zero


type EXT_VERTEX_INPUT_DYNAMIC_STATE_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_VERTEX_INPUT_DYNAMIC_STATE_SPEC_VERSION"
pattern EXT_VERTEX_INPUT_DYNAMIC_STATE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_VERTEX_INPUT_DYNAMIC_STATE_SPEC_VERSION = 2


type EXT_VERTEX_INPUT_DYNAMIC_STATE_EXTENSION_NAME = "VK_EXT_vertex_input_dynamic_state"

-- No documentation found for TopLevel "VK_EXT_VERTEX_INPUT_DYNAMIC_STATE_EXTENSION_NAME"
pattern EXT_VERTEX_INPUT_DYNAMIC_STATE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_VERTEX_INPUT_DYNAMIC_STATE_EXTENSION_NAME = "VK_EXT_vertex_input_dynamic_state"

