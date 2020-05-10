{-# language CPP #-}
module Vulkan.Extensions.VK_NV_device_generated_commands  ( cmdExecuteGeneratedCommandsNV
                                                          , cmdPreprocessGeneratedCommandsNV
                                                          , cmdBindPipelineShaderGroupNV
                                                          , getGeneratedCommandsMemoryRequirementsNV
                                                          , createIndirectCommandsLayoutNV
                                                          , withIndirectCommandsLayoutNV
                                                          , destroyIndirectCommandsLayoutNV
                                                          , PhysicalDeviceDeviceGeneratedCommandsFeaturesNV(..)
                                                          , PhysicalDeviceDeviceGeneratedCommandsPropertiesNV(..)
                                                          , GraphicsShaderGroupCreateInfoNV(..)
                                                          , GraphicsPipelineShaderGroupsCreateInfoNV(..)
                                                          , BindShaderGroupIndirectCommandNV(..)
                                                          , BindIndexBufferIndirectCommandNV(..)
                                                          , BindVertexBufferIndirectCommandNV(..)
                                                          , SetStateFlagsIndirectCommandNV(..)
                                                          , IndirectCommandsStreamNV(..)
                                                          , IndirectCommandsLayoutTokenNV(..)
                                                          , IndirectCommandsLayoutCreateInfoNV(..)
                                                          , GeneratedCommandsInfoNV(..)
                                                          , GeneratedCommandsMemoryRequirementsInfoNV(..)
                                                          , IndirectCommandsLayoutUsageFlagBitsNV( INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV
                                                                                                 , INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV
                                                                                                 , INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV
                                                                                                 , ..
                                                                                                 )
                                                          , IndirectCommandsLayoutUsageFlagsNV
                                                          , IndirectStateFlagBitsNV( INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV
                                                                                   , ..
                                                                                   )
                                                          , IndirectStateFlagsNV
                                                          , IndirectCommandsTokenTypeNV( INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV
                                                                                       , ..
                                                                                       )
                                                          , NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION
                                                          , pattern NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION
                                                          , NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
                                                          , pattern NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
                                                          , IndirectCommandsLayoutNV(..)
                                                          ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
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
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.BaseType (bool32ToBool)
import Vulkan.Core10.BaseType (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.CStruct.Extends (withSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.BaseType (Bool32)
import Vulkan.Core10.BaseType (Bool32(..))
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.BaseType (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindPipelineShaderGroupNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdExecuteGeneratedCommandsNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPreprocessGeneratedCommandsNV))
import Vulkan.Dynamic (DeviceCmds(pVkCreateIndirectCommandsLayoutNV))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyIndirectCommandsLayoutNV))
import Vulkan.Dynamic (DeviceCmds(pVkGetGeneratedCommandsMemoryRequirementsNV))
import Vulkan.Core10.BaseType (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.Core10.BaseType (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.IndexType (IndexType)
import Vulkan.Extensions.Handles (IndirectCommandsLayoutNV)
import Vulkan.Extensions.Handles (IndirectCommandsLayoutNV(..))
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (MemoryRequirements2)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint)
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint(..))
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Core10.Pipeline (PipelineShaderStageCreateInfo)
import Vulkan.Core10.Pipeline (PipelineTessellationStateCreateInfo)
import Vulkan.Core10.Pipeline (PipelineVertexInputStateCreateInfo)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (IndirectCommandsLayoutNV(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdExecuteGeneratedCommandsNV
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> Ptr GeneratedCommandsInfoNV -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> Ptr GeneratedCommandsInfoNV -> IO ()

-- | vkCmdExecuteGeneratedCommandsNV - Performs the generation and execution
-- of commands on the device
--
-- == Valid Usage
--
-- -   [[VUID-{refpage}-None-02690]] If a 'Vulkan.Core10.Handles.ImageView'
--     is sampled with 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' as a
--     result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   [[VUID-{refpage}-None-02691]] If a 'Vulkan.Core10.Handles.ImageView'
--     is accessed using atomic operations as a result of this command,
--     then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   [[VUID-{refpage}-None-02692]] If a 'Vulkan.Core10.Handles.ImageView'
--     is sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   [[VUID-{refpage}-filterCubic-02694]] Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   [[VUID-{refpage}-filterCubicMinmax-02695]] Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' with a
--     reduction mode of either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering together with minmax filtering, as
--     specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   [[VUID-{refpage}-flags-02696]] Any 'Vulkan.Core10.Handles.Image'
--     created with a 'Vulkan.Core10.Image.ImageCreateInfo'::@flags@
--     containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   [[VUID-{refpage}-None-02697]] For each set /n/ that is statically
--     used by the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline
--     bind point used by this command, a descriptor set /must/ have been
--     bound to /n/ at the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   [[VUID-{refpage}-None-02698]] For each push constant that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a push constant value
--     /must/ have been set for the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   [[VUID-{refpage}-None-02699]] Descriptors in each bound descriptor
--     set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid if they are statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command
--
-- -   [[VUID-{refpage}-None-02700]] A valid pipeline /must/ be bound to
--     the pipeline bind point used by this command
--
-- -   [[VUID-{refpage}-commandBuffer-02701]] If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command requires any dynamic state, that state
--     /must/ have been set for @commandBuffer@, and done so after any
--     previously bound pipeline with the corresponding state not specified
--     as dynamic
--
-- -   [[VUID-{refpage}-None-02859]] There /must/ not have been any calls
--     to dynamic state setting commands for any state not specified as
--     dynamic in the 'Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command, since that pipeline was
--     bound
--
-- -   [[VUID-{refpage}-None-02702]] If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used to sample from any
--     'Vulkan.Core10.Handles.Image' with a
--     'Vulkan.Core10.Handles.ImageView' of the type
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY', in
--     any shader stage
--
-- -   [[VUID-{refpage}-None-02703]] If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   [[VUID-{refpage}-None-02704]] If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   [[VUID-{refpage}-None-02705]] If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   [[VUID-{refpage}-None-02706]] If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   [[VUID-{refpage}-commandBuffer-02707]] If @commandBuffer@ is an
--     unprotected command buffer, any resource accessed by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be a protected resource
--
-- -   [[VUID-{refpage}-renderPass-02684]] The current render pass /must/
--     be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   [[VUID-{refpage}-subpass-02685]] The subpass index of the current
--     render pass /must/ be equal to the @subpass@ member of the
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   [[VUID-{refpage}-None-02686]] Every input attachment used by the
--     current subpass /must/ be bound to the pipeline via a descriptor set
--
-- -   [[VUID-{refpage}-None-02687]] Image subresources used as attachments
--     in the current render pass /must/ not be accessed in any way other
--     than as an attachment by this command
--
-- -   [[VUID-{refpage}-maxMultiviewInstanceIndex-02688]] If the draw is
--     recorded in a render pass instance with multiview enabled, the
--     maximum instance index /must/ be less than or equal to
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@
--
-- -   [[VUID-{refpage}-sampleLocationsEnable-02689]] If the bound graphics
--     pipeline was created with
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Vulkan.Core10.BaseType.TRUE' and the current subpass has a
--     depth\/stencil attachment, then that attachment /must/ have been
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   [[VUID-{refpage}-None-04007]] All vertex input bindings accessed via
--     vertex input variables declared in the vertex shader entry point’s
--     interface /must/ have either valid or
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' buffers bound
--
-- -   [[VUID-{refpage}-None-04008]] If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, all vertex input bindings accessed via
--     vertex input variables declared in the vertex shader entry point’s
--     interface /must/ not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   [[VUID-{refpage}-None-02721]] For a given vertex buffer binding, any
--     attribute data fetched /must/ be entirely contained within the
--     corresponding vertex buffer binding, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input ???>
--
-- -   @commandBuffer@ /must/ not be a protected command buffer
--
-- -   If @isPreprocessed@ is 'Vulkan.Core10.BaseType.TRUE' then
--     'cmdPreprocessGeneratedCommandsNV' /must/ have already been executed
--     on the device, using the same @pGeneratedCommandsInfo@ content as
--     well as the content of the input buffers it references (all except
--     'GeneratedCommandsInfoNV'::@preprocessBuffer@). Furthermore
--     @pGeneratedCommandsInfo@\`s @indirectCommandsLayout@ /must/ have
--     been created with the
--     'INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV' bit set
--
-- -   'GeneratedCommandsInfoNV'::@pipeline@ /must/ match the current bound
--     pipeline at 'GeneratedCommandsInfoNV'::@pipelineBindPoint@
--
-- -   Transform feedback /must/ not be active
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#feature-device-generated-commands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pGeneratedCommandsInfo@ /must/ be a valid pointer to a valid
--     'GeneratedCommandsInfoNV' structure
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   This command /must/ only be called inside of a render pass instance
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'GeneratedCommandsInfoNV'
cmdExecuteGeneratedCommandsNV :: forall io
                               . (MonadIO io)
                              => -- | @commandBuffer@ is the command buffer into which the command is
                                 -- recorded.
                                 CommandBuffer
                              -> -- | @isPreprocessed@ represents whether the input data has already been
                                 -- preprocessed on the device. If it is 'Vulkan.Core10.BaseType.FALSE' this
                                 -- command will implicitly trigger the preprocessing step, otherwise not.
                                 ("isPreprocessed" ::: Bool)
                              -> -- | @pGeneratedCommandsInfo@ is a pointer to an instance of the
                                 -- 'GeneratedCommandsInfoNV' structure containing parameters affecting the
                                 -- generation of commands.
                                 GeneratedCommandsInfoNV
                              -> io ()
cmdExecuteGeneratedCommandsNV commandBuffer isPreprocessed generatedCommandsInfo = liftIO . evalContT $ do
  let vkCmdExecuteGeneratedCommandsNVPtr = pVkCmdExecuteGeneratedCommandsNV (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdExecuteGeneratedCommandsNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdExecuteGeneratedCommandsNV is null" Nothing Nothing
  let vkCmdExecuteGeneratedCommandsNV' = mkVkCmdExecuteGeneratedCommandsNV vkCmdExecuteGeneratedCommandsNVPtr
  pGeneratedCommandsInfo <- ContT $ withCStruct (generatedCommandsInfo)
  lift $ vkCmdExecuteGeneratedCommandsNV' (commandBufferHandle (commandBuffer)) (boolToBool32 (isPreprocessed)) pGeneratedCommandsInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPreprocessGeneratedCommandsNV
  :: FunPtr (Ptr CommandBuffer_T -> Ptr GeneratedCommandsInfoNV -> IO ()) -> Ptr CommandBuffer_T -> Ptr GeneratedCommandsInfoNV -> IO ()

-- | vkCmdPreprocessGeneratedCommandsNV - Performs preprocessing for
-- generated commands
--
-- == Valid Usage
--
-- -   @commandBuffer@ /must/ not be a protected command buffer
--
-- -   @pGeneratedCommandsInfo@\`s @indirectCommandsLayout@ /must/ have
--     been created with the
--     'INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV' bit set
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#feature-device-generated-commands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pGeneratedCommandsInfo@ /must/ be a valid pointer to a valid
--     'GeneratedCommandsInfoNV' structure
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'GeneratedCommandsInfoNV'
cmdPreprocessGeneratedCommandsNV :: forall io
                                  . (MonadIO io)
                                 => -- | @commandBuffer@ is the command buffer which does the preprocessing.
                                    CommandBuffer
                                 -> -- | @pGeneratedCommandsInfo@ is a pointer to an instance of the
                                    -- 'GeneratedCommandsInfoNV' structure containing parameters affecting the
                                    -- preprocessing step.
                                    GeneratedCommandsInfoNV
                                 -> io ()
cmdPreprocessGeneratedCommandsNV commandBuffer generatedCommandsInfo = liftIO . evalContT $ do
  let vkCmdPreprocessGeneratedCommandsNVPtr = pVkCmdPreprocessGeneratedCommandsNV (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdPreprocessGeneratedCommandsNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPreprocessGeneratedCommandsNV is null" Nothing Nothing
  let vkCmdPreprocessGeneratedCommandsNV' = mkVkCmdPreprocessGeneratedCommandsNV vkCmdPreprocessGeneratedCommandsNVPtr
  pGeneratedCommandsInfo <- ContT $ withCStruct (generatedCommandsInfo)
  lift $ vkCmdPreprocessGeneratedCommandsNV' (commandBufferHandle (commandBuffer)) pGeneratedCommandsInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindPipelineShaderGroupNV
  :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> Pipeline -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> PipelineBindPoint -> Pipeline -> Word32 -> IO ()

-- | vkCmdBindPipelineShaderGroupNV - Bind a pipeline object
--
-- == Valid Usage
--
-- -   @groupIndex@ /must/ be @0@ or less than the effective
--     'GraphicsPipelineShaderGroupsCreateInfoNV'::@groupCount@ including
--     the referenced pipelines
--
-- -   The @pipelineBindPoint@ /must/ be
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   The same restrictions as
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindPipeline' apply as if
--     the bound pipeline was created only with the Shader Group from the
--     @groupIndex@ information
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#feature-device-generated-commands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pipelineBindPoint@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
--
-- -   @pipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   Both of @commandBuffer@, and @pipeline@ /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
cmdBindPipelineShaderGroupNV :: forall io
                              . (MonadIO io)
                             => -- | @commandBuffer@ is the command buffer that the pipeline will be bound
                                -- to.
                                CommandBuffer
                             -> -- | @pipelineBindPoint@ is a
                                -- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
                                -- specifying to which bind point the pipeline is bound.
                                PipelineBindPoint
                             -> -- | @pipeline@ is the pipeline to be bound.
                                Pipeline
                             -> -- | @groupIndex@ is the shader group to be bound.
                                ("groupIndex" ::: Word32)
                             -> io ()
cmdBindPipelineShaderGroupNV commandBuffer pipelineBindPoint pipeline groupIndex = liftIO $ do
  let vkCmdBindPipelineShaderGroupNVPtr = pVkCmdBindPipelineShaderGroupNV (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdBindPipelineShaderGroupNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindPipelineShaderGroupNV is null" Nothing Nothing
  let vkCmdBindPipelineShaderGroupNV' = mkVkCmdBindPipelineShaderGroupNV vkCmdBindPipelineShaderGroupNVPtr
  vkCmdBindPipelineShaderGroupNV' (commandBufferHandle (commandBuffer)) (pipelineBindPoint) (pipeline) (groupIndex)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetGeneratedCommandsMemoryRequirementsNV
  :: FunPtr (Ptr Device_T -> Ptr GeneratedCommandsMemoryRequirementsInfoNV -> Ptr (SomeStruct MemoryRequirements2) -> IO ()) -> Ptr Device_T -> Ptr GeneratedCommandsMemoryRequirementsInfoNV -> Ptr (SomeStruct MemoryRequirements2) -> IO ()

-- | vkGetGeneratedCommandsMemoryRequirementsNV - Retrieve the buffer
-- allocation requirements for generated commands
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#feature-device-generated-commands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     'GeneratedCommandsMemoryRequirementsInfoNV' structure
--
-- -   @pMemoryRequirements@ /must/ be a valid pointer to a
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
--     structure
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device',
-- 'GeneratedCommandsMemoryRequirementsInfoNV',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
getGeneratedCommandsMemoryRequirementsNV :: forall a io
                                          . (Extendss MemoryRequirements2 a, PokeChain a, PeekChain a, MonadIO io)
                                         => -- | @device@ is the logical device that owns the buffer.
                                            Device
                                         -> -- | @pInfo@ is a pointer to an instance of the
                                            -- 'GeneratedCommandsMemoryRequirementsInfoNV' structure containing
                                            -- parameters required for the memory requirements query.
                                            GeneratedCommandsMemoryRequirementsInfoNV
                                         -> io (MemoryRequirements2 a)
getGeneratedCommandsMemoryRequirementsNV device info = liftIO . evalContT $ do
  let vkGetGeneratedCommandsMemoryRequirementsNVPtr = pVkGetGeneratedCommandsMemoryRequirementsNV (deviceCmds (device :: Device))
  lift $ unless (vkGetGeneratedCommandsMemoryRequirementsNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetGeneratedCommandsMemoryRequirementsNV is null" Nothing Nothing
  let vkGetGeneratedCommandsMemoryRequirementsNV' = mkVkGetGeneratedCommandsMemoryRequirementsNV vkGetGeneratedCommandsMemoryRequirementsNVPtr
  pInfo <- ContT $ withCStruct (info)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2 _))
  lift $ vkGetGeneratedCommandsMemoryRequirementsNV' (deviceHandle (device)) pInfo (forgetExtensions (pPMemoryRequirements))
  pMemoryRequirements <- lift $ peekCStruct @(MemoryRequirements2 _) pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateIndirectCommandsLayoutNV
  :: FunPtr (Ptr Device_T -> Ptr IndirectCommandsLayoutCreateInfoNV -> Ptr AllocationCallbacks -> Ptr IndirectCommandsLayoutNV -> IO Result) -> Ptr Device_T -> Ptr IndirectCommandsLayoutCreateInfoNV -> Ptr AllocationCallbacks -> Ptr IndirectCommandsLayoutNV -> IO Result

-- | vkCreateIndirectCommandsLayoutNV - Create an indirect command layout
-- object
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#feature-device-generated-commands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'IndirectCommandsLayoutCreateInfoNV' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   @pIndirectCommandsLayout@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'IndirectCommandsLayoutCreateInfoNV',
-- 'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV'
createIndirectCommandsLayoutNV :: forall io
                                . (MonadIO io)
                               => -- | @device@ is the logical device that creates the indirect command layout.
                                  Device
                               -> -- | @pCreateInfo@ is a pointer to an instance of the
                                  -- 'IndirectCommandsLayoutCreateInfoNV' structure containing parameters
                                  -- affecting creation of the indirect command layout.
                                  IndirectCommandsLayoutCreateInfoNV
                               -> -- | @pAllocator@ controls host memory allocation as described in the
                                  -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                  -- chapter.
                                  ("allocator" ::: Maybe AllocationCallbacks)
                               -> io (IndirectCommandsLayoutNV)
createIndirectCommandsLayoutNV device createInfo allocator = liftIO . evalContT $ do
  let vkCreateIndirectCommandsLayoutNVPtr = pVkCreateIndirectCommandsLayoutNV (deviceCmds (device :: Device))
  lift $ unless (vkCreateIndirectCommandsLayoutNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateIndirectCommandsLayoutNV is null" Nothing Nothing
  let vkCreateIndirectCommandsLayoutNV' = mkVkCreateIndirectCommandsLayoutNV vkCreateIndirectCommandsLayoutNVPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPIndirectCommandsLayout <- ContT $ bracket (callocBytes @IndirectCommandsLayoutNV 8) free
  r <- lift $ vkCreateIndirectCommandsLayoutNV' (deviceHandle (device)) pCreateInfo pAllocator (pPIndirectCommandsLayout)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pIndirectCommandsLayout <- lift $ peek @IndirectCommandsLayoutNV pPIndirectCommandsLayout
  pure $ (pIndirectCommandsLayout)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createIndirectCommandsLayoutNV' and 'destroyIndirectCommandsLayoutNV'
--
-- To ensure that 'destroyIndirectCommandsLayoutNV' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the first argument.
-- To just extract the pair pass '(,)' as the first argument.
--
withIndirectCommandsLayoutNV :: forall io r . MonadIO io => Device -> IndirectCommandsLayoutCreateInfoNV -> Maybe AllocationCallbacks -> (io (IndirectCommandsLayoutNV) -> ((IndirectCommandsLayoutNV) -> io ()) -> r) -> r
withIndirectCommandsLayoutNV device pCreateInfo pAllocator b =
  b (createIndirectCommandsLayoutNV device pCreateInfo pAllocator)
    (\(o0) -> destroyIndirectCommandsLayoutNV device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyIndirectCommandsLayoutNV
  :: FunPtr (Ptr Device_T -> IndirectCommandsLayoutNV -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> IndirectCommandsLayoutNV -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyIndirectCommandsLayoutNV - Destroy an indirect commands layout
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @indirectCommandsLayout@ /must/
--     have completed execution
--
-- -   If 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @indirectCommandsLayout@ was created, a compatible set
--     of callbacks /must/ be provided here
--
-- -   If no 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @indirectCommandsLayout@ was created, @pAllocator@
--     /must/ be @NULL@
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#feature-device-generated-commands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   If @indirectCommandsLayout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @indirectCommandsLayout@
--     /must/ be a valid
--     'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   If @indirectCommandsLayout@ is a valid handle, it /must/ have been
--     created, allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @indirectCommandsLayout@ /must/ be externally
--     synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV'
destroyIndirectCommandsLayoutNV :: forall io
                                 . (MonadIO io)
                                => -- | @device@ is the logical device that destroys the layout.
                                   Device
                                -> -- | @indirectCommandsLayout@ is the layout to destroy.
                                   IndirectCommandsLayoutNV
                                -> -- | @pAllocator@ controls host memory allocation as described in the
                                   -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                   -- chapter.
                                   ("allocator" ::: Maybe AllocationCallbacks)
                                -> io ()
destroyIndirectCommandsLayoutNV device indirectCommandsLayout allocator = liftIO . evalContT $ do
  let vkDestroyIndirectCommandsLayoutNVPtr = pVkDestroyIndirectCommandsLayoutNV (deviceCmds (device :: Device))
  lift $ unless (vkDestroyIndirectCommandsLayoutNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyIndirectCommandsLayoutNV is null" Nothing Nothing
  let vkDestroyIndirectCommandsLayoutNV' = mkVkDestroyIndirectCommandsLayoutNV vkDestroyIndirectCommandsLayoutNVPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyIndirectCommandsLayoutNV' (deviceHandle (device)) (indirectCommandsLayout) pAllocator
  pure $ ()


-- | VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV - Structure describing
-- the device-generated commands features that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceDeviceGeneratedCommandsFeaturesNV'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceDeviceGeneratedCommandsFeaturesNV' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceDeviceGeneratedCommandsFeaturesNV' /can/ also be used in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable
-- the features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDeviceGeneratedCommandsFeaturesNV = PhysicalDeviceDeviceGeneratedCommandsFeaturesNV
  { -- | @deviceGeneratedCommands@ indicates whether the implementation supports
    -- functionality to generate commands on the device. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#device-generated-commands Device-Generated Commands>.
    deviceGeneratedCommands :: Bool }
  deriving (Typeable, Eq)
deriving instance Show PhysicalDeviceDeviceGeneratedCommandsFeaturesNV

instance ToCStruct PhysicalDeviceDeviceGeneratedCommandsFeaturesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDeviceGeneratedCommandsFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (deviceGeneratedCommands))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDeviceGeneratedCommandsFeaturesNV where
  peekCStruct p = do
    deviceGeneratedCommands <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDeviceGeneratedCommandsFeaturesNV
             (bool32ToBool deviceGeneratedCommands)

instance Storable PhysicalDeviceDeviceGeneratedCommandsFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDeviceGeneratedCommandsFeaturesNV where
  zero = PhysicalDeviceDeviceGeneratedCommandsFeaturesNV
           zero


-- | VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV - Structure
-- describing push descriptor limits that can be supported by an
-- implementation
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDeviceGeneratedCommandsPropertiesNV = PhysicalDeviceDeviceGeneratedCommandsPropertiesNV
  { -- | @maxGraphicsShaderGroupCount@ is the maximum number of shader groups in
    -- 'GraphicsPipelineShaderGroupsCreateInfoNV'.
    maxGraphicsShaderGroupCount :: Word32
  , -- | @maxIndirectSequenceCount@ is the maximum number of sequences in
    -- 'GeneratedCommandsInfoNV' and in
    -- 'GeneratedCommandsMemoryRequirementsInfoNV'.
    maxIndirectSequenceCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV" "maxIndirectCommandsTokenCount"
    maxIndirectCommandsTokenCount :: Word32
  , -- | @maxIndirectCommandsStreamCount@ is the maximum number of streams in
    -- 'IndirectCommandsLayoutCreateInfoNV'.
    maxIndirectCommandsStreamCount :: Word32
  , -- | @maxIndirectCommandsTokenOffset@ is the maximum offset in
    -- 'IndirectCommandsLayoutTokenNV'.
    maxIndirectCommandsTokenOffset :: Word32
  , -- | @maxIndirectCommandsStreamStride@ is the maximum stream stride in
    -- 'IndirectCommandsLayoutCreateInfoNV'.
    maxIndirectCommandsStreamStride :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV" "minSequencesCountBufferOffsetAlignment"
    minSequencesCountBufferOffsetAlignment :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV" "minSequencesIndexBufferOffsetAlignment"
    minSequencesIndexBufferOffsetAlignment :: Word32
  , -- | @minIndirectCommandsBufferOffsetAlignment@ is the minimum alignment for
    -- memory addresses used in 'IndirectCommandsStreamNV' and as preprocess
    -- buffer in 'GeneratedCommandsInfoNV'.
    minIndirectCommandsBufferOffsetAlignment :: Word32
  }
  deriving (Typeable, Eq)
deriving instance Show PhysicalDeviceDeviceGeneratedCommandsPropertiesNV

instance ToCStruct PhysicalDeviceDeviceGeneratedCommandsPropertiesNV where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDeviceGeneratedCommandsPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxGraphicsShaderGroupCount)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxIndirectSequenceCount)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxIndirectCommandsTokenCount)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (maxIndirectCommandsStreamCount)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (maxIndirectCommandsTokenOffset)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (maxIndirectCommandsStreamStride)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (minSequencesCountBufferOffsetAlignment)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (minSequencesIndexBufferOffsetAlignment)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (minIndirectCommandsBufferOffsetAlignment)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceDeviceGeneratedCommandsPropertiesNV where
  peekCStruct p = do
    maxGraphicsShaderGroupCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxIndirectSequenceCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxIndirectCommandsTokenCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    maxIndirectCommandsStreamCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    maxIndirectCommandsTokenOffset <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    maxIndirectCommandsStreamStride <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    minSequencesCountBufferOffsetAlignment <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    minSequencesIndexBufferOffsetAlignment <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    minIndirectCommandsBufferOffsetAlignment <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pure $ PhysicalDeviceDeviceGeneratedCommandsPropertiesNV
             maxGraphicsShaderGroupCount maxIndirectSequenceCount maxIndirectCommandsTokenCount maxIndirectCommandsStreamCount maxIndirectCommandsTokenOffset maxIndirectCommandsStreamStride minSequencesCountBufferOffsetAlignment minSequencesIndexBufferOffsetAlignment minIndirectCommandsBufferOffsetAlignment

instance Storable PhysicalDeviceDeviceGeneratedCommandsPropertiesNV where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDeviceGeneratedCommandsPropertiesNV where
  zero = PhysicalDeviceDeviceGeneratedCommandsPropertiesNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkGraphicsShaderGroupCreateInfoNV - Structure specifying override
-- parameters for each shader group
--
-- == Valid Usage
--
-- -   For @stageCount@, the same restrictions as in
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@stageCount@
--     apply
--
-- -   For @pStages@, the same restrictions as in
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pStages@ apply
--
-- -   For @pVertexInputState@, the same restrictions as in
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pVertexInputState@
--     apply
--
-- -   For @pTessellationState@, the same restrictions as in
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pTessellationState@
--     apply
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @pStages@ /must/ be a valid pointer to an array of @stageCount@
--     valid 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo'
--     structures
--
-- -   @stageCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'GraphicsPipelineShaderGroupsCreateInfoNV',
-- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo',
-- 'Vulkan.Core10.Pipeline.PipelineTessellationStateCreateInfo',
-- 'Vulkan.Core10.Pipeline.PipelineVertexInputStateCreateInfo',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data GraphicsShaderGroupCreateInfoNV = GraphicsShaderGroupCreateInfoNV
  { -- | @pStages@ is an array of size @stageCount@ structures of type
    -- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo' describing the
    -- set of the shader stages to be included in this shader group.
    stages :: Vector (SomeStruct PipelineShaderStageCreateInfo)
  , -- | @pVertexInputState@ is a pointer to an instance of the
    -- 'Vulkan.Core10.Pipeline.PipelineVertexInputStateCreateInfo' structure.
    vertexInputState :: Maybe (SomeStruct PipelineVertexInputStateCreateInfo)
  , -- | @pTessellationState@ is a pointer to an instance of the
    -- 'Vulkan.Core10.Pipeline.PipelineTessellationStateCreateInfo' structure,
    -- and is ignored if the shader group does not include a tessellation
    -- control shader stage and tessellation evaluation shader stage.
    tessellationState :: Maybe (SomeStruct PipelineTessellationStateCreateInfo)
  }
  deriving (Typeable)
deriving instance Show GraphicsShaderGroupCreateInfoNV

instance ToCStruct GraphicsShaderGroupCreateInfoNV where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsShaderGroupCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (stages)) :: Word32))
    pPStages' <- ContT $ allocaBytesAligned @(PipelineShaderStageCreateInfo _) ((Data.Vector.length (stages)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPStages' `plusPtr` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _))) (e) . ($ ())) (stages)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _)))) (pPStages')
    pVertexInputState'' <- case (vertexInputState) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (PipelineVertexInputStateCreateInfo '[])) $ \cont -> withSomeCStruct @PipelineVertexInputStateCreateInfo (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr (PipelineVertexInputStateCreateInfo _)))) pVertexInputState''
    pTessellationState'' <- case (tessellationState) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (PipelineTessellationStateCreateInfo '[])) $ \cont -> withSomeCStruct @PipelineTessellationStateCreateInfo (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (PipelineTessellationStateCreateInfo _)))) pTessellationState''
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPStages' <- ContT $ allocaBytesAligned @(PipelineShaderStageCreateInfo _) ((Data.Vector.length (mempty)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPStages' `plusPtr` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _))) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _)))) (pPStages')
    lift $ f

instance FromCStruct GraphicsShaderGroupCreateInfoNV where
  peekCStruct p = do
    stageCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pStages <- peek @(Ptr (PipelineShaderStageCreateInfo _)) ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo a))))
    pStages' <- generateM (fromIntegral stageCount) (\i -> peekSomeCStruct (forgetExtensions ((pStages `advancePtrBytes` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _)))))
    pVertexInputState <- peek @(Ptr (PipelineVertexInputStateCreateInfo _)) ((p `plusPtr` 32 :: Ptr (Ptr (PipelineVertexInputStateCreateInfo a))))
    pVertexInputState' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pVertexInputState
    pTessellationState <- peek @(Ptr (PipelineTessellationStateCreateInfo _)) ((p `plusPtr` 40 :: Ptr (Ptr (PipelineTessellationStateCreateInfo a))))
    pTessellationState' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pTessellationState
    pure $ GraphicsShaderGroupCreateInfoNV
             pStages' pVertexInputState' pTessellationState'

instance Zero GraphicsShaderGroupCreateInfoNV where
  zero = GraphicsShaderGroupCreateInfoNV
           mempty
           Nothing
           Nothing


-- | VkGraphicsPipelineShaderGroupsCreateInfoNV - Structure specifying
-- parameters of a newly created multi shader group pipeline
--
-- = Description
--
-- When referencing shader groups by index, groups defined in the
-- referenced pipelines are treated as if they were defined as additional
-- entries in @pGroups@. They are appended in the order they appear in the
-- @pPipelines@ array and in the @pGroups@ array when those pipelines were
-- defined.
--
-- The application /must/ maintain the lifetime of all such referenced
-- pipelines based on the pipelines that make use of them.
--
-- == Valid Usage
--
-- -   @groupCount@ /must/ be at least @1@ and as maximum
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@maxGraphicsShaderGroupCount@
--
-- -   The sum of @groupCount@ including those groups added from referenced
--     @pPipelines@ /must/ also be as maximum
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@maxGraphicsShaderGroupCount@
--
-- -   The state of the first element of @pGroups@ /must/ match its
--     equivalent within the parent’s
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'
--
-- -   Each element of @pGroups@ /must/ in combination with the rest of the
--     pipeline state yield a valid state configuration
--
-- -   All elements of @pGroups@ /must/ use the same shader stage
--     combinations unless any mesh shader stage is used, then either
--     combination of task and mesh or just mesh shader is valid
--
-- -   Mesh and regular primitive shading stages cannot be mixed across
--     @pGroups@
--
-- -   Each element of the @pPipelines@ member of @libraries@ /must/ have
--     been created with identical state to the pipeline currently created
--     except the state that can be overriden by
--     'GraphicsShaderGroupCreateInfoNV'
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#feature-device-generated-commands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV'
--
-- -   @pGroups@ /must/ be a valid pointer to an array of @groupCount@
--     valid 'GraphicsShaderGroupCreateInfoNV' structures
--
-- -   If @pipelineCount@ is not @0@, @pPipelines@ /must/ be a valid
--     pointer to an array of @pipelineCount@ valid
--     'Vulkan.Core10.Handles.Pipeline' handles
--
-- -   @groupCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'GraphicsShaderGroupCreateInfoNV', 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data GraphicsPipelineShaderGroupsCreateInfoNV = GraphicsPipelineShaderGroupsCreateInfoNV
  { -- | @pGroups@ is an array of 'GraphicsShaderGroupCreateInfoNV' values
    -- specifying which state of the original
    -- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' each shader group
    -- overrides.
    groups :: Vector GraphicsShaderGroupCreateInfoNV
  , -- | @pPipelines@ is an array of graphics 'Vulkan.Core10.Handles.Pipeline',
    -- which are referenced within the created pipeline, including all their
    -- shader groups.
    pipelines :: Vector Pipeline
  }
  deriving (Typeable)
deriving instance Show GraphicsPipelineShaderGroupsCreateInfoNV

instance ToCStruct GraphicsPipelineShaderGroupsCreateInfoNV where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsPipelineShaderGroupsCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (groups)) :: Word32))
    pPGroups' <- ContT $ allocaBytesAligned @GraphicsShaderGroupCreateInfoNV ((Data.Vector.length (groups)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPGroups' `plusPtr` (48 * (i)) :: Ptr GraphicsShaderGroupCreateInfoNV) (e) . ($ ())) (groups)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr GraphicsShaderGroupCreateInfoNV))) (pPGroups')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (pipelines)) :: Word32))
    pPPipelines' <- ContT $ allocaBytesAligned @Pipeline ((Data.Vector.length (pipelines)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPipelines' `plusPtr` (8 * (i)) :: Ptr Pipeline) (e)) (pipelines)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Pipeline))) (pPPipelines')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPGroups' <- ContT $ allocaBytesAligned @GraphicsShaderGroupCreateInfoNV ((Data.Vector.length (mempty)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPGroups' `plusPtr` (48 * (i)) :: Ptr GraphicsShaderGroupCreateInfoNV) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr GraphicsShaderGroupCreateInfoNV))) (pPGroups')
    pPPipelines' <- ContT $ allocaBytesAligned @Pipeline ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPipelines' `plusPtr` (8 * (i)) :: Ptr Pipeline) (e)) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Pipeline))) (pPPipelines')
    lift $ f

instance FromCStruct GraphicsPipelineShaderGroupsCreateInfoNV where
  peekCStruct p = do
    groupCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pGroups <- peek @(Ptr GraphicsShaderGroupCreateInfoNV) ((p `plusPtr` 24 :: Ptr (Ptr GraphicsShaderGroupCreateInfoNV)))
    pGroups' <- generateM (fromIntegral groupCount) (\i -> peekCStruct @GraphicsShaderGroupCreateInfoNV ((pGroups `advancePtrBytes` (48 * (i)) :: Ptr GraphicsShaderGroupCreateInfoNV)))
    pipelineCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pPipelines <- peek @(Ptr Pipeline) ((p `plusPtr` 40 :: Ptr (Ptr Pipeline)))
    pPipelines' <- generateM (fromIntegral pipelineCount) (\i -> peek @Pipeline ((pPipelines `advancePtrBytes` (8 * (i)) :: Ptr Pipeline)))
    pure $ GraphicsPipelineShaderGroupsCreateInfoNV
             pGroups' pPipelines'

instance Zero GraphicsPipelineShaderGroupsCreateInfoNV where
  zero = GraphicsPipelineShaderGroupsCreateInfoNV
           mempty
           mempty


-- | VkBindShaderGroupIndirectCommandNV - Structure specifying input data for
-- a single shader group command token
--
-- == Valid Usage
--
-- -   The current bound graphics pipeline, as well as the pipelines it may
--     reference, /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV'
--
-- -   The @index@ /must/ be within range of the accessible shader groups
--     of the current bound graphics pipeline. See
--     'cmdBindPipelineShaderGroupNV' for further details
--
-- = See Also
--
-- No cross-references are available
data BindShaderGroupIndirectCommandNV = BindShaderGroupIndirectCommandNV
  { -- No documentation found for Nested "VkBindShaderGroupIndirectCommandNV" "groupIndex"
    groupIndex :: Word32 }
  deriving (Typeable, Eq)
deriving instance Show BindShaderGroupIndirectCommandNV

instance ToCStruct BindShaderGroupIndirectCommandNV where
  withCStruct x f = allocaBytesAligned 4 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindShaderGroupIndirectCommandNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (groupIndex)
    f
  cStructSize = 4
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    f

instance FromCStruct BindShaderGroupIndirectCommandNV where
  peekCStruct p = do
    groupIndex <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pure $ BindShaderGroupIndirectCommandNV
             groupIndex

instance Storable BindShaderGroupIndirectCommandNV where
  sizeOf ~_ = 4
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindShaderGroupIndirectCommandNV where
  zero = BindShaderGroupIndirectCommandNV
           zero


-- | VkBindIndexBufferIndirectCommandNV - Structure specifying input data for
-- a single index buffer command token
--
-- == Valid Usage
--
-- -   The buffer’s usage flag from which the address was acquired /must/
--     have the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDEX_BUFFER_BIT'
--     bit set
--
-- -   The @bufferAddress@ /must/ be aligned to the @indexType@ used
--
-- -   Each element of the buffer from which the address was acquired and
--     that is non-sparse /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   @indexType@ /must/ be a valid
--     'Vulkan.Core10.Enums.IndexType.IndexType' value
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.DeviceAddress',
-- 'Vulkan.Core10.Enums.IndexType.IndexType'
data BindIndexBufferIndirectCommandNV = BindIndexBufferIndirectCommandNV
  { -- | @bufferAddress@ specifies a physical address of the
    -- 'Vulkan.Core10.Handles.Buffer' used as index buffer.
    bufferAddress :: DeviceAddress
  , -- | @size@ is the byte size range which is available for this operation from
    -- the provided address.
    size :: Word32
  , -- | @indexType@ is a 'Vulkan.Core10.Enums.IndexType.IndexType' value
    -- specifying how indices are treated. Instead of the Vulkan enum values, a
    -- custom @uint32_t@ value /can/ be mapped to an
    -- 'Vulkan.Core10.Enums.IndexType.IndexType' by specifying the
    -- 'IndirectCommandsLayoutTokenNV'::@pIndexTypes@ and
    -- 'IndirectCommandsLayoutTokenNV'::@pIndexTypeValues@ arrays.
    indexType :: IndexType
  }
  deriving (Typeable, Eq)
deriving instance Show BindIndexBufferIndirectCommandNV

instance ToCStruct BindIndexBufferIndirectCommandNV where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindIndexBufferIndirectCommandNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (bufferAddress)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (size)
    poke ((p `plusPtr` 12 :: Ptr IndexType)) (indexType)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr IndexType)) (zero)
    f

instance FromCStruct BindIndexBufferIndirectCommandNV where
  peekCStruct p = do
    bufferAddress <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    size <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    indexType <- peek @IndexType ((p `plusPtr` 12 :: Ptr IndexType))
    pure $ BindIndexBufferIndirectCommandNV
             bufferAddress size indexType

instance Storable BindIndexBufferIndirectCommandNV where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindIndexBufferIndirectCommandNV where
  zero = BindIndexBufferIndirectCommandNV
           zero
           zero
           zero


-- | VkBindVertexBufferIndirectCommandNV - Structure specifying input data
-- for a single vertex buffer command token
--
-- == Valid Usage
--
-- -   The buffer’s usage flag from which the address was acquired /must/
--     have the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_VERTEX_BUFFER_BIT'
--     bit set
--
-- -   Each element of the buffer from which the address was acquired and
--     that is non-sparse /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.DeviceAddress'
data BindVertexBufferIndirectCommandNV = BindVertexBufferIndirectCommandNV
  { -- | @bufferAddress@ specifies a physical address of the
    -- 'Vulkan.Core10.Handles.Buffer' used as vertex input binding.
    bufferAddress :: DeviceAddress
  , -- | @size@ is the byte size range which is available for this operation from
    -- the provided address.
    size :: Word32
  , -- | @stride@ is the byte size stride for this vertex input binding as in
    -- 'Vulkan.Core10.Pipeline.VertexInputBindingDescription'::@stride@. It is
    -- only used if 'IndirectCommandsLayoutTokenNV'::@vertexDynamicStride@ was
    -- set, otherwise the stride is inherited from the current bound graphics
    -- pipeline.
    stride :: Word32
  }
  deriving (Typeable, Eq)
deriving instance Show BindVertexBufferIndirectCommandNV

instance ToCStruct BindVertexBufferIndirectCommandNV where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindVertexBufferIndirectCommandNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (bufferAddress)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (size)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (stride)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    f

instance FromCStruct BindVertexBufferIndirectCommandNV where
  peekCStruct p = do
    bufferAddress <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    size <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    stride <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pure $ BindVertexBufferIndirectCommandNV
             bufferAddress size stride

instance Storable BindVertexBufferIndirectCommandNV where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindVertexBufferIndirectCommandNV where
  zero = BindVertexBufferIndirectCommandNV
           zero
           zero
           zero


-- | VkSetStateFlagsIndirectCommandNV - Structure specifying input data for a
-- single state flag command token
--
-- = See Also
--
-- No cross-references are available
data SetStateFlagsIndirectCommandNV = SetStateFlagsIndirectCommandNV
  { -- | @data@ encodes packed state that this command alters.
    --
    -- -   Bit @0@: If set represents
    --     'Vulkan.Core10.Enums.FrontFace.FRONT_FACE_CLOCKWISE', otherwise
    --     'Vulkan.Core10.Enums.FrontFace.FRONT_FACE_COUNTER_CLOCKWISE'
    data' :: Word32 }
  deriving (Typeable, Eq)
deriving instance Show SetStateFlagsIndirectCommandNV

instance ToCStruct SetStateFlagsIndirectCommandNV where
  withCStruct x f = allocaBytesAligned 4 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SetStateFlagsIndirectCommandNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (data')
    f
  cStructSize = 4
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    f

instance FromCStruct SetStateFlagsIndirectCommandNV where
  peekCStruct p = do
    data' <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pure $ SetStateFlagsIndirectCommandNV
             data'

instance Storable SetStateFlagsIndirectCommandNV where
  sizeOf ~_ = 4
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SetStateFlagsIndirectCommandNV where
  zero = SetStateFlagsIndirectCommandNV
           zero


-- | VkIndirectCommandsStreamNV - Structure specifying input streams for
-- generated command tokens
--
-- == Valid Usage
--
-- -   The @buffer@’s usage flag /must/ have the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   The @offset@ /must/ be aligned to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@minIndirectCommandsBufferOffsetAlignment@
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   @buffer@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.BaseType.DeviceSize',
-- 'GeneratedCommandsInfoNV'
data IndirectCommandsStreamNV = IndirectCommandsStreamNV
  { -- | @buffer@ specifies the 'Vulkan.Core10.Handles.Buffer' storing the
    -- functional arguments for each sequence. These arguments /can/ be written
    -- by the device.
    buffer :: Buffer
  , -- | @offset@ specified an offset into @buffer@ where the arguments start.
    offset :: DeviceSize
  }
  deriving (Typeable, Eq)
deriving instance Show IndirectCommandsStreamNV

instance ToCStruct IndirectCommandsStreamNV where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsStreamNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Buffer)) (buffer)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (offset)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct IndirectCommandsStreamNV where
  peekCStruct p = do
    buffer <- peek @Buffer ((p `plusPtr` 0 :: Ptr Buffer))
    offset <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    pure $ IndirectCommandsStreamNV
             buffer offset

instance Storable IndirectCommandsStreamNV where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero IndirectCommandsStreamNV where
  zero = IndirectCommandsStreamNV
           zero
           zero


-- | VkIndirectCommandsLayoutTokenNV - Struct specifying the details of an
-- indirect command layout token
--
-- == Valid Usage
--
-- -   @stream@ /must/ be smaller than
--     'IndirectCommandsLayoutCreateInfoNV'::@streamCount@
--
-- -   @offset@ /must/ be less than or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@maxIndirectCommandsTokenOffset@
--
-- -   If @tokenType@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV',
--     @vertexBindingUnit@ /must/ stay within device supported limits for
--     the appropriate commands
--
-- -   If @tokenType@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV',
--     @pushconstantPipelineLayout@ /must/ be valid
--
-- -   If @tokenType@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV',
--     @pushconstantOffset@ /must/ be a multiple of @4@
--
-- -   If @tokenType@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV',
--     @pushconstantSize@ /must/ be a multiple of @4@
--
-- -   If @tokenType@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV',
--     @pushconstantOffset@ /must/ be less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPushConstantsSize@
--
-- -   If @tokenType@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV',
--     @pushconstantSize@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPushConstantsSize@
--     minus @pushconstantOffset@
--
-- -   If @tokenType@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV',
--     for each byte in the range specified by @pushconstantOffset@ and
--     @pushconstantSize@ and for each shader stage in
--     @pushconstantShaderStageFlags@, there /must/ be a push constant
--     range in @pushconstantPipelineLayout@ that includes that byte and
--     that stage
--
-- -   If @tokenType@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV',
--     for each byte in the range specified by @pushconstantOffset@ and
--     @pushconstantSize@ and for each push constant range that overlaps
--     that byte, @pushconstantShaderStageFlags@ /must/ include all stages
--     in that push constant range’s
--     'Vulkan.Core10.PipelineLayout.PushConstantRange'::@pushconstantShaderStageFlags@
--
-- -   If @tokenType@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV',
--     @indirectStateFlags@ /must/ not be ´0´
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @tokenType@ /must/ be a valid 'IndirectCommandsTokenTypeNV' value
--
-- -   If @pushconstantPipelineLayout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pushconstantPipelineLayout@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   @pushconstantShaderStageFlags@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' values
--
-- -   @indirectStateFlags@ /must/ be a valid combination of
--     'IndirectStateFlagBitsNV' values
--
-- -   If @indexTypeCount@ is not @0@, @pIndexTypes@ /must/ be a valid
--     pointer to an array of @indexTypeCount@ valid
--     'Vulkan.Core10.Enums.IndexType.IndexType' values
--
-- -   If @indexTypeCount@ is not @0@, @pIndexTypeValues@ /must/ be a valid
--     pointer to an array of @indexTypeCount@ @uint32_t@ values
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.IndexType.IndexType',
-- 'IndirectCommandsLayoutCreateInfoNV', 'IndirectCommandsTokenTypeNV',
-- 'IndirectStateFlagsNV', 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data IndirectCommandsLayoutTokenNV = IndirectCommandsLayoutTokenNV
  { -- | @tokenType@ specifies the token command type.
    tokenType :: IndirectCommandsTokenTypeNV
  , -- | @stream@ is the index of the input stream that contains the token
    -- argument data.
    stream :: Word32
  , -- | @offset@ is a relative starting offset within the input stream memory
    -- for the token argument data.
    offset :: Word32
  , -- | @vertexBindingUnit@ is used for the vertex buffer binding command.
    vertexBindingUnit :: Word32
  , -- | @vertexDynamicStride@ sets if the vertex buffer stride is provided by
    -- the binding command rather than the current bound graphics pipeline
    -- state.
    vertexDynamicStride :: Bool
  , -- | @pushconstantPipelineLayout@ is the
    -- 'Vulkan.Core10.Handles.PipelineLayout' used for the push constant
    -- command.
    pushconstantPipelineLayout :: PipelineLayout
  , -- | @pushconstantShaderStageFlags@ are the shader stage flags used for the
    -- push constant command.
    pushconstantShaderStageFlags :: ShaderStageFlags
  , -- | @pushconstantOffset@ is the offset used for the push constant command.
    pushconstantOffset :: Word32
  , -- | @pushconstantSize@ is the size used for the push constant command.
    pushconstantSize :: Word32
  , -- | @indirectStateFlags@ are the active states for the state flag command.
    indirectStateFlags :: IndirectStateFlagsNV
  , -- | @pIndexTypes@ is the used 'Vulkan.Core10.Enums.IndexType.IndexType' for
    -- the corresponding @uint32_t@ value entry in @pIndexTypeValues@.
    indexTypes :: Vector IndexType
  , -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNV" "pIndexTypeValues"
    indexTypeValues :: Vector Word32
  }
  deriving (Typeable)
deriving instance Show IndirectCommandsLayoutTokenNV

instance ToCStruct IndirectCommandsLayoutTokenNV where
  withCStruct x f = allocaBytesAligned 88 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsLayoutTokenNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr IndirectCommandsTokenTypeNV)) (tokenType)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (stream)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (offset)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (vertexBindingUnit)
    lift $ poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (vertexDynamicStride))
    lift $ poke ((p `plusPtr` 40 :: Ptr PipelineLayout)) (pushconstantPipelineLayout)
    lift $ poke ((p `plusPtr` 48 :: Ptr ShaderStageFlags)) (pushconstantShaderStageFlags)
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (pushconstantOffset)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (pushconstantSize)
    lift $ poke ((p `plusPtr` 60 :: Ptr IndirectStateFlagsNV)) (indirectStateFlags)
    let pIndexTypesLength = Data.Vector.length $ (indexTypes)
    lift $ unless ((Data.Vector.length $ (indexTypeValues)) == pIndexTypesLength) $
      throwIO $ IOError Nothing InvalidArgument "" "pIndexTypeValues and pIndexTypes must have the same length" Nothing Nothing
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) ((fromIntegral pIndexTypesLength :: Word32))
    pPIndexTypes' <- ContT $ allocaBytesAligned @IndexType ((Data.Vector.length (indexTypes)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPIndexTypes' `plusPtr` (4 * (i)) :: Ptr IndexType) (e)) (indexTypes)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr IndexType))) (pPIndexTypes')
    pPIndexTypeValues' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (indexTypeValues)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPIndexTypeValues' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (indexTypeValues)
    lift $ poke ((p `plusPtr` 80 :: Ptr (Ptr Word32))) (pPIndexTypeValues')
    lift $ f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr IndirectCommandsTokenTypeNV)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    pPIndexTypes' <- ContT $ allocaBytesAligned @IndexType ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPIndexTypes' `plusPtr` (4 * (i)) :: Ptr IndexType) (e)) (mempty)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr IndexType))) (pPIndexTypes')
    pPIndexTypeValues' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPIndexTypeValues' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 80 :: Ptr (Ptr Word32))) (pPIndexTypeValues')
    lift $ f

instance FromCStruct IndirectCommandsLayoutTokenNV where
  peekCStruct p = do
    tokenType <- peek @IndirectCommandsTokenTypeNV ((p `plusPtr` 16 :: Ptr IndirectCommandsTokenTypeNV))
    stream <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    offset <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    vertexBindingUnit <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    vertexDynamicStride <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    pushconstantPipelineLayout <- peek @PipelineLayout ((p `plusPtr` 40 :: Ptr PipelineLayout))
    pushconstantShaderStageFlags <- peek @ShaderStageFlags ((p `plusPtr` 48 :: Ptr ShaderStageFlags))
    pushconstantOffset <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    pushconstantSize <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    indirectStateFlags <- peek @IndirectStateFlagsNV ((p `plusPtr` 60 :: Ptr IndirectStateFlagsNV))
    indexTypeCount <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    pIndexTypes <- peek @(Ptr IndexType) ((p `plusPtr` 72 :: Ptr (Ptr IndexType)))
    pIndexTypes' <- generateM (fromIntegral indexTypeCount) (\i -> peek @IndexType ((pIndexTypes `advancePtrBytes` (4 * (i)) :: Ptr IndexType)))
    pIndexTypeValues <- peek @(Ptr Word32) ((p `plusPtr` 80 :: Ptr (Ptr Word32)))
    pIndexTypeValues' <- generateM (fromIntegral indexTypeCount) (\i -> peek @Word32 ((pIndexTypeValues `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ IndirectCommandsLayoutTokenNV
             tokenType stream offset vertexBindingUnit (bool32ToBool vertexDynamicStride) pushconstantPipelineLayout pushconstantShaderStageFlags pushconstantOffset pushconstantSize indirectStateFlags pIndexTypes' pIndexTypeValues'

instance Zero IndirectCommandsLayoutTokenNV where
  zero = IndirectCommandsLayoutTokenNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           mempty
           mempty


-- | VkIndirectCommandsLayoutCreateInfoNV - Structure specifying the
-- parameters of a newly created indirect commands layout object
--
-- = Description
--
-- The following code illustrates some of the flags:
--
-- > void cmdProcessAllSequences(cmd, pipeline, indirectCommandsLayout, pIndirectCommandsTokens, sequencesCount, indexbuffer, indexbufferOffset)
-- > {
-- >   for (s = 0; s < sequencesCount; s++)
-- >   {
-- >     sUsed = s;
-- >
-- >     if (indirectCommandsLayout.flags & VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV) {
-- >       sUsed = indexbuffer.load_uint32( sUsed * sizeof(uint32_t) + indexbufferOffset);
-- >     }
-- >
-- >     if (indirectCommandsLayout.flags & VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV) {
-- >       sUsed = incoherent_implementation_dependent_permutation[ sUsed ];
-- >     }
-- >
-- >     cmdProcessSequence( cmd, pipeline, indirectCommandsLayout, pIndirectCommandsTokens, sUsed );
-- >   }
-- > }
--
-- == Valid Usage
--
-- -   The @pipelineBindPoint@ /must/ be
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   @tokenCount@ /must/ be greater than @0@ and less than or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@maxIndirectCommandsTokenCount@
--
-- -   If @pTokens@ contains an entry of
--     'INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV' it /must/ be the
--     first element of the array and there /must/ be only a single element
--     of such token type
--
-- -   If @pTokens@ contains an entry of
--     'INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV' there /must/ be only a
--     single element of such token type
--
-- -   All state tokens in @pTokens@ /must/ occur prior work provoking
--     tokens ('INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV')
--
-- -   The content of @pTokens@ /must/ include one single work provoking
--     token that is compatible with the @pipelineBindPoint@
--
-- -   @streamCount@ /must/ be greater than @0@ and less or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@maxIndirectCommandsStreamCount@
--
-- -   each element of @pStreamStrides@ /must/ be greater than \`0\`and
--     less than or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@maxIndirectCommandsStreamStride@.
--     Furthermore the alignment of each token input /must/ be ensured
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be a valid combination of
--     'IndirectCommandsLayoutUsageFlagBitsNV' values
--
-- -   @flags@ /must/ not be @0@
--
-- -   @pipelineBindPoint@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
--
-- -   @pTokens@ /must/ be a valid pointer to an array of @tokenCount@
--     valid 'IndirectCommandsLayoutTokenNV' structures
--
-- -   @pStreamStrides@ /must/ be a valid pointer to an array of
--     @streamCount@ @uint32_t@ values
--
-- -   @tokenCount@ /must/ be greater than @0@
--
-- -   @streamCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'IndirectCommandsLayoutTokenNV', 'IndirectCommandsLayoutUsageFlagsNV',
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createIndirectCommandsLayoutNV'
data IndirectCommandsLayoutCreateInfoNV = IndirectCommandsLayoutCreateInfoNV
  { -- | @flags@ is a bitmask of 'IndirectCommandsLayoutUsageFlagBitsNV'
    -- specifying usage hints of this layout.
    flags :: IndirectCommandsLayoutUsageFlagsNV
  , -- | @pipelineBindPoint@ is the
    -- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' that this
    -- layout targets.
    pipelineBindPoint :: PipelineBindPoint
  , -- | @pTokens@ is an array describing each command token in detail. See
    -- 'IndirectCommandsTokenTypeNV' and 'IndirectCommandsLayoutTokenNV' below
    -- for details.
    tokens :: Vector IndirectCommandsLayoutTokenNV
  , -- | @pStreamStrides@ is an array defining the byte stride for each input
    -- stream.
    streamStrides :: Vector Word32
  }
  deriving (Typeable)
deriving instance Show IndirectCommandsLayoutCreateInfoNV

instance ToCStruct IndirectCommandsLayoutCreateInfoNV where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsLayoutCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr IndirectCommandsLayoutUsageFlagsNV)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr PipelineBindPoint)) (pipelineBindPoint)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (tokens)) :: Word32))
    pPTokens' <- ContT $ allocaBytesAligned @IndirectCommandsLayoutTokenNV ((Data.Vector.length (tokens)) * 88) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPTokens' `plusPtr` (88 * (i)) :: Ptr IndirectCommandsLayoutTokenNV) (e) . ($ ())) (tokens)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr IndirectCommandsLayoutTokenNV))) (pPTokens')
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (streamStrides)) :: Word32))
    pPStreamStrides' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (streamStrides)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPStreamStrides' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (streamStrides)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr Word32))) (pPStreamStrides')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr IndirectCommandsLayoutUsageFlagsNV)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr PipelineBindPoint)) (zero)
    pPTokens' <- ContT $ allocaBytesAligned @IndirectCommandsLayoutTokenNV ((Data.Vector.length (mempty)) * 88) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPTokens' `plusPtr` (88 * (i)) :: Ptr IndirectCommandsLayoutTokenNV) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr IndirectCommandsLayoutTokenNV))) (pPTokens')
    pPStreamStrides' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPStreamStrides' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr Word32))) (pPStreamStrides')
    lift $ f

instance FromCStruct IndirectCommandsLayoutCreateInfoNV where
  peekCStruct p = do
    flags <- peek @IndirectCommandsLayoutUsageFlagsNV ((p `plusPtr` 16 :: Ptr IndirectCommandsLayoutUsageFlagsNV))
    pipelineBindPoint <- peek @PipelineBindPoint ((p `plusPtr` 20 :: Ptr PipelineBindPoint))
    tokenCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pTokens <- peek @(Ptr IndirectCommandsLayoutTokenNV) ((p `plusPtr` 32 :: Ptr (Ptr IndirectCommandsLayoutTokenNV)))
    pTokens' <- generateM (fromIntegral tokenCount) (\i -> peekCStruct @IndirectCommandsLayoutTokenNV ((pTokens `advancePtrBytes` (88 * (i)) :: Ptr IndirectCommandsLayoutTokenNV)))
    streamCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pStreamStrides <- peek @(Ptr Word32) ((p `plusPtr` 48 :: Ptr (Ptr Word32)))
    pStreamStrides' <- generateM (fromIntegral streamCount) (\i -> peek @Word32 ((pStreamStrides `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ IndirectCommandsLayoutCreateInfoNV
             flags pipelineBindPoint pTokens' pStreamStrides'

instance Zero IndirectCommandsLayoutCreateInfoNV where
  zero = IndirectCommandsLayoutCreateInfoNV
           zero
           zero
           mempty
           mempty


-- | VkGeneratedCommandsInfoNV - Structure specifying parameters for the
-- generation of commands
--
-- == Valid Usage
--
-- -   The provided @pipeline@ /must/ match the pipeline bound at execution
--     time
--
-- -   If the @indirectCommandsLayout@ uses a token of
--     'INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV', then the @pipeline@
--     /must/ have been created with multiple shader groups
--
-- -   If the @indirectCommandsLayout@ uses a token of
--     'INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV', then the @pipeline@
--     /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV'
--     set in 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@flags@
--
-- -   If the @indirectCommandsLayout@ uses a token of
--     'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV', then the
--     @pipeline@\`s 'Vulkan.Core10.Handles.PipelineLayout' /must/ match
--     the 'IndirectCommandsLayoutTokenNV'::@pushconstantPipelineLayout@
--
-- -   @streamCount@ /must/ match the @indirectCommandsLayout@’s
--     @streamCount@
--
-- -   @sequencesCount@ /must/ be less or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@maxIndirectSequenceCount@
--     and 'GeneratedCommandsMemoryRequirementsInfoNV'::@maxSequencesCount@
--     that was used to determine the @preprocessSize@
--
-- -   @preprocessBuffer@ /must/ have the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set in its usage flag
--
-- -   @preprocessOffset@ /must/ be aligned to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@minIndirectCommandsBufferOffsetAlignment@
--
-- -   If @preprocessBuffer@ is non-sparse then it /must/ be bound
--     completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @preprocessSize@ /must/ be at least equal to the memory
--     requirement\`s size returned by
--     'getGeneratedCommandsMemoryRequirementsNV' using the matching inputs
--     (@indirectCommandsLayout@, …​) as within this structure
--
-- -   @sequencesCountBuffer@ /can/ be set if the actual used count of
--     sequences is sourced from the provided buffer. In that case the
--     @sequencesCount@ serves as upper bound
--
-- -   If @sequencesCountBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', its usage flag /must/ have
--     the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   If @sequencesCountBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @sequencesCountOffset@
--     /must/ be aligned to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@minSequencesCountBufferOffsetAlignment@
--
-- -   If @sequencesCountBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' and is non-sparse then it
--     /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   If @indirectCommandsLayout@’s
--     'INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV' is set,
--     @sequencesIndexBuffer@ /must/ be set otherwise it /must/ be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   If @sequencesIndexBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', its usage flag /must/ have
--     the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   If @sequencesIndexBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @sequencesIndexOffset@
--     /must/ be aligned to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@minSequencesIndexBufferOffsetAlignment@
--
-- -   If @sequencesIndexBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' and is non-sparse then it
--     /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @pipelineBindPoint@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
--
-- -   @pipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   @indirectCommandsLayout@ /must/ be a valid
--     'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV' handle
--
-- -   @pStreams@ /must/ be a valid pointer to an array of @streamCount@
--     valid 'IndirectCommandsStreamNV' structures
--
-- -   @preprocessBuffer@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer'
--     handle
--
-- -   If @sequencesCountBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @sequencesCountBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   If @sequencesIndexBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @sequencesIndexBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   @streamCount@ /must/ be greater than @0@
--
-- -   Each of @indirectCommandsLayout@, @pipeline@, @preprocessBuffer@,
--     @sequencesCountBuffer@, and @sequencesIndexBuffer@ that are valid
--     handles of non-ignored parameters /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.BaseType.DeviceSize',
-- 'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV',
-- 'IndirectCommandsStreamNV', 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdExecuteGeneratedCommandsNV', 'cmdPreprocessGeneratedCommandsNV'
data GeneratedCommandsInfoNV = GeneratedCommandsInfoNV
  { -- | @pipelineBindPoint@ is the
    -- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' used for the
    -- @pipeline@.
    pipelineBindPoint :: PipelineBindPoint
  , -- | @pipeline@ is the 'Vulkan.Core10.Handles.Pipeline' used in the
    -- generation and execution process.
    pipeline :: Pipeline
  , -- | @indirectCommandsLayout@ is the
    -- 'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV' that provides the
    -- command sequence to generate.
    indirectCommandsLayout :: IndirectCommandsLayoutNV
  , -- | @pStreams@ provides an array of 'IndirectCommandsStreamNV' that provide
    -- the input data for the tokens used in @indirectCommandsLayout@.
    streams :: Vector IndirectCommandsStreamNV
  , -- | @sequencesCount@ is the maximum number of sequences to reserve. If
    -- @sequencesCountBuffer@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', this
    -- is also the actual number of sequences generated.
    sequencesCount :: Word32
  , -- | @preprocessBuffer@ is the 'Vulkan.Core10.Handles.Buffer' that is used
    -- for preprocessing the input data for execution. If this structure is
    -- used with 'cmdExecuteGeneratedCommandsNV' with its @isPreprocessed@ set
    -- to 'Vulkan.Core10.BaseType.TRUE', then the preprocessing step is skipped
    -- and data is only read from this buffer.
    preprocessBuffer :: Buffer
  , -- | @preprocessOffset@ is the byte offset into @preprocessBuffer@ where the
    -- preprocessed data is stored.
    preprocessOffset :: DeviceSize
  , -- | @preprocessSize@ is the maximum byte size within the @preprocessBuffer@
    -- after the @preprocessOffset@ that is available for preprocessing.
    preprocessSize :: DeviceSize
  , -- | @sequencesCountBuffer@ is a 'Vulkan.Core10.Handles.Buffer' in which the
    -- actual number of sequences is provided as single @uint32_t@ value.
    sequencesCountBuffer :: Buffer
  , -- | @sequencesCountOffset@ is the byte offset into @sequencesCountBuffer@
    -- where the count value is stored.
    sequencesCountOffset :: DeviceSize
  , -- | @sequencesIndexBuffer@ is a 'Vulkan.Core10.Handles.Buffer' that encodes
    -- the used sequence indices as @uint32_t@ array.
    sequencesIndexBuffer :: Buffer
  , -- | @sequencesIndexOffset@ is the byte offset into @sequencesIndexBuffer@
    -- where the index values start.
    sequencesIndexOffset :: DeviceSize
  }
  deriving (Typeable)
deriving instance Show GeneratedCommandsInfoNV

instance ToCStruct GeneratedCommandsInfoNV where
  withCStruct x f = allocaBytesAligned 120 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeneratedCommandsInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineBindPoint)) (pipelineBindPoint)
    lift $ poke ((p `plusPtr` 24 :: Ptr Pipeline)) (pipeline)
    lift $ poke ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutNV)) (indirectCommandsLayout)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (streams)) :: Word32))
    pPStreams' <- ContT $ allocaBytesAligned @IndirectCommandsStreamNV ((Data.Vector.length (streams)) * 16) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPStreams' `plusPtr` (16 * (i)) :: Ptr IndirectCommandsStreamNV) (e) . ($ ())) (streams)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr IndirectCommandsStreamNV))) (pPStreams')
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (sequencesCount)
    lift $ poke ((p `plusPtr` 64 :: Ptr Buffer)) (preprocessBuffer)
    lift $ poke ((p `plusPtr` 72 :: Ptr DeviceSize)) (preprocessOffset)
    lift $ poke ((p `plusPtr` 80 :: Ptr DeviceSize)) (preprocessSize)
    lift $ poke ((p `plusPtr` 88 :: Ptr Buffer)) (sequencesCountBuffer)
    lift $ poke ((p `plusPtr` 96 :: Ptr DeviceSize)) (sequencesCountOffset)
    lift $ poke ((p `plusPtr` 104 :: Ptr Buffer)) (sequencesIndexBuffer)
    lift $ poke ((p `plusPtr` 112 :: Ptr DeviceSize)) (sequencesIndexOffset)
    lift $ f
  cStructSize = 120
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineBindPoint)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Pipeline)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutNV)) (zero)
    pPStreams' <- ContT $ allocaBytesAligned @IndirectCommandsStreamNV ((Data.Vector.length (mempty)) * 16) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPStreams' `plusPtr` (16 * (i)) :: Ptr IndirectCommandsStreamNV) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr IndirectCommandsStreamNV))) (pPStreams')
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 64 :: Ptr Buffer)) (zero)
    lift $ poke ((p `plusPtr` 72 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 80 :: Ptr DeviceSize)) (zero)
    lift $ f

instance FromCStruct GeneratedCommandsInfoNV where
  peekCStruct p = do
    pipelineBindPoint <- peek @PipelineBindPoint ((p `plusPtr` 16 :: Ptr PipelineBindPoint))
    pipeline <- peek @Pipeline ((p `plusPtr` 24 :: Ptr Pipeline))
    indirectCommandsLayout <- peek @IndirectCommandsLayoutNV ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutNV))
    streamCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pStreams <- peek @(Ptr IndirectCommandsStreamNV) ((p `plusPtr` 48 :: Ptr (Ptr IndirectCommandsStreamNV)))
    pStreams' <- generateM (fromIntegral streamCount) (\i -> peekCStruct @IndirectCommandsStreamNV ((pStreams `advancePtrBytes` (16 * (i)) :: Ptr IndirectCommandsStreamNV)))
    sequencesCount <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    preprocessBuffer <- peek @Buffer ((p `plusPtr` 64 :: Ptr Buffer))
    preprocessOffset <- peek @DeviceSize ((p `plusPtr` 72 :: Ptr DeviceSize))
    preprocessSize <- peek @DeviceSize ((p `plusPtr` 80 :: Ptr DeviceSize))
    sequencesCountBuffer <- peek @Buffer ((p `plusPtr` 88 :: Ptr Buffer))
    sequencesCountOffset <- peek @DeviceSize ((p `plusPtr` 96 :: Ptr DeviceSize))
    sequencesIndexBuffer <- peek @Buffer ((p `plusPtr` 104 :: Ptr Buffer))
    sequencesIndexOffset <- peek @DeviceSize ((p `plusPtr` 112 :: Ptr DeviceSize))
    pure $ GeneratedCommandsInfoNV
             pipelineBindPoint pipeline indirectCommandsLayout pStreams' sequencesCount preprocessBuffer preprocessOffset preprocessSize sequencesCountBuffer sequencesCountOffset sequencesIndexBuffer sequencesIndexOffset

instance Zero GeneratedCommandsInfoNV where
  zero = GeneratedCommandsInfoNV
           zero
           zero
           zero
           mempty
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkGeneratedCommandsMemoryRequirementsInfoNV - Structure specifying
-- parameters for the reservation of preprocess buffer space
--
-- == Valid Usage
--
-- -   @maxSequencesCount@ /must/ be less or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@maxIndirectSequenceCount@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @pipelineBindPoint@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
--
-- -   @pipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   @indirectCommandsLayout@ /must/ be a valid
--     'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV' handle
--
-- -   Both of @indirectCommandsLayout@, and @pipeline@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV',
-- 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getGeneratedCommandsMemoryRequirementsNV'
data GeneratedCommandsMemoryRequirementsInfoNV = GeneratedCommandsMemoryRequirementsInfoNV
  { -- | @pipelineBindPoint@ is the
    -- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' of the
    -- @pipeline@ that this buffer memory is intended to be used with during
    -- the execution.
    pipelineBindPoint :: PipelineBindPoint
  , -- | @pipeline@ is the 'Vulkan.Core10.Handles.Pipeline' that this buffer
    -- memory is intended to be used with during the execution.
    pipeline :: Pipeline
  , -- | @indirectCommandsLayout@ is the
    -- 'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV' that this buffer
    -- memory is intended to be used with.
    indirectCommandsLayout :: IndirectCommandsLayoutNV
  , -- | @maxSequencesCount@ is the maximum number of sequences that this buffer
    -- memory in combination with the other state provided /can/ be used with.
    maxSequencesCount :: Word32
  }
  deriving (Typeable, Eq)
deriving instance Show GeneratedCommandsMemoryRequirementsInfoNV

instance ToCStruct GeneratedCommandsMemoryRequirementsInfoNV where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeneratedCommandsMemoryRequirementsInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineBindPoint)) (pipelineBindPoint)
    poke ((p `plusPtr` 24 :: Ptr Pipeline)) (pipeline)
    poke ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutNV)) (indirectCommandsLayout)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (maxSequencesCount)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineBindPoint)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Pipeline)) (zero)
    poke ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutNV)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    f

instance FromCStruct GeneratedCommandsMemoryRequirementsInfoNV where
  peekCStruct p = do
    pipelineBindPoint <- peek @PipelineBindPoint ((p `plusPtr` 16 :: Ptr PipelineBindPoint))
    pipeline <- peek @Pipeline ((p `plusPtr` 24 :: Ptr Pipeline))
    indirectCommandsLayout <- peek @IndirectCommandsLayoutNV ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutNV))
    maxSequencesCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pure $ GeneratedCommandsMemoryRequirementsInfoNV
             pipelineBindPoint pipeline indirectCommandsLayout maxSequencesCount

instance Storable GeneratedCommandsMemoryRequirementsInfoNV where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GeneratedCommandsMemoryRequirementsInfoNV where
  zero = GeneratedCommandsMemoryRequirementsInfoNV
           zero
           zero
           zero
           zero


-- | VkIndirectCommandsLayoutUsageFlagBitsNV - Bitmask specifying allowed
-- usage of an indirect commands layout
--
-- = See Also
--
-- 'IndirectCommandsLayoutUsageFlagsNV'
newtype IndirectCommandsLayoutUsageFlagBitsNV = IndirectCommandsLayoutUsageFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV' specifies
-- that the layout is always used with the manual preprocessing step
-- through calling 'cmdPreprocessGeneratedCommandsNV' and executed by
-- 'cmdExecuteGeneratedCommandsNV' with @isPreprocessed@ set to
-- 'Vulkan.Core10.BaseType.TRUE'.
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV = IndirectCommandsLayoutUsageFlagBitsNV 0x00000001
-- | 'INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV' specifies that
-- the input data for the sequences is not implicitly indexed from
-- 0..sequencesUsed but a user provided 'Vulkan.Core10.Handles.Buffer'
-- encoding the index is provided.
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV = IndirectCommandsLayoutUsageFlagBitsNV 0x00000002
-- | 'INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV' specifies
-- that the processing of sequences /can/ happen at an
-- implementation-dependent order, which is not: guaranteed to be coherent
-- using the same input data.
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV = IndirectCommandsLayoutUsageFlagBitsNV 0x00000004

type IndirectCommandsLayoutUsageFlagsNV = IndirectCommandsLayoutUsageFlagBitsNV

instance Show IndirectCommandsLayoutUsageFlagBitsNV where
  showsPrec p = \case
    INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV -> showString "INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV"
    INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV -> showString "INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV"
    INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV -> showString "INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV"
    IndirectCommandsLayoutUsageFlagBitsNV x -> showParen (p >= 11) (showString "IndirectCommandsLayoutUsageFlagBitsNV 0x" . showHex x)

instance Read IndirectCommandsLayoutUsageFlagBitsNV where
  readPrec = parens (choose [("INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV", pure INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV)
                            , ("INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV", pure INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV)
                            , ("INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV", pure INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "IndirectCommandsLayoutUsageFlagBitsNV")
                       v <- step readPrec
                       pure (IndirectCommandsLayoutUsageFlagBitsNV v)))


-- | VkIndirectStateFlagBitsNV - Bitmask specifiying state that can be
-- altered on the device
--
-- = See Also
--
-- 'IndirectStateFlagsNV'
newtype IndirectStateFlagBitsNV = IndirectStateFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV' allows to toggle the
-- 'Vulkan.Core10.Enums.FrontFace.FrontFace' rasterization state for
-- subsequent draw operations.
pattern INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV = IndirectStateFlagBitsNV 0x00000001

type IndirectStateFlagsNV = IndirectStateFlagBitsNV

instance Show IndirectStateFlagBitsNV where
  showsPrec p = \case
    INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV -> showString "INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV"
    IndirectStateFlagBitsNV x -> showParen (p >= 11) (showString "IndirectStateFlagBitsNV 0x" . showHex x)

instance Read IndirectStateFlagBitsNV where
  readPrec = parens (choose [("INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV", pure INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "IndirectStateFlagBitsNV")
                       v <- step readPrec
                       pure (IndirectStateFlagBitsNV v)))


-- | VkIndirectCommandsTokenTypeNV - Enum specifying token commands
--
-- = Description
--
-- \'
--
-- +-------------------------------------------------+------------------------------------------------------------------+
-- | Token type                                      | Equivalent command                                               |
-- +=================================================+==================================================================+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV'  | 'cmdBindPipelineShaderGroupNV'                                   |
-- +-------------------------------------------------+------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV'   | -                                                                |
-- +-------------------------------------------------+------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV'  | 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'         |
-- +-------------------------------------------------+------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV' | 'Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers'       |
-- +-------------------------------------------------+------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV' | 'Vulkan.Core10.CommandBufferBuilding.cmdPushConstants'           |
-- +-------------------------------------------------+------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV'  | 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect'     |
-- +-------------------------------------------------+------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV'          | 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect'            |
-- +-------------------------------------------------+------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV'    | 'Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectNV' |
-- +-------------------------------------------------+------------------------------------------------------------------+
--
-- Supported indirect command tokens
--
-- = See Also
--
-- 'IndirectCommandsLayoutTokenNV'
newtype IndirectCommandsTokenTypeNV = IndirectCommandsTokenTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV = IndirectCommandsTokenTypeNV 0
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV = IndirectCommandsTokenTypeNV 1
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV = IndirectCommandsTokenTypeNV 2
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV = IndirectCommandsTokenTypeNV 3
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV = IndirectCommandsTokenTypeNV 4
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV = IndirectCommandsTokenTypeNV 5
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV = IndirectCommandsTokenTypeNV 6
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV = IndirectCommandsTokenTypeNV 7
{-# complete INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV :: IndirectCommandsTokenTypeNV #-}

instance Show IndirectCommandsTokenTypeNV where
  showsPrec p = \case
    INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV -> showString "INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV"
    INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV -> showString "INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV"
    INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV -> showString "INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV"
    INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV -> showString "INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV"
    INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV -> showString "INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV"
    INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV -> showString "INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV"
    INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV -> showString "INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV"
    INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV -> showString "INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV"
    IndirectCommandsTokenTypeNV x -> showParen (p >= 11) (showString "IndirectCommandsTokenTypeNV " . showsPrec 11 x)

instance Read IndirectCommandsTokenTypeNV where
  readPrec = parens (choose [("INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV", pure INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV)
                            , ("INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV", pure INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV)
                            , ("INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV", pure INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV)
                            , ("INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV", pure INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV)
                            , ("INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV", pure INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV)
                            , ("INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV", pure INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV)
                            , ("INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV", pure INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV)
                            , ("INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV", pure INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "IndirectCommandsTokenTypeNV")
                       v <- step readPrec
                       pure (IndirectCommandsTokenTypeNV v)))


type NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION"
pattern NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION :: forall a . Integral a => a
pattern NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 3


type NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME = "VK_NV_device_generated_commands"

-- No documentation found for TopLevel "VK_NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME"
pattern NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME = "VK_NV_device_generated_commands"

