{-# language CPP #-}
-- | = Name
--
-- VK_EXT_custom_resolve - device extension
--
-- = VK_EXT_custom_resolve
--
-- [__Name String__]
--     @VK_EXT_custom_resolve@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     629
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_VERSION_1_3
--
--     -   Interacts with VK_KHR_dynamic_rendering
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_custom_resolve] @zmike%0A*Here describe the issue or question you have about the VK_EXT_custom_resolve extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_custom_resolve.adoc VK_EXT_custom_resolve>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-05-13
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension interacts with @VK_KHR_dynamic_rendering@
--
--     -   This extension interacts with
--         @VK_EXT_dynamic_rendering_unused_attachments@
--
--     -   This extension interacts with @VK_EXT_fragment_density_map@
--
--     -   This extension interacts with @VK_EXT_graphics_pipeline_library@
--
--     -   This extension interacts with @VK_EXT_shader_object@
--
-- [__Contributors__]
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Connor Abbott, Valve
--
--     -   Samuel Pitoiset, Valve
--
--     -   Matthew Netsch, Qualcomm
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Ting Wei, ARM
--
--     -   Ricardo Garcia, Igalia
--
--     -   Spencer Fricke, LunarG
--
--     -   Piers Daniell, Nvidia
--
-- == Description
--
-- This extension provides functionality for using shaders to resolve
-- multisample rendering attachments.
--
-- It builds upon mechanics introduced by
-- VK_QCOM_render_pass_shader_resolve, additionally adding support for
-- dynamic rendering.
--
-- == New Commands
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- is supported:
--
-- -   'cmdBeginCustomResolveEXT'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCustomResolveFeaturesEXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- is supported:
--
-- -   'BeginCustomResolveInfoEXT'
--
-- -   Extending
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo',
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo',
--     'Vulkan.Extensions.VK_EXT_shader_object.ShaderCreateInfoEXT':
--
--     -   'CustomResolveCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_CUSTOM_RESOLVE_EXTENSION_NAME'
--
-- -   'EXT_CUSTOM_RESOLVE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_RESOLVE_FEATURES_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SubpassDescriptionFlagBits':
--
--     -   'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_CUSTOM_RESOLVE_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- is supported:
--
-- -   Extending 'Vulkan.Core13.Enums.RenderingFlagBits.RenderingFlagBits':
--
--     -   'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_CUSTOM_RESOLVE_BIT_EXT'
--
--     -   'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_FRAGMENT_REGION_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits':
--
--     -   'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_CUSTOM_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BEGIN_CUSTOM_RESOLVE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CUSTOM_RESOLVE_CREATE_INFO_EXT'
--
-- == Issues
--
-- 1) How will this work with shader objects?
--
-- Some vendors emit an epilog at the end of the FS that stores each
-- color\/depth\/stencil attachment to the appropriate tilebuffer location,
-- and to do that they need to know the layout of the tilebuffer which
-- depends on the attachment formats\/sample counts. We agreed that for
-- shader object the FS epilog is emitted dynamically when the draw
-- happens.
--
-- == Version History
--
-- -   Revision 1, 2025-05-13 (Mike Blumenkrantz)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_custom_resolve Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_custom_resolve  ( cmdBeginCustomResolveEXT
                                                , BeginCustomResolveInfoEXT(..)
                                                , PhysicalDeviceCustomResolveFeaturesEXT(..)
                                                , CustomResolveCreateInfoEXT(..)
                                                , EXT_CUSTOM_RESOLVE_SPEC_VERSION
                                                , pattern EXT_CUSTOM_RESOLVE_SPEC_VERSION
                                                , EXT_CUSTOM_RESOLVE_EXTENSION_NAME
                                                , pattern EXT_CUSTOM_RESOLVE_EXTENSION_NAME
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
import Data.Vector (generateM)
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
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginCustomResolveEXT))
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BEGIN_CUSTOM_RESOLVE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CUSTOM_RESOLVE_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_RESOLVE_FEATURES_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginCustomResolveEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr BeginCustomResolveInfoEXT -> IO ()) -> Ptr CommandBuffer_T -> Ptr BeginCustomResolveInfoEXT -> IO ()

-- | vkCmdBeginCustomResolveEXT - Begins a shader resolve operation
--
-- = Description
--
-- Following this call, any @resolveImageView@ with @resolveMode@ set to
-- 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_CUSTOM_BIT_EXT'
-- will be written by outputs which would otherwise have written to the
-- @imageView@ image until the end of the current render pass instance.
--
-- Following this call, the fragment area /may/ be reduced to (1,1) if a
-- fragment density map is attached. If this occurs, reads of input
-- attachments mapped to a color, depth, or stencil attachment return the
-- value for the original larger fragment containing the smaller fragment.
-- Reads of input attachments not mapped to a color, depth, or stencil
-- attachment use the new fragment area.
--
-- Because the content of any depth\/stencil resolve attachment as well as
-- any color resolve attachment is undefined at the beginning of a resolve
-- operation, any depth testing, stencil testing, or blending operation
-- which sources these undefined values also has undefined result value.
--
-- During a custom resolve pass, multiple fragment invocations writing to
-- the same (x, y, layer, view, sample) coordinate, i.e. overdraw, will
-- produce undefined behavior.
--
-- Implementations are allowed to implement custom resolve attachment
-- writes through other mechanisms than framebuffer attachment writes,
-- which would normally obey rules of rasterization order.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBeginCustomResolveEXT-commandBuffer-11517# The current
--     render pass instance /must/ have been started or resumed by
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     in this @commandBuffer@
--
-- -   #VUID-vkCmdBeginCustomResolveEXT-None-11518#
--     'cmdBeginCustomResolveEXT' /must/ not have already been recorded in
--     the current render pass instance
--
-- -   #VUID-vkCmdBeginCustomResolveEXT-None-11519# The current render pass
--     instance /must/ have specified
--     'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_CUSTOM_RESOLVE_BIT_EXT'
--
-- -   #VUID-vkCmdBeginCustomResolveEXT-None-11520# The current render pass
--     instance /must/ not have specified
--     'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_SUSPENDING_BIT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBeginCustomResolveEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBeginCustomResolveEXT-pBeginCustomResolveInfo-parameter#
--     If @pBeginCustomResolveInfo@ is not @NULL@,
--     @pBeginCustomResolveInfo@ /must/ be a valid pointer to a valid
--     'BeginCustomResolveInfoEXT' structure
--
-- -   #VUID-vkCmdBeginCustomResolveEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBeginCustomResolveEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdBeginCustomResolveEXT-renderpass# This command /must/
--     only be called inside of a render pass instance
--
-- -   #VUID-vkCmdBeginCustomResolveEXT-suspended# This command /must/ not
--     be called between suspended render pass instances
--
-- -   #VUID-vkCmdBeginCustomResolveEXT-videocoding# This command /must/
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | VK_QUEUE_GRAPHICS_BIT                                                                                                 | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdBeginCustomResolveEXT is affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_custom_resolve VK_EXT_custom_resolve>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'BeginCustomResolveInfoEXT', 'Vulkan.Core10.Handles.CommandBuffer'
cmdBeginCustomResolveEXT :: forall io
                          . (MonadIO io)
                         => -- | @commandBuffer@ is the command buffer in which to record the command.
                            CommandBuffer
                         -> -- | @pBeginCustomResolveInfo@ is an optional struct with which to extend
                            -- functionality.
                            ("beginCustomResolveInfo" ::: Maybe BeginCustomResolveInfoEXT)
                         -> io ()
cmdBeginCustomResolveEXT commandBuffer
                           beginCustomResolveInfo = liftIO . evalContT $ do
  let vkCmdBeginCustomResolveEXTPtr = pVkCmdBeginCustomResolveEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBeginCustomResolveEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginCustomResolveEXT is null" Nothing Nothing
  let vkCmdBeginCustomResolveEXT' = mkVkCmdBeginCustomResolveEXT vkCmdBeginCustomResolveEXTPtr
  pBeginCustomResolveInfo <- case (beginCustomResolveInfo) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkCmdBeginCustomResolveEXT" (vkCmdBeginCustomResolveEXT'
                                                          (commandBufferHandle (commandBuffer))
                                                          pBeginCustomResolveInfo)
  pure $ ()


-- | VkBeginCustomResolveInfoEXT - Structure specifying shader resolve
-- information
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_custom_resolve VK_EXT_custom_resolve>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdBeginCustomResolveEXT'
data BeginCustomResolveInfoEXT = BeginCustomResolveInfoEXT
  {}
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BeginCustomResolveInfoEXT)
#endif
deriving instance Show BeginCustomResolveInfoEXT

instance ToCStruct BeginCustomResolveInfoEXT where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BeginCustomResolveInfoEXT f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BEGIN_CUSTOM_RESOLVE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BEGIN_CUSTOM_RESOLVE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct BeginCustomResolveInfoEXT where
  peekCStruct _ = pure $ BeginCustomResolveInfoEXT
                           

instance Storable BeginCustomResolveInfoEXT where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BeginCustomResolveInfoEXT where
  zero = BeginCustomResolveInfoEXT
           


-- | VkPhysicalDeviceCustomResolveFeaturesEXT - Structure indicating support
-- for shader resolves
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceCustomResolveFeaturesEXT' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceCustomResolveFeaturesEXT', it /must/ add an instance of
-- the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_custom_resolve VK_EXT_custom_resolve>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCustomResolveFeaturesEXT = PhysicalDeviceCustomResolveFeaturesEXT
  { -- | #features-customResolve# @customResolve@ specifies that the
    -- implementation supports render pass resolves using shaders.
    customResolve :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCustomResolveFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceCustomResolveFeaturesEXT

instance ToCStruct PhysicalDeviceCustomResolveFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCustomResolveFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_RESOLVE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (customResolve))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_RESOLVE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceCustomResolveFeaturesEXT where
  peekCStruct p = do
    customResolve <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceCustomResolveFeaturesEXT
             (bool32ToBool customResolve)

instance Storable PhysicalDeviceCustomResolveFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCustomResolveFeaturesEXT where
  zero = PhysicalDeviceCustomResolveFeaturesEXT
           zero


-- | VkCustomResolveCreateInfoEXT - Structure specifying format info for
-- custom resolves
--
-- = Description
--
-- If the @pNext@ chain includes this structure for one of:
--
-- -   a 'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo' for a
--     pipeline created without a 'Vulkan.Core10.Handles.RenderPass'
--
-- -   a 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo' for a
--     secondary command buffer within a render pass instance begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'.
--
-- it specifies the formats used in custom resolves within the same render
-- pass. It also specifies that the corresponding object will be used in a
-- render pass which contains a custom resolve operation.
--
-- If the @pNext@ chain includes this structure for a
-- 'Vulkan.Extensions.VK_EXT_shader_object.ShaderCreateInfoEXT' for a
-- fragment shader object, it only specifies that the fragment shader will
-- be used in a custom resolve operation.
--
-- If a graphics pipeline is created with a valid
-- 'Vulkan.Core10.Handles.RenderPass', parameters of this structure are
-- ignored.
--
-- If @customResolve@ is 'Vulkan.Core10.FundamentalTypes.FALSE', the
-- pipeline /can/ only be used outside the custom resolve section. If
-- @customResolve@ is 'Vulkan.Core10.FundamentalTypes.TRUE', the pipeline
-- /can/ only be used inside the custom resolve section.
--
-- When a dynamic render pass instance contains a custom resolve operation
-- and the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
-- feature is not enabled , all pipelines used to draw in such render pass
-- /must/ include this structure and have identical format information in
-- it. When a dynamic render pass does not contain a custom resolve
-- operation and the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
-- feature is not enabled , all pipelines used to draw in such render pass
-- /must/ not include this structure.
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
-- feature is enabled, then when this structure is not included in the
-- @pNext@ chain for
-- 'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo',
-- @customResolve@ is 'Vulkan.Core10.FundamentalTypes.FALSE',
-- @colorAttachmentCount@ is @0@, and @depthAttachmentFormat@ and
-- @stencilAttachmentFormat@ are
-- 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'.
--
-- If @depthAttachmentFormat@, @stencilAttachmentFormat@, or any element of
-- @pColorAttachmentFormats@ is
-- 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it indicates that the
-- corresponding attachment is unused within the resolve portion of the
-- render pass. Valid formats indicate that an attachment /can/ be used -
-- but it is still valid to set the attachment to @NULL@ when beginning
-- rendering.
--
-- When passed as a @pNext@ member to a
-- 'Vulkan.Extensions.VK_EXT_shader_object.ShaderCreateInfoEXT' struct for
-- use with fragment density maps, the @colorAttachmentCount@,
-- @pColorAttachmentFormats@, @depthAttachmentFormat@, and
-- @stencilAttachmentFormat@ members of this struct are ignored. When not
-- passed as a @pNext@ member, @customResolve@ is
-- 'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- == Valid Usage
--
-- -   #VUID-VkCustomResolveCreateInfoEXT-colorAttachmentCount-11507#
--     @colorAttachmentCount@ /must/ be less than or equal to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxColorAttachments maxColorAttachments>
--
-- -   #VUID-VkCustomResolveCreateInfoEXT-depthAttachmentFormat-11508# If
--     @depthAttachmentFormat@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it /must/ be a format
--     that includes a depth component
--
-- -   #VUID-VkCustomResolveCreateInfoEXT-depthAttachmentFormat-11509# If
--     @depthAttachmentFormat@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it /must/ be a format
--     with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#potential-format-features potential format features>
--     that include
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkCustomResolveCreateInfoEXT-pColorAttachmentFormats-11510# If
--     any element of @pColorAttachmentFormats@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it /must/ be a format
--     with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#potential-format-features potential format features>
--     that include
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--     , or
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_LINEAR_COLOR_ATTACHMENT_BIT_NV'
--     if the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-linearColorAttachment linearColorAttachment>
--     feature is enabled
--
-- -   #VUID-VkCustomResolveCreateInfoEXT-stencilAttachmentFormat-11511# If
--     @stencilAttachmentFormat@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it /must/ be a format
--     that includes a stencil aspect
--
-- -   #VUID-VkCustomResolveCreateInfoEXT-stencilAttachmentFormat-11512# If
--     @stencilAttachmentFormat@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it /must/ be a format
--     with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#potential-format-features potential format features>
--     that include
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkCustomResolveCreateInfoEXT-depthAttachmentFormat-11513# If
--     @depthAttachmentFormat@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED' and
--     @stencilAttachmentFormat@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED',
--     @depthAttachmentFormat@ /must/ equal @stencilAttachmentFormat@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCustomResolveCreateInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CUSTOM_RESOLVE_CREATE_INFO_EXT'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_custom_resolve VK_EXT_custom_resolve>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data CustomResolveCreateInfoEXT = CustomResolveCreateInfoEXT
  { -- | @customResolve@ indicates whether this pipeline will be used for a
    -- resolve operation.
    customResolve :: Bool
  , -- | @pColorAttachmentFormats@ is a pointer to an array of
    -- 'Vulkan.Core10.Enums.Format.Format' values defining the format of color
    -- resolve attachments used in custom resolves in the same render pass.
    colorAttachmentFormats :: Vector Format
  , -- | @depthAttachmentFormat@ is a 'Vulkan.Core10.Enums.Format.Format' value
    -- defining the format of the depth resolve attachment used in custom
    -- resolves in the same render pass.
    depthAttachmentFormat :: Format
  , -- | @stencilAttachmentFormat@ is a 'Vulkan.Core10.Enums.Format.Format' value
    -- defining the format of the stencil resolve attachment used in custom
    -- resolves in the same render pass.
    stencilAttachmentFormat :: Format
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CustomResolveCreateInfoEXT)
#endif
deriving instance Show CustomResolveCreateInfoEXT

instance ToCStruct CustomResolveCreateInfoEXT where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CustomResolveCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CUSTOM_RESOLVE_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (customResolve))
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (colorAttachmentFormats)) :: Word32))
    pPColorAttachmentFormats' <- ContT $ allocaBytes @Format ((Data.Vector.length (colorAttachmentFormats)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPColorAttachmentFormats' `plusPtr` (4 * (i)) :: Ptr Format) (e)) (colorAttachmentFormats)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Format))) (pPColorAttachmentFormats')
    lift $ poke ((p `plusPtr` 32 :: Ptr Format)) (depthAttachmentFormat)
    lift $ poke ((p `plusPtr` 36 :: Ptr Format)) (stencilAttachmentFormat)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CUSTOM_RESOLVE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Format)) (zero)
    f

instance FromCStruct CustomResolveCreateInfoEXT where
  peekCStruct p = do
    customResolve <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    colorAttachmentCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pColorAttachmentFormats <- peek @(Ptr Format) ((p `plusPtr` 24 :: Ptr (Ptr Format)))
    pColorAttachmentFormats' <- generateM (fromIntegral colorAttachmentCount) (\i -> peek @Format ((pColorAttachmentFormats `advancePtrBytes` (4 * (i)) :: Ptr Format)))
    depthAttachmentFormat <- peek @Format ((p `plusPtr` 32 :: Ptr Format))
    stencilAttachmentFormat <- peek @Format ((p `plusPtr` 36 :: Ptr Format))
    pure $ CustomResolveCreateInfoEXT
             (bool32ToBool customResolve)
             pColorAttachmentFormats'
             depthAttachmentFormat
             stencilAttachmentFormat

instance Zero CustomResolveCreateInfoEXT where
  zero = CustomResolveCreateInfoEXT
           zero
           mempty
           zero
           zero


type EXT_CUSTOM_RESOLVE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_CUSTOM_RESOLVE_SPEC_VERSION"
pattern EXT_CUSTOM_RESOLVE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_CUSTOM_RESOLVE_SPEC_VERSION = 1


type EXT_CUSTOM_RESOLVE_EXTENSION_NAME = "VK_EXT_custom_resolve"

-- No documentation found for TopLevel "VK_EXT_CUSTOM_RESOLVE_EXTENSION_NAME"
pattern EXT_CUSTOM_RESOLVE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_CUSTOM_RESOLVE_EXTENSION_NAME = "VK_EXT_custom_resolve"

