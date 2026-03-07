{-# language CPP #-}
-- | = Name
--
-- VK_KHR_maintenance10 - device extension
--
-- = VK_KHR_maintenance10
--
-- [__Name String__]
--     @VK_KHR_maintenance10@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     631
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
--     -   Interacts with VK_VERSION_1_4
--
--     -   Interacts with VK_KHR_copy_commands2
--
--     -   Interacts with VK_KHR_dynamic_rendering
--
--     -   Interacts with VK_KHR_dynamic_rendering_local_read
--
--     -   Interacts with VK_KHR_format_feature_flags2
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_maintenance10] @zmike%0A*Here describe the issue or question you have about the VK_KHR_maintenance10 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_maintenance10.adoc VK_KHR_maintenance10>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-05-13
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension interacts with @VK_KHR_format_feature_flags2@
--
--     -   This extension interacts with @VK_EXT_extended_dynamic_state3@
--
--     -   This extension interacts with
--         @VK_KHR_dynamic_rendering_local_read@
--
--     -   This extension interacts with @VK_KHR_depth_stencil_resolve@
--
-- [__Contributors__]
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Piers Daniell, NVIDIA
--
--     -   Hans-Kristian Arntzen, Valve
--
-- == Description
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance10 VK_KHR_maintenance10>
-- adds a collection of minor features, none of which would warrant an
-- entire extension of their own.
--
-- The new features are as follows:
--
-- -   New image format feature bits that indicate support for copying
--     depth or stencil aspects using non-graphics queue families
--
-- -   If
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleMaskEXT'
--     is called with @pSampleMask@ set to @NULL@, it is treated as if the
--     mask has all bits set to @1@.
--
-- -   Add vkCmdEndRendering2KHR as an extensible version of
--     vkCmdEndRendering
--
-- -   Add input attachment information to dynamic rendering
--
-- -   Require that vertex inputs follow sRGB encoding when those formats
--     are used, instead of being underspecified.
--
-- -   Add a query to determine if sRGB images are resolved in nonlinear or
--     linear space by default
--
-- -   Add an optional feature to allow applications to override the
--     default sRGB resolve behavior
--
-- -   Add resolve mode and depth-stencil resolve support to
--     'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.cmdResolveImage2'
--     to bring it in-line with render pass attachment resolves
--
-- == New Commands
--
-- -   'cmdEndRendering2KHR'
--
-- == New Structures
--
-- -   'RenderingEndInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMaintenance10FeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMaintenance10PropertiesKHR'
--
-- -   Extending
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingAttachmentInfo':
--
--     -   'RenderingAttachmentFlagsInfoKHR'
--
-- -   Extending
--     'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.ResolveImageInfo2':
--
--     -   'ResolveImageModeInfoKHR'
--
-- == New Enums
--
-- -   'RenderingAttachmentFlagBitsKHR'
--
-- -   'ResolveImageFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'RenderingAttachmentFlagsKHR'
--
-- -   'ResolveImageFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_MAINTENANCE_10_EXTENSION_NAME'
--
-- -   'KHR_MAINTENANCE_10_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.AttachmentDescriptionFlagBits':
--
--     -   'Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.ATTACHMENT_DESCRIPTION_RESOLVE_ENABLE_TRANSFER_FUNCTION_BIT_KHR'
--
--     -   'Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.ATTACHMENT_DESCRIPTION_RESOLVE_SKIP_TRANSFER_FUNCTION_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_10_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_10_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_ATTACHMENT_FLAGS_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_END_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RESOLVE_IMAGE_MODE_INFO_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.4 Vulkan Version 1.4>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering_local_read VK_KHR_dynamic_rendering_local_read>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- is supported:
--
-- -   Extending 'RenderingAttachmentFlagBitsKHR':
--
--     -   'RENDERING_ATTACHMENT_INPUT_ATTACHMENT_FEEDBACK_BIT_KHR'
--
-- -   Extending 'Vulkan.Core13.Enums.RenderingFlagBits.RenderingFlagBits':
--
--     -   'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_LOCAL_READ_CONCURRENT_ACCESS_CONTROL_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_format_feature_flags2 VK_KHR_format_feature_flags2>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_DEPTH_COPY_ON_COMPUTE_QUEUE_BIT_KHR'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_DEPTH_COPY_ON_TRANSFER_QUEUE_BIT_KHR'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STENCIL_COPY_ON_COMPUTE_QUEUE_BIT_KHR'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STENCIL_COPY_ON_TRANSFER_QUEUE_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>
-- is supported:
--
-- -   Extending 'ResolveImageFlagBitsKHR':
--
--     -   'RESOLVE_IMAGE_ENABLE_TRANSFER_FUNCTION_BIT_KHR'
--
--     -   'RESOLVE_IMAGE_SKIP_TRANSFER_FUNCTION_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- is supported:
--
-- -   Extending 'RenderingAttachmentFlagBitsKHR':
--
--     -   'RENDERING_ATTACHMENT_RESOLVE_ENABLE_TRANSFER_FUNCTION_BIT_KHR'
--
--     -   'RENDERING_ATTACHMENT_RESOLVE_SKIP_TRANSFER_FUNCTION_BIT_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2025-05-13 (Mike Blumenkrantz)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_maintenance10 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_maintenance10  ( cmdEndRendering2KHR
                                               , PhysicalDeviceMaintenance10PropertiesKHR(..)
                                               , PhysicalDeviceMaintenance10FeaturesKHR(..)
                                               , RenderingEndInfoKHR(..)
                                               , RenderingAttachmentFlagsInfoKHR(..)
                                               , ResolveImageModeInfoKHR(..)
                                               , RenderingAttachmentFlagsKHR
                                               , RenderingAttachmentFlagBitsKHR( RENDERING_ATTACHMENT_RESOLVE_ENABLE_TRANSFER_FUNCTION_BIT_KHR
                                                                               , RENDERING_ATTACHMENT_RESOLVE_SKIP_TRANSFER_FUNCTION_BIT_KHR
                                                                               , RENDERING_ATTACHMENT_INPUT_ATTACHMENT_FEEDBACK_BIT_KHR
                                                                               , ..
                                                                               )
                                               , ResolveImageFlagsKHR
                                               , ResolveImageFlagBitsKHR( RESOLVE_IMAGE_ENABLE_TRANSFER_FUNCTION_BIT_KHR
                                                                        , RESOLVE_IMAGE_SKIP_TRANSFER_FUNCTION_BIT_KHR
                                                                        , ..
                                                                        )
                                               , KHR_MAINTENANCE_10_SPEC_VERSION
                                               , pattern KHR_MAINTENANCE_10_SPEC_VERSION
                                               , KHR_MAINTENANCE_10_EXTENSION_NAME
                                               , pattern KHR_MAINTENANCE_10_EXTENSION_NAME
                                               ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
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
import Data.Type.Equality ((:~:)(Refl))
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
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndRendering2KHR))
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map_offset (RenderPassFragmentDensityMapOffsetEndInfoEXT)
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlagBits)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_10_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_10_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_ATTACHMENT_FLAGS_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_END_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RESOLVE_IMAGE_MODE_INFO_KHR))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndRendering2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct RenderingEndInfoKHR) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct RenderingEndInfoKHR) -> IO ()

-- | vkCmdEndRendering2KHR - End a dynamic render pass instance
--
-- = Description
--
-- If the value of @pRenderingInfo->flags@ used to begin this render pass
-- instance included
-- 'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_SUSPENDING_BIT', then
-- this render pass is suspended and will be resumed later in
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-submission-order submission order>.
--
-- There is no implicit ordering between separate render passes, even in
-- the same command buffer, and even when the attachments match. Some
-- applications rely on the continuation of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-order rasterization order>
-- between multiple render passes with attachments defined in the same way,
-- in order to perform non-rendering operations (such as copies or compute
-- operations) between draw calls, but this has never been required by the
-- specification. There is also no explicit barrier currently in the API
-- that provides the guarantee that applications rely on without additional
-- performance penalties.
--
-- New applications should avoid relying on this ordering until an
-- appropriate barrier is added to the API.
--
-- Implementations where applications are performing this splitting are
-- encouraged to continue supporting this guarantee until a suitable
-- barrier is added to the API.
--
-- Existing applications relying on this ordering should expect that it
-- will continue working on platforms where it currently does. Once a new
-- extension adds support for a new barrier, developers are encouraged to
-- adapt their applications to use this when available.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdEndRendering2KHR-None-10610# The current render pass
--     instance /must/ have been begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--
-- -   #VUID-vkCmdEndRendering2KHR-commandBuffer-10611# The current render
--     pass instance /must/ have been begun in @commandBuffer@
--
-- -   #VUID-vkCmdEndRendering2KHR-None-10612# This command /must/ not be
--     recorded when transform feedback is active
--
-- -   #VUID-vkCmdEndRendering2KHR-None-10613# If
--     'Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery'* was called
--     within the render pass, the corresponding
--     'Vulkan.Core10.CommandBufferBuilding.cmdEndQuery'* /must/ have been
--     called subsequently within the same subpass
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdEndRendering2KHR-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdEndRendering2KHR-pRenderingEndInfo-parameter# If
--     @pRenderingEndInfo@ is not @NULL@, @pRenderingEndInfo@ /must/ be a
--     valid pointer to a valid 'RenderingEndInfoKHR' structure
--
-- -   #VUID-vkCmdEndRendering2KHR-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdEndRendering2KHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdEndRendering2KHR-renderpass# This command /must/ only be
--     called inside of a render pass instance
--
-- -   #VUID-vkCmdEndRendering2KHR-suspended# This command /must/ not be
--     called between suspended render pass instances
--
-- -   #VUID-vkCmdEndRendering2KHR-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       | State                                                                                                                                  |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdEndRendering2KHR is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map_offset VK_EXT_fragment_density_map_offset>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance10 VK_KHR_maintenance10>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'RenderingEndInfoKHR'
cmdEndRendering2KHR :: forall a io
                     . (Extendss RenderingEndInfoKHR a, PokeChain a, MonadIO io)
                    => -- | @commandBuffer@ is the command buffer in which to record the command.
                       CommandBuffer
                    -> -- | @pRenderingEndInfo@ is @NULL@ or a pointer to a 'RenderingEndInfoKHR'
                       -- structure containing information about how the render pass will be
                       -- ended.
                       ("renderingEndInfo" ::: Maybe (RenderingEndInfoKHR a))
                    -> io ()
cmdEndRendering2KHR commandBuffer renderingEndInfo = liftIO . evalContT $ do
  let vkCmdEndRendering2KHRPtr = pVkCmdEndRendering2KHR (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdEndRendering2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndRendering2KHR is null" Nothing Nothing
  let vkCmdEndRendering2KHR' = mkVkCmdEndRendering2KHR vkCmdEndRendering2KHRPtr
  pRenderingEndInfo <- case (renderingEndInfo) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkCmdEndRendering2KHR" (vkCmdEndRendering2KHR'
                                                     (commandBufferHandle (commandBuffer))
                                                     (forgetExtensions pRenderingEndInfo))
  pure $ ()


-- | VkPhysicalDeviceMaintenance10PropertiesKHR - Structure describing
-- various implementation-defined properties introduced with
-- VK_KHR_maintenance10
--
-- = Description
--
-- Implementations supporting
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-maintenance10 maintenance10>
-- /should/ set @resolveSrgbFormatAppliesTransferFunction@ to
-- 'Vulkan.Core10.FundamentalTypes.TRUE'.
--
-- If the 'PhysicalDeviceMaintenance10PropertiesKHR' structure is included
-- in the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance10 VK_KHR_maintenance10>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance10PropertiesKHR = PhysicalDeviceMaintenance10PropertiesKHR
  { -- | #limits-rgba4OpaqueBlackSwizzled# @rgba4OpaqueBlackSwizzled@ indicates
    -- whether correct swizzling is applied to the opaque black border color
    -- when using either the
    -- 'Vulkan.Core10.Enums.Format.FORMAT_B4G4R4A4_UNORM_PACK16' or
    -- 'Vulkan.Core10.Enums.Format.FORMAT_R4G4B4A4_UNORM_PACK16' format. If it
    -- is 'Vulkan.Core10.FundamentalTypes.TRUE', the implementation will
    -- correctly produce an opaque black border color with these formats. If it
    -- is 'Vulkan.Core10.FundamentalTypes.FALSE', the implementation /may/ swap
    -- the first channel with the alpha channel for the border color when
    -- sampling.
    rgba4OpaqueBlackSwizzled :: Bool
  , -- | #limits-resolveSrgbFormatAppliesTransferFunction#
    -- @resolveSrgbFormatAppliesTransferFunction@ indicates whether resolving a
    -- multi-sampled sRGB format to single-sampled sRGB by a weighted average
    -- converts the samples to linear before averaging. This applies to both
    -- attachment resolves in a render pass and standalone resolve commands. If
    -- 'Vulkan.Core10.FundamentalTypes.TRUE', implementation always converts to
    -- linear before averaging unless overridden. If
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', implementation never converts to
    -- linear before averaging unless overridden.
    resolveSrgbFormatAppliesTransferFunction :: Bool
  , -- | #limits-resolveSrgbFormatSupportsTransferFunctionControl#
    -- @resolveSrgbFormatSupportsTransferFunctionControl@ indicates whether the
    -- implementation supports overriding the default behavior in
    -- @resolveSrgbFormatAppliesTransferFunction@ in render passes and
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.cmdResolveImage2'.
    resolveSrgbFormatSupportsTransferFunctionControl :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance10PropertiesKHR)
#endif
deriving instance Show PhysicalDeviceMaintenance10PropertiesKHR

instance ToCStruct PhysicalDeviceMaintenance10PropertiesKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance10PropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_10_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (rgba4OpaqueBlackSwizzled))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (resolveSrgbFormatAppliesTransferFunction))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (resolveSrgbFormatSupportsTransferFunctionControl))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_10_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMaintenance10PropertiesKHR where
  peekCStruct p = do
    rgba4OpaqueBlackSwizzled <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    resolveSrgbFormatAppliesTransferFunction <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    resolveSrgbFormatSupportsTransferFunctionControl <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceMaintenance10PropertiesKHR
             (bool32ToBool rgba4OpaqueBlackSwizzled)
             (bool32ToBool resolveSrgbFormatAppliesTransferFunction)
             (bool32ToBool resolveSrgbFormatSupportsTransferFunctionControl)

instance Storable PhysicalDeviceMaintenance10PropertiesKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance10PropertiesKHR where
  zero = PhysicalDeviceMaintenance10PropertiesKHR
           zero
           zero
           zero


-- | VkPhysicalDeviceMaintenance10FeaturesKHR - Structure describing whether
-- the implementation supports maintenance10 functionality
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance10FeaturesKHR' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceMaintenance10FeaturesKHR', it /must/ add an instance of
-- the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance10 VK_KHR_maintenance10>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance10FeaturesKHR = PhysicalDeviceMaintenance10FeaturesKHR
  { -- | #features-maintenance10# @maintenance10@ indicates that the
    -- implementation supports the following:
    --
    -- -   New image format feature bits that indicate support for copying
    --     depth or stencil aspects using non-graphics queue families
    --
    -- -   If
    --     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleMaskEXT'
    --     is called with @pSampleMask@ set to @NULL@, it is treated as if the
    --     mask has all bits set to @1@.
    --
    -- -   Add 'cmdEndRendering2KHR' as an extensible version of
    --     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdEndRendering'
    --
    -- -   Add input attachment information to dynamic rendering
    --
    -- -   Require that vertex inputs follow sRGB encoding when those formats
    --     are used, instead of being underspecified.
    --
    -- -   Add a query to determine if sRGB images are resolved in nonlinear or
    --     linear space by default
    --
    -- -   Add an optional feature to allow applications to override the
    --     default sRGB resolve behavior
    --
    -- -   Add resolve mode and depth-stencil resolve support to
    --     'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.cmdResolveImage2'
    --     to bring it in-line with render pass attachment resolves
    maintenance10 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance10FeaturesKHR)
#endif
deriving instance Show PhysicalDeviceMaintenance10FeaturesKHR

instance ToCStruct PhysicalDeviceMaintenance10FeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance10FeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_10_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (maintenance10))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_10_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMaintenance10FeaturesKHR where
  peekCStruct p = do
    maintenance10 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMaintenance10FeaturesKHR
             (bool32ToBool maintenance10)

instance Storable PhysicalDeviceMaintenance10FeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance10FeaturesKHR where
  zero = PhysicalDeviceMaintenance10FeaturesKHR
           zero


-- | VkRenderingEndInfoKHR - Structure specifying render pass end information
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderingEndInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_END_INFO_KHR'
--
-- -   #VUID-VkRenderingEndInfoKHR-pNext-pNext# @pNext@ /must/ be @NULL@ or
--     a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_fragment_density_map_offset.RenderPassFragmentDensityMapOffsetEndInfoEXT'
--
-- -   #VUID-VkRenderingEndInfoKHR-sType-unique# The @sType@ value of each
--     structure in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map_offset VK_EXT_fragment_density_map_offset>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance10 VK_KHR_maintenance10>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdEndRendering2KHR', 'cmdEndRendering2KHR'
data RenderingEndInfoKHR (es :: [Type]) = RenderingEndInfoKHR
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderingEndInfoKHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (RenderingEndInfoKHR es)

instance Extensible RenderingEndInfoKHR where
  extensibleTypeName = "RenderingEndInfoKHR"
  setNext _ next' = RenderingEndInfoKHR{next = next'}
  getNext RenderingEndInfoKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends RenderingEndInfoKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @RenderPassFragmentDensityMapOffsetEndInfoEXT = Just f
    | otherwise = Nothing

instance ( Extendss RenderingEndInfoKHR es
         , PokeChain es ) => ToCStruct (RenderingEndInfoKHR es) where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderingEndInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_END_INFO_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_END_INFO_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance ( Extendss RenderingEndInfoKHR es
         , PeekChain es ) => FromCStruct (RenderingEndInfoKHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    pure $ RenderingEndInfoKHR
             next

instance es ~ '[] => Zero (RenderingEndInfoKHR es) where
  zero = RenderingEndInfoKHR
           ()


-- | VkRenderingAttachmentFlagsInfoKHR - Structure specifying flags extending
-- a rendering attachment
--
-- == Valid Usage
--
-- -   #VUID-VkRenderingAttachmentFlagsInfoKHR-flags-11755# @flags@ /must/
--     not include 'RENDERING_ATTACHMENT_INPUT_ATTACHMENT_FEEDBACK_BIT_KHR'
--     if the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dynamicRenderingLocalRead dynamicRenderingLocalRead>
--     feature is not enabled
--
-- -   #VUID-VkRenderingAttachmentFlagsInfoKHR-flags-11756# If @flags@
--     includes
--     'RENDERING_ATTACHMENT_RESOLVE_SKIP_TRANSFER_FUNCTION_BIT_KHR',
--     @flags@ /must/ not include
--     'RENDERING_ATTACHMENT_RESOLVE_ENABLE_TRANSFER_FUNCTION_BIT_KHR'
--
-- -   #VUID-VkRenderingAttachmentFlagsInfoKHR-flags-11757# If @flags@
--     includes
--     'RENDERING_ATTACHMENT_RESOLVE_SKIP_TRANSFER_FUNCTION_BIT_KHR' or
--     'RENDERING_ATTACHMENT_RESOLVE_ENABLE_TRANSFER_FUNCTION_BIT_KHR',
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-resolveSrgbFormatSupportsTransferFunctionControl resolveSrgbFormatSupportsTransferFunctionControl>
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderingAttachmentFlagsInfoKHR-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_ATTACHMENT_FLAGS_INFO_KHR'
--
-- -   #VUID-VkRenderingAttachmentFlagsInfoKHR-flags-parameter# @flags@
--     /must/ be a valid combination of 'RenderingAttachmentFlagBitsKHR'
--     values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance10 VK_KHR_maintenance10>,
-- 'RenderingAttachmentFlagsKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RenderingAttachmentFlagsInfoKHR = RenderingAttachmentFlagsInfoKHR
  { -- | @flags@ is a bitmask of 'RenderingAttachmentFlagsKHR'
    flags :: RenderingAttachmentFlagsKHR }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderingAttachmentFlagsInfoKHR)
#endif
deriving instance Show RenderingAttachmentFlagsInfoKHR

instance ToCStruct RenderingAttachmentFlagsInfoKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderingAttachmentFlagsInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_ATTACHMENT_FLAGS_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RenderingAttachmentFlagsKHR)) (flags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_ATTACHMENT_FLAGS_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct RenderingAttachmentFlagsInfoKHR where
  peekCStruct p = do
    flags <- peek @RenderingAttachmentFlagsKHR ((p `plusPtr` 16 :: Ptr RenderingAttachmentFlagsKHR))
    pure $ RenderingAttachmentFlagsInfoKHR
             flags

instance Storable RenderingAttachmentFlagsInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RenderingAttachmentFlagsInfoKHR where
  zero = RenderingAttachmentFlagsInfoKHR
           zero


-- | VkResolveImageModeInfoKHR - Structure specifying additional control for
-- VkResolveImageInfo2
--
-- == Valid Usage
--
-- -   #VUID-VkResolveImageModeInfoKHR-flags-10995# If @flags@ includes
--     'RESOLVE_IMAGE_SKIP_TRANSFER_FUNCTION_BIT_KHR', @flags@ /must/ not
--     include 'RESOLVE_IMAGE_ENABLE_TRANSFER_FUNCTION_BIT_KHR'
--
-- -   #VUID-VkResolveImageModeInfoKHR-flags-10996# If @flags@ includes
--     'RESOLVE_IMAGE_SKIP_TRANSFER_FUNCTION_BIT_KHR' or
--     'RESOLVE_IMAGE_ENABLE_TRANSFER_FUNCTION_BIT_KHR',
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-resolveSrgbFormatSupportsTransferFunctionControl resolveSrgbFormatSupportsTransferFunctionControl>
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkResolveImageModeInfoKHR-flags-10997# If @flags@ includes
--     'RESOLVE_IMAGE_SKIP_TRANSFER_FUNCTION_BIT_KHR' or
--     'RESOLVE_IMAGE_ENABLE_TRANSFER_FUNCTION_BIT_KHR', @resolveMode@
--     /must/ be equal to
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_AVERAGE_BIT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkResolveImageModeInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RESOLVE_IMAGE_MODE_INFO_KHR'
--
-- -   #VUID-VkResolveImageModeInfoKHR-flags-parameter# @flags@ /must/ be a
--     valid combination of 'ResolveImageFlagBitsKHR' values
--
-- -   #VUID-VkResolveImageModeInfoKHR-resolveMode-parameter# If
--     @resolveMode@ is not @0@, @resolveMode@ /must/ be a valid
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits' value
--
-- -   #VUID-VkResolveImageModeInfoKHR-stencilResolveMode-parameter# If
--     @stencilResolveMode@ is not @0@, @stencilResolveMode@ /must/ be a
--     valid 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits'
--     value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance10 VK_KHR_maintenance10>,
-- 'ResolveImageFlagsKHR',
-- 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ResolveImageModeInfoKHR = ResolveImageModeInfoKHR
  { -- | @flags@ is a bitmask of 'ResolveImageFlagBitsKHR'.
    flags :: ResolveImageFlagsKHR
  , -- | @resolveMode@ is a
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits' value
    -- defining how @srcImage@ will be resolved into @dstImage@ when resolving
    -- non-stencil values.
    resolveMode :: ResolveModeFlagBits
  , -- | @stencilResolveMode@ is a
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits' value
    -- defining how @srcImage@ will be resolved into @dstImage@ when resolving
    -- stencil values.
    stencilResolveMode :: ResolveModeFlagBits
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ResolveImageModeInfoKHR)
#endif
deriving instance Show ResolveImageModeInfoKHR

instance ToCStruct ResolveImageModeInfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ResolveImageModeInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RESOLVE_IMAGE_MODE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ResolveImageFlagsKHR)) (flags)
    poke ((p `plusPtr` 20 :: Ptr ResolveModeFlagBits)) (resolveMode)
    poke ((p `plusPtr` 24 :: Ptr ResolveModeFlagBits)) (stencilResolveMode)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RESOLVE_IMAGE_MODE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ResolveImageModeInfoKHR where
  peekCStruct p = do
    flags <- peek @ResolveImageFlagsKHR ((p `plusPtr` 16 :: Ptr ResolveImageFlagsKHR))
    resolveMode <- peek @ResolveModeFlagBits ((p `plusPtr` 20 :: Ptr ResolveModeFlagBits))
    stencilResolveMode <- peek @ResolveModeFlagBits ((p `plusPtr` 24 :: Ptr ResolveModeFlagBits))
    pure $ ResolveImageModeInfoKHR
             flags resolveMode stencilResolveMode

instance Storable ResolveImageModeInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ResolveImageModeInfoKHR where
  zero = ResolveImageModeInfoKHR
           zero
           zero
           zero


type RenderingAttachmentFlagsKHR = RenderingAttachmentFlagBitsKHR

-- | VkRenderingAttachmentFlagBitsKHR - Bitmask specifying additional
-- properties of a rendering attachment
--
-- = Description
--
-- -   'RENDERING_ATTACHMENT_INPUT_ATTACHMENT_FEEDBACK_BIT_KHR' specifies
--     that the attachment /can/ be used concurrently as both an input
--     attachment and a write-only attachment during the render pass,
--     creating a feedback loop while processing a fragment, and without a
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_BY_REGION_BIT'
--     barrier separating the write attachment and input attachment usage.
--     Using this flag does not remove the general requirement to use a
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_BY_REGION_BIT'
--     barrier to resolve hazards when two different fragments accesses a
--     particular attachment region, where one of them performs an
--     attachment write, and a subsequent fragment performs an input
--     attachment read. If
--     'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_LOCAL_READ_CONCURRENT_ACCESS_CONTROL_BIT_KHR'
--     is specified in the rendering info, this flag /must/ be set for an
--     attachment to be used concurrently as an input attachment and a
--     write attachment in this manner. If
--     'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_LOCAL_READ_CONCURRENT_ACCESS_CONTROL_BIT_KHR'
--     is not specified in the rendering info, this flag is implied to be
--     set for any attachment which has a combination of image layouts and
--     image view usage flags which support input attachment usage.
--
-- -   'RENDERING_ATTACHMENT_RESOLVE_SKIP_TRANSFER_FUNCTION_BIT_KHR'
--     specifies that resolve operations happening to an sRGB encoded
--     attachment /must/ not convert samples from nonlinear to linear
--     before averaging.
--
-- -   'RENDERING_ATTACHMENT_RESOLVE_ENABLE_TRANSFER_FUNCTION_BIT_KHR'
--     specifies that resolve operations happening to an sRGB encoded
--     attachment /must/ convert samples from nonlinear to linear before
--     averaging.
--
-- 'RENDERING_ATTACHMENT_INPUT_ATTACHMENT_FEEDBACK_BIT_KHR' is intended to
-- give implementations similar information as a subpass where an
-- attachment could be used as both a color attachment and input
-- attachment. Some implementations require extra work to make this
-- scenario work beyond just considering the image layouts. Implementations
-- which have no such considerations may treat this flag as a noop. The
-- primary use case for this flag is to enable feedback loops inside a
-- single shader.
--
-- Applications are encouraged to use
-- 'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_LOCAL_READ_CONCURRENT_ACCESS_CONTROL_BIT_KHR'
-- if
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-maintenance10 maintenance10>
-- is available and they use feedback loops with
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering_local_read VK_KHR_dynamic_rendering_local_read>.
-- Feedback loops are still allowed when not using the rendering flag, but
-- the performance implication was an oversight in the original definition
-- of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering_local_read VK_KHR_dynamic_rendering_local_read>.
--
-- In some scenarios, resolving sRGB in nonlinear space instead of the
-- expected linear space can improve perceptual aliasing at the cost of
-- inaccurate color blending.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance10 VK_KHR_maintenance10>,
-- 'RenderingAttachmentFlagsKHR'
newtype RenderingAttachmentFlagBitsKHR = RenderingAttachmentFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkRenderingAttachmentFlagBitsKHR" "VK_RENDERING_ATTACHMENT_RESOLVE_ENABLE_TRANSFER_FUNCTION_BIT_KHR"
pattern RENDERING_ATTACHMENT_RESOLVE_ENABLE_TRANSFER_FUNCTION_BIT_KHR = RenderingAttachmentFlagBitsKHR 0x00000004

-- No documentation found for Nested "VkRenderingAttachmentFlagBitsKHR" "VK_RENDERING_ATTACHMENT_RESOLVE_SKIP_TRANSFER_FUNCTION_BIT_KHR"
pattern RENDERING_ATTACHMENT_RESOLVE_SKIP_TRANSFER_FUNCTION_BIT_KHR = RenderingAttachmentFlagBitsKHR 0x00000002

-- No documentation found for Nested "VkRenderingAttachmentFlagBitsKHR" "VK_RENDERING_ATTACHMENT_INPUT_ATTACHMENT_FEEDBACK_BIT_KHR"
pattern RENDERING_ATTACHMENT_INPUT_ATTACHMENT_FEEDBACK_BIT_KHR = RenderingAttachmentFlagBitsKHR 0x00000001

conNameRenderingAttachmentFlagBitsKHR :: String
conNameRenderingAttachmentFlagBitsKHR = "RenderingAttachmentFlagBitsKHR"

enumPrefixRenderingAttachmentFlagBitsKHR :: String
enumPrefixRenderingAttachmentFlagBitsKHR = "RENDERING_ATTACHMENT_"

showTableRenderingAttachmentFlagBitsKHR :: [(RenderingAttachmentFlagBitsKHR, String)]
showTableRenderingAttachmentFlagBitsKHR =
  [
    ( RENDERING_ATTACHMENT_RESOLVE_ENABLE_TRANSFER_FUNCTION_BIT_KHR
    , "RESOLVE_ENABLE_TRANSFER_FUNCTION_BIT_KHR"
    )
  ,
    ( RENDERING_ATTACHMENT_RESOLVE_SKIP_TRANSFER_FUNCTION_BIT_KHR
    , "RESOLVE_SKIP_TRANSFER_FUNCTION_BIT_KHR"
    )
  ,
    ( RENDERING_ATTACHMENT_INPUT_ATTACHMENT_FEEDBACK_BIT_KHR
    , "INPUT_ATTACHMENT_FEEDBACK_BIT_KHR"
    )
  ]

instance Show RenderingAttachmentFlagBitsKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixRenderingAttachmentFlagBitsKHR
      showTableRenderingAttachmentFlagBitsKHR
      conNameRenderingAttachmentFlagBitsKHR
      (\(RenderingAttachmentFlagBitsKHR x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read RenderingAttachmentFlagBitsKHR where
  readPrec =
    enumReadPrec
      enumPrefixRenderingAttachmentFlagBitsKHR
      showTableRenderingAttachmentFlagBitsKHR
      conNameRenderingAttachmentFlagBitsKHR
      RenderingAttachmentFlagBitsKHR

type ResolveImageFlagsKHR = ResolveImageFlagBitsKHR

-- | VkResolveImageFlagBitsKHR - Bitmask specifying additional properties of
-- a resolve image operation
--
-- = Description
--
-- -   'RESOLVE_IMAGE_SKIP_TRANSFER_FUNCTION_BIT_KHR' specifies that
--     resolve operations happening to an sRGB encoded image /must/ not
--     convert samples from nonlinear to linear before averaging.
--
-- -   'RESOLVE_IMAGE_ENABLE_TRANSFER_FUNCTION_BIT_KHR' specifies that
--     resolve operations happening to an sRGB encoded image /must/ convert
--     samples from nonlinear to linear before averaging.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance10 VK_KHR_maintenance10>,
-- 'ResolveImageFlagsKHR'
newtype ResolveImageFlagBitsKHR = ResolveImageFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkResolveImageFlagBitsKHR" "VK_RESOLVE_IMAGE_ENABLE_TRANSFER_FUNCTION_BIT_KHR"
pattern RESOLVE_IMAGE_ENABLE_TRANSFER_FUNCTION_BIT_KHR = ResolveImageFlagBitsKHR 0x00000002

-- No documentation found for Nested "VkResolveImageFlagBitsKHR" "VK_RESOLVE_IMAGE_SKIP_TRANSFER_FUNCTION_BIT_KHR"
pattern RESOLVE_IMAGE_SKIP_TRANSFER_FUNCTION_BIT_KHR = ResolveImageFlagBitsKHR 0x00000001

conNameResolveImageFlagBitsKHR :: String
conNameResolveImageFlagBitsKHR = "ResolveImageFlagBitsKHR"

enumPrefixResolveImageFlagBitsKHR :: String
enumPrefixResolveImageFlagBitsKHR = "RESOLVE_IMAGE_"

showTableResolveImageFlagBitsKHR :: [(ResolveImageFlagBitsKHR, String)]
showTableResolveImageFlagBitsKHR =
  [
    ( RESOLVE_IMAGE_ENABLE_TRANSFER_FUNCTION_BIT_KHR
    , "ENABLE_TRANSFER_FUNCTION_BIT_KHR"
    )
  ,
    ( RESOLVE_IMAGE_SKIP_TRANSFER_FUNCTION_BIT_KHR
    , "SKIP_TRANSFER_FUNCTION_BIT_KHR"
    )
  ]

instance Show ResolveImageFlagBitsKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixResolveImageFlagBitsKHR
      showTableResolveImageFlagBitsKHR
      conNameResolveImageFlagBitsKHR
      (\(ResolveImageFlagBitsKHR x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ResolveImageFlagBitsKHR where
  readPrec =
    enumReadPrec
      enumPrefixResolveImageFlagBitsKHR
      showTableResolveImageFlagBitsKHR
      conNameResolveImageFlagBitsKHR
      ResolveImageFlagBitsKHR

type KHR_MAINTENANCE_10_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_10_SPEC_VERSION"
pattern KHR_MAINTENANCE_10_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_MAINTENANCE_10_SPEC_VERSION = 1


type KHR_MAINTENANCE_10_EXTENSION_NAME = "VK_KHR_maintenance10"

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_10_EXTENSION_NAME"
pattern KHR_MAINTENANCE_10_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_MAINTENANCE_10_EXTENSION_NAME = "VK_KHR_maintenance10"

