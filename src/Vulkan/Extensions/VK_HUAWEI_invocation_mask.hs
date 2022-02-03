{-# language CPP #-}
-- | = Name
--
-- VK_HUAWEI_invocation_mask - device extension
--
-- == VK_HUAWEI_invocation_mask
--
-- [__Name String__]
--     @VK_HUAWEI_invocation_mask@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     371
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_ray_tracing_pipeline@
--
--     -   Requires @VK_KHR_synchronization2@
--
-- [__Contact__]
--
--     -   Yunpeng Zhu
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_HUAWEI_invocation_mask] @yunxingzhu%0A<<Here describe the issue or question you have about the VK_HUAWEI_invocation_mask extension>> >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_HUAWEI_invocation_mask.asciidoc VK_HUAWEI_invocation_mask>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-05-27
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires @VK_KHR_ray_tracing_pipeline@, which
--         allow to bind an invocation mask image before the ray tracing
--         command
--
--     -   This extension requires @VK_KHR_synchronization2@, which allows
--         new pipeline stage for the invocation mask image
--
-- [__Contributors__]
--
--     -   Yunpeng Zhu, HuaWei
--
-- == Description
--
-- The rays to trace may be sparse in some use cases. For example, the
-- scene only have a few regions to reflect. Providing an invocation mask
-- image to the ray tracing commands could potentially give the hardware
-- the hint to do certain optimization without invoking an additional pass
-- to compact the ray buffer.
--
-- == New Commands
--
-- -   'cmdBindInvocationMaskHUAWEI'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceInvocationMaskFeaturesHUAWEI'
--
-- == New Enum Constants
--
-- -   'HUAWEI_INVOCATION_MASK_EXTENSION_NAME'
--
-- -   'HUAWEI_INVOCATION_MASK_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_INVOCATION_MASK_FEATURES_HUAWEI'
--
-- == Examples
--
-- RT mask is updated before each traceRay.
--
-- Step 1. Generate InvocationMask.
--
-- > //the rt mask image bind as color attachment in the fragment shader
-- > Layout(location = 2) out vec4 outRTmask
-- > vec4 mask = vec4(x,x,x,x);
-- > outRTmask = mask;
--
-- Step 2. traceRay with InvocationMask
--
-- > vkCmdBindPipeline(
-- >     commandBuffers[imageIndex],
-- >     VK_PIPELINE_BIND_POINT_RAY_TRACING_KHR, m_rtPipeline);
-- >     vkCmdBindDescriptorSets(commandBuffers[imageIndex],
-- >     VK_PIPELINE_BIND_POINT_RAY_TRACING_NV,
-- >     m_rtPipelineLayout, 0, 1, &m_rtDescriptorSet,
-- >     0, nullptr);
-- >
-- > vkCmdBindInvocationMaskHUAWEI(
-- >     commandBuffers[imageIndex],
-- >     InvocationMaskimageView,
-- >     InvocationMaskimageLayout);
-- >     vkCmdTraceRaysKHR(commandBuffers[imageIndex],
-- >     pRaygenShaderBindingTable,
-- >     pMissShaderBindingTable,
-- >     swapChainExtent.width,
-- >     swapChainExtent.height, 1);
--
-- == Version History
--
-- -   Revision 1, 2021-05-27 (Yunpeng Zhu)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'PhysicalDeviceInvocationMaskFeaturesHUAWEI',
-- 'cmdBindInvocationMaskHUAWEI'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_HUAWEI_invocation_mask Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_HUAWEI_invocation_mask  ( cmdBindInvocationMaskHUAWEI
                                                    , PhysicalDeviceInvocationMaskFeaturesHUAWEI(..)
                                                    , HUAWEI_INVOCATION_MASK_SPEC_VERSION
                                                    , pattern HUAWEI_INVOCATION_MASK_SPEC_VERSION
                                                    , HUAWEI_INVOCATION_MASK_EXTENSION_NAME
                                                    , pattern HUAWEI_INVOCATION_MASK_EXTENSION_NAME
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
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindInvocationMaskHUAWEI))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(..))
import Vulkan.Core10.Handles (ImageView)
import Vulkan.Core10.Handles (ImageView(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_INVOCATION_MASK_FEATURES_HUAWEI))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindInvocationMaskHUAWEI
  :: FunPtr (Ptr CommandBuffer_T -> ImageView -> ImageLayout -> IO ()) -> Ptr CommandBuffer_T -> ImageView -> ImageLayout -> IO ()

-- | vkCmdBindInvocationMaskHUAWEI - Bind an invocation mask image on a
-- command buffer
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBindInvocationMaskHUAWEI-None-04976# The
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-invocationMask invocation mask image>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdBindInvocationMaskHUAWEI-imageView-04977# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', it /must/ be a
--     valid 'Vulkan.Core10.Handles.ImageView' handle of type
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D'
--
-- -   #VUID-vkCmdBindInvocationMaskHUAWEI-imageView-04978# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', it /must/ have a
--     format of 'Vulkan.Core10.Enums.Format.FORMAT_R8_UINT'
--
-- -   #VUID-vkCmdBindInvocationMaskHUAWEI-imageView-04979# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', it /must/ have been
--     created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INVOCATION_MASK_BIT_HUAWEI'
--     set
--
-- -   #VUID-vkCmdBindInvocationMaskHUAWEI-imageView-04980# If @imageView@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageLayout@
--     /must/ be 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-vkCmdBindInvocationMaskHUAWEI-width-04981# Thread mask image
--     resolution must match the @width@ and @height@ in
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.cmdTraceRaysKHR'
--
-- -   #VUID-vkCmdBindInvocationMaskHUAWEI-None-04982# Each element in the
--     invocation mask image /must/ have the value @0@ or @1@. The value 1
--     means the invocation is active
--
-- -   #VUID-vkCmdBindInvocationMaskHUAWEI-width-04983# @width@ in
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.cmdTraceRaysKHR'
--     should be 1
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindInvocationMaskHUAWEI-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindInvocationMaskHUAWEI-imageView-parameter# If
--     @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @imageView@ /must/ be a valid 'Vulkan.Core10.Handles.ImageView'
--     handle
--
-- -   #VUID-vkCmdBindInvocationMaskHUAWEI-imageLayout-parameter#
--     @imageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-vkCmdBindInvocationMaskHUAWEI-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindInvocationMaskHUAWEI-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdBindInvocationMaskHUAWEI-renderpass# This command /must/
--     only be called outside of a render pass instance
--
-- -   #VUID-vkCmdBindInvocationMaskHUAWEI-commonparent# Both of
--     @commandBuffer@, and @imageView@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_HUAWEI_invocation_mask VK_HUAWEI_invocation_mask>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Handles.ImageView'
cmdBindInvocationMaskHUAWEI :: forall io
                             . (MonadIO io)
                            => -- | @commandBuffer@ is the command buffer into which the command will be
                               -- recorded
                               CommandBuffer
                            -> -- | @imageView@ is an image view handle specifying the invocation mask image
                               -- @imageView@ /may/ be set to 'Vulkan.Core10.APIConstants.NULL_HANDLE',
                               -- which is equivalent to specifying a view of an image filled with ones
                               -- value.
                               ImageView
                            -> -- | @imageLayout@ is the layout that the image subresources accessible from
                               -- @imageView@ will be in when the invocation mask image is accessed
                               ImageLayout
                            -> io ()
cmdBindInvocationMaskHUAWEI commandBuffer imageView imageLayout = liftIO $ do
  let vkCmdBindInvocationMaskHUAWEIPtr = pVkCmdBindInvocationMaskHUAWEI (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdBindInvocationMaskHUAWEIPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindInvocationMaskHUAWEI is null" Nothing Nothing
  let vkCmdBindInvocationMaskHUAWEI' = mkVkCmdBindInvocationMaskHUAWEI vkCmdBindInvocationMaskHUAWEIPtr
  traceAroundEvent "vkCmdBindInvocationMaskHUAWEI" (vkCmdBindInvocationMaskHUAWEI' (commandBufferHandle (commandBuffer)) (imageView) (imageLayout))
  pure $ ()


-- | VkPhysicalDeviceInvocationMaskFeaturesHUAWEI - Structure describing
-- invocation mask features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceInvocationMaskFeaturesHUAWEI' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceInvocationMaskFeaturesHUAWEI' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_HUAWEI_invocation_mask VK_HUAWEI_invocation_mask>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceInvocationMaskFeaturesHUAWEI = PhysicalDeviceInvocationMaskFeaturesHUAWEI
  { -- | #features-invocationMask# @invocationMask@ indicates that the
    -- implementation supports the use of an invocation mask image to optimize
    -- the ray dispatch.
    invocationMask :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceInvocationMaskFeaturesHUAWEI)
#endif
deriving instance Show PhysicalDeviceInvocationMaskFeaturesHUAWEI

instance ToCStruct PhysicalDeviceInvocationMaskFeaturesHUAWEI where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceInvocationMaskFeaturesHUAWEI{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_INVOCATION_MASK_FEATURES_HUAWEI)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (invocationMask))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_INVOCATION_MASK_FEATURES_HUAWEI)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceInvocationMaskFeaturesHUAWEI where
  peekCStruct p = do
    invocationMask <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceInvocationMaskFeaturesHUAWEI
             (bool32ToBool invocationMask)

instance Storable PhysicalDeviceInvocationMaskFeaturesHUAWEI where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceInvocationMaskFeaturesHUAWEI where
  zero = PhysicalDeviceInvocationMaskFeaturesHUAWEI
           zero


type HUAWEI_INVOCATION_MASK_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_HUAWEI_INVOCATION_MASK_SPEC_VERSION"
pattern HUAWEI_INVOCATION_MASK_SPEC_VERSION :: forall a . Integral a => a
pattern HUAWEI_INVOCATION_MASK_SPEC_VERSION = 1


type HUAWEI_INVOCATION_MASK_EXTENSION_NAME = "VK_HUAWEI_invocation_mask"

-- No documentation found for TopLevel "VK_HUAWEI_INVOCATION_MASK_EXTENSION_NAME"
pattern HUAWEI_INVOCATION_MASK_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern HUAWEI_INVOCATION_MASK_EXTENSION_NAME = "VK_HUAWEI_invocation_mask"

