{-# language CPP #-}
-- | = Name
--
-- VK_KHR_fragment_shading_rate - device extension
--
-- == VK_KHR_fragment_shading_rate
--
-- [__Name String__]
--     @VK_KHR_fragment_shading_rate@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     227
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_create_renderpass2@
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_fragment_shading_rate:%20&body=@tobski%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-05-06
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_fragment_shading_rate.html SPV_KHR_fragment_shading_rate>.
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Guennadi Riguer, AMD
--
--     -   Matthaeus Chajdas, AMD
--
--     -   Pat Brown, Nvidia
--
--     -   Matthew Netsch, Qualcomm
--
--     -   Slawomir Grajewski, Intel
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Jeff Bolz, Nvidia
--
--     -   Contributors to the VK_NV_shading_rate_image specification
--
--     -   Contributors to the VK_EXT_fragment_density_map specification
--
-- == Description
--
-- This extension adds the ability to change the rate at which fragments
-- are shaded. Rather than the usual single fragment invocation for each
-- pixel covered by a primitive, multiple pixels can be shaded by a single
-- fragment shader invocation.
--
-- Up to three methods are available to the application to change the
-- fragment shading rate:
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-pipeline>,
--     which allows the specification of a rate per-draw.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-primitive>,
--     which allows the specification of a rate per primitive, specified
--     during shading.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment>,
--     which allows the specification of a rate per-region of the
--     framebuffer, specified in a specialized image attachment.
--
-- Additionally, these rates can all be specified and combined in order to
-- adjust the overall detail in the image at each point.
--
-- This functionality can be used to focus shading efforts where higher
-- levels of detail are needed in some parts of a scene compared to others.
-- This can be particularly useful in high resolution rendering, or for XR
-- contexts.
--
-- This extension also adds support for the @SPV_KHR_fragment_shading_rate@
-- extension which enables setting the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-primitive primitive fragment shading rate>,
-- and allows querying the final shading rate from a fragment shader.
--
-- == New Commands
--
-- -   'cmdSetFragmentShadingRateKHR'
--
-- -   'getPhysicalDeviceFragmentShadingRatesKHR'
--
-- == New Structures
--
-- -   'PhysicalDeviceFragmentShadingRateKHR'
--
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo':
--
--     -   'PipelineFragmentShadingRateStateCreateInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFragmentShadingRateFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceFragmentShadingRatePropertiesKHR'
--
-- -   Extending
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDescription2':
--
--     -   'FragmentShadingRateAttachmentInfoKHR'
--
-- == New Enums
--
-- -   'FragmentShadingRateCombinerOpKHR'
--
-- == New Enum Constants
--
-- -   'KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME'
--
-- -   'KHR_FRAGMENT_SHADING_RATE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits':
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits':
--
--     -   'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.ImageLayout.ImageLayout':
--
--     -   'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_STATE_CREATE_INFO_KHR'
--
-- == Version History
--
-- -   Revision 1, 2020-05-06 (Tobias Hector)
--
--     -   Initial revision
--
-- = See Also
--
-- 'FragmentShadingRateAttachmentInfoKHR',
-- 'FragmentShadingRateCombinerOpKHR',
-- 'PhysicalDeviceFragmentShadingRateFeaturesKHR',
-- 'PhysicalDeviceFragmentShadingRateKHR',
-- 'PhysicalDeviceFragmentShadingRatePropertiesKHR',
-- 'PipelineFragmentShadingRateStateCreateInfoKHR',
-- 'cmdSetFragmentShadingRateKHR',
-- 'getPhysicalDeviceFragmentShadingRatesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_fragment_shading_rate Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_fragment_shading_rate  ( cmdSetFragmentShadingRateKHR
                                                       , getPhysicalDeviceFragmentShadingRatesKHR
                                                       , FragmentShadingRateAttachmentInfoKHR(..)
                                                       , PipelineFragmentShadingRateStateCreateInfoKHR(..)
                                                       , PhysicalDeviceFragmentShadingRateFeaturesKHR(..)
                                                       , PhysicalDeviceFragmentShadingRatePropertiesKHR(..)
                                                       , PhysicalDeviceFragmentShadingRateKHR(..)
                                                       , FragmentShadingRateCombinerOpKHR( FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR
                                                                                         , FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR
                                                                                         , FRAGMENT_SHADING_RATE_COMBINER_OP_MIN_KHR
                                                                                         , FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_KHR
                                                                                         , FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR
                                                                                         , ..
                                                                                         )
                                                       , KHR_FRAGMENT_SHADING_RATE_SPEC_VERSION
                                                       , pattern KHR_FRAGMENT_SHADING_RATE_SPEC_VERSION
                                                       , KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME
                                                       , pattern KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME
                                                       ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
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
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
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
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (withSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (AttachmentReference2)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetFragmentShadingRateKHR))
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceFragmentShadingRatesKHR))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlags)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_STATE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetFragmentShadingRateKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr Extent2D -> Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR) -> IO ()) -> Ptr CommandBuffer_T -> Ptr Extent2D -> Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR) -> IO ()

-- | vkCmdSetFragmentShadingRateKHR - Set pipeline fragment shading rate
-- dynamically
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-pipelineFragmentShadingRate-04507#
--     If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineFragmentShadingRate pipelineFragmentShadingRate>
--     is not enabled, @pFragmentSize->width@ /must/ be @1@
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-pipelineFragmentShadingRate-04508#
--     If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineFragmentShadingRate pipelineFragmentShadingRate>
--     is not enabled, @pFragmentSize->height@ /must/ be @1@
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-pipelineFragmentShadingRate-04509#
--     One of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineFragmentShadingRate pipelineFragmentShadingRate>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitiveFragmentShadingRate primitiveFragmentShadingRate>,
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     /must/ be enabled
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-primitiveFragmentShadingRate-04510#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#feature-primitiveFragmentShadingRate primitiveFragmentShadingRate feature>
--     is not enabled, @combinerOps@[0] /must/ be
--     'FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR'
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-attachmentFragmentShadingRate-04511#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#feature-attachmentFragmentShadingRate attachmentFragmentShadingRate feature>
--     is not enabled, @combinerOps@[1] /must/ be
--     'FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR'
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-fragmentSizeNonTrivialCombinerOps-04512#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-fragmentShadingRateNonTrivialCombinerOps fragmentSizeNonTrivialCombinerOps>
--     limit is not supported, elements of @combinerOps@ /must/ be either
--     'FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR' or
--     'FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR'
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-pFragmentSize-04513#
--     @pFragmentSize->width@ /must/ be greater than or equal to @1@
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-pFragmentSize-04514#
--     @pFragmentSize->height@ /must/ be greater than or equal to @1@
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-pFragmentSize-04515#
--     @pFragmentSize->width@ /must/ be a power-of-two value
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-pFragmentSize-04516#
--     @pFragmentSize->height@ /must/ be a power-of-two value
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-pFragmentSize-04517#
--     @pFragmentSize->width@ /must/ be less than or equal to @4@
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-pFragmentSize-04518#
--     @pFragmentSize->height@ /must/ be less than or equal to @4@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-pFragmentSize-parameter#
--     @pFragmentSize@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.FundamentalTypes.Extent2D' structure
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-combinerOps-parameter# Any
--     given element of @combinerOps@ /must/ be a valid
--     'FragmentShadingRateCombinerOpKHR' value
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-commandBuffer-cmdpool# The
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'FragmentShadingRateCombinerOpKHR'
cmdSetFragmentShadingRateKHR :: forall io
                              . (MonadIO io)
                             => -- | @commandBuffer@ is the command buffer into which the command will be
                                -- recorded.
                                CommandBuffer
                             -> -- | @pFragmentSize@ specifies the pipeline fragment shading rate for
                                -- subsequent drawing commands.
                                ("fragmentSize" ::: Extent2D)
                             -> -- | @combinerOps@ specifies a 'FragmentShadingRateCombinerOpKHR' determining
                                -- how the
                                -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-pipeline pipeline>,
                                -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-primitive primitive>,
                                -- and
                                -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment attachment shading rates>
                                -- are
                                -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-combining combined>
                                -- for fragments generated by subsequent drawing commands.
                                ("combinerOps" ::: (FragmentShadingRateCombinerOpKHR, FragmentShadingRateCombinerOpKHR))
                             -> io ()
cmdSetFragmentShadingRateKHR commandBuffer fragmentSize combinerOps = liftIO . evalContT $ do
  let vkCmdSetFragmentShadingRateKHRPtr = pVkCmdSetFragmentShadingRateKHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetFragmentShadingRateKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetFragmentShadingRateKHR is null" Nothing Nothing
  let vkCmdSetFragmentShadingRateKHR' = mkVkCmdSetFragmentShadingRateKHR vkCmdSetFragmentShadingRateKHRPtr
  pFragmentSize <- ContT $ withCStruct (fragmentSize)
  pCombinerOps <- ContT $ allocaBytesAligned @(FixedArray 2 FragmentShadingRateCombinerOpKHR) 8 4
  let pCombinerOps' = lowerArrayPtr pCombinerOps
  lift $ case (combinerOps) of
    (e0, e1) -> do
      poke (pCombinerOps' :: Ptr FragmentShadingRateCombinerOpKHR) (e0)
      poke (pCombinerOps' `plusPtr` 4 :: Ptr FragmentShadingRateCombinerOpKHR) (e1)
  lift $ traceAroundEvent "vkCmdSetFragmentShadingRateKHR" (vkCmdSetFragmentShadingRateKHR' (commandBufferHandle (commandBuffer)) pFragmentSize (pCombinerOps))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceFragmentShadingRatesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr PhysicalDeviceFragmentShadingRateKHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr PhysicalDeviceFragmentShadingRateKHR -> IO Result

-- | vkGetPhysicalDeviceFragmentShadingRatesKHR - Get available shading rates
-- for a physical device
--
-- = Description
--
-- If @pFragmentShadingRates@ is @NULL@, then the number of fragment
-- shading rates available is returned in @pFragmentShadingRateCount@.
-- Otherwise, @pFragmentShadingRateCount@ /must/ point to a variable set by
-- the user to the number of elements in the @pFragmentShadingRates@ array,
-- and on return the variable is overwritten with the number of structures
-- actually written to @pFragmentShadingRates@. If
-- @pFragmentShadingRateCount@ is less than the number of fragment shading
-- rates available, at most @pFragmentShadingRateCount@ structures will be
-- written, and 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned
-- instead of 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate that not
-- all the available fragment shading rates were returned.
--
-- The returned array of fragment shading rates /must/ be ordered from
-- largest @fragmentSize.width@ value to smallest, and each set of fragment
-- shading rates with the same @fragmentSize.width@ value /must/ be ordered
-- from largest @fragmentSize.height@ to smallest. Any two entries in the
-- array /must/ not have the same @fragmentSize@ values.
--
-- For any entry in the array, the following rules also apply:
--
-- -   The value of @fragmentSize.width@ /must/ be less than or equal to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxFragmentSize maxFragmentSize.width>.
--
-- -   The value of @fragmentSize.width@ /must/ be greater than or equal to
--     @1@.
--
-- -   The value of @fragmentSize.width@ /must/ be a power-of-two.
--
-- -   The value of @fragmentSize.height@ /must/ be less than or equal to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxFragmentSize maxFragmentSize.height>.
--
-- -   The value of @fragmentSize.height@ /must/ be greater than or equal
--     to @1@.
--
-- -   The value of @fragmentSize.height@ /must/ be a power-of-two.
--
-- -   The highest sample count in @sampleCounts@ /must/ be less than or
--     equal to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxFragmentShadingRateRasterizationSamples maxFragmentShadingRateRasterizationSamples>.
--
-- -   The product of @fragmentSize.width@, @fragmentSize.height@, and the
--     highest sample count in @sampleCounts@ /must/ be less than or equal
--     to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxFragmentShadingRateCoverageSamples maxFragmentShadingRateCoverageSamples>.
--
-- Implementations /must/ support at least the following shading rates:
--
-- +--------------------------------------------------------------+-----------------------------------+
-- | @sampleCounts@                                               | @fragmentSize@                    |
-- +==============================================================+===================================+
-- | 'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT' | {2,2}                             |
-- | |                                                            |                                   |
-- | 'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_4_BIT' |                                   |
-- +--------------------------------------------------------------+-----------------------------------+
-- | 'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT' | {2,1}                             |
-- | |                                                            |                                   |
-- | 'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_4_BIT' |                                   |
-- +--------------------------------------------------------------+-----------------------------------+
-- | ~0                                                           | {1,1}                             |
-- +--------------------------------------------------------------+-----------------------------------+
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-framebufferColorSampleCounts framebufferColorSampleCounts>,
-- includes 'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_2_BIT',
-- the required rates /must/ also include
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_2_BIT'.
--
-- Note
--
-- Including the {1,1} fragment size is done for completeness; it has no
-- actual effect on the support of rendering without setting the fragment
-- size. All sample counts and render pass transforms are supported for
-- this rate.
--
-- The returned set of fragment shading rates /must/ be returned in the
-- native (rotated) coordinate system. For rasterization using render pass
-- @transform@ not equal to
-- 'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR',
-- the application /must/ transform the returned fragment shading rates
-- into the current (unrotated) coordinate system to get the supported
-- rates for that transform.
--
-- Note
--
-- For example, consider an implementation returning support for 4x2, but
-- not 2x4 in the set of supported fragment shading rates. This means that
-- for transforms
-- 'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR'
-- and
-- 'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR',
-- 2x4 is a supported rate, but 4x2 is an unsupported rate.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceFragmentShadingRatesKHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceFragmentShadingRatesKHR-pFragmentShadingRateCount-parameter#
--     @pFragmentShadingRateCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceFragmentShadingRatesKHR-pFragmentShadingRates-parameter#
--     If the value referenced by @pFragmentShadingRateCount@ is not @0@,
--     and @pFragmentShadingRates@ is not @NULL@, @pFragmentShadingRates@
--     /must/ be a valid pointer to an array of @pFragmentShadingRateCount@
--     'PhysicalDeviceFragmentShadingRateKHR' structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'PhysicalDeviceFragmentShadingRateKHR'
getPhysicalDeviceFragmentShadingRatesKHR :: forall io
                                          . (MonadIO io)
                                         => -- | @physicalDevice@ is the handle to the physical device whose properties
                                            -- will be queried.
                                            PhysicalDevice
                                         -> io (Result, ("fragmentShadingRates" ::: Vector PhysicalDeviceFragmentShadingRateKHR))
getPhysicalDeviceFragmentShadingRatesKHR physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceFragmentShadingRatesKHRPtr = pVkGetPhysicalDeviceFragmentShadingRatesKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceFragmentShadingRatesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceFragmentShadingRatesKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceFragmentShadingRatesKHR' = mkVkGetPhysicalDeviceFragmentShadingRatesKHR vkGetPhysicalDeviceFragmentShadingRatesKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPFragmentShadingRateCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceFragmentShadingRatesKHR" (vkGetPhysicalDeviceFragmentShadingRatesKHR' physicalDevice' (pPFragmentShadingRateCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pFragmentShadingRateCount <- lift $ peek @Word32 pPFragmentShadingRateCount
  pPFragmentShadingRates <- ContT $ bracket (callocBytes @PhysicalDeviceFragmentShadingRateKHR ((fromIntegral (pFragmentShadingRateCount)) * 32)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPFragmentShadingRates `advancePtrBytes` (i * 32) :: Ptr PhysicalDeviceFragmentShadingRateKHR) . ($ ())) [0..(fromIntegral (pFragmentShadingRateCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceFragmentShadingRatesKHR" (vkGetPhysicalDeviceFragmentShadingRatesKHR' physicalDevice' (pPFragmentShadingRateCount) ((pPFragmentShadingRates)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pFragmentShadingRateCount' <- lift $ peek @Word32 pPFragmentShadingRateCount
  pFragmentShadingRates' <- lift $ generateM (fromIntegral (pFragmentShadingRateCount')) (\i -> peekCStruct @PhysicalDeviceFragmentShadingRateKHR (((pPFragmentShadingRates) `advancePtrBytes` (32 * (i)) :: Ptr PhysicalDeviceFragmentShadingRateKHR)))
  pure $ ((r'), pFragmentShadingRates')


-- | VkFragmentShadingRateAttachmentInfoKHR - Structure specifying a fragment
-- shading rate attachment for a subpass
--
-- = Description
--
-- If no shading rate attachment is specified, or if this structure is not
-- specified, the implementation behaves as if a valid shading rate
-- attachment was specified with all texels specifying a single pixel per
-- fragment.
--
-- == Valid Usage
--
-- -   #VUID-VkFragmentShadingRateAttachmentInfoKHR-pFragmentShadingRateAttachment-04524#
--     If @pFragmentShadingRateAttachment@ is not @NULL@ and its
--     @attachment@ member is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', its @layout@ member
--     /must/ be equal to
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR'
--
-- -   #VUID-VkFragmentShadingRateAttachmentInfoKHR-pFragmentShadingRateAttachment-04525#
--     If @pFragmentShadingRateAttachment@ is not @NULL@ and its
--     @attachment@ member is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED',
--     @shadingRateAttachmentTexelSize.width@ /must/ be a power of two
--     value
--
-- -   #VUID-VkFragmentShadingRateAttachmentInfoKHR-pFragmentShadingRateAttachment-04526#
--     If @pFragmentShadingRateAttachment@ is not @NULL@ and its
--     @attachment@ member is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED',
--     @shadingRateAttachmentTexelSize.width@ /must/ be less than or equal
--     to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxFragmentShadingRateAttachmentTexelSize maxFragmentShadingRateAttachmentTexelSize.width>
--
-- -   #VUID-VkFragmentShadingRateAttachmentInfoKHR-pFragmentShadingRateAttachment-04527#
--     If @pFragmentShadingRateAttachment@ is not @NULL@ and its
--     @attachment@ member is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED',
--     @shadingRateAttachmentTexelSize.width@ /must/ be greater than or
--     equal to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-minFragmentShadingRateAttachmentTexelSize minFragmentShadingRateAttachmentTexelSize.width>
--
-- -   #VUID-VkFragmentShadingRateAttachmentInfoKHR-pFragmentShadingRateAttachment-04528#
--     If @pFragmentShadingRateAttachment@ is not @NULL@ and its
--     @attachment@ member is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED',
--     @shadingRateAttachmentTexelSize.height@ /must/ be a power of two
--     value
--
-- -   #VUID-VkFragmentShadingRateAttachmentInfoKHR-pFragmentShadingRateAttachment-04529#
--     If @pFragmentShadingRateAttachment@ is not @NULL@ and its
--     @attachment@ member is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED',
--     @shadingRateAttachmentTexelSize.height@ /must/ be less than or equal
--     to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxFragmentShadingRateAttachmentTexelSize maxFragmentShadingRateAttachmentTexelSize.height>
--
-- -   #VUID-VkFragmentShadingRateAttachmentInfoKHR-pFragmentShadingRateAttachment-04530#
--     If @pFragmentShadingRateAttachment@ is not @NULL@ and its
--     @attachment@ member is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED',
--     @shadingRateAttachmentTexelSize.height@ /must/ be greater than or
--     equal to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-minFragmentShadingRateAttachmentTexelSize minFragmentShadingRateAttachmentTexelSize.height>
--
-- -   #VUID-VkFragmentShadingRateAttachmentInfoKHR-pFragmentShadingRateAttachment-04531#
--     If @pFragmentShadingRateAttachment@ is not @NULL@ and its
--     @attachment@ member is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', the quotient of
--     @shadingRateAttachmentTexelSize.width@ and
--     @shadingRateAttachmentTexelSize.height@ /must/ be less than or equal
--     to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxFragmentShadingRateAttachmentTexelSizeAspectRatio maxFragmentShadingRateAttachmentTexelSizeAspectRatio>
--
-- -   #VUID-VkFragmentShadingRateAttachmentInfoKHR-pFragmentShadingRateAttachment-04532#
--     If @pFragmentShadingRateAttachment@ is not @NULL@ and its
--     @attachment@ member is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', the quotient of
--     @shadingRateAttachmentTexelSize.height@ and
--     @shadingRateAttachmentTexelSize.width@ /must/ be less than or equal
--     to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxFragmentShadingRateAttachmentTexelSizeAspectRatio maxFragmentShadingRateAttachmentTexelSizeAspectRatio>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkFragmentShadingRateAttachmentInfoKHR-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR'
--
-- -   #VUID-VkFragmentShadingRateAttachmentInfoKHR-pFragmentShadingRateAttachment-parameter#
--     If @pFragmentShadingRateAttachment@ is not @NULL@,
--     @pFragmentShadingRateAttachment@ /must/ be a valid pointer to a
--     valid
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.AttachmentReference2'
--     structure
--
-- = See Also
--
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.AttachmentReference2',
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data FragmentShadingRateAttachmentInfoKHR = FragmentShadingRateAttachmentInfoKHR
  { -- | @pFragmentShadingRateAttachment@ is @NULL@ or a pointer to a
    -- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.AttachmentReference2'
    -- structure defining the fragment shading rate attachment for this
    -- subpass.
    fragmentShadingRateAttachment :: Maybe (SomeStruct AttachmentReference2)
  , -- | @shadingRateAttachmentTexelSize@ specifies the size of the portion of
    -- the framebuffer corresponding to each texel in
    -- @pFragmentShadingRateAttachment@.
    shadingRateAttachmentTexelSize :: Extent2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FragmentShadingRateAttachmentInfoKHR)
#endif
deriving instance Show FragmentShadingRateAttachmentInfoKHR

instance ToCStruct FragmentShadingRateAttachmentInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FragmentShadingRateAttachmentInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pFragmentShadingRateAttachment'' <- case (fragmentShadingRateAttachment) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (AttachmentReference2 '[])) $ \cont -> withSomeCStruct @AttachmentReference2 (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (AttachmentReference2 _)))) pFragmentShadingRateAttachment''
    lift $ poke ((p `plusPtr` 24 :: Ptr Extent2D)) (shadingRateAttachmentTexelSize)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (zero)
    f

instance FromCStruct FragmentShadingRateAttachmentInfoKHR where
  peekCStruct p = do
    pFragmentShadingRateAttachment <- peek @(Ptr (AttachmentReference2 _)) ((p `plusPtr` 16 :: Ptr (Ptr (AttachmentReference2 _))))
    pFragmentShadingRateAttachment' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pFragmentShadingRateAttachment
    shadingRateAttachmentTexelSize <- peekCStruct @Extent2D ((p `plusPtr` 24 :: Ptr Extent2D))
    pure $ FragmentShadingRateAttachmentInfoKHR
             pFragmentShadingRateAttachment' shadingRateAttachmentTexelSize

instance Zero FragmentShadingRateAttachmentInfoKHR where
  zero = FragmentShadingRateAttachmentInfoKHR
           Nothing
           zero


-- | VkPipelineFragmentShadingRateStateCreateInfoKHR - Structure specifying
-- parameters controlling the fragment shading rate
--
-- = Description
--
-- If the @pNext@ chain of
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' includes a
-- 'PipelineFragmentShadingRateStateCreateInfoKHR' structure, then that
-- structure includes parameters that control the pipeline fragment shading
-- rate.
--
-- If this structure is not present, @fragmentSize@ is considered to be
-- equal to (1,1), and both elements of @combinerOps@ are considered to be
-- equal to 'FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineFragmentShadingRateStateCreateInfoKHR-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_STATE_CREATE_INFO_KHR'
--
-- -   #VUID-VkPipelineFragmentShadingRateStateCreateInfoKHR-combinerOps-parameter#
--     Any given element of @combinerOps@ /must/ be a valid
--     'FragmentShadingRateCombinerOpKHR' value
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'FragmentShadingRateCombinerOpKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineFragmentShadingRateStateCreateInfoKHR = PipelineFragmentShadingRateStateCreateInfoKHR
  { -- | @fragmentSize@ specifies a 'Vulkan.Core10.FundamentalTypes.Extent2D'
    -- structure containing the fragment size used to define the pipeline
    -- fragment shading rate for drawing commands using this pipeline.
    fragmentSize :: Extent2D
  , -- | @combinerOps@ specifies a 'FragmentShadingRateCombinerOpKHR' value
    -- determining how the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-pipeline pipeline>,
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-primitive primitive>,
    -- and
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment attachment shading rates>
    -- are
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-combining combined>
    -- for fragments generated by drawing commands using the created pipeline.
    combinerOps :: (FragmentShadingRateCombinerOpKHR, FragmentShadingRateCombinerOpKHR)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineFragmentShadingRateStateCreateInfoKHR)
#endif
deriving instance Show PipelineFragmentShadingRateStateCreateInfoKHR

instance ToCStruct PipelineFragmentShadingRateStateCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineFragmentShadingRateStateCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_STATE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (fragmentSize)
    let pCombinerOps' = lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR)))
    case (combinerOps) of
      (e0, e1) -> do
        poke (pCombinerOps' :: Ptr FragmentShadingRateCombinerOpKHR) (e0)
        poke (pCombinerOps' `plusPtr` 4 :: Ptr FragmentShadingRateCombinerOpKHR) (e1)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_STATE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (zero)
    let pCombinerOps' = lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR)))
    case ((zero, zero)) of
      (e0, e1) -> do
        poke (pCombinerOps' :: Ptr FragmentShadingRateCombinerOpKHR) (e0)
        poke (pCombinerOps' `plusPtr` 4 :: Ptr FragmentShadingRateCombinerOpKHR) (e1)
    f

instance FromCStruct PipelineFragmentShadingRateStateCreateInfoKHR where
  peekCStruct p = do
    fragmentSize <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    let pcombinerOps = lowerArrayPtr @FragmentShadingRateCombinerOpKHR ((p `plusPtr` 24 :: Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR)))
    combinerOps0 <- peek @FragmentShadingRateCombinerOpKHR ((pcombinerOps `advancePtrBytes` 0 :: Ptr FragmentShadingRateCombinerOpKHR))
    combinerOps1 <- peek @FragmentShadingRateCombinerOpKHR ((pcombinerOps `advancePtrBytes` 4 :: Ptr FragmentShadingRateCombinerOpKHR))
    pure $ PipelineFragmentShadingRateStateCreateInfoKHR
             fragmentSize ((combinerOps0, combinerOps1))

instance Storable PipelineFragmentShadingRateStateCreateInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineFragmentShadingRateStateCreateInfoKHR where
  zero = PipelineFragmentShadingRateStateCreateInfoKHR
           zero
           (zero, zero)


-- | VkPhysicalDeviceFragmentShadingRateFeaturesKHR - Structure indicating
-- support for variable rate fragment shading
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceFragmentShadingRateFeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceFragmentShadingRateFeaturesKHR' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentShadingRateFeaturesKHR = PhysicalDeviceFragmentShadingRateFeaturesKHR
  { -- | #features-pipelineFragmentShadingRate# @pipelineFragmentShadingRate@
    -- indicates that the implementation supports the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-pipeline pipeline fragment shading rate>.
    pipelineFragmentShadingRate :: Bool
  , -- | #features-primitiveFragmentShadingRate# @primitiveFragmentShadingRate@
    -- indicates that the implementation supports the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-primitive primitive fragment shading rate>.
    primitiveFragmentShadingRate :: Bool
  , -- | #features-attachmentFragmentShadingRate# @attachmentFragmentShadingRate@
    -- indicates that the implementation supports the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment attachment fragment shading rate>.
    attachmentFragmentShadingRate :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentShadingRateFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceFragmentShadingRateFeaturesKHR

instance ToCStruct PhysicalDeviceFragmentShadingRateFeaturesKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentShadingRateFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (pipelineFragmentShadingRate))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (primitiveFragmentShadingRate))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (attachmentFragmentShadingRate))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFragmentShadingRateFeaturesKHR where
  peekCStruct p = do
    pipelineFragmentShadingRate <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    primitiveFragmentShadingRate <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    attachmentFragmentShadingRate <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceFragmentShadingRateFeaturesKHR
             (bool32ToBool pipelineFragmentShadingRate) (bool32ToBool primitiveFragmentShadingRate) (bool32ToBool attachmentFragmentShadingRate)

instance Storable PhysicalDeviceFragmentShadingRateFeaturesKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentShadingRateFeaturesKHR where
  zero = PhysicalDeviceFragmentShadingRateFeaturesKHR
           zero
           zero
           zero


-- | VkPhysicalDeviceFragmentShadingRatePropertiesKHR - Structure describing
-- variable fragment shading rate limits that can be supported by an
-- implementation
--
-- = Description
--
-- Note
--
-- Multiplication of the combiner rates using the fragment width\/height in
-- linear space is equivalent to an addition of those values in log2 space.
-- Some implementations inadvertently implemented an addition in linear
-- space due to unclear requirements originating outside of this
-- specification. This resulted in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-fragmentShadingRateStrictMultiplyCombiner fragmentShadingRateStrictMultiplyCombiner>
-- being added. Fortunately, this only affects situations where a rate of 1
-- in either dimension is combined with another rate of 1. All other
-- combinations result in the exact same result as if multiplication was
-- performed in linear space due to the clamping logic, and the fact that
-- both the sum and product of 2 and 2 are equal. In many cases, this limit
-- will not affect the correct operation of applications.
--
-- If the 'PhysicalDeviceFragmentShadingRatePropertiesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- These properties are related to
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-primsrast-fragment-shading-rate fragment shading rates>.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentShadingRatePropertiesKHR = PhysicalDeviceFragmentShadingRatePropertiesKHR
  { -- | #limits-minFragmentShadingRateAttachmentTexelSize#
    -- @minFragmentShadingRateAttachmentTexelSize@ indicates minimum supported
    -- width and height of the portion of the framebuffer corresponding to each
    -- texel in a fragment shading rate attachment. Each value /must/ be less
    -- than or equal to the values in
    -- @maxFragmentShadingRateAttachmentTexelSize@. Each value /must/ be a
    -- power-of-two. It /must/ be (0,0) if the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
    -- feature is not supported.
    minFragmentShadingRateAttachmentTexelSize :: Extent2D
  , -- | #limits-maxFragmentShadingRateAttachmentTexelSize#
    -- @maxFragmentShadingRateAttachmentTexelSize@ indicates maximum supported
    -- width and height of the portion of the framebuffer corresponding to each
    -- texel in a fragment shading rate attachment. Each value /must/ be
    -- greater than or equal to the values in
    -- @minFragmentShadingRateAttachmentTexelSize@. Each value /must/ be a
    -- power-of-two. It /must/ be (0,0) if the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
    -- feature is not supported.
    maxFragmentShadingRateAttachmentTexelSize :: Extent2D
  , -- | #limits-maxFragmentShadingRateAttachmentTexelSizeAspectRatio#
    -- @maxFragmentShadingRateAttachmentTexelSizeAspectRatio@ indicates the
    -- maximum ratio between the width and height of the portion of the
    -- framebuffer corresponding to each texel in a fragment shading rate
    -- attachment. @maxFragmentShadingRateAttachmentTexelSizeAspectRatio@
    -- /must/ be a power-of-two value, and /must/ be less than or equal to
    -- max(@maxFragmentShadingRateAttachmentTexelSize.width@ \/
    -- minFragmentShadingRateAttachmentTexelSize.height,
    -- @maxFragmentShadingRateAttachmentTexelSize.height@ \/
    -- minFragmentShadingRateAttachmentTexelSize.width). It /must/ be 0 if the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
    -- feature is not supported.
    maxFragmentShadingRateAttachmentTexelSizeAspectRatio :: Word32
  , -- | #limits-primitiveFragmentShadingRateWithMultipleViewports#
    -- @primitiveFragmentShadingRateWithMultipleViewports@ specifies whether
    -- the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-primitive primitive fragment shading rate>
    -- /can/ be used when multiple viewports are used. If this value is
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', only a single viewport /must/ be
    -- used, and applications /must/ not write to the @ViewportMaskNV@ or
    -- @ViewportIndex@ built-in when setting @PrimitiveShadingRateKHR@. It
    -- /must/ be 'Vulkan.Core10.FundamentalTypes.FALSE' if the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderOutputViewportIndex shaderOutputViewportIndex>
    -- feature, the
    -- <VK_EXT_shader_viewport_index_layer.html VK_EXT_shader_viewport_index_layer>
    -- extension, or the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
    -- feature is not supported, or if the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitiveFragmentShadingRate primitiveFragmentShadingRate>
    -- feature is not supported.
    primitiveFragmentShadingRateWithMultipleViewports :: Bool
  , -- | #limits-layeredShadingRateAttachments# @layeredShadingRateAttachments@
    -- specifies whether a shading rate attachment image view /can/ be created
    -- with multiple layers. If this value is
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', when creating an image view with
    -- a @usage@ that includes
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR',
    -- @layerCount@ /must/ be @1@. It /must/ be
    -- 'Vulkan.Core10.FundamentalTypes.FALSE' if the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiview multiview>
    -- feature, the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderOutputViewportIndex shaderOutputViewportIndex>
    -- feature, the
    -- <VK_EXT_shader_viewport_index_layer.html VK_EXT_shader_viewport_index_layer>
    -- extension, or the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
    -- feature is not supported, or if the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
    -- feature is not supported.
    layeredShadingRateAttachments :: Bool
  , -- | #limits-fragmentShadingRateNonTrivialCombinerOps#
    -- @fragmentShadingRateNonTrivialCombinerOps@ specifies whether
    -- 'FragmentShadingRateCombinerOpKHR' enums other than
    -- 'FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR' or
    -- 'FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR' /can/ be used. It /must/
    -- be 'Vulkan.Core10.FundamentalTypes.FALSE' unless either the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitiveFragmentShadingRate primitiveFragmentShadingRate>
    -- or
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
    -- feature is supported.
    fragmentShadingRateNonTrivialCombinerOps :: Bool
  , -- | #limits-maxFragmentSize# @maxFragmentSize@ indicates the maximum
    -- supported width and height of a fragment. Its @width@ and @height@
    -- members /must/ both be power-of-two values. This limit is purely
    -- informational, and is not validated.
    maxFragmentSize :: Extent2D
  , -- | #limits-maxFragmentSizeAspectRatio# @maxFragmentSizeAspectRatio@
    -- indicates the maximum ratio between the width and height of a fragment.
    -- @maxFragmentSizeAspectRatio@ /must/ be a power-of-two value, and /must/
    -- be less than or equal to the maximum of the @width@ and @height@ members
    -- of @maxFragmentSize@. This limit is purely informational, and is not
    -- validated.
    maxFragmentSizeAspectRatio :: Word32
  , -- | #limits-maxFragmentShadingRateCoverageSamples#
    -- @maxFragmentShadingRateCoverageSamples@ specifies the maximum number of
    -- coverage samples supported in a single fragment.
    -- @maxFragmentShadingRateCoverageSamples@ /must/ be less than or equal to
    -- the product of the @width@ and @height@ members of @maxFragmentSize@,
    -- and the sample count reported by
    -- @maxFragmentShadingRateRasterizationSamples@.
    -- @maxFragmentShadingRateCoverageSamples@ /must/ be less than or equal to
    -- @maxSampleMaskWords@ * 32 if @fragmentShadingRateWithShaderSampleMask@
    -- is supported. This limit is purely informational, and is not validated.
    maxFragmentShadingRateCoverageSamples :: Word32
  , -- | #limits-maxFragmentShadingRateRasterizationSamples#
    -- @maxFragmentShadingRateRasterizationSamples@ is a
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
    -- specifying the maximum sample rate supported when a fragment covers
    -- multiple pixels. This limit is purely informational, and is not
    -- validated.
    maxFragmentShadingRateRasterizationSamples :: SampleCountFlagBits
  , -- | #limits-fragmentShadingRateWithShaderDepthStencilWrites#
    -- @fragmentShadingRateWithShaderDepthStencilWrites@ specifies whether the
    -- implementation supports writing @FragDepth@ or @FragStencilRefEXT@ from
    -- a fragment shader for multi-pixel fragments. If this value is
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', writing to those built-ins will
    -- clamp the fragment shading rate to (1,1).
    fragmentShadingRateWithShaderDepthStencilWrites :: Bool
  , -- | #limits-fragmentShadingRateWithSampleMask#
    -- @fragmentShadingRateWithSampleMask@ specifies whether the the
    -- implementation supports setting valid bits of
    -- 'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@pSampleMask@
    -- to @0@ for multi-pixel fragments. If this value is
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', zeroing valid bits in the sample
    -- mask will clamp the fragment shading rate to (1,1).
    fragmentShadingRateWithSampleMask :: Bool
  , -- | #limits-fragmentShadingRateWithShaderSampleMask#
    -- @fragmentShadingRateWithShaderSampleMask@ specifies whether the
    -- implementation supports reading or writing
    -- 'Vulkan.Core10.FundamentalTypes.SampleMask' for multi-pixel fragments.
    -- If this value is 'Vulkan.Core10.FundamentalTypes.FALSE', using that
    -- built-in will clamp the fragment shading rate to (1,1).
    fragmentShadingRateWithShaderSampleMask :: Bool
  , -- | #limits-fragmentShadingRateWithConservativeRasterization#
    -- @fragmentShadingRateWithConservativeRasterization@ specifies whether
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-conservativeraster conservative rasterization>
    -- is supported for multi-pixel fragments. It /must/ be
    -- 'Vulkan.Core10.FundamentalTypes.FALSE' if
    -- <VK_EXT_conservative_rasterization.html VK_EXT_conservative_rasterization>
    -- is not supported. If this value is
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', using
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-conservativeraster conservative rasterization>
    -- will clamp the fragment shading rate to (1,1).
    fragmentShadingRateWithConservativeRasterization :: Bool
  , -- | #limits-fragmentShadingRateWithFragmentShaderInterlock#
    -- @fragmentShadingRateWithFragmentShaderInterlock@ specifies whether
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-fragment-shader-interlock fragment shader interlock>
    -- is supported for multi-pixel fragments. It /must/ be
    -- 'Vulkan.Core10.FundamentalTypes.FALSE' if
    -- <VK_EXT_fragment_shader_interlock.html VK_EXT_fragment_shader_interlock>
    -- is not supported. If this value is
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', using
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-fragment-shader-interlock fragment shader interlock>
    -- will clamp the fragment shading rate to (1,1).
    fragmentShadingRateWithFragmentShaderInterlock :: Bool
  , -- | #limits-fragmentShadingRateWithCustomSampleLocations#
    -- @fragmentShadingRateWithCustomSampleLocations@ specifies whether
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primrast-samplelocations custom sample locations>
    -- are supported for multi-pixel fragments. It /must/ be
    -- 'Vulkan.Core10.FundamentalTypes.FALSE' if
    -- <VK_EXT_sample_locations.html VK_EXT_sample_locations> is not supported.
    -- If this value is 'Vulkan.Core10.FundamentalTypes.FALSE', using
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primrast-samplelocations custom sample locations>
    -- will clamp the fragment shading rate to (1,1).
    fragmentShadingRateWithCustomSampleLocations :: Bool
  , -- | #limits-fragmentShadingRateStrictMultiplyCombiner#
    -- @fragmentShadingRateStrictMultiplyCombiner@ specifies whether
    -- VK_FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR accurately performs a
    -- multiplication or not. Implementations where this value is
    -- 'Vulkan.Core10.FundamentalTypes.FALSE' will instead combine rates with
    -- an addition. If @fragmentShadingRateNonTrivialCombinerOps@ is
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', implementations /must/ report
    -- this as 'Vulkan.Core10.FundamentalTypes.FALSE'. If
    -- @fragmentShadingRateNonTrivialCombinerOps@ is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE', implementations /should/ report
    -- this as 'Vulkan.Core10.FundamentalTypes.TRUE'.
    fragmentShadingRateStrictMultiplyCombiner :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentShadingRatePropertiesKHR)
#endif
deriving instance Show PhysicalDeviceFragmentShadingRatePropertiesKHR

instance ToCStruct PhysicalDeviceFragmentShadingRatePropertiesKHR where
  withCStruct x f = allocaBytesAligned 96 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentShadingRatePropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (minFragmentShadingRateAttachmentTexelSize)
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (maxFragmentShadingRateAttachmentTexelSize)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (maxFragmentShadingRateAttachmentTexelSizeAspectRatio)
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (primitiveFragmentShadingRateWithMultipleViewports))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (layeredShadingRateAttachments))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateNonTrivialCombinerOps))
    poke ((p `plusPtr` 48 :: Ptr Extent2D)) (maxFragmentSize)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (maxFragmentSizeAspectRatio)
    poke ((p `plusPtr` 60 :: Ptr Word32)) (maxFragmentShadingRateCoverageSamples)
    poke ((p `plusPtr` 64 :: Ptr SampleCountFlagBits)) (maxFragmentShadingRateRasterizationSamples)
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateWithShaderDepthStencilWrites))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateWithSampleMask))
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateWithShaderSampleMask))
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateWithConservativeRasterization))
    poke ((p `plusPtr` 84 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateWithFragmentShaderInterlock))
    poke ((p `plusPtr` 88 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateWithCustomSampleLocations))
    poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateStrictMultiplyCombiner))
    f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 64 :: Ptr SampleCountFlagBits)) (zero)
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 84 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 88 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFragmentShadingRatePropertiesKHR where
  peekCStruct p = do
    minFragmentShadingRateAttachmentTexelSize <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    maxFragmentShadingRateAttachmentTexelSize <- peekCStruct @Extent2D ((p `plusPtr` 24 :: Ptr Extent2D))
    maxFragmentShadingRateAttachmentTexelSizeAspectRatio <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    primitiveFragmentShadingRateWithMultipleViewports <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    layeredShadingRateAttachments <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    fragmentShadingRateNonTrivialCombinerOps <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    maxFragmentSize <- peekCStruct @Extent2D ((p `plusPtr` 48 :: Ptr Extent2D))
    maxFragmentSizeAspectRatio <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    maxFragmentShadingRateCoverageSamples <- peek @Word32 ((p `plusPtr` 60 :: Ptr Word32))
    maxFragmentShadingRateRasterizationSamples <- peek @SampleCountFlagBits ((p `plusPtr` 64 :: Ptr SampleCountFlagBits))
    fragmentShadingRateWithShaderDepthStencilWrites <- peek @Bool32 ((p `plusPtr` 68 :: Ptr Bool32))
    fragmentShadingRateWithSampleMask <- peek @Bool32 ((p `plusPtr` 72 :: Ptr Bool32))
    fragmentShadingRateWithShaderSampleMask <- peek @Bool32 ((p `plusPtr` 76 :: Ptr Bool32))
    fragmentShadingRateWithConservativeRasterization <- peek @Bool32 ((p `plusPtr` 80 :: Ptr Bool32))
    fragmentShadingRateWithFragmentShaderInterlock <- peek @Bool32 ((p `plusPtr` 84 :: Ptr Bool32))
    fragmentShadingRateWithCustomSampleLocations <- peek @Bool32 ((p `plusPtr` 88 :: Ptr Bool32))
    fragmentShadingRateStrictMultiplyCombiner <- peek @Bool32 ((p `plusPtr` 92 :: Ptr Bool32))
    pure $ PhysicalDeviceFragmentShadingRatePropertiesKHR
             minFragmentShadingRateAttachmentTexelSize maxFragmentShadingRateAttachmentTexelSize maxFragmentShadingRateAttachmentTexelSizeAspectRatio (bool32ToBool primitiveFragmentShadingRateWithMultipleViewports) (bool32ToBool layeredShadingRateAttachments) (bool32ToBool fragmentShadingRateNonTrivialCombinerOps) maxFragmentSize maxFragmentSizeAspectRatio maxFragmentShadingRateCoverageSamples maxFragmentShadingRateRasterizationSamples (bool32ToBool fragmentShadingRateWithShaderDepthStencilWrites) (bool32ToBool fragmentShadingRateWithSampleMask) (bool32ToBool fragmentShadingRateWithShaderSampleMask) (bool32ToBool fragmentShadingRateWithConservativeRasterization) (bool32ToBool fragmentShadingRateWithFragmentShaderInterlock) (bool32ToBool fragmentShadingRateWithCustomSampleLocations) (bool32ToBool fragmentShadingRateStrictMultiplyCombiner)

instance Storable PhysicalDeviceFragmentShadingRatePropertiesKHR where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentShadingRatePropertiesKHR where
  zero = PhysicalDeviceFragmentShadingRatePropertiesKHR
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
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceFragmentShadingRateKHR - Structure returning information
-- about sample count specific additional multisampling capabilities
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceFragmentShadingRatesKHR'
data PhysicalDeviceFragmentShadingRateKHR = PhysicalDeviceFragmentShadingRateKHR
  { -- | @sampleCounts@ is a bitmask of sample counts for which the shading rate
    -- described by @fragmentSize@ is supported.
    sampleCounts :: SampleCountFlags
  , -- | @fragmentSize@ is a 'Vulkan.Core10.FundamentalTypes.Extent2D' describing
    -- the width and height of a supported shading rate.
    fragmentSize :: Extent2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentShadingRateKHR)
#endif
deriving instance Show PhysicalDeviceFragmentShadingRateKHR

instance ToCStruct PhysicalDeviceFragmentShadingRateKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentShadingRateKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SampleCountFlags)) (sampleCounts)
    poke ((p `plusPtr` 20 :: Ptr Extent2D)) (fragmentSize)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SampleCountFlags)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Extent2D)) (zero)
    f

instance FromCStruct PhysicalDeviceFragmentShadingRateKHR where
  peekCStruct p = do
    sampleCounts <- peek @SampleCountFlags ((p `plusPtr` 16 :: Ptr SampleCountFlags))
    fragmentSize <- peekCStruct @Extent2D ((p `plusPtr` 20 :: Ptr Extent2D))
    pure $ PhysicalDeviceFragmentShadingRateKHR
             sampleCounts fragmentSize

instance Storable PhysicalDeviceFragmentShadingRateKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentShadingRateKHR where
  zero = PhysicalDeviceFragmentShadingRateKHR
           zero
           zero


-- | VkFragmentShadingRateCombinerOpKHR - Control how fragment shading rates
-- are combined
--
-- = Description
--
-- where combine(Axy,Bxy) is the combine operation, and Axy and Bxy are the
-- inputs to the operation.
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-fragmentShadingRateStrictMultiplyCombiner fragmentShadingRateStrictMultiplyCombiner>
-- is 'Vulkan.Core10.FundamentalTypes.FALSE', using
-- 'FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR' with values of 1 for both A
-- and B in the same dimension results in the value 2 being produced for
-- that dimension. See the definition of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-fragmentShadingRateStrictMultiplyCombiner fragmentShadingRateStrictMultiplyCombiner>
-- for more information.
--
-- These operations are performed in a component-wise fashion.
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PipelineFragmentShadingRateEnumStateCreateInfoNV',
-- 'PipelineFragmentShadingRateStateCreateInfoKHR',
-- 'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.cmdSetFragmentShadingRateEnumNV',
-- 'cmdSetFragmentShadingRateKHR'
newtype FragmentShadingRateCombinerOpKHR = FragmentShadingRateCombinerOpKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR' specifies a combiner
-- operation of combine(Axy,Bxy) = Axy.
pattern FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR    = FragmentShadingRateCombinerOpKHR 0
-- | 'FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR' specifies a combiner
-- operation of combine(Axy,Bxy) = Bxy.
pattern FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR = FragmentShadingRateCombinerOpKHR 1
-- | 'FRAGMENT_SHADING_RATE_COMBINER_OP_MIN_KHR' specifies a combiner
-- operation of combine(Axy,Bxy) = min(Axy,Bxy).
pattern FRAGMENT_SHADING_RATE_COMBINER_OP_MIN_KHR     = FragmentShadingRateCombinerOpKHR 2
-- | 'FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_KHR' specifies a combiner
-- operation of combine(Axy,Bxy) = max(Axy,Bxy).
pattern FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_KHR     = FragmentShadingRateCombinerOpKHR 3
-- | 'FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR' specifies a combiner
-- operation of combine(Axy,Bxy) = Axy*Bxy.
pattern FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR     = FragmentShadingRateCombinerOpKHR 4
{-# complete FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR,
             FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR,
             FRAGMENT_SHADING_RATE_COMBINER_OP_MIN_KHR,
             FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_KHR,
             FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR :: FragmentShadingRateCombinerOpKHR #-}

conNameFragmentShadingRateCombinerOpKHR :: String
conNameFragmentShadingRateCombinerOpKHR = "FragmentShadingRateCombinerOpKHR"

enumPrefixFragmentShadingRateCombinerOpKHR :: String
enumPrefixFragmentShadingRateCombinerOpKHR = "FRAGMENT_SHADING_RATE_COMBINER_OP_"

showTableFragmentShadingRateCombinerOpKHR :: [(FragmentShadingRateCombinerOpKHR, String)]
showTableFragmentShadingRateCombinerOpKHR =
  [ (FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR   , "KEEP_KHR")
  , (FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR, "REPLACE_KHR")
  , (FRAGMENT_SHADING_RATE_COMBINER_OP_MIN_KHR    , "MIN_KHR")
  , (FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_KHR    , "MAX_KHR")
  , (FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR    , "MUL_KHR")
  ]

instance Show FragmentShadingRateCombinerOpKHR where
  showsPrec = enumShowsPrec enumPrefixFragmentShadingRateCombinerOpKHR
                            showTableFragmentShadingRateCombinerOpKHR
                            conNameFragmentShadingRateCombinerOpKHR
                            (\(FragmentShadingRateCombinerOpKHR x) -> x)
                            (showsPrec 11)

instance Read FragmentShadingRateCombinerOpKHR where
  readPrec = enumReadPrec enumPrefixFragmentShadingRateCombinerOpKHR
                          showTableFragmentShadingRateCombinerOpKHR
                          conNameFragmentShadingRateCombinerOpKHR
                          FragmentShadingRateCombinerOpKHR


type KHR_FRAGMENT_SHADING_RATE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_FRAGMENT_SHADING_RATE_SPEC_VERSION"
pattern KHR_FRAGMENT_SHADING_RATE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_FRAGMENT_SHADING_RATE_SPEC_VERSION = 1


type KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME = "VK_KHR_fragment_shading_rate"

-- No documentation found for TopLevel "VK_KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME"
pattern KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME = "VK_KHR_fragment_shading_rate"

