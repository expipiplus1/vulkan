{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_fragment_shading_rate  ( cmdSetFragmentShadingRateKHR
                                                       , getPhysicalDeviceFragmentShadingRatesKHR
                                                       , pattern IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR
                                                       , pattern ACCESS_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR
                                                       , pattern IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
                                                       , pattern PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
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
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
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
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
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
import Data.Word (Word32)
import Text.Read.Lex (Lexeme(Ident))
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceFragmentShadingRatesKHR))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlags)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlagBits(ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV))
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlagBits(IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV))
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
--     is not enabled, @pFragmentSize@->@width@ /must/ be @1@
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-pipelineFragmentShadingRate-04508#
--     If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineFragmentShadingRate pipelineFragmentShadingRate>
--     is not enabled, @pFragmentSize@->@height@ /must/ be @1@
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
--     @pFragmentSize@->@width@ /must/ be greater than or equal to @1@
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-pFragmentSize-04514#
--     @pFragmentSize@->@height@ /must/ be greater than or equal to @1@
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-pFragmentSize-04515#
--     @pFragmentSize@->@width@ /must/ be a power-of-two value
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-pFragmentSize-04516#
--     @pFragmentSize@->@height@ /must/ be a power-of-two value
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-pFragmentSize-04517#
--     @pFragmentSize@->@width@ /must/ be less than or equal to @4@
--
-- -   #VUID-vkCmdSetFragmentShadingRateKHR-pFragmentSize-04518#
--     @pFragmentSize@->@height@ /must/ be less than or equal to @4@
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
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'FragmentShadingRateCombinerOpKHR'
cmdSetFragmentShadingRateKHR :: forall io
                              . (MonadIO io)
                             => -- | @commandBuffer@ is the command buffer into which the command will be
                                -- recorded.
                                CommandBuffer
                             -> -- | @pFragmentSize@ specifies the pipeline fragment shading rate for
                                -- subsequent draw commands.
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
  lift $ vkCmdSetFragmentShadingRateKHR' (commandBufferHandle (commandBuffer)) pFragmentSize (pCombinerOps)
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
-- size. All sample counts are supported for this rate.
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
  r <- lift $ vkGetPhysicalDeviceFragmentShadingRatesKHR' physicalDevice' (pPFragmentShadingRateCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pFragmentShadingRateCount <- lift $ peek @Word32 pPFragmentShadingRateCount
  pPFragmentShadingRates <- ContT $ bracket (callocBytes @PhysicalDeviceFragmentShadingRateKHR ((fromIntegral (pFragmentShadingRateCount)) * 32)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPFragmentShadingRates `advancePtrBytes` (i * 32) :: Ptr PhysicalDeviceFragmentShadingRateKHR) . ($ ())) [0..(fromIntegral (pFragmentShadingRateCount)) - 1]
  r' <- lift $ vkGetPhysicalDeviceFragmentShadingRatesKHR' physicalDevice' (pPFragmentShadingRateCount) ((pPFragmentShadingRates))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pFragmentShadingRateCount' <- lift $ peek @Word32 pPFragmentShadingRateCount
  pFragmentShadingRates' <- lift $ generateM (fromIntegral (pFragmentShadingRateCount')) (\i -> peekCStruct @PhysicalDeviceFragmentShadingRateKHR (((pPFragmentShadingRates) `advancePtrBytes` (32 * (i)) :: Ptr PhysicalDeviceFragmentShadingRateKHR)))
  pure $ ((r'), pFragmentShadingRates')


-- No documentation found for TopLevel "VK_IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR"
pattern IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR = IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV


-- No documentation found for TopLevel "VK_ACCESS_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR"
pattern ACCESS_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR = ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV


-- No documentation found for TopLevel "VK_IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR"
pattern IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR = IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR"
pattern PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR = PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV


-- | VkFragmentShadingRateAttachmentInfoKHR - Structure specifying a fragment
-- shading rate attachment for a subpass
--
-- = Description
--
-- If no shading rate attachment is specified, or if this structure isn’t
-- specified, the implementation behaves as if a valid shading rate
-- attachment was specified with all texels specifying a single pixel per
-- fragment.
--
-- == Valid Usage
--
-- -   #VUID-VkFragmentShadingRateAttachmentInfoKHR-pFragmentShadingRateAttachment-04523#
--     If @pFragmentShadingRateAttachment@ is not @NULL@ and its
--     @attachment@ member is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' then it /must/ have a
--     format whose features contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-VkFragmentShadingRateAttachmentInfoKHR-pFragmentShadingRateAttachment-04524#
--     If @pFragmentShadingRateAttachment@ is not @NULL@ and its
--     @attachment@ member is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', its @layout@ member
--     /must/ be equal to
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' or
--     'IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR'
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
  { -- | @pFragmentShadingRateAttachment@ is an optional
    -- 'Vulkan.Extensions.VK_KHR_create_renderpass2.AttachmentReference2KHR'
    -- structure defining the fragment shading rate attachment for this
    -- subpass.
    fragmentShadingRateAttachment :: SomeStruct AttachmentReference2
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
    pFragmentShadingRateAttachment'' <- ContT @_ @_ @(Ptr (AttachmentReference2 '[])) $ \cont -> withSomeCStruct @AttachmentReference2 (fragmentShadingRateAttachment) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (AttachmentReference2 _)))) pFragmentShadingRateAttachment''
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr Extent2D)) (shadingRateAttachmentTexelSize) . ($ ())
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pFragmentShadingRateAttachment'' <- ContT @_ @_ @(Ptr (AttachmentReference2 '[])) $ \cont -> withSomeCStruct @AttachmentReference2 ((SomeStruct zero)) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (AttachmentReference2 _)))) pFragmentShadingRateAttachment''
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr Extent2D)) (zero) . ($ ())
    lift $ f

instance FromCStruct FragmentShadingRateAttachmentInfoKHR where
  peekCStruct p = do
    pFragmentShadingRateAttachment <- peekSomeCStruct . forgetExtensions =<< peek ((p `plusPtr` 16 :: Ptr (Ptr (AttachmentReference2 a))))
    shadingRateAttachmentTexelSize <- peekCStruct @Extent2D ((p `plusPtr` 24 :: Ptr Extent2D))
    pure $ FragmentShadingRateAttachmentInfoKHR
             pFragmentShadingRateAttachment shadingRateAttachmentTexelSize

instance Zero FragmentShadingRateAttachmentInfoKHR where
  zero = FragmentShadingRateAttachmentInfoKHR
           (SomeStruct zero)
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
    -- fragment shading rate for draw commands using this pipeline.
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
  pokeCStruct p PipelineFragmentShadingRateStateCreateInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_STATE_CREATE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr Extent2D)) (fragmentSize) . ($ ())
    let pCombinerOps' = lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR)))
    lift $ case (combinerOps) of
      (e0, e1) -> do
        poke (pCombinerOps' :: Ptr FragmentShadingRateCombinerOpKHR) (e0)
        poke (pCombinerOps' `plusPtr` 4 :: Ptr FragmentShadingRateCombinerOpKHR) (e1)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_STATE_CREATE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr Extent2D)) (zero) . ($ ())
    let pCombinerOps' = lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR)))
    lift $ case ((zero, zero)) of
      (e0, e1) -> do
        poke (pCombinerOps' :: Ptr FragmentShadingRateCombinerOpKHR) (e0)
        poke (pCombinerOps' `plusPtr` 4 :: Ptr FragmentShadingRateCombinerOpKHR) (e1)
    lift $ f

instance FromCStruct PipelineFragmentShadingRateStateCreateInfoKHR where
  peekCStruct p = do
    fragmentSize <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    let pcombinerOps = lowerArrayPtr @FragmentShadingRateCombinerOpKHR ((p `plusPtr` 24 :: Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR)))
    combinerOps0 <- peek @FragmentShadingRateCombinerOpKHR ((pcombinerOps `advancePtrBytes` 0 :: Ptr FragmentShadingRateCombinerOpKHR))
    combinerOps1 <- peek @FragmentShadingRateCombinerOpKHR ((pcombinerOps `advancePtrBytes` 4 :: Ptr FragmentShadingRateCombinerOpKHR))
    pure $ PipelineFragmentShadingRateStateCreateInfoKHR
             fragmentSize ((combinerOps0, combinerOps1))

instance Zero PipelineFragmentShadingRateStateCreateInfoKHR where
  zero = PipelineFragmentShadingRateStateCreateInfoKHR
           zero
           (zero, zero)


-- | VkPhysicalDeviceFragmentShadingRateFeaturesKHR - Structure indicating
-- support for variable rate fragment shading
--
-- = Members
--
-- The members of the 'PhysicalDeviceFragmentShadingRateFeaturesKHR'
-- structure describe the following features:
--
-- = Description
--
-- -   #features-pipelineFragmentShadingRate# @pipelineFragmentShadingRate@
--     indicates that the implementation supports the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-pipeline pipeline fragment shading rate>.
--
-- -   #features-primitiveFragmentShadingRate#
--     @primitiveFragmentShadingRate@ indicates that the implementation
--     supports the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-primitive primitive fragment shading rate>.
--
-- -   #features-attachmentFragmentShadingRate#
--     @attachmentFragmentShadingRate@ indicates that the implementation
--     supports the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment attachment fragment shading rate>.
--
-- If the 'PhysicalDeviceFragmentShadingRateFeaturesKHR' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceFragmentShadingRateFeaturesKHR' /can/ also be used in the
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceFragmentShadingRateFeaturesKHR-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_FEATURES_KHR'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentShadingRateFeaturesKHR = PhysicalDeviceFragmentShadingRateFeaturesKHR
  { -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRateFeaturesKHR" "pipelineFragmentShadingRate"
    pipelineFragmentShadingRate :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRateFeaturesKHR" "primitiveFragmentShadingRate"
    primitiveFragmentShadingRate :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRateFeaturesKHR" "attachmentFragmentShadingRate"
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
-- = Members
--
-- The members of the 'PhysicalDeviceFragmentShadingRatePropertiesKHR'
-- structure describe the following implementation-dependent properties
-- related to
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-primsrast-fragment-shading-rate fragment shading rates>:
--
-- = Description
--
-- -   @sType@ is the type of this structure.
--
-- -   @pNext@ is @NULL@ or a pointer to a structure extending this
--     structure.
--
-- -   #limits-minFragmentShadingRateAttachmentTexelSize#
--     @minFragmentShadingRateAttachmentTexelSize@ indicates minimum
--     supported width and height of the portion of the framebuffer
--     corresponding to each texel in a fragment shading rate attachment.
--     Each value /must/ be less than or equal to the values in
--     @maxFragmentShadingRateAttachmentTexelSize@. Each value /must/ be a
--     power-of-two. It /must/ be (0,0) if the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     feature is not supported.
--
-- -   #limits-maxFragmentShadingRateAttachmentTexelSize#
--     @maxFragmentShadingRateAttachmentTexelSize@ indicates maximum
--     supported width and height of the portion of the framebuffer
--     corresponding to each texel in a fragment shading rate attachment.
--     Each value /must/ be greater than or equal to the values in
--     @minFragmentShadingRateAttachmentTexelSize@. Each value /must/ be a
--     power-of-two. It /must/ be (0,0) if the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     feature is not supported.
--
-- -   #limits-maxFragmentShadingRateAttachmentTexelSizeAspectRatio#
--     @maxFragmentShadingRateAttachmentTexelSizeAspectRatio@ indicates the
--     maximum ratio between the width and height of the portion of the
--     framebuffer corresponding to each texel in a fragment shading rate
--     attachment. @maxFragmentShadingRateAttachmentTexelSizeAspectRatio@
--     /must/ be a power-of-two value, and /must/ be less than or equal to
--     max(@maxFragmentShadingRateAttachmentTexelSize.width@ \/
--     minFragmentShadingRateAttachmentTexelSize.height,
--     @maxFragmentShadingRateAttachmentTexelSize.height@ \/
--     minFragmentShadingRateAttachmentTexelSize.width). It /must/ be 0 if
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     feature is not supported.
--
-- -   #limits-primitiveFragmentShadingRateWithMultipleViewports#
--     @primitiveFragmentShadingRateWithMultipleViewports@ specifies
--     whether the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-primitive primitive fragment shading rate>
--     /can/ be used when multiple viewports are used. If this value is
--     'Vulkan.Core10.FundamentalTypes.FALSE', only a single viewport
--     /must/ be used, and applications /must/ not write to the
--     @ViewportMaskNV@ or @ViewportIndex@ built-in when setting
--     @PrimitiveShadingRateKHR@. It /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE' if the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderOutputViewportIndex shaderOutputViewportIndex>
--     feature, the
--     <VK_EXT_shader_viewport_index_layer.html VK_EXT_shader_viewport_index_layer>
--     extension, or the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not supported, or if the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitiveFragmentShadingRate primitiveFragmentShadingRate>
--     feature is not supported.
--
-- -   #limits-layeredShadingRateAttachments#
--     @layeredShadingRateAttachments@ specifies whether a shading rate
--     attachment image view /can/ be created with multiple layers. If this
--     value is 'Vulkan.Core10.FundamentalTypes.FALSE', when creating an
--     image view with a @usage@ that includes
--     'IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR', @layerCount@
--     /must/ be @1@. It /must/ be 'Vulkan.Core10.FundamentalTypes.FALSE'
--     if the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiview multiview>
--     feature, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderOutputViewportIndex shaderOutputViewportIndex>
--     feature, the
--     <VK_EXT_shader_viewport_index_layer.html VK_EXT_shader_viewport_index_layer>
--     extension, or the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not supported, or if the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     feature is not supported.
--
-- -   #limits-fragmentShadingRateNonTrivialCombinerOps#
--     @fragmentShadingRateNonTrivialCombinerOps@ specifies whether
--     'FragmentShadingRateCombinerOpKHR' enums other than
--     'FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR' or
--     'FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR' /can/ be used. It
--     /must/ be 'Vulkan.Core10.FundamentalTypes.FALSE' unless either the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitiveFragmentShadingRate primitiveFragmentShadingRate>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     feature is supported.
--
-- -   #limits-maxFragmentSize# @maxFragmentSize@ indicates the maximum
--     supported width and height of a fragment. Its @width@ and @height@
--     members /must/ both be power-of-two values. This limit is purely
--     informational, and is not validated.
--
-- -   #limits-maxFragmentSizeAspectRatio# @maxFragmentSizeAspectRatio@
--     indicates the maximum ratio between the width and height of a
--     fragment. @maxFragmentSizeAspectRatio@ /must/ be a power-of-two
--     value, and /must/ be less than or equal to the maximum of the
--     @width@ and @height@ members of @maxFragmentSize@. This limit is
--     purely informational, and is not validated.
--
-- -   #limits-maxFragmentShadingRateCoverageSamples#
--     @maxFragmentShadingRateCoverageSamples@ specifies the maximum number
--     of coverage samples supported in a single fragment.
--     @maxFragmentShadingRateCoverageSamples@ /must/ be less than or equal
--     to the product of the @width@ and @height@ members of
--     @maxFragmentSize@, and the sample count reported by
--     @maxFragmentShadingRateRasterizationSamples@.
--     @maxFragmentShadingRateCoverageSamples@ /must/ be less than or equal
--     to @maxSampleMaskWords@ * 32 if
--     @fragmentShadingRateWithShaderSampleMask@ is supported. This limit
--     is purely informational, and is not validated.
--
-- -   #limits-maxFragmentShadingRateRasterizationSamples#
--     @maxFragmentShadingRateRasterizationSamples@ specifies the maximum
--     sample rate supported when a fragment covers multiple pixels. This
--     limit is purely informational, and is not validated.
--
-- -   #limits-fragmentShadingRateWithShaderDepthStencilWrites#
--     @fragmentShadingRateWithShaderDepthStencilWrites@ specifies whether
--     the implementation supports writing @FragDepth@ or
--     @FragStencilRefEXT@ from a fragment shader for multi-pixel
--     fragments. If this value is 'Vulkan.Core10.FundamentalTypes.FALSE',
--     writing to those built-ins will clamp the fragment shading rate to
--     (1,1).
--
-- -   #limits-fragmentShadingRateWithSampleMask#
--     @fragmentShadingRateWithSampleMask@ specifies whether the the
--     implementation supports setting valid bits of
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::pSampleMask
--     to @0@ for multi-pixel fragments. If this value is
--     'Vulkan.Core10.FundamentalTypes.FALSE', zeroing valid bits in the
--     sample mask will clamp the fragment shading rate to (1,1).
--
-- -   #limits-fragmentShadingRateWithShaderSampleMask#
--     @fragmentShadingRateWithShaderSampleMask@ specifies whether the
--     implementation supports reading or writing
--     'Vulkan.Core10.FundamentalTypes.SampleMask' for multi-pixel
--     fragments. If this value is 'Vulkan.Core10.FundamentalTypes.FALSE',
--     using that built-in will clamp the fragment shading rate to (1,1).
--
-- -   #limits-fragmentShadingRateWithConservativeRasterization#
--     @fragmentShadingRateWithConservativeRasterization@ specifies whether
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-conservativeraster conservative rasterization>
--     is supported for multi-pixel fragments. It /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE' if
--     <VK_EXT_conservative_rasterization.html VK_EXT_conservative_rasterization>
--     is not supported. If this value is
--     'Vulkan.Core10.FundamentalTypes.FALSE', using
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-conservativeraster conservative rasterization>
--     will clamp the fragment shading rate to (1,1).
--
-- -   #limits-fragmentShadingRateWithFragmentShaderInterlock#
--     @fragmentShadingRateWithFragmentShaderInterlock@ specifies whether
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-fragment-shader-interlock fragment shader interlock>
--     is supported for multi-pixel fragments. It /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE' if
--     <VK_EXT_fragment_shader_interlock.html VK_EXT_fragment_shader_interlock>
--     is not supported. If this value is
--     'Vulkan.Core10.FundamentalTypes.FALSE', using
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-fragment-shader-interlock fragment shader interlock>
--     will clamp the fragment shading rate to (1,1).
--
-- -   #limits-fragmentShadingRateWithCustomSampleLocations#
--     @fragmentShadingRateWithCustomSampleLocations@ specifies whether
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primrast-samplelocations custom sample locations>
--     are supported for multi-pixel fragments. It /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE' if
--     <VK_EXT_sample_locations.html VK_EXT_sample_locations> is not
--     supported. If this value is 'Vulkan.Core10.FundamentalTypes.FALSE',
--     using
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primrast-samplelocations custom sample locations>
--     will clamp the fragment shading rate to (1,1).
--
-- -   #limits-fragmentShadingRateStrictMultiplyCombiner#
--     @fragmentShadingRateStrictMultiplyCombiner@ specifies whether
--     VK_FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR accurately performs a
--     multiplication or not. Implementations where this value is
--     'Vulkan.Core10.FundamentalTypes.FALSE' will instead combine rates
--     with an addition. If @fragmentShadingRateNonTrivialCombinerOps@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', implementations /must/
--     report this as 'Vulkan.Core10.FundamentalTypes.FALSE'. If
--     @fragmentShadingRateNonTrivialCombinerOps@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', implementations /should/
--     report this as 'Vulkan.Core10.FundamentalTypes.TRUE'.
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
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceFragmentShadingRatePropertiesKHR-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_PROPERTIES_KHR'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentShadingRatePropertiesKHR = PhysicalDeviceFragmentShadingRatePropertiesKHR
  { -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "minFragmentShadingRateAttachmentTexelSize"
    minFragmentShadingRateAttachmentTexelSize :: Extent2D
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "maxFragmentShadingRateAttachmentTexelSize"
    maxFragmentShadingRateAttachmentTexelSize :: Extent2D
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "maxFragmentShadingRateAttachmentTexelSizeAspectRatio"
    maxFragmentShadingRateAttachmentTexelSizeAspectRatio :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "primitiveFragmentShadingRateWithMultipleViewports"
    primitiveFragmentShadingRateWithMultipleViewports :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "layeredShadingRateAttachments"
    layeredShadingRateAttachments :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "fragmentShadingRateNonTrivialCombinerOps"
    fragmentShadingRateNonTrivialCombinerOps :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "maxFragmentSize"
    maxFragmentSize :: Extent2D
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "maxFragmentSizeAspectRatio"
    maxFragmentSizeAspectRatio :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "maxFragmentShadingRateCoverageSamples"
    maxFragmentShadingRateCoverageSamples :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "maxFragmentShadingRateRasterizationSamples"
    maxFragmentShadingRateRasterizationSamples :: SampleCountFlagBits
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "fragmentShadingRateWithShaderDepthStencilWrites"
    fragmentShadingRateWithShaderDepthStencilWrites :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "fragmentShadingRateWithSampleMask"
    fragmentShadingRateWithSampleMask :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "fragmentShadingRateWithShaderSampleMask"
    fragmentShadingRateWithShaderSampleMask :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "fragmentShadingRateWithConservativeRasterization"
    fragmentShadingRateWithConservativeRasterization :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "fragmentShadingRateWithFragmentShaderInterlock"
    fragmentShadingRateWithFragmentShaderInterlock :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "fragmentShadingRateWithCustomSampleLocations"
    fragmentShadingRateWithCustomSampleLocations :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "fragmentShadingRateStrictMultiplyCombiner"
    fragmentShadingRateStrictMultiplyCombiner :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentShadingRatePropertiesKHR)
#endif
deriving instance Show PhysicalDeviceFragmentShadingRatePropertiesKHR

instance ToCStruct PhysicalDeviceFragmentShadingRatePropertiesKHR where
  withCStruct x f = allocaBytesAligned 96 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentShadingRatePropertiesKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_PROPERTIES_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr Extent2D)) (minFragmentShadingRateAttachmentTexelSize) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr Extent2D)) (maxFragmentShadingRateAttachmentTexelSize) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (maxFragmentShadingRateAttachmentTexelSizeAspectRatio)
    lift $ poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (primitiveFragmentShadingRateWithMultipleViewports))
    lift $ poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (layeredShadingRateAttachments))
    lift $ poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateNonTrivialCombinerOps))
    ContT $ pokeCStruct ((p `plusPtr` 48 :: Ptr Extent2D)) (maxFragmentSize) . ($ ())
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (maxFragmentSizeAspectRatio)
    lift $ poke ((p `plusPtr` 60 :: Ptr Word32)) (maxFragmentShadingRateCoverageSamples)
    lift $ poke ((p `plusPtr` 64 :: Ptr SampleCountFlagBits)) (maxFragmentShadingRateRasterizationSamples)
    lift $ poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateWithShaderDepthStencilWrites))
    lift $ poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateWithSampleMask))
    lift $ poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateWithShaderSampleMask))
    lift $ poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateWithConservativeRasterization))
    lift $ poke ((p `plusPtr` 84 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateWithFragmentShaderInterlock))
    lift $ poke ((p `plusPtr` 88 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateWithCustomSampleLocations))
    lift $ poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateStrictMultiplyCombiner))
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_PROPERTIES_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr Extent2D)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr Extent2D)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    ContT $ pokeCStruct ((p `plusPtr` 48 :: Ptr Extent2D)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 60 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 64 :: Ptr SampleCountFlagBits)) (zero)
    lift $ poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 84 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 88 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ f

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
-- -   #VUID-VkPhysicalDeviceFragmentShadingRateKHR-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_KHR'
--
-- -   #VUID-VkPhysicalDeviceFragmentShadingRateKHR-pNext-pNext# @pNext@
--     /must/ be @NULL@
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
  pokeCStruct p PhysicalDeviceFragmentShadingRateKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr SampleCountFlags)) (sampleCounts)
    ContT $ pokeCStruct ((p `plusPtr` 20 :: Ptr Extent2D)) (fragmentSize) . ($ ())
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr SampleCountFlags)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 20 :: Ptr Extent2D)) (zero) . ($ ())
    lift $ f

instance FromCStruct PhysicalDeviceFragmentShadingRateKHR where
  peekCStruct p = do
    sampleCounts <- peek @SampleCountFlags ((p `plusPtr` 16 :: Ptr SampleCountFlags))
    fragmentSize <- peekCStruct @Extent2D ((p `plusPtr` 20 :: Ptr Extent2D))
    pure $ PhysicalDeviceFragmentShadingRateKHR
             sampleCounts fragmentSize

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
pattern FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR = FragmentShadingRateCombinerOpKHR 0
-- | 'FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR' specifies a combiner
-- operation of combine(Axy,Bxy) = Bxy.
pattern FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR = FragmentShadingRateCombinerOpKHR 1
-- | 'FRAGMENT_SHADING_RATE_COMBINER_OP_MIN_KHR' specifies a combiner
-- operation of combine(Axy,Bxy) = min(Axy,Bxy).
pattern FRAGMENT_SHADING_RATE_COMBINER_OP_MIN_KHR = FragmentShadingRateCombinerOpKHR 2
-- | 'FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_KHR' specifies a combiner
-- operation of combine(Axy,Bxy) = max(Axy,Bxy).
pattern FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_KHR = FragmentShadingRateCombinerOpKHR 3
-- | 'FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR' combiner operation of
-- combine(Axy,Bxy) = Axy*Bxy.
pattern FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR = FragmentShadingRateCombinerOpKHR 4
{-# complete FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR,
             FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR,
             FRAGMENT_SHADING_RATE_COMBINER_OP_MIN_KHR,
             FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_KHR,
             FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR :: FragmentShadingRateCombinerOpKHR #-}

instance Show FragmentShadingRateCombinerOpKHR where
  showsPrec p = \case
    FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR -> showString "FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR"
    FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR -> showString "FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR"
    FRAGMENT_SHADING_RATE_COMBINER_OP_MIN_KHR -> showString "FRAGMENT_SHADING_RATE_COMBINER_OP_MIN_KHR"
    FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_KHR -> showString "FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_KHR"
    FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR -> showString "FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR"
    FragmentShadingRateCombinerOpKHR x -> showParen (p >= 11) (showString "FragmentShadingRateCombinerOpKHR " . showsPrec 11 x)

instance Read FragmentShadingRateCombinerOpKHR where
  readPrec = parens (choose [("FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR", pure FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR)
                            , ("FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR", pure FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR)
                            , ("FRAGMENT_SHADING_RATE_COMBINER_OP_MIN_KHR", pure FRAGMENT_SHADING_RATE_COMBINER_OP_MIN_KHR)
                            , ("FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_KHR", pure FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_KHR)
                            , ("FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR", pure FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "FragmentShadingRateCombinerOpKHR")
                       v <- step readPrec
                       pure (FragmentShadingRateCombinerOpKHR v)))


type KHR_FRAGMENT_SHADING_RATE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_FRAGMENT_SHADING_RATE_SPEC_VERSION"
pattern KHR_FRAGMENT_SHADING_RATE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_FRAGMENT_SHADING_RATE_SPEC_VERSION = 1


type KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME = "VK_KHR_fragment_shading_rate"

-- No documentation found for TopLevel "VK_KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME"
pattern KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME = "VK_KHR_fragment_shading_rate"

