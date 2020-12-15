{-# language CPP #-}
-- | = Name
--
-- VK_NV_fragment_shading_rate_enums - device extension
--
-- == VK_NV_fragment_shading_rate_enums
--
-- [__Name String__]
--     @VK_NV_fragment_shading_rate_enums@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     327
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_fragment_shading_rate@
--
-- [__Contact__]
--
--     -   Pat Brown
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NV_fragment_shading_rate_enums:%20&body=@nvpbrown%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-09-02
--
-- [__Contributors__]
--
--     -   Pat Brown, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension builds on the fragment shading rate functionality
-- provided by the VK_KHR_fragment_shading_rate extension, adding support
-- for \"supersample\" fragment shading rates that trigger multiple
-- fragment shader invocations per pixel as well as a \"no invocations\"
-- shading rate that discards any portions of a primitive that would use
-- that shading rate.
--
-- == New Commands
--
-- -   'cmdSetFragmentShadingRateEnumNV'
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo':
--
--     -   'PipelineFragmentShadingRateEnumStateCreateInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFragmentShadingRateEnumsFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceFragmentShadingRateEnumsPropertiesNV'
--
-- == New Enums
--
-- -   'FragmentShadingRateNV'
--
-- -   'FragmentShadingRateTypeNV'
--
-- == New Enum Constants
--
-- -   'NV_FRAGMENT_SHADING_RATE_ENUMS_EXTENSION_NAME'
--
-- -   'NV_FRAGMENT_SHADING_RATE_ENUMS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_PROPERTIES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_ENUM_STATE_CREATE_INFO_NV'
--
-- == Issues
--
-- 1.  Why was this extension created? How should it be named?
--
--     RESOLVED: The primary goal of this extension was to expose support
--     for supersample and \"no invocations\" shading rates, which are
--     supported by the VK_NV_shading_rate_image extension but not by
--     VK_KHR_fragment_shading_rate. Because VK_KHR_fragment_shading_rate
--     specifies the primitive shading rate using a fragment size in
--     pixels, it lacks a good way to specify supersample rates. To deal
--     with this, we defined enums covering shading rates supported by the
--     KHR extension as well as the new shading rates and added structures
--     and APIs accepting shading rate enums instead of fragment sizes.
--
--     Since this extension adds two different types of shading rates, both
--     expressed using enums, we chose the extension name
--     VK_NV_fragment_shading_rate_enums.
--
-- 2.  Is this a standalone extension?
--
--     RESOLVED: No, this extension requires VK_KHR_fragment_shading_rate.
--     In order to use the features of this extension, applications must
--     enable the relevant features of KHR extension.
--
-- 3.  How are the shading rate enums used, and how were the enum values
--     assigned?
--
--     RESOLVED: The shading rates supported by the enums in this extension
--     are accepted as pipeline, primitive, and attachment shading rates
--     and behave identically. For the shading rates also supported by the
--     KHR extension, the values assigned to the corresponding enums are
--     identical to the values already used for the primitive and
--     attachment shading rates in the KHR extension. For those enums, bits
--     0 and 1 specify the base two logarithm of the fragment height and
--     bits 2 and 3 specify the base two logarithm of the fragment width.
--     For the new shading rates added by this extension, we chose to use
--     11 through 14 (10 plus the base two logarithm of the invocation
--     count) for the supersample rates and 15 for the \"no invocations\"
--     rate. None of those values are supported as primitive or attachment
--     shading rates by the KHR extension.
--
-- 4.  Between this extension, VK_KHR_fragment_shading_rate, and
--     VK_NV_shading_rate_image, there are three different ways to specify
--     shading rate state in a pipeline. How should we handle this?
--
--     RESOLVED: We don’t allow the concurrent use of
--     VK_NV_shading_rate_image and VK_KHR_fragment_shading_rate; it is an
--     error to enable shading rate features from both extensions. But we
--     do allow applications to enable this extension together with
--     VK_KHR_fragment_shading_rate together. While we expect that
--     applications will never attach pipeline CreateInfo structures for
--     both this extension and the KHR extension concurrently, Vulkan
--     doesn’t have any precedent forbidding such behavior and instead
--     typically treats a pipeline created without an extension-specific
--     CreateInfo structure as equivalent to one containing default values
--     specified by the extension. Rather than adding such a rule
--     considering the presence or absence of our new CreateInfo structure,
--     we instead included a @shadingRateType@ member to
--     'PipelineFragmentShadingRateEnumStateCreateInfoNV' that selects
--     between using state specified by that structure and state specified
--     by
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'.
--
-- == Version History
--
-- -   Revision 1, 2020-09-02 (pbrown)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'FragmentShadingRateNV', 'FragmentShadingRateTypeNV',
-- 'PhysicalDeviceFragmentShadingRateEnumsFeaturesNV',
-- 'PhysicalDeviceFragmentShadingRateEnumsPropertiesNV',
-- 'PipelineFragmentShadingRateEnumStateCreateInfoNV',
-- 'cmdSetFragmentShadingRateEnumNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_fragment_shading_rate_enums Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_fragment_shading_rate_enums  ( cmdSetFragmentShadingRateEnumNV
                                                            , PhysicalDeviceFragmentShadingRateEnumsFeaturesNV(..)
                                                            , PhysicalDeviceFragmentShadingRateEnumsPropertiesNV(..)
                                                            , PipelineFragmentShadingRateEnumStateCreateInfoNV(..)
                                                            , FragmentShadingRateNV( FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV
                                                                                   , FRAGMENT_SHADING_RATE_1_INVOCATION_PER_1X2_PIXELS_NV
                                                                                   , FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X1_PIXELS_NV
                                                                                   , FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X2_PIXELS_NV
                                                                                   , FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X4_PIXELS_NV
                                                                                   , FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X2_PIXELS_NV
                                                                                   , FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X4_PIXELS_NV
                                                                                   , FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV
                                                                                   , FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV
                                                                                   , FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV
                                                                                   , FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV
                                                                                   , FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV
                                                                                   , ..
                                                                                   )
                                                            , FragmentShadingRateTypeNV( FRAGMENT_SHADING_RATE_TYPE_FRAGMENT_SIZE_NV
                                                                                       , FRAGMENT_SHADING_RATE_TYPE_ENUMS_NV
                                                                                       , ..
                                                                                       )
                                                            , NV_FRAGMENT_SHADING_RATE_ENUMS_SPEC_VERSION
                                                            , pattern NV_FRAGMENT_SHADING_RATE_ENUMS_SPEC_VERSION
                                                            , NV_FRAGMENT_SHADING_RATE_ENUMS_EXTENSION_NAME
                                                            , pattern NV_FRAGMENT_SHADING_RATE_ENUMS_EXTENSION_NAME
                                                            , FragmentShadingRateCombinerOpKHR(..)
                                                            ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
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
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetFragmentShadingRateEnumNV))
import Vulkan.Extensions.VK_KHR_fragment_shading_rate (FragmentShadingRateCombinerOpKHR)
import Vulkan.Extensions.VK_KHR_fragment_shading_rate (FragmentShadingRateCombinerOpKHR(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_PROPERTIES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_ENUM_STATE_CREATE_INFO_NV))
import Vulkan.Extensions.VK_KHR_fragment_shading_rate (FragmentShadingRateCombinerOpKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetFragmentShadingRateEnumNV
  :: FunPtr (Ptr CommandBuffer_T -> FragmentShadingRateNV -> Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR) -> IO ()) -> Ptr CommandBuffer_T -> FragmentShadingRateNV -> Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR) -> IO ()

-- | vkCmdSetFragmentShadingRateEnumNV - Set pipeline fragment shading rate
-- dynamically using enums
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetFragmentShadingRateEnumNV-pipelineFragmentShadingRate-04576#
--     If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineFragmentShadingRate pipelineFragmentShadingRate>
--     is not enabled, @shadingRate@ /must/ be
--     'FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV'
--
-- -   #VUID-vkCmdSetFragmentShadingRateEnumNV-supersampleFragmentShadingRates-04577#
--     If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-supersampleFragmentShadingRates supersampleFragmentShadingRates>
--     is not enabled, @shadingRate@ /must/ not be
--     'FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV',
--     'FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV',
--     'FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV', or
--     'FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV'
--
-- -   #VUID-vkCmdSetFragmentShadingRateEnumNV-noInvocationFragmentShadingRates-04578#
--     If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-noInvocationFragmentShadingRates noInvocationFragmentShadingRates>
--     is not enabled, @shadingRate@ /must/ not be
--     'FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV'
--
-- -   #VUID-vkCmdSetFragmentShadingRateEnumNV-fragmentShadingRateEnums-04579#
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentShadingRateEnums fragmentShadingRateEnums>
--     /must/ be enabled
--
-- -   #VUID-vkCmdSetFragmentShadingRateEnumNV-pipelineFragmentShadingRate-04580#
--     One of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineFragmentShadingRate pipelineFragmentShadingRate>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitiveFragmentShadingRate primitiveFragmentShadingRate>,
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     /must/ be enabled
--
-- -   #VUID-vkCmdSetFragmentShadingRateEnumNV-primitiveFragmentShadingRate-04581#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#feature-primitiveFragmentShadingRate primitiveFragmentShadingRate feature>
--     is not enabled, @combinerOps@[0] /must/ be
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR'
--
-- -   #VUID-vkCmdSetFragmentShadingRateEnumNV-attachmentFragmentShadingRate-04582#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#feature-attachmentFragmentShadingRate attachmentFragmentShadingRate feature>
--     is not enabled, @combinerOps@[1] /must/ be
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR'
--
-- -   #VUID-vkCmdSetFragmentShadingRateEnumNV-fragmentSizeNonTrivialCombinerOps-04583#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-fragmentShadingRateNonTrivialCombinerOps fragmentSizeNonTrivialCombinerOps>
--     limit is not supported, elements of @combinerOps@ /must/ be either
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetFragmentShadingRateEnumNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetFragmentShadingRateEnumNV-shadingRate-parameter#
--     @shadingRate@ /must/ be a valid 'FragmentShadingRateNV' value
--
-- -   #VUID-vkCmdSetFragmentShadingRateEnumNV-combinerOps-parameter# Any
--     given element of @combinerOps@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateCombinerOpKHR'
--     value
--
-- -   #VUID-vkCmdSetFragmentShadingRateEnumNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetFragmentShadingRateEnumNV-commandBuffer-cmdpool# The
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
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateCombinerOpKHR',
-- 'FragmentShadingRateNV'
cmdSetFragmentShadingRateEnumNV :: forall io
                                 . (MonadIO io)
                                => -- | @commandBuffer@ is the command buffer into which the command will be
                                   -- recorded.
                                   CommandBuffer
                                -> -- | @shadingRate@ specifies a 'FragmentShadingRateNV' enum indicating the
                                   -- pipeline fragment shading rate for subsequent draw commands.
                                   FragmentShadingRateNV
                                -> -- | @combinerOps@ specifies a
                                   -- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateCombinerOpKHR'
                                   -- determining how the
                                   -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-pipeline pipeline>,
                                   -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-primitive primitive>,
                                   -- and
                                   -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment attachment shading rates>
                                   -- are
                                   -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-combining combined>
                                   -- for fragments generated by subsequent drawing commands.
                                   ("combinerOps" ::: (FragmentShadingRateCombinerOpKHR, FragmentShadingRateCombinerOpKHR))
                                -> io ()
cmdSetFragmentShadingRateEnumNV commandBuffer shadingRate combinerOps = liftIO . evalContT $ do
  let vkCmdSetFragmentShadingRateEnumNVPtr = pVkCmdSetFragmentShadingRateEnumNV (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetFragmentShadingRateEnumNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetFragmentShadingRateEnumNV is null" Nothing Nothing
  let vkCmdSetFragmentShadingRateEnumNV' = mkVkCmdSetFragmentShadingRateEnumNV vkCmdSetFragmentShadingRateEnumNVPtr
  pCombinerOps <- ContT $ allocaBytesAligned @(FixedArray 2 FragmentShadingRateCombinerOpKHR) 8 4
  let pCombinerOps' = lowerArrayPtr pCombinerOps
  lift $ case (combinerOps) of
    (e0, e1) -> do
      poke (pCombinerOps' :: Ptr FragmentShadingRateCombinerOpKHR) (e0)
      poke (pCombinerOps' `plusPtr` 4 :: Ptr FragmentShadingRateCombinerOpKHR) (e1)
  lift $ traceAroundEvent "vkCmdSetFragmentShadingRateEnumNV" (vkCmdSetFragmentShadingRateEnumNV' (commandBufferHandle (commandBuffer)) (shadingRate) (pCombinerOps))
  pure $ ()


-- | VkPhysicalDeviceFragmentShadingRateEnumsFeaturesNV - Structure
-- indicating support for fragment shading rate enums
--
-- = Members
--
-- The members of the 'PhysicalDeviceFragmentShadingRateEnumsFeaturesNV'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceFragmentShadingRateEnumsFeaturesNV' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceFragmentShadingRateEnumsFeaturesNV' /can/ also be used in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable
-- these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentShadingRateEnumsFeaturesNV = PhysicalDeviceFragmentShadingRateEnumsFeaturesNV
  { -- | #features-fragmentShadingRateEnums# @fragmentShadingRateEnums@ indicates
    -- that the implementation supports specifying fragment shading rates using
    -- the 'FragmentShadingRateNV' enumerated type.
    fragmentShadingRateEnums :: Bool
  , -- | #features-supersampleFragmentShadingRates#
    -- @supersampleFragmentShadingRates@ indicates that the implementation
    -- supports fragment shading rate enum values indicating more than one
    -- invocation per fragment.
    supersampleFragmentShadingRates :: Bool
  , -- | #features-noInvocationFragmentShadingRates#
    -- @noInvocationFragmentShadingRates@ indicates that the implementation
    -- supports a fragment shading rate enum value indicating that no fragment
    -- shaders should be invoked when that shading rate is used.
    noInvocationFragmentShadingRates :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentShadingRateEnumsFeaturesNV)
#endif
deriving instance Show PhysicalDeviceFragmentShadingRateEnumsFeaturesNV

instance ToCStruct PhysicalDeviceFragmentShadingRateEnumsFeaturesNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentShadingRateEnumsFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateEnums))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (supersampleFragmentShadingRates))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (noInvocationFragmentShadingRates))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFragmentShadingRateEnumsFeaturesNV where
  peekCStruct p = do
    fragmentShadingRateEnums <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    supersampleFragmentShadingRates <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    noInvocationFragmentShadingRates <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceFragmentShadingRateEnumsFeaturesNV
             (bool32ToBool fragmentShadingRateEnums) (bool32ToBool supersampleFragmentShadingRates) (bool32ToBool noInvocationFragmentShadingRates)

instance Storable PhysicalDeviceFragmentShadingRateEnumsFeaturesNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentShadingRateEnumsFeaturesNV where
  zero = PhysicalDeviceFragmentShadingRateEnumsFeaturesNV
           zero
           zero
           zero


-- | VkPhysicalDeviceFragmentShadingRateEnumsPropertiesNV - Structure
-- describing fragment shading rate limits that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceFragmentShadingRateEnumsPropertiesNV'
-- structure describe the following implementation-dependent properties
-- related to
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-primsrast-fragment-shading-rate fragment shading rates>:
--
-- = Description
--
-- If the 'PhysicalDeviceFragmentShadingRateEnumsPropertiesNV' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentShadingRateEnumsPropertiesNV = PhysicalDeviceFragmentShadingRateEnumsPropertiesNV
  { -- | #limits-maxFragmentShadingRateInvocationCount#
    -- @maxFragmentShadingRateInvocationCount@ indicates the maximum number of
    -- fragment shader invocations per fragment supported in pipeline,
    -- primitive, and attachment fragment shading rates.
    --
    -- #VUID-VkPhysicalDeviceFragmentShadingRateEnumsPropertiesNV-maxFragmentShadingRateInvocationCount-parameter#
    -- @maxFragmentShadingRateInvocationCount@ /must/ be a valid
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
    maxFragmentShadingRateInvocationCount :: SampleCountFlagBits }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentShadingRateEnumsPropertiesNV)
#endif
deriving instance Show PhysicalDeviceFragmentShadingRateEnumsPropertiesNV

instance ToCStruct PhysicalDeviceFragmentShadingRateEnumsPropertiesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentShadingRateEnumsPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SampleCountFlagBits)) (maxFragmentShadingRateInvocationCount)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SampleCountFlagBits)) (zero)
    f

instance FromCStruct PhysicalDeviceFragmentShadingRateEnumsPropertiesNV where
  peekCStruct p = do
    maxFragmentShadingRateInvocationCount <- peek @SampleCountFlagBits ((p `plusPtr` 16 :: Ptr SampleCountFlagBits))
    pure $ PhysicalDeviceFragmentShadingRateEnumsPropertiesNV
             maxFragmentShadingRateInvocationCount

instance Storable PhysicalDeviceFragmentShadingRateEnumsPropertiesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentShadingRateEnumsPropertiesNV where
  zero = PhysicalDeviceFragmentShadingRateEnumsPropertiesNV
           zero


-- | VkPipelineFragmentShadingRateEnumStateCreateInfoNV - Structure
-- specifying parameters controlling the fragment shading rate using rate
-- enums
--
-- = Description
--
-- If the @pNext@ chain of
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' includes a
-- 'PipelineFragmentShadingRateEnumStateCreateInfoNV' structure, then that
-- structure includes parameters that control the pipeline fragment shading
-- rate.
--
-- If this structure is not present, @shadingRateType@ is considered to be
-- equal to 'FRAGMENT_SHADING_RATE_TYPE_FRAGMENT_SIZE_NV', @shadingRate@ is
-- considered to be equal to
-- 'FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV', and both elements of
-- @combinerOps@ are considered to be equal to
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineFragmentShadingRateEnumStateCreateInfoNV-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_ENUM_STATE_CREATE_INFO_NV'
--
-- -   #VUID-VkPipelineFragmentShadingRateEnumStateCreateInfoNV-shadingRateType-parameter#
--     @shadingRateType@ /must/ be a valid 'FragmentShadingRateTypeNV'
--     value
--
-- -   #VUID-VkPipelineFragmentShadingRateEnumStateCreateInfoNV-shadingRate-parameter#
--     @shadingRate@ /must/ be a valid 'FragmentShadingRateNV' value
--
-- -   #VUID-VkPipelineFragmentShadingRateEnumStateCreateInfoNV-combinerOps-parameter#
--     Any given element of @combinerOps@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateCombinerOpKHR'
--     value
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateCombinerOpKHR',
-- 'FragmentShadingRateNV', 'FragmentShadingRateTypeNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineFragmentShadingRateEnumStateCreateInfoNV = PipelineFragmentShadingRateEnumStateCreateInfoNV
  { -- | @shadingRateType@ specifies a 'FragmentShadingRateTypeNV' value
    -- indicating whether fragment shading rates are specified using fragment
    -- sizes or 'FragmentShadingRateNV' enums.
    shadingRateType :: FragmentShadingRateTypeNV
  , -- | @shadingRate@ specifies a 'FragmentShadingRateNV' value indicating the
    -- pipeline fragment shading rate.
    shadingRate :: FragmentShadingRateNV
  , -- | @combinerOps@ specifies
    -- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateCombinerOpKHR'
    -- values determining how the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-pipeline pipeline>,
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-primitive primitive>,
    -- and
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment attachment shading rates>
    -- are
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-combining combined>
    -- for fragments generated by drawing commands using the created pipeline.
    combinerOps :: (FragmentShadingRateCombinerOpKHR, FragmentShadingRateCombinerOpKHR)
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineFragmentShadingRateEnumStateCreateInfoNV)
#endif
deriving instance Show PipelineFragmentShadingRateEnumStateCreateInfoNV

instance ToCStruct PipelineFragmentShadingRateEnumStateCreateInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineFragmentShadingRateEnumStateCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_ENUM_STATE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr FragmentShadingRateTypeNV)) (shadingRateType)
    poke ((p `plusPtr` 20 :: Ptr FragmentShadingRateNV)) (shadingRate)
    let pCombinerOps' = lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR)))
    case (combinerOps) of
      (e0, e1) -> do
        poke (pCombinerOps' :: Ptr FragmentShadingRateCombinerOpKHR) (e0)
        poke (pCombinerOps' `plusPtr` 4 :: Ptr FragmentShadingRateCombinerOpKHR) (e1)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_ENUM_STATE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr FragmentShadingRateTypeNV)) (zero)
    poke ((p `plusPtr` 20 :: Ptr FragmentShadingRateNV)) (zero)
    let pCombinerOps' = lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR)))
    case ((zero, zero)) of
      (e0, e1) -> do
        poke (pCombinerOps' :: Ptr FragmentShadingRateCombinerOpKHR) (e0)
        poke (pCombinerOps' `plusPtr` 4 :: Ptr FragmentShadingRateCombinerOpKHR) (e1)
    f

instance FromCStruct PipelineFragmentShadingRateEnumStateCreateInfoNV where
  peekCStruct p = do
    shadingRateType <- peek @FragmentShadingRateTypeNV ((p `plusPtr` 16 :: Ptr FragmentShadingRateTypeNV))
    shadingRate <- peek @FragmentShadingRateNV ((p `plusPtr` 20 :: Ptr FragmentShadingRateNV))
    let pcombinerOps = lowerArrayPtr @FragmentShadingRateCombinerOpKHR ((p `plusPtr` 24 :: Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR)))
    combinerOps0 <- peek @FragmentShadingRateCombinerOpKHR ((pcombinerOps `advancePtrBytes` 0 :: Ptr FragmentShadingRateCombinerOpKHR))
    combinerOps1 <- peek @FragmentShadingRateCombinerOpKHR ((pcombinerOps `advancePtrBytes` 4 :: Ptr FragmentShadingRateCombinerOpKHR))
    pure $ PipelineFragmentShadingRateEnumStateCreateInfoNV
             shadingRateType shadingRate ((combinerOps0, combinerOps1))

instance Storable PipelineFragmentShadingRateEnumStateCreateInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineFragmentShadingRateEnumStateCreateInfoNV where
  zero = PipelineFragmentShadingRateEnumStateCreateInfoNV
           zero
           zero
           (zero, zero)


-- | VkFragmentShadingRateNV - Enumeration with fragment shading rates
--
-- = Description
--
-- To use the shading rates
-- 'FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV',
-- 'FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV',
-- 'FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV', and
-- 'FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV' as a pipeline,
-- primitive, or attachment shading rate, the
-- @supersampleFragmentShadingRates@ feature /must/ be enabled. To use the
-- shading rate 'FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV' as a pipeline,
-- primitive, or attachment shading rate, the
-- @noInvocationFragmentShadingRates@ feature /must/ be enabled.
--
-- = See Also
--
-- 'PipelineFragmentShadingRateEnumStateCreateInfoNV',
-- 'cmdSetFragmentShadingRateEnumNV'
newtype FragmentShadingRateNV = FragmentShadingRateNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV' specifies a fragment
-- size of 1x1 pixels.
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV      = FragmentShadingRateNV 0
-- | 'FRAGMENT_SHADING_RATE_1_INVOCATION_PER_1X2_PIXELS_NV' specifies a
-- fragment size of 1x2 pixels.
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_1X2_PIXELS_NV = FragmentShadingRateNV 1
-- | 'FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X1_PIXELS_NV' specifies a
-- fragment size of 2x1 pixels.
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X1_PIXELS_NV = FragmentShadingRateNV 4
-- | 'FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X2_PIXELS_NV' specifies a
-- fragment size of 2x2 pixels.
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X2_PIXELS_NV = FragmentShadingRateNV 5
-- | 'FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X4_PIXELS_NV' specifies a
-- fragment size of 2x4 pixels.
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X4_PIXELS_NV = FragmentShadingRateNV 6
-- | 'FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X2_PIXELS_NV' specifies a
-- fragment size of 4x2 pixels.
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X2_PIXELS_NV = FragmentShadingRateNV 9
-- | 'FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X4_PIXELS_NV' specifies a
-- fragment size of 4x4 pixels.
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X4_PIXELS_NV = FragmentShadingRateNV 10
-- | 'FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV' specifies a fragment
-- size of 1x1 pixels, with two fragment shader invocations per fragment.
pattern FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV     = FragmentShadingRateNV 11
-- | 'FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV' specifies a fragment
-- size of 1x1 pixels, with four fragment shader invocations per fragment.
pattern FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV     = FragmentShadingRateNV 12
-- | 'FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV' specifies a fragment
-- size of 1x1 pixels, with eight fragment shader invocations per fragment.
pattern FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV     = FragmentShadingRateNV 13
-- | 'FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV' specifies a fragment
-- size of 1x1 pixels, with sixteen fragment shader invocations per
-- fragment.
pattern FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV    = FragmentShadingRateNV 14
-- | 'FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV' specifies that any portions of
-- a primitive that use that shading rate should be discarded without
-- invoking any fragment shader.
pattern FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV              = FragmentShadingRateNV 15
{-# complete FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV,
             FRAGMENT_SHADING_RATE_1_INVOCATION_PER_1X2_PIXELS_NV,
             FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X1_PIXELS_NV,
             FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X2_PIXELS_NV,
             FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X4_PIXELS_NV,
             FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X2_PIXELS_NV,
             FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X4_PIXELS_NV,
             FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV,
             FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV,
             FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV,
             FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV,
             FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV :: FragmentShadingRateNV #-}

conNameFragmentShadingRateNV :: String
conNameFragmentShadingRateNV = "FragmentShadingRateNV"

enumPrefixFragmentShadingRateNV :: String
enumPrefixFragmentShadingRateNV = "FRAGMENT_SHADING_RATE_"

showTableFragmentShadingRateNV :: [(FragmentShadingRateNV, String)]
showTableFragmentShadingRateNV =
  [ (FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV     , "1_INVOCATION_PER_PIXEL_NV")
  , (FRAGMENT_SHADING_RATE_1_INVOCATION_PER_1X2_PIXELS_NV, "1_INVOCATION_PER_1X2_PIXELS_NV")
  , (FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X1_PIXELS_NV, "1_INVOCATION_PER_2X1_PIXELS_NV")
  , (FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X2_PIXELS_NV, "1_INVOCATION_PER_2X2_PIXELS_NV")
  , (FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X4_PIXELS_NV, "1_INVOCATION_PER_2X4_PIXELS_NV")
  , (FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X2_PIXELS_NV, "1_INVOCATION_PER_4X2_PIXELS_NV")
  , (FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X4_PIXELS_NV, "1_INVOCATION_PER_4X4_PIXELS_NV")
  , (FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV    , "2_INVOCATIONS_PER_PIXEL_NV")
  , (FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV    , "4_INVOCATIONS_PER_PIXEL_NV")
  , (FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV    , "8_INVOCATIONS_PER_PIXEL_NV")
  , (FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV   , "16_INVOCATIONS_PER_PIXEL_NV")
  , (FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV             , "NO_INVOCATIONS_NV")
  ]

instance Show FragmentShadingRateNV where
  showsPrec = enumShowsPrec enumPrefixFragmentShadingRateNV
                            showTableFragmentShadingRateNV
                            conNameFragmentShadingRateNV
                            (\(FragmentShadingRateNV x) -> x)
                            (showsPrec 11)

instance Read FragmentShadingRateNV where
  readPrec = enumReadPrec enumPrefixFragmentShadingRateNV
                          showTableFragmentShadingRateNV
                          conNameFragmentShadingRateNV
                          FragmentShadingRateNV


-- | VkFragmentShadingRateTypeNV - Enumeration with fragment shading rate
-- types
--
-- = See Also
--
-- 'PipelineFragmentShadingRateEnumStateCreateInfoNV'
newtype FragmentShadingRateTypeNV = FragmentShadingRateTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'FRAGMENT_SHADING_RATE_TYPE_FRAGMENT_SIZE_NV' specifies that a graphics
-- pipeline should obtain its pipeline fragment shading rate and shading
-- rate combiner state from the
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'
-- structure and that any state specified by the
-- 'PipelineFragmentShadingRateEnumStateCreateInfoNV' structure should be
-- ignored.
pattern FRAGMENT_SHADING_RATE_TYPE_FRAGMENT_SIZE_NV = FragmentShadingRateTypeNV 0
-- | 'FRAGMENT_SHADING_RATE_TYPE_ENUMS_NV' specifies that a graphics pipeline
-- should obtain its pipeline fragment shading rate and shading rate
-- combiner state from the
-- 'PipelineFragmentShadingRateEnumStateCreateInfoNV' structure and that
-- any state specified by the
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'
-- structure should be ignored.
pattern FRAGMENT_SHADING_RATE_TYPE_ENUMS_NV         = FragmentShadingRateTypeNV 1
{-# complete FRAGMENT_SHADING_RATE_TYPE_FRAGMENT_SIZE_NV,
             FRAGMENT_SHADING_RATE_TYPE_ENUMS_NV :: FragmentShadingRateTypeNV #-}

conNameFragmentShadingRateTypeNV :: String
conNameFragmentShadingRateTypeNV = "FragmentShadingRateTypeNV"

enumPrefixFragmentShadingRateTypeNV :: String
enumPrefixFragmentShadingRateTypeNV = "FRAGMENT_SHADING_RATE_TYPE_"

showTableFragmentShadingRateTypeNV :: [(FragmentShadingRateTypeNV, String)]
showTableFragmentShadingRateTypeNV =
  [(FRAGMENT_SHADING_RATE_TYPE_FRAGMENT_SIZE_NV, "FRAGMENT_SIZE_NV"), (FRAGMENT_SHADING_RATE_TYPE_ENUMS_NV, "ENUMS_NV")]

instance Show FragmentShadingRateTypeNV where
  showsPrec = enumShowsPrec enumPrefixFragmentShadingRateTypeNV
                            showTableFragmentShadingRateTypeNV
                            conNameFragmentShadingRateTypeNV
                            (\(FragmentShadingRateTypeNV x) -> x)
                            (showsPrec 11)

instance Read FragmentShadingRateTypeNV where
  readPrec = enumReadPrec enumPrefixFragmentShadingRateTypeNV
                          showTableFragmentShadingRateTypeNV
                          conNameFragmentShadingRateTypeNV
                          FragmentShadingRateTypeNV


type NV_FRAGMENT_SHADING_RATE_ENUMS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_FRAGMENT_SHADING_RATE_ENUMS_SPEC_VERSION"
pattern NV_FRAGMENT_SHADING_RATE_ENUMS_SPEC_VERSION :: forall a . Integral a => a
pattern NV_FRAGMENT_SHADING_RATE_ENUMS_SPEC_VERSION = 1


type NV_FRAGMENT_SHADING_RATE_ENUMS_EXTENSION_NAME = "VK_NV_fragment_shading_rate_enums"

-- No documentation found for TopLevel "VK_NV_FRAGMENT_SHADING_RATE_ENUMS_EXTENSION_NAME"
pattern NV_FRAGMENT_SHADING_RATE_ENUMS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_FRAGMENT_SHADING_RATE_ENUMS_EXTENSION_NAME = "VK_NV_fragment_shading_rate_enums"

