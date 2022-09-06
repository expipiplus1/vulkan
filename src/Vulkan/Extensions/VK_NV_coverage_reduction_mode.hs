{-# language CPP #-}
-- | = Name
--
-- VK_NV_coverage_reduction_mode - device extension
--
-- == VK_NV_coverage_reduction_mode
--
-- [__Name String__]
--     @VK_NV_coverage_reduction_mode@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     251
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_NV_framebuffer_mixed_samples@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   Kedarnath Thangudu
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_coverage_reduction_mode] @kthangudu%0A<<Here describe the issue or question you have about the VK_NV_coverage_reduction_mode extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-01-29
--
-- [__Contributors__]
--
--     -   Kedarnath Thangudu, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- When using a framebuffer with mixed samples, a per-fragment coverage
-- reduction operation is performed which generates color sample coverage
-- from the pixel coverage. This extension defines the following modes to
-- control how this reduction is performed.
--
-- -   Merge: When there are more samples in the pixel coverage than color
--     samples, there is an implementation-dependent association of each
--     pixel coverage sample to a color sample. In the merge mode, the
--     color sample coverage is computed such that only if any associated
--     sample in the pixel coverage is covered, the color sample is
--     covered. This is the default mode.
--
-- -   Truncate: When there are more raster samples (N) than color
--     samples(M), there is one to one association of the first M raster
--     samples to the M color samples; other raster samples are ignored.
--
-- When the number of raster samples is equal to the color samples, there
-- is a one to one mapping between them in either of the above modes.
--
-- The new command
-- 'getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV' can be
-- used to query the various raster, color, depth\/stencil sample count and
-- reduction mode combinations that are supported by the implementation.
-- This extension would allow an implementation to support the behavior of
-- both @VK_NV_framebuffer_mixed_samples@ and
-- @VK_AMD_mixed_attachment_samples@ extensions simultaneously.
--
-- == New Commands
--
-- -   'getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV'
--
-- == New Structures
--
-- -   'FramebufferMixedSamplesCombinationNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCoverageReductionModeFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo':
--
--     -   'PipelineCoverageReductionStateCreateInfoNV'
--
-- == New Enums
--
-- -   'CoverageReductionModeNV'
--
-- == New Bitmasks
--
-- -   'PipelineCoverageReductionStateCreateFlagsNV'
--
-- == New Enum Constants
--
-- -   'NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME'
--
-- -   'NV_COVERAGE_REDUCTION_MODE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_FRAMEBUFFER_MIXED_SAMPLES_COMBINATION_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV'
--
-- == Version History
--
-- -   Revision 1, 2019-01-29 (Kedarnath Thangudu)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'CoverageReductionModeNV', 'FramebufferMixedSamplesCombinationNV',
-- 'PhysicalDeviceCoverageReductionModeFeaturesNV',
-- 'PipelineCoverageReductionStateCreateFlagsNV',
-- 'PipelineCoverageReductionStateCreateInfoNV',
-- 'getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_coverage_reduction_mode Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_coverage_reduction_mode  ( getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV
                                                        , PhysicalDeviceCoverageReductionModeFeaturesNV(..)
                                                        , PipelineCoverageReductionStateCreateInfoNV(..)
                                                        , FramebufferMixedSamplesCombinationNV(..)
                                                        , PipelineCoverageReductionStateCreateFlagsNV(..)
                                                        , CoverageReductionModeNV( COVERAGE_REDUCTION_MODE_MERGE_NV
                                                                                 , COVERAGE_REDUCTION_MODE_TRUNCATE_NV
                                                                                 , ..
                                                                                 )
                                                        , NV_COVERAGE_REDUCTION_MODE_SPEC_VERSION
                                                        , pattern NV_COVERAGE_REDUCTION_MODE_SPEC_VERSION
                                                        , NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME
                                                        , pattern NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME
                                                        ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
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
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FRAMEBUFFER_MIXED_SAMPLES_COMBINATION_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr FramebufferMixedSamplesCombinationNV -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr FramebufferMixedSamplesCombinationNV -> IO Result

-- | vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV -
-- Query supported sample count combinations
--
-- = Description
--
-- If @pCombinations@ is @NULL@, then the number of supported combinations
-- for the given @physicalDevice@ is returned in @pCombinationCount@.
-- Otherwise, @pCombinationCount@ /must/ point to a variable set by the
-- user to the number of elements in the @pCombinations@ array, and on
-- return the variable is overwritten with the number of values actually
-- written to @pCombinations@. If the value of @pCombinationCount@ is less
-- than the number of combinations supported for the given
-- @physicalDevice@, at most @pCombinationCount@ values will be written to
-- @pCombinations@, and 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be
-- returned instead of 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate
-- that not all the supported values were returned.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV-pCombinationCount-parameter#
--     @pCombinationCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV-pCombinations-parameter#
--     If the value referenced by @pCombinationCount@ is not @0@, and
--     @pCombinations@ is not @NULL@, @pCombinations@ /must/ be a valid
--     pointer to an array of @pCombinationCount@
--     'FramebufferMixedSamplesCombinationNV' structures
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_coverage_reduction_mode VK_NV_coverage_reduction_mode>,
-- 'FramebufferMixedSamplesCombinationNV',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV :: forall io
                                                                 . (MonadIO io)
                                                                => -- | @physicalDevice@ is the physical device from which to query the set of
                                                                   -- combinations.
                                                                   PhysicalDevice
                                                                -> io (Result, ("combinations" ::: Vector FramebufferMixedSamplesCombinationNV))
getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNVPtr = pVkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV is null" Nothing Nothing
  let vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV' = mkVkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNVPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPCombinationCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV" (vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV' physicalDevice' (pPCombinationCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pCombinationCount <- lift $ peek @Word32 pPCombinationCount
  pPCombinations <- ContT $ bracket (callocBytes @FramebufferMixedSamplesCombinationNV ((fromIntegral (pCombinationCount)) * 32)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPCombinations `advancePtrBytes` (i * 32) :: Ptr FramebufferMixedSamplesCombinationNV) . ($ ())) [0..(fromIntegral (pCombinationCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV" (vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV' physicalDevice' (pPCombinationCount) ((pPCombinations)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pCombinationCount' <- lift $ peek @Word32 pPCombinationCount
  pCombinations' <- lift $ generateM (fromIntegral (pCombinationCount')) (\i -> peekCStruct @FramebufferMixedSamplesCombinationNV (((pPCombinations) `advancePtrBytes` (32 * (i)) :: Ptr FramebufferMixedSamplesCombinationNV)))
  pure $ ((r'), pCombinations')


-- | VkPhysicalDeviceCoverageReductionModeFeaturesNV - Structure describing
-- the coverage reduction mode features that can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceCoverageReductionModeFeaturesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceCoverageReductionModeFeaturesNV' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_coverage_reduction_mode VK_NV_coverage_reduction_mode>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCoverageReductionModeFeaturesNV = PhysicalDeviceCoverageReductionModeFeaturesNV
  { -- | #features-coverageReductionMode# @coverageReductionMode@ indicates
    -- whether the implementation supports coverage reduction modes. See
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragops-coverage-reduction Coverage Reduction>.
    coverageReductionMode :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCoverageReductionModeFeaturesNV)
#endif
deriving instance Show PhysicalDeviceCoverageReductionModeFeaturesNV

instance ToCStruct PhysicalDeviceCoverageReductionModeFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCoverageReductionModeFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (coverageReductionMode))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceCoverageReductionModeFeaturesNV where
  peekCStruct p = do
    coverageReductionMode <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceCoverageReductionModeFeaturesNV
             (bool32ToBool coverageReductionMode)

instance Storable PhysicalDeviceCoverageReductionModeFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCoverageReductionModeFeaturesNV where
  zero = PhysicalDeviceCoverageReductionModeFeaturesNV
           zero


-- | VkPipelineCoverageReductionStateCreateInfoNV - Structure specifying
-- parameters controlling coverage reduction
--
-- = Description
--
-- If this structure is not included in the @pNext@ chain, or if the
-- extension is not enabled, the default coverage reduction mode is
-- inferred as follows:
--
-- -   If the @VK_NV_framebuffer_mixed_samples@ extension is enabled, then
--     it is as if the @coverageReductionMode@ is
--     'COVERAGE_REDUCTION_MODE_MERGE_NV'.
--
-- -   If the @VK_AMD_mixed_attachment_samples@ extension is enabled, then
--     it is as if the @coverageReductionMode@ is
--     'COVERAGE_REDUCTION_MODE_TRUNCATE_NV'.
--
-- -   If both @VK_NV_framebuffer_mixed_samples@ and
--     @VK_AMD_mixed_attachment_samples@ are enabled, then the default
--     coverage reduction mode is implementation-dependent.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineCoverageReductionStateCreateInfoNV-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV'
--
-- -   #VUID-VkPipelineCoverageReductionStateCreateInfoNV-flags-zerobitmask#
--     @flags@ /must/ be @0@
--
-- -   #VUID-VkPipelineCoverageReductionStateCreateInfoNV-coverageReductionMode-parameter#
--     @coverageReductionMode@ /must/ be a valid 'CoverageReductionModeNV'
--     value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_coverage_reduction_mode VK_NV_coverage_reduction_mode>,
-- 'CoverageReductionModeNV',
-- 'PipelineCoverageReductionStateCreateFlagsNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineCoverageReductionStateCreateInfoNV = PipelineCoverageReductionStateCreateInfoNV
  { -- | @flags@ is reserved for future use.
    flags :: PipelineCoverageReductionStateCreateFlagsNV
  , -- | @coverageReductionMode@ is a 'CoverageReductionModeNV' value controlling
    -- how color sample coverage is generated from pixel coverage.
    coverageReductionMode :: CoverageReductionModeNV
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCoverageReductionStateCreateInfoNV)
#endif
deriving instance Show PipelineCoverageReductionStateCreateInfoNV

instance ToCStruct PipelineCoverageReductionStateCreateInfoNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineCoverageReductionStateCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineCoverageReductionStateCreateFlagsNV)) (flags)
    poke ((p `plusPtr` 20 :: Ptr CoverageReductionModeNV)) (coverageReductionMode)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr CoverageReductionModeNV)) (zero)
    f

instance FromCStruct PipelineCoverageReductionStateCreateInfoNV where
  peekCStruct p = do
    flags <- peek @PipelineCoverageReductionStateCreateFlagsNV ((p `plusPtr` 16 :: Ptr PipelineCoverageReductionStateCreateFlagsNV))
    coverageReductionMode <- peek @CoverageReductionModeNV ((p `plusPtr` 20 :: Ptr CoverageReductionModeNV))
    pure $ PipelineCoverageReductionStateCreateInfoNV
             flags coverageReductionMode

instance Storable PipelineCoverageReductionStateCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineCoverageReductionStateCreateInfoNV where
  zero = PipelineCoverageReductionStateCreateInfoNV
           zero
           zero


-- | VkFramebufferMixedSamplesCombinationNV - Structure specifying a
-- supported sample count combination
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_coverage_reduction_mode VK_NV_coverage_reduction_mode>,
-- 'CoverageReductionModeNV',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV'
data FramebufferMixedSamplesCombinationNV = FramebufferMixedSamplesCombinationNV
  { -- | @coverageReductionMode@ is a 'CoverageReductionModeNV' value specifying
    -- the coverage reduction mode.
    coverageReductionMode :: CoverageReductionModeNV
  , -- | @rasterizationSamples@ is a
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' specifying
    -- the number of rasterization samples in the supported combination.
    rasterizationSamples :: SampleCountFlagBits
  , -- | @depthStencilSamples@ specifies the number of samples in the depth
    -- stencil attachment in the supported combination. A value of 0 indicates
    -- the combination does not have a depth stencil attachment.
    depthStencilSamples :: SampleCountFlags
  , -- | @colorSamples@ specifies the number of color samples in a color
    -- attachment in the supported combination. A value of 0 indicates the
    -- combination does not have a color attachment.
    colorSamples :: SampleCountFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FramebufferMixedSamplesCombinationNV)
#endif
deriving instance Show FramebufferMixedSamplesCombinationNV

instance ToCStruct FramebufferMixedSamplesCombinationNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FramebufferMixedSamplesCombinationNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FRAMEBUFFER_MIXED_SAMPLES_COMBINATION_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CoverageReductionModeNV)) (coverageReductionMode)
    poke ((p `plusPtr` 20 :: Ptr SampleCountFlagBits)) (rasterizationSamples)
    poke ((p `plusPtr` 24 :: Ptr SampleCountFlags)) (depthStencilSamples)
    poke ((p `plusPtr` 28 :: Ptr SampleCountFlags)) (colorSamples)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FRAMEBUFFER_MIXED_SAMPLES_COMBINATION_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CoverageReductionModeNV)) (zero)
    poke ((p `plusPtr` 20 :: Ptr SampleCountFlagBits)) (zero)
    poke ((p `plusPtr` 24 :: Ptr SampleCountFlags)) (zero)
    poke ((p `plusPtr` 28 :: Ptr SampleCountFlags)) (zero)
    f

instance FromCStruct FramebufferMixedSamplesCombinationNV where
  peekCStruct p = do
    coverageReductionMode <- peek @CoverageReductionModeNV ((p `plusPtr` 16 :: Ptr CoverageReductionModeNV))
    rasterizationSamples <- peek @SampleCountFlagBits ((p `plusPtr` 20 :: Ptr SampleCountFlagBits))
    depthStencilSamples <- peek @SampleCountFlags ((p `plusPtr` 24 :: Ptr SampleCountFlags))
    colorSamples <- peek @SampleCountFlags ((p `plusPtr` 28 :: Ptr SampleCountFlags))
    pure $ FramebufferMixedSamplesCombinationNV
             coverageReductionMode rasterizationSamples depthStencilSamples colorSamples

instance Storable FramebufferMixedSamplesCombinationNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero FramebufferMixedSamplesCombinationNV where
  zero = FramebufferMixedSamplesCombinationNV
           zero
           zero
           zero
           zero


-- | VkPipelineCoverageReductionStateCreateFlagsNV - Reserved for future use
--
-- = Description
--
-- 'PipelineCoverageReductionStateCreateFlagsNV' is a bitmask type for
-- setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_coverage_reduction_mode VK_NV_coverage_reduction_mode>,
-- 'PipelineCoverageReductionStateCreateInfoNV'
newtype PipelineCoverageReductionStateCreateFlagsNV = PipelineCoverageReductionStateCreateFlagsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineCoverageReductionStateCreateFlagsNV :: String
conNamePipelineCoverageReductionStateCreateFlagsNV = "PipelineCoverageReductionStateCreateFlagsNV"

enumPrefixPipelineCoverageReductionStateCreateFlagsNV :: String
enumPrefixPipelineCoverageReductionStateCreateFlagsNV = ""

showTablePipelineCoverageReductionStateCreateFlagsNV :: [(PipelineCoverageReductionStateCreateFlagsNV, String)]
showTablePipelineCoverageReductionStateCreateFlagsNV = []

instance Show PipelineCoverageReductionStateCreateFlagsNV where
  showsPrec = enumShowsPrec enumPrefixPipelineCoverageReductionStateCreateFlagsNV
                            showTablePipelineCoverageReductionStateCreateFlagsNV
                            conNamePipelineCoverageReductionStateCreateFlagsNV
                            (\(PipelineCoverageReductionStateCreateFlagsNV x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read PipelineCoverageReductionStateCreateFlagsNV where
  readPrec = enumReadPrec enumPrefixPipelineCoverageReductionStateCreateFlagsNV
                          showTablePipelineCoverageReductionStateCreateFlagsNV
                          conNamePipelineCoverageReductionStateCreateFlagsNV
                          PipelineCoverageReductionStateCreateFlagsNV


-- | VkCoverageReductionModeNV - Specify the coverage reduction mode
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_coverage_reduction_mode VK_NV_coverage_reduction_mode>,
-- 'FramebufferMixedSamplesCombinationNV',
-- 'PipelineCoverageReductionStateCreateInfoNV'
newtype CoverageReductionModeNV = CoverageReductionModeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'COVERAGE_REDUCTION_MODE_MERGE_NV' specifies that each color sample will
-- be associated with an implementation-dependent subset of samples in the
-- pixel coverage. If any of those associated samples are covered, the
-- color sample is covered.
pattern COVERAGE_REDUCTION_MODE_MERGE_NV    = CoverageReductionModeNV 0
-- | 'COVERAGE_REDUCTION_MODE_TRUNCATE_NV' specifies that for color samples
-- present in the color attachments, a color sample is covered if the pixel
-- coverage sample with the same
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-multisampling-coverage-mask sample index>
-- i is covered; other pixel coverage samples are discarded.
pattern COVERAGE_REDUCTION_MODE_TRUNCATE_NV = CoverageReductionModeNV 1
{-# complete COVERAGE_REDUCTION_MODE_MERGE_NV,
             COVERAGE_REDUCTION_MODE_TRUNCATE_NV :: CoverageReductionModeNV #-}

conNameCoverageReductionModeNV :: String
conNameCoverageReductionModeNV = "CoverageReductionModeNV"

enumPrefixCoverageReductionModeNV :: String
enumPrefixCoverageReductionModeNV = "COVERAGE_REDUCTION_MODE_"

showTableCoverageReductionModeNV :: [(CoverageReductionModeNV, String)]
showTableCoverageReductionModeNV =
  [(COVERAGE_REDUCTION_MODE_MERGE_NV, "MERGE_NV"), (COVERAGE_REDUCTION_MODE_TRUNCATE_NV, "TRUNCATE_NV")]

instance Show CoverageReductionModeNV where
  showsPrec = enumShowsPrec enumPrefixCoverageReductionModeNV
                            showTableCoverageReductionModeNV
                            conNameCoverageReductionModeNV
                            (\(CoverageReductionModeNV x) -> x)
                            (showsPrec 11)

instance Read CoverageReductionModeNV where
  readPrec = enumReadPrec enumPrefixCoverageReductionModeNV
                          showTableCoverageReductionModeNV
                          conNameCoverageReductionModeNV
                          CoverageReductionModeNV


type NV_COVERAGE_REDUCTION_MODE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_COVERAGE_REDUCTION_MODE_SPEC_VERSION"
pattern NV_COVERAGE_REDUCTION_MODE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_COVERAGE_REDUCTION_MODE_SPEC_VERSION = 1


type NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME = "VK_NV_coverage_reduction_mode"

-- No documentation found for TopLevel "VK_NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME"
pattern NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME = "VK_NV_coverage_reduction_mode"

