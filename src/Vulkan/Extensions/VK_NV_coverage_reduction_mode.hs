{-# language CPP #-}
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

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
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
import Vulkan.NamedType ((:::))
import Vulkan.Core10.BaseType (Bool32)
import Vulkan.Core10.BaseType (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
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
-- @physicalDevice@, at most @pCombinationCount@ values will be written
-- @pCombinations@ and 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be
-- returned instead of 'Vulkan.Core10.Enums.Result.SUCCESS' to indicate
-- that not all the supported values were returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   @pCombinationCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pCombinationCount@ is not @0@, and
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
-- 'FramebufferMixedSamplesCombinationNV',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV :: forall io . MonadIO io => PhysicalDevice -> io (Result, ("combinations" ::: Vector FramebufferMixedSamplesCombinationNV))
getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNVPtr = pVkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV is null" Nothing Nothing
  let vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV' = mkVkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNVPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPCombinationCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV' physicalDevice' (pPCombinationCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pCombinationCount <- lift $ peek @Word32 pPCombinationCount
  pPCombinations <- ContT $ bracket (callocBytes @FramebufferMixedSamplesCombinationNV ((fromIntegral (pCombinationCount)) * 32)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPCombinations `advancePtrBytes` (i * 32) :: Ptr FramebufferMixedSamplesCombinationNV) . ($ ())) [0..(fromIntegral (pCombinationCount)) - 1]
  r' <- lift $ vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV' physicalDevice' (pPCombinationCount) ((pPCombinations))
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
-- The members of the 'PhysicalDeviceCoverageReductionModeFeaturesNV'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceCoverageReductionModeFeaturesNV' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceCoverageReductionModeFeaturesNV' /can/ also be included
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- enable the feature.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCoverageReductionModeFeaturesNV = PhysicalDeviceCoverageReductionModeFeaturesNV
  { -- | @coverageReductionMode@ indicates whether the implementation supports
    -- coverage reduction modes. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-coverage-reduction Coverage Reduction>.
    coverageReductionMode :: Bool }
  deriving (Typeable, Eq)
deriving instance Show PhysicalDeviceCoverageReductionModeFeaturesNV

instance ToCStruct PhysicalDeviceCoverageReductionModeFeaturesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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
-- If this structure is not present, the default coverage reduction mode is
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
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV'
--
-- -   @flags@ /must/ be @0@
--
-- -   @coverageReductionMode@ /must/ be a valid 'CoverageReductionModeNV'
--     value
--
-- = See Also
--
-- 'CoverageReductionModeNV',
-- 'PipelineCoverageReductionStateCreateFlagsNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineCoverageReductionStateCreateInfoNV = PipelineCoverageReductionStateCreateInfoNV
  { -- | @flags@ is reserved for future use.
    flags :: PipelineCoverageReductionStateCreateFlagsNV
  , -- | @coverageReductionMode@ is a 'CoverageReductionModeNV' value controlling
    -- how the /color sample mask/ is generated from the coverage mask.
    coverageReductionMode :: CoverageReductionModeNV
  }
  deriving (Typeable, Eq)
deriving instance Show PipelineCoverageReductionStateCreateInfoNV

instance ToCStruct PipelineCoverageReductionStateCreateInfoNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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
-- 'CoverageReductionModeNV',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV'
data FramebufferMixedSamplesCombinationNV = FramebufferMixedSamplesCombinationNV
  { -- | @coverageReductionMode@ is a 'CoverageReductionModeNV' value specifying
    -- the coverage reduction mode.
    coverageReductionMode :: CoverageReductionModeNV
  , -- | @rasterizationSamples@ specifies the number of rasterization samples in
    -- the supported combination.
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
deriving instance Show FramebufferMixedSamplesCombinationNV

instance ToCStruct FramebufferMixedSamplesCombinationNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
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
-- 'PipelineCoverageReductionStateCreateInfoNV'
newtype PipelineCoverageReductionStateCreateFlagsNV = PipelineCoverageReductionStateCreateFlagsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show PipelineCoverageReductionStateCreateFlagsNV where
  showsPrec p = \case
    PipelineCoverageReductionStateCreateFlagsNV x -> showParen (p >= 11) (showString "PipelineCoverageReductionStateCreateFlagsNV 0x" . showHex x)

instance Read PipelineCoverageReductionStateCreateFlagsNV where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineCoverageReductionStateCreateFlagsNV")
                       v <- step readPrec
                       pure (PipelineCoverageReductionStateCreateFlagsNV v)))


-- | VkCoverageReductionModeNV - Specify the coverage reduction mode
--
-- = See Also
--
-- 'FramebufferMixedSamplesCombinationNV',
-- 'PipelineCoverageReductionStateCreateInfoNV'
newtype CoverageReductionModeNV = CoverageReductionModeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'COVERAGE_REDUCTION_MODE_MERGE_NV': In this mode, there is an
-- implementation-dependent association of each coverage sample to a color
-- sample. The reduced color sample mask is computed such that the bit for
-- each color sample is 1 if any of the associated bits in the fragment’s
-- coverage is on, and 0 otherwise.
pattern COVERAGE_REDUCTION_MODE_MERGE_NV = CoverageReductionModeNV 0
-- | 'COVERAGE_REDUCTION_MODE_TRUNCATE_NV': In this mode, only the first M
-- coverage samples are associated with the color samples such that
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-multisampling-coverage-mask sample index>
-- i maps to color sample index i, where M is the number of color samples.
pattern COVERAGE_REDUCTION_MODE_TRUNCATE_NV = CoverageReductionModeNV 1
{-# complete COVERAGE_REDUCTION_MODE_MERGE_NV,
             COVERAGE_REDUCTION_MODE_TRUNCATE_NV :: CoverageReductionModeNV #-}

instance Show CoverageReductionModeNV where
  showsPrec p = \case
    COVERAGE_REDUCTION_MODE_MERGE_NV -> showString "COVERAGE_REDUCTION_MODE_MERGE_NV"
    COVERAGE_REDUCTION_MODE_TRUNCATE_NV -> showString "COVERAGE_REDUCTION_MODE_TRUNCATE_NV"
    CoverageReductionModeNV x -> showParen (p >= 11) (showString "CoverageReductionModeNV " . showsPrec 11 x)

instance Read CoverageReductionModeNV where
  readPrec = parens (choose [("COVERAGE_REDUCTION_MODE_MERGE_NV", pure COVERAGE_REDUCTION_MODE_MERGE_NV)
                            , ("COVERAGE_REDUCTION_MODE_TRUNCATE_NV", pure COVERAGE_REDUCTION_MODE_TRUNCATE_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "CoverageReductionModeNV")
                       v <- step readPrec
                       pure (CoverageReductionModeNV v)))


type NV_COVERAGE_REDUCTION_MODE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_COVERAGE_REDUCTION_MODE_SPEC_VERSION"
pattern NV_COVERAGE_REDUCTION_MODE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_COVERAGE_REDUCTION_MODE_SPEC_VERSION = 1


type NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME = "VK_NV_coverage_reduction_mode"

-- No documentation found for TopLevel "VK_NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME"
pattern NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME = "VK_NV_coverage_reduction_mode"

