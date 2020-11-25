{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_coverage_reduction_mode"
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
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
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

-- No documentation found for TopLevel "vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV"
getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV :: forall io
                                                                 . (MonadIO io)
                                                                => -- No documentation found for Nested "vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV" "physicalDevice"
                                                                   PhysicalDevice
                                                                -> io (Result, ("combinations" ::: Vector FramebufferMixedSamplesCombinationNV))
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



-- No documentation found for TopLevel "VkPhysicalDeviceCoverageReductionModeFeaturesNV"
data PhysicalDeviceCoverageReductionModeFeaturesNV = PhysicalDeviceCoverageReductionModeFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceCoverageReductionModeFeaturesNV" "coverageReductionMode"
    coverageReductionMode :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCoverageReductionModeFeaturesNV)
#endif
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



-- No documentation found for TopLevel "VkPipelineCoverageReductionStateCreateInfoNV"
data PipelineCoverageReductionStateCreateInfoNV = PipelineCoverageReductionStateCreateInfoNV
  { -- No documentation found for Nested "VkPipelineCoverageReductionStateCreateInfoNV" "flags"
    flags :: PipelineCoverageReductionStateCreateFlagsNV
  , -- No documentation found for Nested "VkPipelineCoverageReductionStateCreateInfoNV" "coverageReductionMode"
    coverageReductionMode :: CoverageReductionModeNV
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCoverageReductionStateCreateInfoNV)
#endif
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



-- No documentation found for TopLevel "VkFramebufferMixedSamplesCombinationNV"
data FramebufferMixedSamplesCombinationNV = FramebufferMixedSamplesCombinationNV
  { -- No documentation found for Nested "VkFramebufferMixedSamplesCombinationNV" "coverageReductionMode"
    coverageReductionMode :: CoverageReductionModeNV
  , -- No documentation found for Nested "VkFramebufferMixedSamplesCombinationNV" "rasterizationSamples"
    rasterizationSamples :: SampleCountFlagBits
  , -- No documentation found for Nested "VkFramebufferMixedSamplesCombinationNV" "depthStencilSamples"
    depthStencilSamples :: SampleCountFlags
  , -- No documentation found for Nested "VkFramebufferMixedSamplesCombinationNV" "colorSamples"
    colorSamples :: SampleCountFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FramebufferMixedSamplesCombinationNV)
#endif
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


-- No documentation found for TopLevel "VkPipelineCoverageReductionStateCreateFlagsNV"
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


-- No documentation found for TopLevel "VkCoverageReductionModeNV"
newtype CoverageReductionModeNV = CoverageReductionModeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkCoverageReductionModeNV" "VK_COVERAGE_REDUCTION_MODE_MERGE_NV"
pattern COVERAGE_REDUCTION_MODE_MERGE_NV    = CoverageReductionModeNV 0
-- No documentation found for Nested "VkCoverageReductionModeNV" "VK_COVERAGE_REDUCTION_MODE_TRUNCATE_NV"
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

