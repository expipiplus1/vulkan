{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_fragment_shading_rate_enums"
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
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

-- No documentation found for TopLevel "vkCmdSetFragmentShadingRateEnumNV"
cmdSetFragmentShadingRateEnumNV :: forall io
                                 . (MonadIO io)
                                => -- No documentation found for Nested "vkCmdSetFragmentShadingRateEnumNV" "commandBuffer"
                                   CommandBuffer
                                -> -- No documentation found for Nested "vkCmdSetFragmentShadingRateEnumNV" "shadingRate"
                                   FragmentShadingRateNV
                                -> -- No documentation found for Nested "vkCmdSetFragmentShadingRateEnumNV" "combinerOps"
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
  lift $ vkCmdSetFragmentShadingRateEnumNV' (commandBufferHandle (commandBuffer)) (shadingRate) (pCombinerOps)
  pure $ ()



-- No documentation found for TopLevel "VkPhysicalDeviceFragmentShadingRateEnumsFeaturesNV"
data PhysicalDeviceFragmentShadingRateEnumsFeaturesNV = PhysicalDeviceFragmentShadingRateEnumsFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRateEnumsFeaturesNV" "fragmentShadingRateEnums"
    fragmentShadingRateEnums :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRateEnumsFeaturesNV" "supersampleFragmentShadingRates"
    supersampleFragmentShadingRates :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRateEnumsFeaturesNV" "noInvocationFragmentShadingRates"
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



-- No documentation found for TopLevel "VkPhysicalDeviceFragmentShadingRateEnumsPropertiesNV"
data PhysicalDeviceFragmentShadingRateEnumsPropertiesNV = PhysicalDeviceFragmentShadingRateEnumsPropertiesNV
  { -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRateEnumsPropertiesNV" "maxFragmentShadingRateInvocationCount"
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



-- No documentation found for TopLevel "VkPipelineFragmentShadingRateEnumStateCreateInfoNV"
data PipelineFragmentShadingRateEnumStateCreateInfoNV = PipelineFragmentShadingRateEnumStateCreateInfoNV
  { -- No documentation found for Nested "VkPipelineFragmentShadingRateEnumStateCreateInfoNV" "shadingRateType"
    shadingRateType :: FragmentShadingRateTypeNV
  , -- No documentation found for Nested "VkPipelineFragmentShadingRateEnumStateCreateInfoNV" "shadingRate"
    shadingRate :: FragmentShadingRateNV
  , -- No documentation found for Nested "VkPipelineFragmentShadingRateEnumStateCreateInfoNV" "combinerOps"
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


-- No documentation found for TopLevel "VkFragmentShadingRateNV"
newtype FragmentShadingRateNV = FragmentShadingRateNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkFragmentShadingRateNV" "VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV"
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV      = FragmentShadingRateNV 0
-- No documentation found for Nested "VkFragmentShadingRateNV" "VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_1X2_PIXELS_NV"
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_1X2_PIXELS_NV = FragmentShadingRateNV 1
-- No documentation found for Nested "VkFragmentShadingRateNV" "VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X1_PIXELS_NV"
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X1_PIXELS_NV = FragmentShadingRateNV 4
-- No documentation found for Nested "VkFragmentShadingRateNV" "VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X2_PIXELS_NV"
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X2_PIXELS_NV = FragmentShadingRateNV 5
-- No documentation found for Nested "VkFragmentShadingRateNV" "VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X4_PIXELS_NV"
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X4_PIXELS_NV = FragmentShadingRateNV 6
-- No documentation found for Nested "VkFragmentShadingRateNV" "VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X2_PIXELS_NV"
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X2_PIXELS_NV = FragmentShadingRateNV 9
-- No documentation found for Nested "VkFragmentShadingRateNV" "VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X4_PIXELS_NV"
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X4_PIXELS_NV = FragmentShadingRateNV 10
-- No documentation found for Nested "VkFragmentShadingRateNV" "VK_FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV"
pattern FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV     = FragmentShadingRateNV 11
-- No documentation found for Nested "VkFragmentShadingRateNV" "VK_FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV"
pattern FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV     = FragmentShadingRateNV 12
-- No documentation found for Nested "VkFragmentShadingRateNV" "VK_FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV"
pattern FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV     = FragmentShadingRateNV 13
-- No documentation found for Nested "VkFragmentShadingRateNV" "VK_FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV"
pattern FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV    = FragmentShadingRateNV 14
-- No documentation found for Nested "VkFragmentShadingRateNV" "VK_FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV"
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


-- No documentation found for TopLevel "VkFragmentShadingRateTypeNV"
newtype FragmentShadingRateTypeNV = FragmentShadingRateTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkFragmentShadingRateTypeNV" "VK_FRAGMENT_SHADING_RATE_TYPE_FRAGMENT_SIZE_NV"
pattern FRAGMENT_SHADING_RATE_TYPE_FRAGMENT_SIZE_NV = FragmentShadingRateTypeNV 0
-- No documentation found for Nested "VkFragmentShadingRateTypeNV" "VK_FRAGMENT_SHADING_RATE_TYPE_ENUMS_NV"
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

