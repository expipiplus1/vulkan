{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_conservative_rasterization"
module Vulkan.Extensions.VK_EXT_conservative_rasterization  ( PhysicalDeviceConservativeRasterizationPropertiesEXT(..)
                                                            , PipelineRasterizationConservativeStateCreateInfoEXT(..)
                                                            , PipelineRasterizationConservativeStateCreateFlagsEXT(..)
                                                            , ConservativeRasterizationModeEXT( CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT
                                                                                              , CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT
                                                                                              , CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT
                                                                                              , ..
                                                                                              )
                                                            , EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION
                                                            , pattern EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION
                                                            , EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
                                                            , pattern EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
                                                            ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT))

-- No documentation found for TopLevel "VkPhysicalDeviceConservativeRasterizationPropertiesEXT"
data PhysicalDeviceConservativeRasterizationPropertiesEXT = PhysicalDeviceConservativeRasterizationPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "primitiveOverestimationSize"
    primitiveOverestimationSize :: Float
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "maxExtraPrimitiveOverestimationSize"
    maxExtraPrimitiveOverestimationSize :: Float
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "extraPrimitiveOverestimationSizeGranularity"
    extraPrimitiveOverestimationSizeGranularity :: Float
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "primitiveUnderestimation"
    primitiveUnderestimation :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "conservativePointAndLineRasterization"
    conservativePointAndLineRasterization :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "degenerateTrianglesRasterized"
    degenerateTrianglesRasterized :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "degenerateLinesRasterized"
    degenerateLinesRasterized :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "fullyCoveredFragmentShaderInputVariable"
    fullyCoveredFragmentShaderInputVariable :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "conservativeRasterizationPostDepthCoverage"
    conservativeRasterizationPostDepthCoverage :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceConservativeRasterizationPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceConservativeRasterizationPropertiesEXT

instance ToCStruct PhysicalDeviceConservativeRasterizationPropertiesEXT where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceConservativeRasterizationPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (primitiveOverestimationSize))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (maxExtraPrimitiveOverestimationSize))
    poke ((p `plusPtr` 24 :: Ptr CFloat)) (CFloat (extraPrimitiveOverestimationSizeGranularity))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (primitiveUnderestimation))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (conservativePointAndLineRasterization))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (degenerateTrianglesRasterized))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (degenerateLinesRasterized))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (fullyCoveredFragmentShaderInputVariable))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (conservativeRasterizationPostDepthCoverage))
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 24 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceConservativeRasterizationPropertiesEXT where
  peekCStruct p = do
    primitiveOverestimationSize <- peek @CFloat ((p `plusPtr` 16 :: Ptr CFloat))
    maxExtraPrimitiveOverestimationSize <- peek @CFloat ((p `plusPtr` 20 :: Ptr CFloat))
    extraPrimitiveOverestimationSizeGranularity <- peek @CFloat ((p `plusPtr` 24 :: Ptr CFloat))
    primitiveUnderestimation <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    conservativePointAndLineRasterization <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    degenerateTrianglesRasterized <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    degenerateLinesRasterized <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    fullyCoveredFragmentShaderInputVariable <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    conservativeRasterizationPostDepthCoverage <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    pure $ PhysicalDeviceConservativeRasterizationPropertiesEXT
             ((\(CFloat a) -> a) primitiveOverestimationSize) ((\(CFloat a) -> a) maxExtraPrimitiveOverestimationSize) ((\(CFloat a) -> a) extraPrimitiveOverestimationSizeGranularity) (bool32ToBool primitiveUnderestimation) (bool32ToBool conservativePointAndLineRasterization) (bool32ToBool degenerateTrianglesRasterized) (bool32ToBool degenerateLinesRasterized) (bool32ToBool fullyCoveredFragmentShaderInputVariable) (bool32ToBool conservativeRasterizationPostDepthCoverage)


instance Storable PhysicalDeviceConservativeRasterizationPropertiesEXT where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceConservativeRasterizationPropertiesEXT where
  zero = PhysicalDeviceConservativeRasterizationPropertiesEXT
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkPipelineRasterizationConservativeStateCreateInfoEXT"
data PipelineRasterizationConservativeStateCreateInfoEXT = PipelineRasterizationConservativeStateCreateInfoEXT
  { -- No documentation found for Nested "VkPipelineRasterizationConservativeStateCreateInfoEXT" "flags"
    flags :: PipelineRasterizationConservativeStateCreateFlagsEXT
  , -- No documentation found for Nested "VkPipelineRasterizationConservativeStateCreateInfoEXT" "conservativeRasterizationMode"
    conservativeRasterizationMode :: ConservativeRasterizationModeEXT
  , -- No documentation found for Nested "VkPipelineRasterizationConservativeStateCreateInfoEXT" "extraPrimitiveOverestimationSize"
    extraPrimitiveOverestimationSize :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineRasterizationConservativeStateCreateInfoEXT)
#endif
deriving instance Show PipelineRasterizationConservativeStateCreateInfoEXT

instance ToCStruct PipelineRasterizationConservativeStateCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineRasterizationConservativeStateCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineRasterizationConservativeStateCreateFlagsEXT)) (flags)
    poke ((p `plusPtr` 20 :: Ptr ConservativeRasterizationModeEXT)) (conservativeRasterizationMode)
    poke ((p `plusPtr` 24 :: Ptr CFloat)) (CFloat (extraPrimitiveOverestimationSize))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr ConservativeRasterizationModeEXT)) (zero)
    poke ((p `plusPtr` 24 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct PipelineRasterizationConservativeStateCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @PipelineRasterizationConservativeStateCreateFlagsEXT ((p `plusPtr` 16 :: Ptr PipelineRasterizationConservativeStateCreateFlagsEXT))
    conservativeRasterizationMode <- peek @ConservativeRasterizationModeEXT ((p `plusPtr` 20 :: Ptr ConservativeRasterizationModeEXT))
    extraPrimitiveOverestimationSize <- peek @CFloat ((p `plusPtr` 24 :: Ptr CFloat))
    pure $ PipelineRasterizationConservativeStateCreateInfoEXT
             flags conservativeRasterizationMode ((\(CFloat a) -> a) extraPrimitiveOverestimationSize)


instance Storable PipelineRasterizationConservativeStateCreateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineRasterizationConservativeStateCreateInfoEXT where
  zero = PipelineRasterizationConservativeStateCreateInfoEXT
           zero
           zero
           zero


-- No documentation found for TopLevel "VkPipelineRasterizationConservativeStateCreateFlagsEXT"
newtype PipelineRasterizationConservativeStateCreateFlagsEXT = PipelineRasterizationConservativeStateCreateFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineRasterizationConservativeStateCreateFlagsEXT :: String
conNamePipelineRasterizationConservativeStateCreateFlagsEXT = "PipelineRasterizationConservativeStateCreateFlagsEXT"

enumPrefixPipelineRasterizationConservativeStateCreateFlagsEXT :: String
enumPrefixPipelineRasterizationConservativeStateCreateFlagsEXT = ""

showTablePipelineRasterizationConservativeStateCreateFlagsEXT
  :: [(PipelineRasterizationConservativeStateCreateFlagsEXT, String)]
showTablePipelineRasterizationConservativeStateCreateFlagsEXT = []


instance Show PipelineRasterizationConservativeStateCreateFlagsEXT where
showsPrec = enumShowsPrec enumPrefixPipelineRasterizationConservativeStateCreateFlagsEXT
                          showTablePipelineRasterizationConservativeStateCreateFlagsEXT
                          conNamePipelineRasterizationConservativeStateCreateFlagsEXT
                          (\(PipelineRasterizationConservativeStateCreateFlagsEXT x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read PipelineRasterizationConservativeStateCreateFlagsEXT where
  readPrec = enumReadPrec enumPrefixPipelineRasterizationConservativeStateCreateFlagsEXT
                          showTablePipelineRasterizationConservativeStateCreateFlagsEXT
                          conNamePipelineRasterizationConservativeStateCreateFlagsEXT
                          PipelineRasterizationConservativeStateCreateFlagsEXT


-- No documentation found for TopLevel "VkConservativeRasterizationModeEXT"
newtype ConservativeRasterizationModeEXT = ConservativeRasterizationModeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkConservativeRasterizationModeEXT" "VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT"
pattern CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT      = ConservativeRasterizationModeEXT 0
-- No documentation found for Nested "VkConservativeRasterizationModeEXT" "VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT"
pattern CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT  = ConservativeRasterizationModeEXT 1
-- No documentation found for Nested "VkConservativeRasterizationModeEXT" "VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT"
pattern CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT = ConservativeRasterizationModeEXT 2
{-# complete CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT,
             CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT,
             CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT :: ConservativeRasterizationModeEXT #-}

conNameConservativeRasterizationModeEXT :: String
conNameConservativeRasterizationModeEXT = "ConservativeRasterizationModeEXT"

enumPrefixConservativeRasterizationModeEXT :: String
enumPrefixConservativeRasterizationModeEXT = "CONSERVATIVE_RASTERIZATION_MODE_"

showTableConservativeRasterizationModeEXT :: [(ConservativeRasterizationModeEXT, String)]
showTableConservativeRasterizationModeEXT =
  [ (CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT     , "DISABLED_EXT")
  , (CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT , "OVERESTIMATE_EXT")
  , (CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT, "UNDERESTIMATE_EXT")
  ]


instance Show ConservativeRasterizationModeEXT where
showsPrec = enumShowsPrec enumPrefixConservativeRasterizationModeEXT
                          showTableConservativeRasterizationModeEXT
                          conNameConservativeRasterizationModeEXT
                          (\(ConservativeRasterizationModeEXT x) -> x)
                          (showsPrec 11)


instance Read ConservativeRasterizationModeEXT where
  readPrec = enumReadPrec enumPrefixConservativeRasterizationModeEXT
                          showTableConservativeRasterizationModeEXT
                          conNameConservativeRasterizationModeEXT
                          ConservativeRasterizationModeEXT


type EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION"
pattern EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION = 1


type EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME = "VK_EXT_conservative_rasterization"

-- No documentation found for TopLevel "VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME"
pattern EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME = "VK_EXT_conservative_rasterization"

