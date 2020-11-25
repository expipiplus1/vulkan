{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_sampler_filter_minmax"
module Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax  ( PhysicalDeviceSamplerFilterMinmaxProperties(..)
                                                                 , SamplerReductionModeCreateInfo(..)
                                                                 , StructureType(..)
                                                                 , FormatFeatureFlagBits(..)
                                                                 , FormatFeatureFlags
                                                                 , SamplerReductionMode(..)
                                                                 ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core12.Enums.SamplerReductionMode (SamplerReductionMode)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(..))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core12.Enums.SamplerReductionMode (SamplerReductionMode(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))

-- No documentation found for TopLevel "VkPhysicalDeviceSamplerFilterMinmaxProperties"
data PhysicalDeviceSamplerFilterMinmaxProperties = PhysicalDeviceSamplerFilterMinmaxProperties
  { -- No documentation found for Nested "VkPhysicalDeviceSamplerFilterMinmaxProperties" "filterMinmaxSingleComponentFormats"
    filterMinmaxSingleComponentFormats :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceSamplerFilterMinmaxProperties" "filterMinmaxImageComponentMapping"
    filterMinmaxImageComponentMapping :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSamplerFilterMinmaxProperties)
#endif
deriving instance Show PhysicalDeviceSamplerFilterMinmaxProperties

instance ToCStruct PhysicalDeviceSamplerFilterMinmaxProperties where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSamplerFilterMinmaxProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (filterMinmaxSingleComponentFormats))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (filterMinmaxImageComponentMapping))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceSamplerFilterMinmaxProperties where
  peekCStruct p = do
    filterMinmaxSingleComponentFormats <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    filterMinmaxImageComponentMapping <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceSamplerFilterMinmaxProperties
             (bool32ToBool filterMinmaxSingleComponentFormats) (bool32ToBool filterMinmaxImageComponentMapping)


instance Storable PhysicalDeviceSamplerFilterMinmaxProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSamplerFilterMinmaxProperties where
  zero = PhysicalDeviceSamplerFilterMinmaxProperties
           zero
           zero



-- No documentation found for TopLevel "VkSamplerReductionModeCreateInfo"
data SamplerReductionModeCreateInfo = SamplerReductionModeCreateInfo
  { -- No documentation found for Nested "VkSamplerReductionModeCreateInfo" "reductionMode"
    reductionMode :: SamplerReductionMode }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SamplerReductionModeCreateInfo)
#endif
deriving instance Show SamplerReductionModeCreateInfo

instance ToCStruct SamplerReductionModeCreateInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SamplerReductionModeCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SamplerReductionMode)) (reductionMode)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SamplerReductionMode)) (zero)
    f

instance FromCStruct SamplerReductionModeCreateInfo where
  peekCStruct p = do
    reductionMode <- peek @SamplerReductionMode ((p `plusPtr` 16 :: Ptr SamplerReductionMode))
    pure $ SamplerReductionModeCreateInfo
             reductionMode


instance Storable SamplerReductionModeCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SamplerReductionModeCreateInfo where
  zero = SamplerReductionModeCreateInfo
           zero

