{-# language CPP #-}
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
import Vulkan.Core10.BaseType (bool32ToBool)
import Vulkan.Core10.BaseType (boolToBool32)
import Vulkan.Core10.BaseType (Bool32)
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
-- | VkPhysicalDeviceSamplerFilterMinmaxProperties - Structure describing
-- sampler filter minmax limits that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceSamplerFilterMinmaxProperties'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceSamplerFilterMinmaxProperties' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- If @filterMinmaxSingleComponentFormats@ is
-- 'Vulkan.Core10.BaseType.TRUE', the following formats /must/ support the
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT'
-- feature with 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', if
-- they support
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_BIT'.
--
-- If the format is a depth\/stencil format, this bit only specifies that
-- the depth aspect (not the stencil aspect) of an image of this format
-- supports min\/max filtering, and that min\/max filtering of the depth
-- aspect is supported when depth compare is disabled in the sampler.
--
-- If @filterMinmaxImageComponentMapping@ is 'Vulkan.Core10.BaseType.FALSE'
-- the component mapping of the image view used with min\/max filtering
-- /must/ have been created with the @r@ component set to
-- 'Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_IDENTITY'. Only
-- the @r@ component of the sampled image value is defined and the other
-- component values are undefined. If @filterMinmaxImageComponentMapping@
-- is 'Vulkan.Core10.BaseType.TRUE' this restriction does not apply and
-- image component mapping works as normal.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSamplerFilterMinmaxProperties = PhysicalDeviceSamplerFilterMinmaxProperties
  { -- | @filterMinmaxSingleComponentFormats@ is a boolean value indicating
    -- whether a minimum set of required formats support min\/max filtering.
    filterMinmaxSingleComponentFormats :: Bool
  , -- | @filterMinmaxImageComponentMapping@ is a boolean value indicating
    -- whether the implementation supports non-identity component mapping of
    -- the image when doing min\/max filtering.
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


-- | VkSamplerReductionModeCreateInfo - Structure specifying sampler
-- reduction mode
--
-- = Description
--
-- If the @pNext@ chain of 'Vulkan.Core10.Sampler.SamplerCreateInfo'
-- includes a 'SamplerReductionModeCreateInfo' structure, then that
-- structure includes a mode that controls how texture filtering combines
-- texel values.
--
-- If this structure is not present, @reductionMode@ is considered to be
-- 'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core12.Enums.SamplerReductionMode.SamplerReductionMode',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SamplerReductionModeCreateInfo = SamplerReductionModeCreateInfo
  { -- | @reductionMode@ is a
    -- 'Vulkan.Core12.Enums.SamplerReductionMode.SamplerReductionMode' value
    -- controlling how texture filtering combines texel values.
    --
    -- @reductionMode@ /must/ be a valid
    -- 'Vulkan.Core12.Enums.SamplerReductionMode.SamplerReductionMode' value
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

