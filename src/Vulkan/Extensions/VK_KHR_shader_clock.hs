{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_shader_clock  ( PhysicalDeviceShaderClockFeaturesKHR(..)
                                              , KHR_SHADER_CLOCK_SPEC_VERSION
                                              , pattern KHR_SHADER_CLOCK_SPEC_VERSION
                                              , KHR_SHADER_CLOCK_EXTENSION_NAME
                                              , pattern KHR_SHADER_CLOCK_EXTENSION_NAME
                                              ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.String (IsString)
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
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR))
-- | VkPhysicalDeviceShaderClockFeaturesKHR - Structure describing features
-- supported by VK_KHR_shader_clock
--
-- = Description
--
-- If the 'PhysicalDeviceShaderClockFeaturesKHR' structure is included in
-- the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceShaderClockFeaturesKHR' can also be included in the
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable the
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderClockFeaturesKHR = PhysicalDeviceShaderClockFeaturesKHR
  { -- | @shaderSubgroupClock@ indicates whether shaders /can/ perform @Subgroup@
    -- scoped clock reads.
    shaderSubgroupClock :: Bool
  , -- | @shaderDeviceClock@ indicates whether shaders /can/ perform
    -- 'Vulkan.Core10.Handles.Device' scoped clock reads.
    shaderDeviceClock :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderClockFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceShaderClockFeaturesKHR

instance ToCStruct PhysicalDeviceShaderClockFeaturesKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderClockFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderSubgroupClock))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (shaderDeviceClock))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderClockFeaturesKHR where
  peekCStruct p = do
    shaderSubgroupClock <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    shaderDeviceClock <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderClockFeaturesKHR
             (bool32ToBool shaderSubgroupClock) (bool32ToBool shaderDeviceClock)

instance Storable PhysicalDeviceShaderClockFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderClockFeaturesKHR where
  zero = PhysicalDeviceShaderClockFeaturesKHR
           zero
           zero


type KHR_SHADER_CLOCK_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_CLOCK_SPEC_VERSION"
pattern KHR_SHADER_CLOCK_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_CLOCK_SPEC_VERSION = 1


type KHR_SHADER_CLOCK_EXTENSION_NAME = "VK_KHR_shader_clock"

-- No documentation found for TopLevel "VK_KHR_SHADER_CLOCK_EXTENSION_NAME"
pattern KHR_SHADER_CLOCK_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_CLOCK_EXTENSION_NAME = "VK_KHR_shader_clock"

