{-# language CPP #-}
module Vulkan.Extensions.VK_NV_compute_shader_derivatives  ( PhysicalDeviceComputeShaderDerivativesFeaturesNV(..)
                                                           , NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION
                                                           , pattern NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION
                                                           , NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME
                                                           , pattern NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV))
-- | VkPhysicalDeviceComputeShaderDerivativesFeaturesNV - Structure
-- describing compute shader derivative features that can be supported by
-- an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceComputeShaderDerivativesFeaturesNV'
-- structure describe the following features:
--
-- = Description
--
-- -   #features-computeDerivativeGroupQuads# @computeDerivativeGroupQuads@
--     indicates that the implementation supports the
--     @ComputeDerivativeGroupQuadsNV@ SPIR-V capability.
--
-- -   #features-computeDerivativeGroupLinear#
--     @computeDerivativeGroupLinear@ indicates that the implementation
--     supports the @ComputeDerivativeGroupLinearNV@ SPIR-V capability.
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-scope-quad>
-- chapter for more information.
--
-- If the 'PhysicalDeviceComputeShaderDerivativesFeaturesNV' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceComputeShaderDerivativesFeaturesNV' /can/ also be
-- included in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to enable features.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceComputeShaderDerivativesFeaturesNV-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceComputeShaderDerivativesFeaturesNV = PhysicalDeviceComputeShaderDerivativesFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceComputeShaderDerivativesFeaturesNV" "computeDerivativeGroupQuads"
    computeDerivativeGroupQuads :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceComputeShaderDerivativesFeaturesNV" "computeDerivativeGroupLinear"
    computeDerivativeGroupLinear :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceComputeShaderDerivativesFeaturesNV)
#endif
deriving instance Show PhysicalDeviceComputeShaderDerivativesFeaturesNV

instance ToCStruct PhysicalDeviceComputeShaderDerivativesFeaturesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceComputeShaderDerivativesFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (computeDerivativeGroupQuads))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (computeDerivativeGroupLinear))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceComputeShaderDerivativesFeaturesNV where
  peekCStruct p = do
    computeDerivativeGroupQuads <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    computeDerivativeGroupLinear <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceComputeShaderDerivativesFeaturesNV
             (bool32ToBool computeDerivativeGroupQuads) (bool32ToBool computeDerivativeGroupLinear)

instance Storable PhysicalDeviceComputeShaderDerivativesFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceComputeShaderDerivativesFeaturesNV where
  zero = PhysicalDeviceComputeShaderDerivativesFeaturesNV
           zero
           zero


type NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION"
pattern NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION :: forall a . Integral a => a
pattern NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION = 1


type NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME = "VK_NV_compute_shader_derivatives"

-- No documentation found for TopLevel "VK_NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME"
pattern NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME = "VK_NV_compute_shader_derivatives"

