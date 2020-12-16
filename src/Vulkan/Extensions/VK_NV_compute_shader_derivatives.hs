{-# language CPP #-}
-- | = Name
--
-- VK_NV_compute_shader_derivatives - device extension
--
-- == VK_NV_compute_shader_derivatives
--
-- [__Name String__]
--     @VK_NV_compute_shader_derivatives@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     202
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Pat Brown
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NV_compute_shader_derivatives:%20&body=@nvpbrown%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-07-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_compute_shader_derivatives.html SPV_NV_compute_shader_derivatives>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nv/GLSL_NV_compute_shader_derivatives.txt GL_NV_compute_shader_derivatives>
--
-- [__Contributors__]
--
--     -   Pat Brown, NVIDIA
--
-- == Description
--
-- This extension adds Vulkan support for the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_compute_shader_derivatives.html SPV_NV_compute_shader_derivatives>
-- SPIR-V extension.
--
-- The SPIR-V extension provides two new execution modes, both of which
-- allow compute shaders to use built-ins that evaluate compute derivatives
-- explicitly or implicitly. Derivatives will be computed via differencing
-- over a 2x2 group of shader invocations. The @DerivativeGroupQuadsNV@
-- execution mode assembles shader invocations into 2x2 groups, where each
-- group has x and y coordinates of the local invocation ID of the form
-- (2m+{0,1}, 2n+{0,1}). The @DerivativeGroupLinearNV@ execution mode
-- assembles shader invocations into 2x2 groups, where each group has local
-- invocation index values of the form 4m+{0,1,2,3}.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceComputeShaderDerivativesFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME'
--
-- -   'NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV'
--
-- == New SPIR-V Capability
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-ComputeDerivativeGroupQuadsNV ComputeDerivativeGroupQuadsNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-ComputeDerivativeGroupLinearNV ComputeDerivativeGroupLinearNV>
--
-- == Issues
--
-- (1) Should we specify that the groups of four shader invocations used
-- for derivatives in a compute shader are the same groups of four
-- invocations that form a “quad” in shader subgroups?
--
-- __RESOLVED__: Yes.
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2018-07-19 (Pat Brown)
--
--     -   Initial draft
--
-- = See Also
--
-- 'PhysicalDeviceComputeShaderDerivativesFeaturesNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_compute_shader_derivatives Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_compute_shader_derivatives  ( PhysicalDeviceComputeShaderDerivativesFeaturesNV(..)
                                                           , NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION
                                                           , pattern NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION
                                                           , NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME
                                                           , pattern NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME
                                                           ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Core10.Enums.StructureType (StructureType)
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
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceComputeShaderDerivativesFeaturesNV = PhysicalDeviceComputeShaderDerivativesFeaturesNV
  { -- | #features-computeDerivativeGroupQuads# @computeDerivativeGroupQuads@
    -- indicates that the implementation supports the
    -- @ComputeDerivativeGroupQuadsNV@ SPIR-V capability.
    computeDerivativeGroupQuads :: Bool
  , -- | #features-computeDerivativeGroupLinear# @computeDerivativeGroupLinear@
    -- indicates that the implementation supports the
    -- @ComputeDerivativeGroupLinearNV@ SPIR-V capability.
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

