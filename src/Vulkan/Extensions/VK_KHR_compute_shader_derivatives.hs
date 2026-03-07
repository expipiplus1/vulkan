{-# language CPP #-}
-- | = Name
--
-- VK_KHR_compute_shader_derivatives - device extension
--
-- = VK_KHR_compute_shader_derivatives
--
-- [__Name String__]
--     @VK_KHR_compute_shader_derivatives@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     512
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_compute_shader_derivatives.html SPV_KHR_compute_shader_derivatives>
--
-- [__Contact__]
--
--     -   Jean-Noe Morissette
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_compute_shader_derivatives] @MagicPoncho%0A*Here describe the issue or question you have about the VK_KHR_compute_shader_derivatives extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_compute_shader_derivatives.adoc VK_KHR_compute_shader_derivatives>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-06-26
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_compute_shader_derivatives.html SPV_KHR_compute_shader_derivatives>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/khr/GLSL_KHR_compute_shader_derivatives.txt GL_KHR_compute_shader_derivatives>
--
-- [__Contributors__]
--
--     -   Jean-Noe Morissette, Epic Games
--
--     -   Daniel Koch, NVIDIA
--
--     -   Pat Brown, NVIDIA
--
--     -   Stu Smith, AMD
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Tobias Hector, AMD
--
--     -   Ralph Potter, Samsung
--
--     -   Pan Gao, Huawei
--
--     -   Samuel (Sheng-Wen) Huang, MediaTek
--
--     -   Graeme Leese, Broadcom
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Matthew Netsh, Qualcomm
--
-- == Description
--
-- This extension adds Vulkan support for the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_compute_shader_derivatives.html SPV_KHR_compute_shader_derivatives>
-- SPIR-V extension.
--
-- The SPIR-V extension provides two new execution modes, both of which
-- allow execution models with defined workgroups to use built-ins that
-- evaluate derivatives explicitly or implicitly. Derivatives will be
-- computed via differencing over a 2x2 group of shader invocations. The
-- @DerivativeGroupQuadsKHR@ execution mode assembles shader invocations
-- into 2x2 groups, where each group has x and y coordinates of the local
-- invocation ID of the form (2m+{0,1}, 2n+{0,1}). The
-- @DerivativeGroupLinearKHR@ execution mode assembles shader invocations
-- into 2x2 groups, where each group has local invocation index values of
-- the form 4m+{0,1,2,3}.
--
-- The new execution modes are supported in compute shaders and optionally
-- (see
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-meshAndTaskShaderDerivatives meshAndTaskShaderDerivatives>)
-- in mesh and task shaders.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceComputeShaderDerivativesFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceComputeShaderDerivativesPropertiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME'
--
-- -   'KHR_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_PROPERTIES_KHR'
--
-- == New SPIR-V Capability
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-ComputeDerivativeGroupQuadsKHR ComputeDerivativeGroupQuadsKHR>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-ComputeDerivativeGroupLinearKHR ComputeDerivativeGroupLinearKHR>
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2023-02-27 (Jean-Noe Morissette)
--
--     -   Initial draft
--
--     -   Add properties and clarify mesh and task support (Daniel Koch)
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_compute_shader_derivatives Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_compute_shader_derivatives  ( PhysicalDeviceComputeShaderDerivativesFeaturesKHR(..)
                                                            , PhysicalDeviceComputeShaderDerivativesPropertiesKHR(..)
                                                            , KHR_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION
                                                            , pattern KHR_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION
                                                            , KHR_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME
                                                            , pattern KHR_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME
                                                            ) where

import Foreign.Marshal.Alloc (allocaBytes)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_PROPERTIES_KHR))
-- | VkPhysicalDeviceComputeShaderDerivativesFeaturesKHR - Structure
-- describing compute shader derivative features that can be supported by
-- an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- See
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-scope-quad Quad shader scope>
-- for more information.
--
-- If the 'PhysicalDeviceComputeShaderDerivativesFeaturesKHR'. structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceComputeShaderDerivativesFeaturesKHR'. /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_compute_shader_derivatives VK_KHR_compute_shader_derivatives>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceComputeShaderDerivativesFeaturesKHR = PhysicalDeviceComputeShaderDerivativesFeaturesKHR
  { -- | #features-computeDerivativeGroupQuads# @computeDerivativeGroupQuads@
    -- indicates that the implementation supports the
    -- @ComputeDerivativeGroupQuadsKHR@ SPIR-V capability.
    computeDerivativeGroupQuads :: Bool
  , -- | #features-computeDerivativeGroupLinear# @computeDerivativeGroupLinear@
    -- indicates that the implementation supports the
    -- @ComputeDerivativeGroupLinearKHR@ SPIR-V capability.
    computeDerivativeGroupLinear :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceComputeShaderDerivativesFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceComputeShaderDerivativesFeaturesKHR

instance ToCStruct PhysicalDeviceComputeShaderDerivativesFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceComputeShaderDerivativesFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (computeDerivativeGroupQuads))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (computeDerivativeGroupLinear))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceComputeShaderDerivativesFeaturesKHR where
  peekCStruct p = do
    computeDerivativeGroupQuads <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    computeDerivativeGroupLinear <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceComputeShaderDerivativesFeaturesKHR
             (bool32ToBool computeDerivativeGroupQuads)
             (bool32ToBool computeDerivativeGroupLinear)

instance Storable PhysicalDeviceComputeShaderDerivativesFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceComputeShaderDerivativesFeaturesKHR where
  zero = PhysicalDeviceComputeShaderDerivativesFeaturesKHR
           zero
           zero


-- | VkPhysicalDeviceComputeShaderDerivativesPropertiesKHR - Structure
-- describing compute shader derivative operations supported by an
-- implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceComputeShaderDerivativesPropertiesKHR'
-- structure describe the following:
--
-- = Description
--
-- If the 'PhysicalDeviceComputeShaderDerivativesPropertiesKHR' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_compute_shader_derivatives VK_KHR_compute_shader_derivatives>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceComputeShaderDerivativesPropertiesKHR = PhysicalDeviceComputeShaderDerivativesPropertiesKHR
  { -- | #limits-meshAndTaskShaderDerivatives# @meshAndTaskShaderDerivatives@
    -- indicates whether the mesh and task shader stages support the
    -- @ComputeDerivativeGroupQuadsKHR@ and @ComputeDerivativeGroupLinearKHR@
    -- SPIR-V capabilities.
    meshAndTaskShaderDerivatives :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceComputeShaderDerivativesPropertiesKHR)
#endif
deriving instance Show PhysicalDeviceComputeShaderDerivativesPropertiesKHR

instance ToCStruct PhysicalDeviceComputeShaderDerivativesPropertiesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceComputeShaderDerivativesPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (meshAndTaskShaderDerivatives))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceComputeShaderDerivativesPropertiesKHR where
  peekCStruct p = do
    meshAndTaskShaderDerivatives <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceComputeShaderDerivativesPropertiesKHR
             (bool32ToBool meshAndTaskShaderDerivatives)

instance Storable PhysicalDeviceComputeShaderDerivativesPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceComputeShaderDerivativesPropertiesKHR where
  zero = PhysicalDeviceComputeShaderDerivativesPropertiesKHR
           zero


type KHR_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION"
pattern KHR_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION = 1


type KHR_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME = "VK_KHR_compute_shader_derivatives"

-- No documentation found for TopLevel "VK_KHR_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME"
pattern KHR_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME = "VK_KHR_compute_shader_derivatives"

