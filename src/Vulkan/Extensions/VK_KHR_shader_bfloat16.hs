{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_bfloat16 - device extension
--
-- = VK_KHR_shader_bfloat16
--
-- [__Name String__]
--     @VK_KHR_shader_bfloat16@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     142
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_KHR_cooperative_matrix
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_bfloat16.html SPV_KHR_bfloat16>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_bfloat16] @tobski%0A*Here describe the issue or question you have about the VK_KHR_shader_bfloat16 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_bfloat16.adoc VK_KHR_shader_bfloat16>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-04-09
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Stu Smith, AMD
--
--     -   Jeff Bolz, Nvidia
--
--     -   Kévin Petit, Arm
--
--     -   David Neto, Google
--
--     -   Graeme Leese, Broadcom
--
--     -   Ruihao Zhang, Qualcomm
--
--     -   Mark Sheppard, Imagination
--
--     -   Ben Ashbaugh, Intel
--
--     -   Dmitry Sidorov, Intel
--
--     -   Victor Mustya, Intel
--
-- == Description
--
-- This extension enables support for bfloat16 (“brain float”) operations
-- in shaders as defined in @SPV_KHR_bfloat16@.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderBfloat16FeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_BFLOAT16_EXTENSION_NAME'
--
-- -   'KHR_SHADER_BFLOAT16_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_BFLOAT16_FEATURES_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_cooperative_matrix VK_KHR_cooperative_matrix>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_NV_cooperative_vector.ComponentTypeKHR':
--
--     -   'Vulkan.Extensions.VK_NV_cooperative_vector.COMPONENT_TYPE_BFLOAT16_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-BFloat16TypeKHR BFloat16TypeKHR>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-BFloat16DotProductKHR BFloat16DotProductKHR>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-BFloat16CooperativeMatrixKHR BFloat16CooperativeMatrixKHR>
--
-- == Version History
--
-- -   Revision 1, 2024-04-09 (Stu Smith)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_shader_bfloat16 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_bfloat16  ( PhysicalDeviceShaderBfloat16FeaturesKHR(..)
                                                 , KHR_SHADER_BFLOAT16_SPEC_VERSION
                                                 , pattern KHR_SHADER_BFLOAT16_SPEC_VERSION
                                                 , KHR_SHADER_BFLOAT16_EXTENSION_NAME
                                                 , pattern KHR_SHADER_BFLOAT16_EXTENSION_NAME
                                                 , ComponentTypeKHR(..)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_BFLOAT16_FEATURES_KHR))
import Vulkan.Extensions.VK_NV_cooperative_vector (ComponentTypeKHR(..))
-- | VkPhysicalDeviceShaderBfloat16FeaturesKHR - Structure describing
-- bfloat16 features that can be supported by the implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderBfloat16FeaturesKHR' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceShaderBfloat16FeaturesKHR', it /must/ add an instance of
-- the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_bfloat16 VK_KHR_shader_bfloat16>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderBfloat16FeaturesKHR = PhysicalDeviceShaderBfloat16FeaturesKHR
  { -- | #features-shaderBFloat16Type# @shaderBFloat16Type@ indicates whether the
    -- implementation supports shaders with the @BFloat16TypeKHR@ capability.
    shaderBFloat16Type :: Bool
  , -- | #features-shaderBFloat16DotProduct# @shaderBFloat16DotProduct@ indicates
    -- whether the implementation supports shaders with the
    -- @BFloat16DotProductKHR@ capability.
    shaderBFloat16DotProduct :: Bool
  , -- | #features-shaderBFloat16CooperativeMatrix#
    -- @shaderBFloat16CooperativeMatrix@ indicates whether the implementation
    -- supports shaders with the @BFloat16CooperativeMatrixKHR@ capability.
    shaderBFloat16CooperativeMatrix :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderBfloat16FeaturesKHR)
#endif
deriving instance Show PhysicalDeviceShaderBfloat16FeaturesKHR

instance ToCStruct PhysicalDeviceShaderBfloat16FeaturesKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderBfloat16FeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_BFLOAT16_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderBFloat16Type))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (shaderBFloat16DotProduct))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (shaderBFloat16CooperativeMatrix))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_BFLOAT16_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderBfloat16FeaturesKHR where
  peekCStruct p = do
    shaderBFloat16Type <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    shaderBFloat16DotProduct <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    shaderBFloat16CooperativeMatrix <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderBfloat16FeaturesKHR
             (bool32ToBool shaderBFloat16Type)
             (bool32ToBool shaderBFloat16DotProduct)
             (bool32ToBool shaderBFloat16CooperativeMatrix)

instance Storable PhysicalDeviceShaderBfloat16FeaturesKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderBfloat16FeaturesKHR where
  zero = PhysicalDeviceShaderBfloat16FeaturesKHR
           zero
           zero
           zero


type KHR_SHADER_BFLOAT16_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_BFLOAT16_SPEC_VERSION"
pattern KHR_SHADER_BFLOAT16_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_BFLOAT16_SPEC_VERSION = 1


type KHR_SHADER_BFLOAT16_EXTENSION_NAME = "VK_KHR_shader_bfloat16"

-- No documentation found for TopLevel "VK_KHR_SHADER_BFLOAT16_EXTENSION_NAME"
pattern KHR_SHADER_BFLOAT16_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_BFLOAT16_EXTENSION_NAME = "VK_KHR_shader_bfloat16"

