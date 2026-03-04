{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_maximal_reconvergence - device extension
--
-- == VK_KHR_shader_maximal_reconvergence
--
-- [__Name String__]
--     @VK_KHR_shader_maximal_reconvergence@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     435
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_maximal_reconvergence.html SPV_KHR_maximal_reconvergence>
--
-- [__Contact__]
--
--     -   Alan Baker
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_maximal_reconvergence] @alan-baker%0A*Here describe the issue or question you have about the VK_KHR_shader_maximal_reconvergence extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_maximal_reconvergence.adoc VK_KHR_shader_maximal_reconvergence>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-11-12
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Requires SPIR-V 1.3.
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_maximal_reconvergence.html SPV_KHR_maximal_reconvergence>
--
-- [__Contributors__]
--
--     -   Alan Baker, Google
--
-- == Description
--
-- This extension allows the use of the @SPV_KHR_maximal_reconvergence@
-- SPIR-V extension in shader modules. @SPV_KHR_maximal_reconvergence@
-- provides stronger guarantees that diverged subgroups will reconverge.
-- These guarantees should match shader author intuition about divergence
-- and reconvergence of invocations based on the structure of the code in
-- the HLL.
--
-- Developers should utilize this extension if they require stronger
-- guarantees about reconvergence than either the core spec or
-- SPV_KHR_subgroup_uniform_control_flow. This extension will define the
-- rules that govern how invocations diverge and reconverge in a way that
-- should match developer intuition. It allows robust programs to be
-- written relying on subgroup operations and other tangled instructions.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_MAXIMAL_RECONVERGENCE_EXTENSION_NAME'
--
-- -   'KHR_SHADER_MAXIMAL_RECONVERGENCE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MAXIMAL_RECONVERGENCE_FEATURES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2021-11-12 (Alan Baker)
--
--     -   Internal draft version
--
-- == See Also
--
-- 'PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_shader_maximal_reconvergence Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_maximal_reconvergence  ( PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR(..)
                                                              , KHR_SHADER_MAXIMAL_RECONVERGENCE_SPEC_VERSION
                                                              , pattern KHR_SHADER_MAXIMAL_RECONVERGENCE_SPEC_VERSION
                                                              , KHR_SHADER_MAXIMAL_RECONVERGENCE_EXTENSION_NAME
                                                              , pattern KHR_SHADER_MAXIMAL_RECONVERGENCE_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MAXIMAL_RECONVERGENCE_FEATURES_KHR))
-- | VkPhysicalDeviceShaderMaximalReconvergenceFeaturesKHR - Structure
-- describing support for shader maximal reconvergence by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the
-- 'Vulkan.Extensions.VK_EXT_private_data.PhysicalDevicePrivateDataFeaturesEXT'
-- structure is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported.
-- 'Vulkan.Extensions.VK_EXT_private_data.PhysicalDevicePrivateDataFeaturesEXT'
-- /can/ also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_maximal_reconvergence VK_KHR_shader_maximal_reconvergence>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR = PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR
  { -- | #features-shaderMaximalReconvergence# @shaderMaximalReconvergence@
    -- specifies whether the implementation supports the shader execution mode
    -- @MaximallyReconvergesKHR@
    shaderMaximalReconvergence :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR

instance ToCStruct PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MAXIMAL_RECONVERGENCE_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderMaximalReconvergence))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MAXIMAL_RECONVERGENCE_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR where
  peekCStruct p = do
    shaderMaximalReconvergence <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR
             (bool32ToBool shaderMaximalReconvergence)

instance Storable PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR where
  zero = PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR
           zero


type KHR_SHADER_MAXIMAL_RECONVERGENCE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_MAXIMAL_RECONVERGENCE_SPEC_VERSION"
pattern KHR_SHADER_MAXIMAL_RECONVERGENCE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_MAXIMAL_RECONVERGENCE_SPEC_VERSION = 1


type KHR_SHADER_MAXIMAL_RECONVERGENCE_EXTENSION_NAME = "VK_KHR_shader_maximal_reconvergence"

-- No documentation found for TopLevel "VK_KHR_SHADER_MAXIMAL_RECONVERGENCE_EXTENSION_NAME"
pattern KHR_SHADER_MAXIMAL_RECONVERGENCE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_MAXIMAL_RECONVERGENCE_EXTENSION_NAME = "VK_KHR_shader_maximal_reconvergence"

