{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_demote_to_helper_invocation - device extension
--
-- == VK_EXT_shader_demote_to_helper_invocation
--
-- [__Name String__]
--     @VK_EXT_shader_demote_to_helper_invocation@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     277
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
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_shader_demote_to_helper_invocation:%20&body=@jeffbolznv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-06-01
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_demote_to_helper_invocation.html SPV_EXT_demote_to_helper_invocation>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension adds Vulkan support for the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_demote_to_helper_invocation.html SPV_EXT_demote_to_helper_invocation>
-- SPIR-V extension. That SPIR-V extension provides a new instruction
-- @OpDemoteToHelperInvocationEXT@ allowing shaders to \"demote\" a
-- fragment shader invocation to behave like a helper invocation for its
-- duration. The demoted invocation will have no further side effects and
-- will not output to the framebuffer, but remains active and can
-- participate in computing derivatives and in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-group-operations group operations>.
-- This is a better match for the \"discard\" instruction in HLSL.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME'
--
-- -   'EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES_EXT'
--
-- == New SPIR-V Capability
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-DemoteToHelperInvocationEXT DemoteToHelperInvocationEXT>
--
-- == Version History
--
-- -   Revision 1, 2019-06-01 (Jeff Bolz)
--
--     -   Initial draft
--
-- = See Also
--
-- 'PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_demote_to_helper_invocation Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_demote_to_helper_invocation  ( PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT(..)
                                                                    , EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_SPEC_VERSION
                                                                    , pattern EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_SPEC_VERSION
                                                                    , EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME
                                                                    , pattern EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES_EXT))
-- | VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT - Structure
-- describing the shader demote to helper invocations features that can be
-- supported by an implementation
--
-- = Members
--
-- The members of the
-- 'PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT' structure
-- describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT'
-- structure is included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT' /can/ also be
-- included in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to enable the feature.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT = PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT
  { -- | #features-shaderDemoteToHelperInvocation#
    -- @shaderDemoteToHelperInvocation@ indicates whether the implementation
    -- supports the SPIR-V @DemoteToHelperInvocationEXT@ capability.
    shaderDemoteToHelperInvocation :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT

instance ToCStruct PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderDemoteToHelperInvocation))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT where
  peekCStruct p = do
    shaderDemoteToHelperInvocation <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT
             (bool32ToBool shaderDemoteToHelperInvocation)

instance Storable PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT where
  zero = PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT
           zero


type EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_SPEC_VERSION"
pattern EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_SPEC_VERSION = 1


type EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME = "VK_EXT_shader_demote_to_helper_invocation"

-- No documentation found for TopLevel "VK_EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME"
pattern EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME = "VK_EXT_shader_demote_to_helper_invocation"

