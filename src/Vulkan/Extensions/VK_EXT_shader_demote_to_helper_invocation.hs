{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_shader_demote_to_helper_invocation  ( PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT(..)
                                                                    , EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_SPEC_VERSION
                                                                    , pattern EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_SPEC_VERSION
                                                                    , EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME
                                                                    , pattern EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME
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
import Vulkan.Core10.BaseType (bool32ToBool)
import Vulkan.Core10.BaseType (boolToBool32)
import Vulkan.Core10.BaseType (Bool32)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT = PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT
  { -- | @shaderDemoteToHelperInvocation@ indicates whether the implementation
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

