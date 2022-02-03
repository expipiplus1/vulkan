{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_shader_demote_to_helper_invocation"
module Vulkan.Core13.Promoted_From_VK_EXT_shader_demote_to_helper_invocation  ( PhysicalDeviceShaderDemoteToHelperInvocationFeatures(..)
                                                                              , StructureType(..)
                                                                              ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceShaderDemoteToHelperInvocationFeatures - Structure
-- describing the shader demote to helper invocations features that can be
-- supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderDemoteToHelperInvocationFeatures' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceShaderDemoteToHelperInvocationFeatures' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_demote_to_helper_invocation VK_EXT_shader_demote_to_helper_invocation>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderDemoteToHelperInvocationFeatures = PhysicalDeviceShaderDemoteToHelperInvocationFeatures
  { -- | #extension-features-shaderDemoteToHelperInvocation#
    -- @shaderDemoteToHelperInvocation@ indicates whether the implementation
    -- supports the SPIR-V @DemoteToHelperInvocationEXT@ capability.
    shaderDemoteToHelperInvocation :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderDemoteToHelperInvocationFeatures)
#endif
deriving instance Show PhysicalDeviceShaderDemoteToHelperInvocationFeatures

instance ToCStruct PhysicalDeviceShaderDemoteToHelperInvocationFeatures where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderDemoteToHelperInvocationFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderDemoteToHelperInvocation))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderDemoteToHelperInvocationFeatures where
  peekCStruct p = do
    shaderDemoteToHelperInvocation <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderDemoteToHelperInvocationFeatures
             (bool32ToBool shaderDemoteToHelperInvocation)

instance Storable PhysicalDeviceShaderDemoteToHelperInvocationFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderDemoteToHelperInvocationFeatures where
  zero = PhysicalDeviceShaderDemoteToHelperInvocationFeatures
           zero

