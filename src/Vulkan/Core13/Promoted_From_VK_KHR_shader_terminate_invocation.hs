{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_shader_terminate_invocation"
module Vulkan.Core13.Promoted_From_VK_KHR_shader_terminate_invocation  ( PhysicalDeviceShaderTerminateInvocationFeatures(..)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TERMINATE_INVOCATION_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceShaderTerminateInvocationFeatures - Structure describing
-- support for the SPIR-V @SPV_KHR_terminate_invocation@ extension
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderTerminateInvocationFeatures' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceShaderTerminateInvocationFeatures' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_terminate_invocation VK_KHR_shader_terminate_invocation>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderTerminateInvocationFeatures = PhysicalDeviceShaderTerminateInvocationFeatures
  { -- | #extension-features-shaderTerminateInvocation#
    -- @shaderTerminateInvocation@ specifies whether the implementation
    -- supports SPIR-V modules that use the @SPV_KHR_terminate_invocation@
    -- extension.
    shaderTerminateInvocation :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderTerminateInvocationFeatures)
#endif
deriving instance Show PhysicalDeviceShaderTerminateInvocationFeatures

instance ToCStruct PhysicalDeviceShaderTerminateInvocationFeatures where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderTerminateInvocationFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TERMINATE_INVOCATION_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderTerminateInvocation))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TERMINATE_INVOCATION_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderTerminateInvocationFeatures where
  peekCStruct p = do
    shaderTerminateInvocation <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderTerminateInvocationFeatures
             (bool32ToBool shaderTerminateInvocation)

instance Storable PhysicalDeviceShaderTerminateInvocationFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderTerminateInvocationFeatures where
  zero = PhysicalDeviceShaderTerminateInvocationFeatures
           zero

