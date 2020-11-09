{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_shader_terminate_invocation  ( PhysicalDeviceShaderTerminateInvocationFeaturesKHR(..)
                                                             , KHR_SHADER_TERMINATE_INVOCATION_SPEC_VERSION
                                                             , pattern KHR_SHADER_TERMINATE_INVOCATION_SPEC_VERSION
                                                             , KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME
                                                             , pattern KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TERMINATE_INVOCATION_FEATURES_KHR))
-- | VkPhysicalDeviceShaderTerminateInvocationFeaturesKHR - Structure
-- describing support for the SPIR-V @SPV_KHR_terminate_invocation@
-- extension
--
-- = Members
--
-- The members of the 'PhysicalDeviceShaderTerminateInvocationFeaturesKHR'
-- structure describe the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderTerminateInvocationFeaturesKHR' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with a value indicating whether the feature is supported.
-- 'PhysicalDeviceShaderTerminateInvocationFeaturesKHR' /can/ also be
-- included in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to enable the features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderTerminateInvocationFeaturesKHR = PhysicalDeviceShaderTerminateInvocationFeaturesKHR
  { -- | #features-shaderTerminateInvocation# @shaderTerminateInvocation@
    -- specifies whether the implementation supports SPIR-V modules that use
    -- the @SPV_KHR_terminate_invocation@ extension.
    shaderTerminateInvocation :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderTerminateInvocationFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceShaderTerminateInvocationFeaturesKHR

instance ToCStruct PhysicalDeviceShaderTerminateInvocationFeaturesKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderTerminateInvocationFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TERMINATE_INVOCATION_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderTerminateInvocation))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TERMINATE_INVOCATION_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderTerminateInvocationFeaturesKHR where
  peekCStruct p = do
    shaderTerminateInvocation <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderTerminateInvocationFeaturesKHR
             (bool32ToBool shaderTerminateInvocation)

instance Storable PhysicalDeviceShaderTerminateInvocationFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderTerminateInvocationFeaturesKHR where
  zero = PhysicalDeviceShaderTerminateInvocationFeaturesKHR
           zero


type KHR_SHADER_TERMINATE_INVOCATION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_TERMINATE_INVOCATION_SPEC_VERSION"
pattern KHR_SHADER_TERMINATE_INVOCATION_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_TERMINATE_INVOCATION_SPEC_VERSION = 1


type KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME = "VK_KHR_shader_terminate_invocation"

-- No documentation found for TopLevel "VK_KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME"
pattern KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME = "VK_KHR_shader_terminate_invocation"

