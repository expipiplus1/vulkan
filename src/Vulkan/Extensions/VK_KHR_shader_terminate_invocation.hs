{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_terminate_invocation - device extension
--
-- == VK_KHR_shader_terminate_invocation
--
-- [__Name String__]
--     @VK_KHR_shader_terminate_invocation@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     216
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
--     -   Jesse Hall
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_shader_terminate_invocation:%20&body=@critsec%20 >
--
-- [__Last Modified Date__]
--     2020-08-11
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Requires the
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_terminate_invocation.html SPV_KHR_terminate_invocation>
--         SPIR-V extension.
--
-- [__Contributors__]
--
--     -   Alan Baker, Google
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Jesse Hall, Google
--
--     -   Ralph Potter, Samsung
--
--     -   Tom Olson, Arm
--
-- == Description
--
-- This extension adds Vulkan support for the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_terminate_invocation.html SPV_KHR_terminate_invocation>
-- SPIR-V extension. That SPIR-V extension provides a new instruction,
-- @OpTerminateInvocation@, which causes a shader invocation to immediately
-- terminate and sets the coverage of shaded samples to @0@; only
-- previously executed instructions will have observable effects. The
-- @OpTerminateInvocation@ instruction, along with the
-- @OpDemoteToHelperInvocation@ instruction from the
-- <VK_EXT_shader_demote_to_helper_invocation.html VK_EXT_shader_demote_to_helper_invocation>
-- extension, together replace the @OpKill@ instruction, which could behave
-- like either of these instructions. @OpTerminateInvocation@ provides the
-- behavior required by the GLSL @discard@ statement, and should be used
-- when available by GLSL compilers and applications that need the GLSL
-- @discard@ behavior.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderTerminateInvocationFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME'
--
-- -   'KHR_SHADER_TERMINATE_INVOCATION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TERMINATE_INVOCATION_FEATURES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2020-08-11 (Jesse Hall)
--
-- = See Also
--
-- 'PhysicalDeviceShaderTerminateInvocationFeaturesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_terminate_invocation Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_terminate_invocation  ( PhysicalDeviceShaderTerminateInvocationFeaturesKHR(..)
                                                             , KHR_SHADER_TERMINATE_INVOCATION_SPEC_VERSION
                                                             , pattern KHR_SHADER_TERMINATE_INVOCATION_SPEC_VERSION
                                                             , KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME
                                                             , pattern KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TERMINATE_INVOCATION_FEATURES_KHR))
-- | VkPhysicalDeviceShaderTerminateInvocationFeaturesKHR - Structure
-- describing support for the SPIR-V @SPV_KHR_terminate_invocation@
-- extension
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderTerminateInvocationFeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceShaderTerminateInvocationFeaturesKHR' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
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

