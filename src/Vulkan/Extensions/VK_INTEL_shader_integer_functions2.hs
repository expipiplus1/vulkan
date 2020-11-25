{-# language CPP #-}
-- | = Name
--
-- VK_INTEL_shader_integer_functions2 - device extension
--
-- = Registered Extension Number
--
-- 210
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- -   Requires @VK_KHR_get_physical_device_properties2@
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-04-30
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Ian Romanick, Intel
--
--     -   Ben Ashbaugh, Intel
--
-- == Description
--
-- This extension adds support for several new integer instructions in
-- SPIR-V for use in graphics shaders. Many of these instructions have
-- pre-existing counterparts in the Kernel environment.
--
-- The added integer functions are defined by the
-- {spirv}\/INTEL\/SPV_INTEL_shader_integer_functions2.html[@SPV_INTEL_shader_integer_functions@]
-- SPIR-V extension and can be used with the
-- GL_INTEL_shader_integer_functions2 GLSL extension.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL'
--
-- == New Enum Constants
--
-- -   'INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME'
--
-- -   'INTEL_SHADER_INTEGER_FUNCTIONS_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL'
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-cooperativeMatrix IntegerFunctions2INTEL>
--
-- == Version History
--
-- -   Revision 1, 2019-04-30 (Ian Romanick)
--
--     -   Initial draft
--
-- = See Also
--
-- 'PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_INTEL_shader_integer_functions2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_INTEL_shader_integer_functions2  ( PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL(..)
                                                             , INTEL_SHADER_INTEGER_FUNCTIONS_2_SPEC_VERSION
                                                             , pattern INTEL_SHADER_INTEGER_FUNCTIONS_2_SPEC_VERSION
                                                             , INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME
                                                             , pattern INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL))
-- | VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL - Structure
-- describing shader integer functions that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL' /can/ also be
-- included in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL = PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL
  { -- | #features-shaderIntegerFunctions2# @shaderIntegerFunctions2@ indicates
    -- that the implementation supports the @IntegerFunctions2INTEL@ SPIR-V
    -- capability.
    shaderIntegerFunctions2 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL)
#endif
deriving instance Show PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL

instance ToCStruct PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderIntegerFunctions2))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL where
  peekCStruct p = do
    shaderIntegerFunctions2 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL
             (bool32ToBool shaderIntegerFunctions2)

instance Storable PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL where
  zero = PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL
           zero


type INTEL_SHADER_INTEGER_FUNCTIONS_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_SPEC_VERSION"
pattern INTEL_SHADER_INTEGER_FUNCTIONS_2_SPEC_VERSION :: forall a . Integral a => a
pattern INTEL_SHADER_INTEGER_FUNCTIONS_2_SPEC_VERSION = 1


type INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME = "VK_INTEL_shader_integer_functions2"

-- No documentation found for TopLevel "VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME"
pattern INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME = "VK_INTEL_shader_integer_functions2"

