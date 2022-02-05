{-# language CPP #-}
-- | = Name
--
-- VK_AMD_shader_core_properties2 - device extension
--
-- == VK_AMD_shader_core_properties2
--
-- [__Name String__]
--     @VK_AMD_shader_core_properties2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     228
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_AMD_shader_core_properties@
--
-- [__Contact__]
--
--     -   Matthaeus G. Chajdas
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMD_shader_core_properties2] @anteru%0A<<Here describe the issue or question you have about the VK_AMD_shader_core_properties2 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-07-26
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- This extension exposes additional shader core properties for a target
-- physical device through the @VK_KHR_get_physical_device_properties2@
-- extension.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderCoreProperties2AMD'
--
-- == New Enums
--
-- -   'ShaderCorePropertiesFlagBitsAMD'
--
-- == New Bitmasks
--
-- -   'ShaderCorePropertiesFlagsAMD'
--
-- == New Enum Constants
--
-- -   'AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME'
--
-- -   'AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD'
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2019-07-26 (Matthaeus G. Chajdas)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'PhysicalDeviceShaderCoreProperties2AMD',
-- 'ShaderCorePropertiesFlagBitsAMD', 'ShaderCorePropertiesFlagsAMD'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_AMD_shader_core_properties2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_shader_core_properties2  ( PhysicalDeviceShaderCoreProperties2AMD(..)
                                                         , ShaderCorePropertiesFlagsAMD
                                                         , ShaderCorePropertiesFlagBitsAMD(..)
                                                         , AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION
                                                         , pattern AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION
                                                         , AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME
                                                         , pattern AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME
                                                         ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD))
-- | VkPhysicalDeviceShaderCoreProperties2AMD - Structure describing shader
-- core properties that can be supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceShaderCoreProperties2AMD' structure is included in
-- the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_shader_core_properties2 VK_AMD_shader_core_properties2>,
-- 'ShaderCorePropertiesFlagsAMD',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderCoreProperties2AMD = PhysicalDeviceShaderCoreProperties2AMD
  { -- | #features-shaderCoreFeatures# @shaderCoreFeatures@ is a bitmask of
    -- 'ShaderCorePropertiesFlagBitsAMD' indicating the set of features
    -- supported by the shader core.
    shaderCoreFeatures :: ShaderCorePropertiesFlagsAMD
  , -- | #limits-activeComputeUnitCount# @activeComputeUnitCount@ is an unsigned
    -- integer value indicating the number of compute units that have been
    -- enabled.
    activeComputeUnitCount :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderCoreProperties2AMD)
#endif
deriving instance Show PhysicalDeviceShaderCoreProperties2AMD

instance ToCStruct PhysicalDeviceShaderCoreProperties2AMD where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderCoreProperties2AMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ShaderCorePropertiesFlagsAMD)) (shaderCoreFeatures)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (activeComputeUnitCount)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ShaderCorePropertiesFlagsAMD)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceShaderCoreProperties2AMD where
  peekCStruct p = do
    shaderCoreFeatures <- peek @ShaderCorePropertiesFlagsAMD ((p `plusPtr` 16 :: Ptr ShaderCorePropertiesFlagsAMD))
    activeComputeUnitCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ PhysicalDeviceShaderCoreProperties2AMD
             shaderCoreFeatures activeComputeUnitCount

instance Storable PhysicalDeviceShaderCoreProperties2AMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderCoreProperties2AMD where
  zero = PhysicalDeviceShaderCoreProperties2AMD
           zero
           zero


type ShaderCorePropertiesFlagsAMD = ShaderCorePropertiesFlagBitsAMD

-- | VkShaderCorePropertiesFlagBitsAMD - Bitmask specifying shader core
-- properties
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_shader_core_properties2 VK_AMD_shader_core_properties2>,
-- 'PhysicalDeviceShaderCoreProperties2AMD', 'ShaderCorePropertiesFlagsAMD'
newtype ShaderCorePropertiesFlagBitsAMD = ShaderCorePropertiesFlagBitsAMD Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameShaderCorePropertiesFlagBitsAMD :: String
conNameShaderCorePropertiesFlagBitsAMD = "ShaderCorePropertiesFlagBitsAMD"

enumPrefixShaderCorePropertiesFlagBitsAMD :: String
enumPrefixShaderCorePropertiesFlagBitsAMD = ""

showTableShaderCorePropertiesFlagBitsAMD :: [(ShaderCorePropertiesFlagBitsAMD, String)]
showTableShaderCorePropertiesFlagBitsAMD = []

instance Show ShaderCorePropertiesFlagBitsAMD where
  showsPrec = enumShowsPrec enumPrefixShaderCorePropertiesFlagBitsAMD
                            showTableShaderCorePropertiesFlagBitsAMD
                            conNameShaderCorePropertiesFlagBitsAMD
                            (\(ShaderCorePropertiesFlagBitsAMD x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read ShaderCorePropertiesFlagBitsAMD where
  readPrec = enumReadPrec enumPrefixShaderCorePropertiesFlagBitsAMD
                          showTableShaderCorePropertiesFlagBitsAMD
                          conNameShaderCorePropertiesFlagBitsAMD
                          ShaderCorePropertiesFlagBitsAMD


type AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION"
pattern AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION = 1


type AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME = "VK_AMD_shader_core_properties2"

-- No documentation found for TopLevel "VK_AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME"
pattern AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME = "VK_AMD_shader_core_properties2"

