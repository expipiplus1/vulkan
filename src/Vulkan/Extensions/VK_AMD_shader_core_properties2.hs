{-# language CPP #-}
-- No documentation found for Chapter "VK_AMD_shader_core_properties2"
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
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD))

-- No documentation found for TopLevel "VkPhysicalDeviceShaderCoreProperties2AMD"
data PhysicalDeviceShaderCoreProperties2AMD = PhysicalDeviceShaderCoreProperties2AMD
  { -- No documentation found for Nested "VkPhysicalDeviceShaderCoreProperties2AMD" "shaderCoreFeatures"
    shaderCoreFeatures :: ShaderCorePropertiesFlagsAMD
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCoreProperties2AMD" "activeComputeUnitCount"
    activeComputeUnitCount :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderCoreProperties2AMD)
#endif
deriving instance Show PhysicalDeviceShaderCoreProperties2AMD

instance ToCStruct PhysicalDeviceShaderCoreProperties2AMD where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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

-- No documentation found for TopLevel "VkShaderCorePropertiesFlagBitsAMD"
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

