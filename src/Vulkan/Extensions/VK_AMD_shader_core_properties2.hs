{-# language CPP #-}
module Vulkan.Extensions.VK_AMD_shader_core_properties2  ( PhysicalDeviceShaderCoreProperties2AMD(..)
                                                         , ShaderCorePropertiesFlagBitsAMD(..)
                                                         , ShaderCorePropertiesFlagsAMD
                                                         , AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION
                                                         , pattern AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION
                                                         , AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME
                                                         , pattern AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME
                                                         ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Vulkan.Core10.BaseType (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD))
-- | VkPhysicalDeviceShaderCoreProperties2AMD - Structure describing shader
-- core properties that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceShaderCoreProperties2AMD' structure
-- describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderCoreProperties2AMD' structure is included in
-- the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'ShaderCorePropertiesFlagsAMD',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderCoreProperties2AMD = PhysicalDeviceShaderCoreProperties2AMD
  { -- | @shaderCoreFeatures@ is a bitmask of 'ShaderCorePropertiesFlagBitsAMD'
    -- indicating the set of features supported by the shader core.
    shaderCoreFeatures :: ShaderCorePropertiesFlagsAMD
  , -- | @activeComputeUnitCount@ is an unsigned integer value indicating the
    -- number of compute units that have been enabled.
    activeComputeUnitCount :: Word32
  }
  deriving (Typeable)
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


-- | VkShaderCorePropertiesFlagBitsAMD - Bitmask specifying shader core
-- properties
--
-- = See Also
--
-- 'PhysicalDeviceShaderCoreProperties2AMD', 'ShaderCorePropertiesFlagsAMD'
newtype ShaderCorePropertiesFlagBitsAMD = ShaderCorePropertiesFlagBitsAMD Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



type ShaderCorePropertiesFlagsAMD = ShaderCorePropertiesFlagBitsAMD

instance Show ShaderCorePropertiesFlagBitsAMD where
  showsPrec p = \case
    ShaderCorePropertiesFlagBitsAMD x -> showParen (p >= 11) (showString "ShaderCorePropertiesFlagBitsAMD 0x" . showHex x)

instance Read ShaderCorePropertiesFlagBitsAMD where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "ShaderCorePropertiesFlagBitsAMD")
                       v <- step readPrec
                       pure (ShaderCorePropertiesFlagBitsAMD v)))


type AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION"
pattern AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION = 1


type AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME = "VK_AMD_shader_core_properties2"

-- No documentation found for TopLevel "VK_AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME"
pattern AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME = "VK_AMD_shader_core_properties2"

