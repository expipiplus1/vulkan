{-# language CPP #-}
module Vulkan.Extensions.VK_AMD_texture_gather_bias_lod  ( TextureLODGatherFormatPropertiesAMD(..)
                                                         , AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION
                                                         , pattern AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION
                                                         , AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME
                                                         , pattern AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD))
-- | VkTextureLODGatherFormatPropertiesAMD - Structure informing whether or
-- not texture gather bias\/LOD functionality is supported for a given
-- image format and a given physical device.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data TextureLODGatherFormatPropertiesAMD = TextureLODGatherFormatPropertiesAMD
  { -- | @supportsTextureGatherLODBiasAMD@ tells if the image format can be used
    -- with texture gather bias\/LOD functions, as introduced by the
    -- @VK_AMD_texture_gather_bias_lod@ extension. This field is set by the
    -- implementation. User-specified value is ignored.
    supportsTextureGatherLODBiasAMD :: Bool }
  deriving (Typeable, Eq)
deriving instance Show TextureLODGatherFormatPropertiesAMD

instance ToCStruct TextureLODGatherFormatPropertiesAMD where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TextureLODGatherFormatPropertiesAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (supportsTextureGatherLODBiasAMD))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct TextureLODGatherFormatPropertiesAMD where
  peekCStruct p = do
    supportsTextureGatherLODBiasAMD <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ TextureLODGatherFormatPropertiesAMD
             (bool32ToBool supportsTextureGatherLODBiasAMD)

instance Storable TextureLODGatherFormatPropertiesAMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero TextureLODGatherFormatPropertiesAMD where
  zero = TextureLODGatherFormatPropertiesAMD
           zero


type AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION"
pattern AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION = 1


type AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME = "VK_AMD_texture_gather_bias_lod"

-- No documentation found for TopLevel "VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME"
pattern AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME = "VK_AMD_texture_gather_bias_lod"

