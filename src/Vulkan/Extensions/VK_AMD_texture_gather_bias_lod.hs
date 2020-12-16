{-# language CPP #-}
-- | = Name
--
-- VK_AMD_texture_gather_bias_lod - device extension
--
-- == VK_AMD_texture_gather_bias_lod
--
-- [__Name String__]
--     @VK_AMD_texture_gather_bias_lod@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     42
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
--     -   Rex Xu
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_AMD_texture_gather_bias_lod:%20&body=@amdrexu%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-03-21
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/AMD/SPV_AMD_texture_gather_bias_lod.html SPV_AMD_texture_gather_bias_lod>
--
-- [__Contributors__]
--
--     -   Dominik Witczak, AMD
--
--     -   Daniel Rakos, AMD
--
--     -   Graham Sellers, AMD
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Qun Lin, AMD
--
--     -   Rex Xu, AMD
--
--     -   Timothy Lottes, AMD
--
-- == Description
--
-- This extension adds two related features.
--
-- Firstly, support for the following SPIR-V extension in Vulkan is added:
--
-- -   @SPV_AMD_texture_gather_bias_lod@
--
-- Secondly, the extension allows the application to query which formats
-- can be used together with the new function prototypes introduced by the
-- SPIR-V extension.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.ImageFormatProperties2':
--
--     -   'TextureLODGatherFormatPropertiesAMD'
--
-- == New Enum Constants
--
-- -   'AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME'
--
-- -   'AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD'
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-ImageGatherBiasLodAMD ImageGatherBiasLodAMD>
--
-- == Examples
--
-- > struct VkTextureLODGatherFormatPropertiesAMD
-- > {
-- >     VkStructureType sType;
-- >     const void*     pNext;
-- >     VkBool32        supportsTextureGatherLODBiasAMD;
-- > };
-- >
-- > // ----------------------------------------------------------------------------------------
-- > // How to detect if an image format can be used with the new function prototypes.
-- > VkPhysicalDeviceImageFormatInfo2   formatInfo;
-- > VkImageFormatProperties2           formatProps;
-- > VkTextureLODGatherFormatPropertiesAMD textureLODGatherSupport;
-- >
-- > textureLODGatherSupport.sType = VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD;
-- > textureLODGatherSupport.pNext = nullptr;
-- >
-- > formatInfo.sType  = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2;
-- > formatInfo.pNext  = nullptr;
-- > formatInfo.format = ...;
-- > formatInfo.type   = ...;
-- > formatInfo.tiling = ...;
-- > formatInfo.usage  = ...;
-- > formatInfo.flags  = ...;
-- >
-- > formatProps.sType = VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2;
-- > formatProps.pNext = &textureLODGatherSupport;
-- >
-- > vkGetPhysicalDeviceImageFormatProperties2(physical_device, &formatInfo, &formatProps);
-- >
-- > if (textureLODGatherSupport.supportsTextureGatherLODBiasAMD == VK_TRUE)
-- > {
-- >     // physical device supports SPV_AMD_texture_gather_bias_lod for the specified
-- >     // format configuration.
-- > }
-- > else
-- > {
-- >     // physical device does not support SPV_AMD_texture_gather_bias_lod for the
-- >     // specified format configuration.
-- > }
--
-- == Version History
--
-- -   Revision 1, 2017-03-21 (Dominik Witczak)
--
--     -   Initial draft
--
-- = See Also
--
-- 'TextureLODGatherFormatPropertiesAMD'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_texture_gather_bias_lod Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_texture_gather_bias_lod  ( TextureLODGatherFormatPropertiesAMD(..)
                                                         , AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION
                                                         , pattern AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION
                                                         , AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME
                                                         , pattern AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD))
-- | VkTextureLODGatherFormatPropertiesAMD - Structure informing whether or
-- not texture gather bias\/LOD functionality is supported for a given
-- image format and a given physical device.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data TextureLODGatherFormatPropertiesAMD = TextureLODGatherFormatPropertiesAMD
  { -- | @supportsTextureGatherLODBiasAMD@ tells if the image format can be used
    -- with texture gather bias\/LOD functions, as introduced by the
    -- @VK_AMD_texture_gather_bias_lod@ extension. This field is set by the
    -- implementation. User-specified value is ignored.
    supportsTextureGatherLODBiasAMD :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TextureLODGatherFormatPropertiesAMD)
#endif
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

