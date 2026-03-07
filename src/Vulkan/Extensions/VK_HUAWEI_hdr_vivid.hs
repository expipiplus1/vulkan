{-# language CPP #-}
-- | = Name
--
-- VK_HUAWEI_hdr_vivid - device extension
--
-- = VK_HUAWEI_hdr_vivid
--
-- [__Name String__]
--     @VK_HUAWEI_hdr_vivid@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     591
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_hdr_metadata VK_EXT_hdr_metadata>
--
-- [__Contact__]
--
--     -   Zehui Lin
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_HUAWEI_hdr_vivid] @bactlink%0A*Here describe the issue or question you have about the VK_HUAWEI_hdr_vivid extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-10-08
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Juntao Li, Huawei
--
--     -   Pan Gao, Huawei
--
--     -   Xiufeng Zhang, Huawei
--
--     -   Zehui Lin, Huawei
--
-- == Description
--
-- This extension allows applications to assign HDR Vivid (T\/UWA
-- 005.1-2022) metadata to swapchains.
--
-- HDR Vivid is an HDR standard released by UWA (UHD World Association). It
-- defines tone mapping from the metadata to better preserve the creator’s
-- intentions and achieve better consistency across devices with different
-- display capabilities.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Extensions.VK_EXT_hdr_metadata.HdrMetadataEXT':
--
--     -   'HdrVividDynamicMetadataHUAWEI'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceHdrVividFeaturesHUAWEI'
--
-- == New Enum Constants
--
-- -   'HUAWEI_HDR_VIVID_EXTENSION_NAME'
--
-- -   'HUAWEI_HDR_VIVID_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_HDR_VIVID_DYNAMIC_METADATA_HUAWEI'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_HDR_VIVID_FEATURES_HUAWEI'
--
-- == Version History
--
-- -   Revision 1, 2024-10-08 (Zehui Lin)
--
--     -   Initial version
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_HUAWEI_hdr_vivid Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_HUAWEI_hdr_vivid  ( HdrVividDynamicMetadataHUAWEI(..)
                                              , PhysicalDeviceHdrVividFeaturesHUAWEI(..)
                                              , HUAWEI_HDR_VIVID_SPEC_VERSION
                                              , pattern HUAWEI_HDR_VIVID_SPEC_VERSION
                                              , HUAWEI_HDR_VIVID_EXTENSION_NAME
                                              , pattern HUAWEI_HDR_VIVID_EXTENSION_NAME
                                              ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Coerce (coerce)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(..))
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word64)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_HDR_VIVID_DYNAMIC_METADATA_HUAWEI))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_HDR_VIVID_FEATURES_HUAWEI))
-- | VkHdrVividDynamicMetadataHUAWEI - specify HDR Vivid dynamic metadata
--
-- = Description
--
-- The HDR Vivid metadata is intended to be used as defined in the T\/UWA
-- 005.1-2022 specification. The validity and use of this data is outside
-- the scope of Vulkan.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_HUAWEI_hdr_vivid VK_HUAWEI_hdr_vivid>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data HdrVividDynamicMetadataHUAWEI = HdrVividDynamicMetadataHUAWEI
  { -- | @dynamicMetadataSize@ is the size in bytes of the dynamic metadata.
    --
    -- #VUID-VkHdrVividDynamicMetadataHUAWEI-dynamicMetadataSize-arraylength#
    -- @dynamicMetadataSize@ /must/ be greater than @0@
    dynamicMetadataSize :: Word64
  , -- | @pDynamicMetadata@ is a pointer to the dynamic metadata.
    --
    -- #VUID-VkHdrVividDynamicMetadataHUAWEI-pDynamicMetadata-parameter#
    -- @pDynamicMetadata@ /must/ be a valid pointer to an array of
    -- @dynamicMetadataSize@ bytes
    dynamicMetadata :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HdrVividDynamicMetadataHUAWEI)
#endif
deriving instance Show HdrVividDynamicMetadataHUAWEI

instance ToCStruct HdrVividDynamicMetadataHUAWEI where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HdrVividDynamicMetadataHUAWEI{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_HDR_VIVID_DYNAMIC_METADATA_HUAWEI)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (dynamicMetadataSize))
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (dynamicMetadata)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_HDR_VIVID_DYNAMIC_METADATA_HUAWEI)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct HdrVividDynamicMetadataHUAWEI where
  peekCStruct p = do
    dynamicMetadataSize <- peek @CSize ((p `plusPtr` 16 :: Ptr CSize))
    pDynamicMetadata <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ HdrVividDynamicMetadataHUAWEI
             (coerce @CSize @Word64 dynamicMetadataSize) pDynamicMetadata

instance Storable HdrVividDynamicMetadataHUAWEI where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HdrVividDynamicMetadataHUAWEI where
  zero = HdrVividDynamicMetadataHUAWEI
           zero
           zero


-- | VkPhysicalDeviceHdrVividFeaturesHUAWEI - Structure describing whether
-- HDR Vivid metadata is supported
--
-- = Description
--
-- If the 'PhysicalDeviceHdrVividFeaturesHUAWEI' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceHdrVividFeaturesHUAWEI' /can/ also be used in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_HUAWEI_hdr_vivid VK_HUAWEI_hdr_vivid>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceHdrVividFeaturesHUAWEI = PhysicalDeviceHdrVividFeaturesHUAWEI
  { -- | #features-hdrVivid# @hdrVivid@ specifies whether HDR Vivid metadata is
    -- supported.
    hdrVivid :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceHdrVividFeaturesHUAWEI)
#endif
deriving instance Show PhysicalDeviceHdrVividFeaturesHUAWEI

instance ToCStruct PhysicalDeviceHdrVividFeaturesHUAWEI where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceHdrVividFeaturesHUAWEI{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_HDR_VIVID_FEATURES_HUAWEI)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (hdrVivid))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_HDR_VIVID_FEATURES_HUAWEI)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceHdrVividFeaturesHUAWEI where
  peekCStruct p = do
    hdrVivid <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceHdrVividFeaturesHUAWEI
             (bool32ToBool hdrVivid)

instance Storable PhysicalDeviceHdrVividFeaturesHUAWEI where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceHdrVividFeaturesHUAWEI where
  zero = PhysicalDeviceHdrVividFeaturesHUAWEI
           zero


type HUAWEI_HDR_VIVID_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_HUAWEI_HDR_VIVID_SPEC_VERSION"
pattern HUAWEI_HDR_VIVID_SPEC_VERSION :: forall a . Integral a => a
pattern HUAWEI_HDR_VIVID_SPEC_VERSION = 1


type HUAWEI_HDR_VIVID_EXTENSION_NAME = "VK_HUAWEI_hdr_vivid"

-- No documentation found for TopLevel "VK_HUAWEI_HDR_VIVID_EXTENSION_NAME"
pattern HUAWEI_HDR_VIVID_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern HUAWEI_HDR_VIVID_EXTENSION_NAME = "VK_HUAWEI_hdr_vivid"

