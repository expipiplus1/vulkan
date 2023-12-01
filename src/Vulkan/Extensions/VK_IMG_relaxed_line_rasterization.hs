{-# language CPP #-}
-- | = Name
--
-- VK_IMG_relaxed_line_rasterization - device extension
--
-- == VK_IMG_relaxed_line_rasterization
--
-- [__Name String__]
--     @VK_IMG_relaxed_line_rasterization@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     111
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   James Fitzpatrick
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_IMG_relaxed_line_rasterization] @jamesfitzpatrick%0A*Here describe the issue or question you have about the VK_IMG_relaxed_line_rasterization extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-10-22
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   James Fitzpatrick, Imagination
--
--     -   Andrew Garrard, Imagination
--
--     -   Alex Walters, Imagination
--
-- == Description
--
-- OpenGL specifies that implementations should rasterize lines using the
-- diamond exit rule (a slightly modified version of Bresenham’s
-- algorithm). To implement OpenGL some implementations have a device-level
-- compatibility mode to rasterize lines according to the OpenGL
-- specification.
--
-- This extension allows OpenGL emulation layers to enable the OpenGL
-- compatible line rasterization mode of such implementations.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRelaxedLineRasterizationFeaturesIMG'
--
-- == New Enum Constants
--
-- -   'IMG_RELAXED_LINE_RASTERIZATION_EXTENSION_NAME'
--
-- -   'IMG_RELAXED_LINE_RASTERIZATION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RELAXED_LINE_RASTERIZATION_FEATURES_IMG'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2023-10-22 (James Fitzpatrick)
--
--     -   Initial version
--
-- == See Also
--
-- 'PhysicalDeviceRelaxedLineRasterizationFeaturesIMG'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_IMG_relaxed_line_rasterization Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_IMG_relaxed_line_rasterization  ( PhysicalDeviceRelaxedLineRasterizationFeaturesIMG(..)
                                                            , IMG_RELAXED_LINE_RASTERIZATION_SPEC_VERSION
                                                            , pattern IMG_RELAXED_LINE_RASTERIZATION_SPEC_VERSION
                                                            , IMG_RELAXED_LINE_RASTERIZATION_EXTENSION_NAME
                                                            , pattern IMG_RELAXED_LINE_RASTERIZATION_EXTENSION_NAME
                                                            ) where

import Foreign.Marshal.Alloc (allocaBytes)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RELAXED_LINE_RASTERIZATION_FEATURES_IMG))
-- | VkPhysicalDeviceRelaxedLineRasterizationFeaturesIMG - Structure
-- describing relaxed line rasterization features that can be supported by
-- an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceRelaxedLineRasterizationFeaturesIMG' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceRelaxedLineRasterizationFeaturesIMG' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_IMG_relaxed_line_rasterization VK_IMG_relaxed_line_rasterization>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRelaxedLineRasterizationFeaturesIMG = PhysicalDeviceRelaxedLineRasterizationFeaturesIMG
  { -- | #features-relaxedLineRasterization# @relaxedLineRasterization@ indicates
    -- that the implementation supports relaxed line rasterization control.
    relaxedLineRasterization :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRelaxedLineRasterizationFeaturesIMG)
#endif
deriving instance Show PhysicalDeviceRelaxedLineRasterizationFeaturesIMG

instance ToCStruct PhysicalDeviceRelaxedLineRasterizationFeaturesIMG where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRelaxedLineRasterizationFeaturesIMG{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RELAXED_LINE_RASTERIZATION_FEATURES_IMG)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (relaxedLineRasterization))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RELAXED_LINE_RASTERIZATION_FEATURES_IMG)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRelaxedLineRasterizationFeaturesIMG where
  peekCStruct p = do
    relaxedLineRasterization <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceRelaxedLineRasterizationFeaturesIMG
             (bool32ToBool relaxedLineRasterization)

instance Storable PhysicalDeviceRelaxedLineRasterizationFeaturesIMG where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRelaxedLineRasterizationFeaturesIMG where
  zero = PhysicalDeviceRelaxedLineRasterizationFeaturesIMG
           zero


type IMG_RELAXED_LINE_RASTERIZATION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_IMG_RELAXED_LINE_RASTERIZATION_SPEC_VERSION"
pattern IMG_RELAXED_LINE_RASTERIZATION_SPEC_VERSION :: forall a . Integral a => a
pattern IMG_RELAXED_LINE_RASTERIZATION_SPEC_VERSION = 1


type IMG_RELAXED_LINE_RASTERIZATION_EXTENSION_NAME = "VK_IMG_relaxed_line_rasterization"

-- No documentation found for TopLevel "VK_IMG_RELAXED_LINE_RASTERIZATION_EXTENSION_NAME"
pattern IMG_RELAXED_LINE_RASTERIZATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern IMG_RELAXED_LINE_RASTERIZATION_EXTENSION_NAME = "VK_IMG_relaxed_line_rasterization"

