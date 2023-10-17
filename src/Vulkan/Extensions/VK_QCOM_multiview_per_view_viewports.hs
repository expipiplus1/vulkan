{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_multiview_per_view_viewports - device extension
--
-- == VK_QCOM_multiview_per_view_viewports
--
-- [__Name String__]
--     @VK_QCOM_multiview_per_view_viewports@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     489
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Jeff Leger
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_multiview_per_view_viewports] @jackohound%0A*Here describe the issue or question you have about the VK_QCOM_multiview_per_view_viewports extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-11-22
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension interacts with @VK_KHR_dynamic_rendering@
--
--     -   This extension interacts with @VK_EXT_extended_dynamic_state@
--
-- [__Contributors__]
--
--     -   Jeff Leger, Qualcomm
--
--     -   Jonathan Tinkham, Qualcomm
--
--     -   Jonathan Wicks, Qualcomm
--
-- == Description
--
-- Certain use cases for multiview have a need for specifying a separate
-- viewport and scissor for each view, without using shader-based viewport
-- indexing as introduced with @VK_EXT_shader_viewport_index_layer@.
--
-- This extension adds a new way to control ViewportIndex with multiview.
-- When the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-multiview-per-view-viewports multiviewPerViewViewports>
-- feature is enabled and if the last pre-rasterization shader entry
-- point’s interface does not use the @ViewportIndex@ built-in decoration,
-- then each view of a multiview render pass instance will use a viewport
-- and scissor index equal to the @ViewIndex@.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_MULTIVIEW_PER_VIEW_VIEWPORTS_EXTENSION_NAME'
--
-- -   'QCOM_MULTIVIEW_PER_VIEW_VIEWPORTS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_VIEWPORTS_FEATURES_QCOM'
--
-- == Issues
--
-- 1) Is is possible to enable\/disable the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-multiview-per-view-viewports multiviewPerViewViewports>
-- feature for individual render pass instances?
--
-- __RESOLVED__: No, when the multiviewPerViewViewports feature is enabled
-- during vkCreateDevice, then all created render pass instances (including
-- dynamic render passes from @VK_KHR_dynamic_rendering@) and all created
-- VkPipelines will have the feature enabled. This approach was chosen
-- because it simplifies application code and there is no known use case
-- enable\/disable the feature for individual render passes or pipelines.
--
-- 2) When this extension is used, is the value of @ViewportIndex@
-- implicitly written by the last pre-rasterization shader stage and can
-- the value of @ViewportIndex@ be read in the fragment shader?
--
-- __RESOLVED__: No, use of the extension extension does not add an
-- implicit write to @ViewportIndex@ in any shader stage, and additionally,
-- the value of @ViewportIndex@ in the fragment shader is undefined.
--
-- == Version History
--
-- -   Revision 1, 2022-11-22 (Jeff Leger)
--
-- == See Also
--
-- 'PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QCOM_multiview_per_view_viewports Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_multiview_per_view_viewports  ( PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM(..)
                                                               , QCOM_MULTIVIEW_PER_VIEW_VIEWPORTS_SPEC_VERSION
                                                               , pattern QCOM_MULTIVIEW_PER_VIEW_VIEWPORTS_SPEC_VERSION
                                                               , QCOM_MULTIVIEW_PER_VIEW_VIEWPORTS_EXTENSION_NAME
                                                               , pattern QCOM_MULTIVIEW_PER_VIEW_VIEWPORTS_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_VIEWPORTS_FEATURES_QCOM))
-- | VkPhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM - Structure
-- describing multiview per view viewports features that can be supported
-- by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_multiview_per_view_viewports VK_QCOM_multiview_per_view_viewports>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM = PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM
  { -- | #features-multiview-per-view-viewports# @multiviewPerViewViewports@
    -- indicates that the implementation supports multiview per-view viewports.
    multiviewPerViewViewports :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM)
#endif
deriving instance Show PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM

instance ToCStruct PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_VIEWPORTS_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (multiviewPerViewViewports))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_VIEWPORTS_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM where
  peekCStruct p = do
    multiviewPerViewViewports <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM
             (bool32ToBool multiviewPerViewViewports)

instance Storable PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM where
  zero = PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM
           zero


type QCOM_MULTIVIEW_PER_VIEW_VIEWPORTS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_QCOM_MULTIVIEW_PER_VIEW_VIEWPORTS_SPEC_VERSION"
pattern QCOM_MULTIVIEW_PER_VIEW_VIEWPORTS_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_MULTIVIEW_PER_VIEW_VIEWPORTS_SPEC_VERSION = 1


type QCOM_MULTIVIEW_PER_VIEW_VIEWPORTS_EXTENSION_NAME = "VK_QCOM_multiview_per_view_viewports"

-- No documentation found for TopLevel "VK_QCOM_MULTIVIEW_PER_VIEW_VIEWPORTS_EXTENSION_NAME"
pattern QCOM_MULTIVIEW_PER_VIEW_VIEWPORTS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_MULTIVIEW_PER_VIEW_VIEWPORTS_EXTENSION_NAME = "VK_QCOM_multiview_per_view_viewports"

