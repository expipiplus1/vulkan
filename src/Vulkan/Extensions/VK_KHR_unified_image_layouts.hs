{-# language CPP #-}
-- | = Name
--
-- VK_KHR_unified_image_layouts - device extension
--
-- = VK_KHR_unified_image_layouts
--
-- [__Name String__]
--     @VK_KHR_unified_image_layouts@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     528
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_VERSION_1_3
--
--     -   Interacts with VK_EXT_attachment_feedback_loop_layout
--
--     -   Interacts with VK_KHR_dynamic_rendering
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_unified_image_layouts] @syoussefi%0A*Here describe the issue or question you have about the VK_KHR_unified_image_layouts extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_unified_image_layouts.adoc VK_KHR_unified_image_layouts>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-10-15
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension interacts with
--         @VK_EXT_attachment_feedback_loop_layout@
--
--     -   This extension interacts with @VK_KHR_video_decode_queue@
--
--     -   This extension interacts with @VK_KHR_video_encode_queue@
--
--     -   This extension interacts with
--         @VK_KHR_video_encode_quantization_map@
--
-- [__Contributors__]
--
--     -   Ahmed Abdelkhalek, AMD
--
--     -   Tobias Hector, AMD
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Ting Wei, ARM
--
--     -   Faith Ekstrand, Collabora
--
--     -   Lina Versace, Google
--
--     -   Shahbaz Youssefi, Google
--
--     -   James Fitzpatrick, Imagination
--
--     -   Daniel Story, Nintendo
--
--     -   James Jones, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Tony Zlatinski, NVIDIA
--
--     -   Matthew Netsch, Qualcomm
--
--     -   Patrick Boyle, Qualcomm
--
--     -   Daniel Rakos, RasterGrid
--
--     -   Ralph Potter, Samsung
--
--     -   Hans-Kristian Arntzen, VALVE
--
--     -   Samuel Pitoiset, VALVE
--
-- == Description
--
-- This extension significantly simplifies synchronization in Vulkan by
-- removing the need for image layout transitions in most cases. In
-- particular, it guarantees that using the
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' layout everywhere
-- possible is just as efficient as using the other layouts.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceUnifiedImageLayoutsFeaturesKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_attachment_feedback_loop_layout VK_EXT_attachment_feedback_loop_layout>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingAttachmentInfo':
--
--     -   'AttachmentFeedbackLoopInfoEXT'
--
-- == New Enum Constants
--
-- -   'KHR_UNIFIED_IMAGE_LAYOUTS_EXTENSION_NAME'
--
-- -   'KHR_UNIFIED_IMAGE_LAYOUTS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFIED_IMAGE_LAYOUTS_FEATURES_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_attachment_feedback_loop_layout VK_EXT_attachment_feedback_loop_layout>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ATTACHMENT_FEEDBACK_LOOP_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2024-10-15 (Shahbaz Youssefi)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_unified_image_layouts Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_unified_image_layouts  ( PhysicalDeviceUnifiedImageLayoutsFeaturesKHR(..)
                                                       , AttachmentFeedbackLoopInfoEXT(..)
                                                       , KHR_UNIFIED_IMAGE_LAYOUTS_SPEC_VERSION
                                                       , pattern KHR_UNIFIED_IMAGE_LAYOUTS_SPEC_VERSION
                                                       , KHR_UNIFIED_IMAGE_LAYOUTS_EXTENSION_NAME
                                                       , pattern KHR_UNIFIED_IMAGE_LAYOUTS_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ATTACHMENT_FEEDBACK_LOOP_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFIED_IMAGE_LAYOUTS_FEATURES_KHR))
-- | VkPhysicalDeviceUnifiedImageLayoutsFeaturesKHR - Structure describing
-- whether the implementation provides unified image layouts
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceUnifiedImageLayoutsFeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceUnifiedImageLayoutsFeaturesKHR', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_unified_image_layouts VK_KHR_unified_image_layouts>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceUnifiedImageLayoutsFeaturesKHR = PhysicalDeviceUnifiedImageLayoutsFeaturesKHR
  { -- | #features-unifiedImageLayouts# @unifiedImageLayouts@ specifies whether
    -- usage of 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL', where
    -- valid, incurs no loss in efficiency. Additionally, it indicates whether
    -- it /can/ be used in place of
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'.
    unifiedImageLayouts :: Bool
  , -- | #features-unifiedImageLayoutsVideo# @unifiedImageLayoutsVideo@ specifies
    -- whether 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' can be
    -- used in place of any of the following image layouts with no loss in
    -- efficiency.
    --
    -- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageLayout VK_IMAGE_LAYOUT_VIDEO_DECODE_DST_KHR>
    --
    -- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageLayout VK_IMAGE_LAYOUT_VIDEO_DECODE_SRC_KHR>
    --
    -- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageLayout VK_IMAGE_LAYOUT_VIDEO_DECODE_DPB_KHR>
    --
    -- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageLayout VK_IMAGE_LAYOUT_VIDEO_ENCODE_DST_KHR>
    --
    -- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageLayout VK_IMAGE_LAYOUT_VIDEO_ENCODE_SRC_KHR>
    --
    -- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageLayout VK_IMAGE_LAYOUT_VIDEO_ENCODE_DPB_KHR>
    --
    -- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageLayout VK_IMAGE_LAYOUT_VIDEO_ENCODE_QUANTIZATION_MAP_KHR>
    unifiedImageLayoutsVideo :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceUnifiedImageLayoutsFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceUnifiedImageLayoutsFeaturesKHR

instance ToCStruct PhysicalDeviceUnifiedImageLayoutsFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceUnifiedImageLayoutsFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFIED_IMAGE_LAYOUTS_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (unifiedImageLayouts))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (unifiedImageLayoutsVideo))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFIED_IMAGE_LAYOUTS_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceUnifiedImageLayoutsFeaturesKHR where
  peekCStruct p = do
    unifiedImageLayouts <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    unifiedImageLayoutsVideo <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceUnifiedImageLayoutsFeaturesKHR
             (bool32ToBool unifiedImageLayouts)
             (bool32ToBool unifiedImageLayoutsVideo)

instance Storable PhysicalDeviceUnifiedImageLayoutsFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceUnifiedImageLayoutsFeaturesKHR where
  zero = PhysicalDeviceUnifiedImageLayoutsFeaturesKHR
           zero
           zero


-- | VkAttachmentFeedbackLoopInfoEXT - Structure specifying whether feedback
-- loop is enabled for an attachment
--
-- == Valid Usage
--
-- -   #VUID-VkAttachmentFeedbackLoopInfoEXT-unifiedImageLayouts-10782# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-unifiedImageLayouts unifiedImageLayouts>
--     feature is not enabled, @feedbackLoopEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAttachmentFeedbackLoopInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ATTACHMENT_FEEDBACK_LOOP_INFO_EXT'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_attachment_feedback_loop_layout VK_EXT_attachment_feedback_loop_layout>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_unified_image_layouts VK_KHR_unified_image_layouts>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AttachmentFeedbackLoopInfoEXT = AttachmentFeedbackLoopInfoEXT
  { -- | @feedbackLoopEnable@ specifies that
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-feedbackloop feedback loop is enabled>
    -- for the attachment identified by
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingAttachmentInfo'::@imageView@.
    feedbackLoopEnable :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AttachmentFeedbackLoopInfoEXT)
#endif
deriving instance Show AttachmentFeedbackLoopInfoEXT

instance ToCStruct AttachmentFeedbackLoopInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AttachmentFeedbackLoopInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ATTACHMENT_FEEDBACK_LOOP_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (feedbackLoopEnable))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ATTACHMENT_FEEDBACK_LOOP_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct AttachmentFeedbackLoopInfoEXT where
  peekCStruct p = do
    feedbackLoopEnable <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ AttachmentFeedbackLoopInfoEXT
             (bool32ToBool feedbackLoopEnable)

instance Storable AttachmentFeedbackLoopInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AttachmentFeedbackLoopInfoEXT where
  zero = AttachmentFeedbackLoopInfoEXT
           zero


type KHR_UNIFIED_IMAGE_LAYOUTS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_UNIFIED_IMAGE_LAYOUTS_SPEC_VERSION"
pattern KHR_UNIFIED_IMAGE_LAYOUTS_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_UNIFIED_IMAGE_LAYOUTS_SPEC_VERSION = 1


type KHR_UNIFIED_IMAGE_LAYOUTS_EXTENSION_NAME = "VK_KHR_unified_image_layouts"

-- No documentation found for TopLevel "VK_KHR_UNIFIED_IMAGE_LAYOUTS_EXTENSION_NAME"
pattern KHR_UNIFIED_IMAGE_LAYOUTS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_UNIFIED_IMAGE_LAYOUTS_EXTENSION_NAME = "VK_KHR_unified_image_layouts"

