{-# language CPP #-}
-- | = Name
--
-- VK_EXT_attachment_feedback_loop_layout - device extension
--
-- == VK_EXT_attachment_feedback_loop_layout
--
-- [__Name String__]
--     @VK_EXT_attachment_feedback_loop_layout@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     340
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Joshua Ashton
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_attachment_feedback_loop_layout] @Joshua-Ashton%0A*Here describe the issue or question you have about the VK_EXT_attachment_feedback_loop_layout extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_attachment_feedback_loop_layout.adoc VK_EXT_attachment_feedback_loop_layout>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-04-04
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Joshua Ashton, Valve
--
--     -   Faith Ekstrand, Collabora
--
--     -   Bas Nieuwenhuizen, Google
--
--     -   Samuel Iglesias Gonsálvez, Igalia
--
--     -   Ralph Potter, Samsung
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Ricardo Garcia, Igalia
--
-- == Description
--
-- This extension adds a new image layout,
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT',
-- which allows applications to have an image layout in which they are able
-- to both render to and sample\/fetch from the same subresource of an
-- image in a given render pass.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_EXTENSION_NAME'
--
-- -   'EXT_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits':
--
--     -   'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_FEEDBACK_LOOP_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.ImageLayout.ImageLayout':
--
--     -   'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 2, 2022-04-04 (Joshua Ashton)
--
--     -   Renamed from VALVE to EXT.
--
-- -   Revision 1, 2021-03-09 (Joshua Ashton)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_attachment_feedback_loop_layout Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_attachment_feedback_loop_layout  ( PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT(..)
                                                                 , EXT_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_SPEC_VERSION
                                                                 , pattern EXT_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_SPEC_VERSION
                                                                 , EXT_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_EXTENSION_NAME
                                                                 , pattern EXT_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_FEATURES_EXT))
-- | VkPhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT - Structure
-- indicating support for a render feedback loop image layout
--
-- = Members
--
-- This structure describes the following feature:
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_attachment_feedback_loop_layout VK_EXT_attachment_feedback_loop_layout>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT = PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT
  { -- | #features-attachmentFeedbackLoopLayout# @attachmentFeedbackLoopLayout@
    -- indicates whether the implementation supports using
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
    -- image layout for images created with
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'.
    attachmentFeedbackLoopLayout :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT

instance ToCStruct PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (attachmentFeedbackLoopLayout))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT where
  peekCStruct p = do
    attachmentFeedbackLoopLayout <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT
             (bool32ToBool attachmentFeedbackLoopLayout)

instance Storable PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT where
  zero = PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT
           zero


type EXT_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_SPEC_VERSION"
pattern EXT_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_SPEC_VERSION = 2


type EXT_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_EXTENSION_NAME = "VK_EXT_attachment_feedback_loop_layout"

-- No documentation found for TopLevel "VK_EXT_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_EXTENSION_NAME"
pattern EXT_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_EXTENSION_NAME = "VK_EXT_attachment_feedback_loop_layout"

