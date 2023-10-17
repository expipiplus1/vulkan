{-# language CPP #-}
-- | = Name
--
-- VK_EXT_dynamic_rendering_unused_attachments - device extension
--
-- == VK_EXT_dynamic_rendering_unused_attachments
--
-- [__Name String__]
--     @VK_EXT_dynamic_rendering_unused_attachments@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     500
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--     and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Version 1.3>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_dynamic_rendering_unused_attachments] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_EXT_dynamic_rendering_unused_attachments extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_dynamic_rendering_unused_attachments.adoc VK_EXT_dynamic_rendering_unused_attachments>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-05-22
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Daniel Story, Nintendo
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   James Fitzpatrick, Imagination Technologies
--
--     -   Pan Gao, Huawei Technologies
--
--     -   Ricardo Garcia, Igalia
--
--     -   Stu Smith, AMD
--
-- == Description
--
-- This extension lifts some restrictions in the @VK_KHR_dynamic_rendering@
-- extension to allow render pass instances and bound pipelines within
-- those render pass instances to have an unused attachment specified in
-- one but not the other. It also allows pipelines to use different formats
-- in a render pass as long the attachment is NULL.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_EXTENSION_NAME'
--
-- -   'EXT_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_FEATURES_EXT'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2023-05-22 (Piers Daniell)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_dynamic_rendering_unused_attachments Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_dynamic_rendering_unused_attachments  ( PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT(..)
                                                                      , EXT_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_SPEC_VERSION
                                                                      , pattern EXT_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_SPEC_VERSION
                                                                      , EXT_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_EXTENSION_NAME
                                                                      , pattern EXT_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_FEATURES_EXT))
-- | VkPhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT - Structure
-- describing the dynamic rendering unused attachment features that can be
-- supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT'
-- structure is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT'
-- /can/ also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_dynamic_rendering_unused_attachments VK_EXT_dynamic_rendering_unused_attachments>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT = PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT
  { -- | #features-dynamicRenderingUnusedAttachments#
    -- @dynamicRenderingUnusedAttachments@ indicates that the implementation
    -- supports binding graphics pipelines within a render pass instance where
    -- any pipeline
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@
    -- element with a format other than
    -- 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED' is allowed with a
    -- corresponding
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
    -- element with a @imageView@ equal to
    -- 'Vulkan.Core10.APIConstants.NULL_HANDLE', or any pipeline
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@
    -- element with a 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED' format is
    -- allowed with a corresponding
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
    -- element with a non-'Vulkan.Core10.APIConstants.NULL_HANDLE' @imageView@.
    -- Also a
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
    -- other than 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED' is allowed with
    -- a 'Vulkan.Core10.APIConstants.NULL_HANDLE'
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment@,
    -- or a
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
    -- of 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED' is allowed with a
    -- non-'Vulkan.Core10.APIConstants.NULL_HANDLE'
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment@.
    -- Also a
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
    -- other than 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED' is allowed with
    -- a 'Vulkan.Core10.APIConstants.NULL_HANDLE'
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment@,
    -- or a
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
    -- of 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED' is allowed with a
    -- non-'Vulkan.Core10.APIConstants.NULL_HANDLE'
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment@.
    -- Any writes to a
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@,
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment@,
    -- or
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment@
    -- with 'Vulkan.Core10.APIConstants.NULL_HANDLE' are discarded.
    dynamicRenderingUnusedAttachments :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT

instance ToCStruct PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (dynamicRenderingUnusedAttachments))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT where
  peekCStruct p = do
    dynamicRenderingUnusedAttachments <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT
             (bool32ToBool dynamicRenderingUnusedAttachments)

instance Storable PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT where
  zero = PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT
           zero


type EXT_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_SPEC_VERSION"
pattern EXT_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_SPEC_VERSION = 1


type EXT_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_EXTENSION_NAME = "VK_EXT_dynamic_rendering_unused_attachments"

-- No documentation found for TopLevel "VK_EXT_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_EXTENSION_NAME"
pattern EXT_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_EXTENSION_NAME = "VK_EXT_dynamic_rendering_unused_attachments"

