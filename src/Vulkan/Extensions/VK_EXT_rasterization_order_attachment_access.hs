{-# language CPP #-}
-- | = Name
--
-- VK_EXT_rasterization_order_attachment_access - device extension
--
-- == VK_EXT_rasterization_order_attachment_access
--
-- [__Name String__]
--     @VK_EXT_rasterization_order_attachment_access@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     464
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_rasterization_order_attachment_access] @janharaldfredriksen-arm%0A*Here describe the issue or question you have about the VK_EXT_rasterization_order_attachment_access extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_rasterization_order_attachment_access.adoc VK_EXT_rasterization_order_attachment_access>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-07-04
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Jan-Harald Fredriksen, Arm
--
-- == Description
--
-- This extension extends the mechanism of input attachments to allow
-- access to framebuffer attachments that are used both as input and as
-- color or depth\/stencil attachments from one fragment to the next, in
-- rasterization order, without explicit synchronization.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT'
--
-- == New Enums
--
-- -   'Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits.PipelineColorBlendStateCreateFlagBits'
--
-- -   'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PipelineDepthStencilStateCreateFlagBits'
--
-- == New Enum Constants
--
-- -   'EXT_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_EXTENSION_NAME'
--
-- -   'EXT_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits.PipelineColorBlendStateCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits.PIPELINE_COLOR_BLEND_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PipelineDepthStencilStateCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_FEATURES_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SubpassDescriptionFlagBits':
--
--     -   'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_COLOR_ACCESS_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_EXT'
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2022-07-04 (Jan-Harald Fredriksen)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT',
-- 'Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits.PipelineColorBlendStateCreateFlagBits',
-- 'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PipelineDepthStencilStateCreateFlagBits'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_rasterization_order_attachment_access Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_rasterization_order_attachment_access  ( PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT(..)
                                                                       , EXT_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_SPEC_VERSION
                                                                       , pattern EXT_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_SPEC_VERSION
                                                                       , EXT_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_EXTENSION_NAME
                                                                       , pattern EXT_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_FEATURES_EXT))
-- | VkPhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT -
-- Structure describing whether rasterization order attachment access can
-- be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT'
-- structure is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT'
-- /can/ also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_rasterization_order_attachment_access VK_EXT_rasterization_order_attachment_access>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT = PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT
  { -- | #features-rasterizationOrderColorAttachmentAccess#
    -- @rasterizationOrderColorAttachmentAccess@ indicates that rasterization
    -- order access to color and input attachments is supported by the
    -- implementation.
    rasterizationOrderColorAttachmentAccess :: Bool
  , -- | #features-rasterizationOrderDepthAttachmentAccess#
    -- @rasterizationOrderDepthAttachmentAccess@ indicates that rasterization
    -- order access to the depth aspect of depth\/stencil and input attachments
    -- is supported by the implementation.
    rasterizationOrderDepthAttachmentAccess :: Bool
  , -- | #features-rasterizationOrderStencilAttachmentAccess#
    -- @rasterizationOrderStencilAttachmentAccess@ indicates that rasterization
    -- order access to the stencil aspect of depth\/stencil and input
    -- attachments is supported by the implementation.
    rasterizationOrderStencilAttachmentAccess :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT

instance ToCStruct PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (rasterizationOrderColorAttachmentAccess))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (rasterizationOrderDepthAttachmentAccess))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (rasterizationOrderStencilAttachmentAccess))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT where
  peekCStruct p = do
    rasterizationOrderColorAttachmentAccess <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    rasterizationOrderDepthAttachmentAccess <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    rasterizationOrderStencilAttachmentAccess <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT
             (bool32ToBool rasterizationOrderColorAttachmentAccess)
             (bool32ToBool rasterizationOrderDepthAttachmentAccess)
             (bool32ToBool rasterizationOrderStencilAttachmentAccess)

instance Storable PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT where
  zero = PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT
           zero
           zero
           zero


type EXT_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_SPEC_VERSION"
pattern EXT_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_SPEC_VERSION = 1


type EXT_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_EXTENSION_NAME = "VK_EXT_rasterization_order_attachment_access"

-- No documentation found for TopLevel "VK_EXT_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_EXTENSION_NAME"
pattern EXT_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_EXTENSION_NAME = "VK_EXT_rasterization_order_attachment_access"

