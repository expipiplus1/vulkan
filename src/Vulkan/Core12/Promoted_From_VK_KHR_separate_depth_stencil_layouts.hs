{-# language CPP #-}
module Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts  ( PhysicalDeviceSeparateDepthStencilLayoutsFeatures(..)
                                                                          , AttachmentReferenceStencilLayout(..)
                                                                          , AttachmentDescriptionStencilLayout(..)
                                                                          , ImageLayout(..)
                                                                          , StructureType(..)
                                                                          ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures - Structure
-- describing whether the implementation can do depth and stencil image
-- barriers separately
--
-- = Members
--
-- The members of the 'PhysicalDeviceSeparateDepthStencilLayoutsFeatures'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceSeparateDepthStencilLayoutsFeatures' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceSeparateDepthStencilLayoutsFeatures' /can/ also be
-- included in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to enable the feature.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSeparateDepthStencilLayoutsFeatures = PhysicalDeviceSeparateDepthStencilLayoutsFeatures
  { -- | #extension-features-separateDepthStencilLayouts#
    -- @separateDepthStencilLayouts@ indicates whether the implementation
    -- supports a 'Vulkan.Core10.OtherTypes.ImageMemoryBarrier' for a
    -- depth\/stencil image with only one of
    -- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
    -- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT' set,
    -- and whether
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
    -- or
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
    -- can be used.
    separateDepthStencilLayouts :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSeparateDepthStencilLayoutsFeatures)
#endif
deriving instance Show PhysicalDeviceSeparateDepthStencilLayoutsFeatures

instance ToCStruct PhysicalDeviceSeparateDepthStencilLayoutsFeatures where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSeparateDepthStencilLayoutsFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (separateDepthStencilLayouts))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceSeparateDepthStencilLayoutsFeatures where
  peekCStruct p = do
    separateDepthStencilLayouts <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceSeparateDepthStencilLayoutsFeatures
             (bool32ToBool separateDepthStencilLayouts)

instance Storable PhysicalDeviceSeparateDepthStencilLayoutsFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSeparateDepthStencilLayoutsFeatures where
  zero = PhysicalDeviceSeparateDepthStencilLayoutsFeatures
           zero


-- | VkAttachmentReferenceStencilLayout - Structure specifying an attachment
-- description
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AttachmentReferenceStencilLayout = AttachmentReferenceStencilLayout
  { -- | @stencilLayout@ is a 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
    -- specifying the layout the stencil aspect of the attachment uses during
    -- the subpass.
    --
    -- #VUID-VkAttachmentReferenceStencilLayout-stencilLayout-03318#
    -- @stencilLayout@ /must/ not be
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED',
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED',
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL',
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL',
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL',
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL',
    -- or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PRESENT_SRC_KHR'
    --
    -- #VUID-VkAttachmentReferenceStencilLayout-stencilLayout-parameter#
    -- @stencilLayout@ /must/ be a valid
    -- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
    stencilLayout :: ImageLayout }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AttachmentReferenceStencilLayout)
#endif
deriving instance Show AttachmentReferenceStencilLayout

instance ToCStruct AttachmentReferenceStencilLayout where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AttachmentReferenceStencilLayout{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageLayout)) (stencilLayout)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct AttachmentReferenceStencilLayout where
  peekCStruct p = do
    stencilLayout <- peek @ImageLayout ((p `plusPtr` 16 :: Ptr ImageLayout))
    pure $ AttachmentReferenceStencilLayout
             stencilLayout

instance Storable AttachmentReferenceStencilLayout where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AttachmentReferenceStencilLayout where
  zero = AttachmentReferenceStencilLayout
           zero


-- | VkAttachmentDescriptionStencilLayout - Structure specifying an
-- attachment description
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AttachmentDescriptionStencilLayout = AttachmentDescriptionStencilLayout
  { -- | @stencilInitialLayout@ is the layout the stencil aspect of the
    -- attachment image subresource will be in when a render pass instance
    -- begins.
    --
    -- #VUID-VkAttachmentDescriptionStencilLayout-stencilInitialLayout-03308#
    -- @stencilInitialLayout@ /must/ not be
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL',
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL',
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL',
    -- or
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
    --
    -- #VUID-VkAttachmentDescriptionStencilLayout-stencilInitialLayout-parameter#
    -- @stencilInitialLayout@ /must/ be a valid
    -- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
    stencilInitialLayout :: ImageLayout
  , -- | @stencilFinalLayout@ is the layout the stencil aspect of the attachment
    -- image subresource will be transitioned to when a render pass instance
    -- ends.
    --
    -- #VUID-VkAttachmentDescriptionStencilLayout-stencilFinalLayout-03309#
    -- @stencilFinalLayout@ /must/ not be
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL',
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL',
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL',
    -- or
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
    --
    -- #VUID-VkAttachmentDescriptionStencilLayout-stencilFinalLayout-03310#
    -- @stencilFinalLayout@ /must/ not be
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED' or
    -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED'
    --
    -- #VUID-VkAttachmentDescriptionStencilLayout-stencilFinalLayout-parameter#
    -- @stencilFinalLayout@ /must/ be a valid
    -- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
    stencilFinalLayout :: ImageLayout
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AttachmentDescriptionStencilLayout)
#endif
deriving instance Show AttachmentDescriptionStencilLayout

instance ToCStruct AttachmentDescriptionStencilLayout where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AttachmentDescriptionStencilLayout{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageLayout)) (stencilInitialLayout)
    poke ((p `plusPtr` 20 :: Ptr ImageLayout)) (stencilFinalLayout)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageLayout)) (zero)
    poke ((p `plusPtr` 20 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct AttachmentDescriptionStencilLayout where
  peekCStruct p = do
    stencilInitialLayout <- peek @ImageLayout ((p `plusPtr` 16 :: Ptr ImageLayout))
    stencilFinalLayout <- peek @ImageLayout ((p `plusPtr` 20 :: Ptr ImageLayout))
    pure $ AttachmentDescriptionStencilLayout
             stencilInitialLayout stencilFinalLayout

instance Storable AttachmentDescriptionStencilLayout where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AttachmentDescriptionStencilLayout where
  zero = AttachmentDescriptionStencilLayout
           zero
           zero

