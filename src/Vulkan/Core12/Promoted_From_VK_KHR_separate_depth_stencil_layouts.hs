{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_separate_depth_stencil_layouts"
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

-- No documentation found for TopLevel "VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures"
data PhysicalDeviceSeparateDepthStencilLayoutsFeatures = PhysicalDeviceSeparateDepthStencilLayoutsFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures" "separateDepthStencilLayouts"
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



-- No documentation found for TopLevel "VkAttachmentReferenceStencilLayout"
data AttachmentReferenceStencilLayout = AttachmentReferenceStencilLayout
  { -- No documentation found for Nested "VkAttachmentReferenceStencilLayout" "stencilLayout"
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



-- No documentation found for TopLevel "VkAttachmentDescriptionStencilLayout"
data AttachmentDescriptionStencilLayout = AttachmentDescriptionStencilLayout
  { -- No documentation found for Nested "VkAttachmentDescriptionStencilLayout" "stencilInitialLayout"
    stencilInitialLayout :: ImageLayout
  , -- No documentation found for Nested "VkAttachmentDescriptionStencilLayout" "stencilFinalLayout"
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

