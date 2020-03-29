{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NVX_multiview_per_view_attributes  ( PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX(..)
                                                                        , NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION
                                                                        , pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION
                                                                        , NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
                                                                        , pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
                                                                        ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX))
-- | VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX - Structure
-- describing multiview limits that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX' structure
-- describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX' structure
-- is included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX = PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
  { -- | @perViewPositionAllComponents@ is 'Graphics.Vulkan.Core10.BaseType.TRUE'
    -- if the implementation supports per-view position values that differ in
    -- components other than the X component.
    perViewPositionAllComponents :: Bool }
  deriving (Typeable)
deriving instance Show PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX

instance ToCStruct PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (perViewPositionAllComponents))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  peekCStruct p = do
    perViewPositionAllComponents <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             (bool32ToBool perViewPositionAllComponents)

instance Storable PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  zero = PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
           zero


type NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION"
pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION :: forall a . Integral a => a
pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION = 1


type NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME = "VK_NVX_multiview_per_view_attributes"

-- No documentation found for TopLevel "VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME"
pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME = "VK_NVX_multiview_per_view_attributes"

