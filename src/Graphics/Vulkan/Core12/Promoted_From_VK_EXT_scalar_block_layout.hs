{-# language CPP #-}
module Graphics.Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout  ( PhysicalDeviceScalarBlockLayoutFeatures(..)
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
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceScalarBlockLayoutFeatures - Structure indicating support
-- for scalar block layouts
--
-- = Members
--
-- The members of the 'PhysicalDeviceScalarBlockLayoutFeatures' structure
-- describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceScalarBlockLayoutFeatures' structure is included
-- in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceScalarBlockLayoutFeatures' /can/ also be included in the
-- @pNext@ chain of 'Graphics.Vulkan.Core10.Device.DeviceCreateInfo' to
-- enable this feature.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceScalarBlockLayoutFeatures = PhysicalDeviceScalarBlockLayoutFeatures
  { -- | @scalarBlockLayout@ indicates that the implementation supports the
    -- layout of resource blocks in shaders using
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-alignment-requirements scalar alignment>.
    scalarBlockLayout :: Bool }
  deriving (Typeable)
deriving instance Show PhysicalDeviceScalarBlockLayoutFeatures

instance ToCStruct PhysicalDeviceScalarBlockLayoutFeatures where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceScalarBlockLayoutFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (scalarBlockLayout))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceScalarBlockLayoutFeatures where
  peekCStruct p = do
    scalarBlockLayout <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceScalarBlockLayoutFeatures
             (bool32ToBool scalarBlockLayout)

instance Storable PhysicalDeviceScalarBlockLayoutFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceScalarBlockLayoutFeatures where
  zero = PhysicalDeviceScalarBlockLayoutFeatures
           zero

