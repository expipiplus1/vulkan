{-# language CPP #-}
module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout  ( PhysicalDeviceUniformBufferStandardLayoutFeatures(..)
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
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceUniformBufferStandardLayoutFeatures - Structure
-- indicating support for std430-like packing in uniform buffers
--
-- = Members
--
-- The members of the 'PhysicalDeviceUniformBufferStandardLayoutFeatures'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceUniformBufferStandardLayoutFeatures' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceUniformBufferStandardLayoutFeatures' /can/ also be
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core10.Device.DeviceCreateInfo' to enable this feature.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceUniformBufferStandardLayoutFeatures = PhysicalDeviceUniformBufferStandardLayoutFeatures
  { -- | @uniformBufferStandardLayout@ indicates that the implementation supports
    -- the same layouts for uniform buffers as for storage and other kinds of
    -- buffers. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-resources-standard-layout Standard Buffer Layout>.
    uniformBufferStandardLayout :: Bool }
  deriving (Typeable)
deriving instance Show PhysicalDeviceUniformBufferStandardLayoutFeatures

instance ToCStruct PhysicalDeviceUniformBufferStandardLayoutFeatures where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceUniformBufferStandardLayoutFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (uniformBufferStandardLayout))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceUniformBufferStandardLayoutFeatures where
  peekCStruct p = do
    uniformBufferStandardLayout <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceUniformBufferStandardLayoutFeatures
             (bool32ToBool uniformBufferStandardLayout)

instance Storable PhysicalDeviceUniformBufferStandardLayoutFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceUniformBufferStandardLayoutFeatures where
  zero = PhysicalDeviceUniformBufferStandardLayoutFeatures
           zero

