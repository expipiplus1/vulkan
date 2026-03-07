{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_index_type_uint8Roadmap"
module Vulkan.Core14.Promoted_From_VK_KHR_index_type_uint8Roadmap  ( PhysicalDeviceIndexTypeUint8Features(..)
                                                                   , IndexType(..)
                                                                   , StructureType(..)
                                                                   ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES))
import Vulkan.Core10.Enums.IndexType (IndexType(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceIndexTypeUint8Features - Structure describing whether
-- uint8 index type can be used
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceIndexTypeUint8Features' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceIndexTypeUint8Features', it /must/ add an instance of the
-- structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_index_type_uint8 VK_EXT_index_type_uint8>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_index_type_uint8 VK_KHR_index_type_uint8>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceIndexTypeUint8Features = PhysicalDeviceIndexTypeUint8Features
  { -- | #extension-features-indexTypeUint8# @indexTypeUint8@ indicates that
    -- 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT8' can be used with
    -- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.cmdBindIndexBuffer2'
    -- and 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'.
    indexTypeUint8 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceIndexTypeUint8Features)
#endif
deriving instance Show PhysicalDeviceIndexTypeUint8Features

instance ToCStruct PhysicalDeviceIndexTypeUint8Features where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceIndexTypeUint8Features{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (indexTypeUint8))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceIndexTypeUint8Features where
  peekCStruct p = do
    indexTypeUint8 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceIndexTypeUint8Features
             (bool32ToBool indexTypeUint8)

instance Storable PhysicalDeviceIndexTypeUint8Features where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceIndexTypeUint8Features where
  zero = PhysicalDeviceIndexTypeUint8Features
           zero

