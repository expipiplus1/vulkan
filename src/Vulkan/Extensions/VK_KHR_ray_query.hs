{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_ray_query"
module Vulkan.Extensions.VK_KHR_ray_query  ( PhysicalDeviceRayQueryFeaturesKHR(..)
                                           , KHR_RAY_QUERY_SPEC_VERSION
                                           , pattern KHR_RAY_QUERY_SPEC_VERSION
                                           , KHR_RAY_QUERY_EXTENSION_NAME
                                           , pattern KHR_RAY_QUERY_EXTENSION_NAME
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
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_QUERY_FEATURES_KHR))

-- No documentation found for TopLevel "VkPhysicalDeviceRayQueryFeaturesKHR"
data PhysicalDeviceRayQueryFeaturesKHR = PhysicalDeviceRayQueryFeaturesKHR
  { -- No documentation found for Nested "VkPhysicalDeviceRayQueryFeaturesKHR" "rayQuery"
    rayQuery :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRayQueryFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceRayQueryFeaturesKHR

instance ToCStruct PhysicalDeviceRayQueryFeaturesKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRayQueryFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_QUERY_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (rayQuery))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_QUERY_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRayQueryFeaturesKHR where
  peekCStruct p = do
    rayQuery <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceRayQueryFeaturesKHR
             (bool32ToBool rayQuery)


instance Storable PhysicalDeviceRayQueryFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRayQueryFeaturesKHR where
  zero = PhysicalDeviceRayQueryFeaturesKHR
           zero


type KHR_RAY_QUERY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_RAY_QUERY_SPEC_VERSION"
pattern KHR_RAY_QUERY_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_RAY_QUERY_SPEC_VERSION = 1


type KHR_RAY_QUERY_EXTENSION_NAME = "VK_KHR_ray_query"

-- No documentation found for TopLevel "VK_KHR_RAY_QUERY_EXTENSION_NAME"
pattern KHR_RAY_QUERY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_RAY_QUERY_EXTENSION_NAME = "VK_KHR_ray_query"

