{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_pipeline_creation_cache_control"
module Vulkan.Extensions.VK_EXT_pipeline_creation_cache_control  ( pattern ERROR_PIPELINE_COMPILE_REQUIRED_EXT
                                                                 , PhysicalDevicePipelineCreationCacheControlFeaturesEXT(..)
                                                                 , EXT_PIPELINE_CREATION_CACHE_CONTROL_SPEC_VERSION
                                                                 , pattern EXT_PIPELINE_CREATION_CACHE_CONTROL_SPEC_VERSION
                                                                 , EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME
                                                                 , pattern EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME
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
import Vulkan.Core10.Enums.Result (Result(PIPELINE_COMPILE_REQUIRED_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT))
-- No documentation found for TopLevel "VK_ERROR_PIPELINE_COMPILE_REQUIRED_EXT"
pattern ERROR_PIPELINE_COMPILE_REQUIRED_EXT = PIPELINE_COMPILE_REQUIRED_EXT



-- No documentation found for TopLevel "VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT"
data PhysicalDevicePipelineCreationCacheControlFeaturesEXT = PhysicalDevicePipelineCreationCacheControlFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT" "pipelineCreationCacheControl"
    pipelineCreationCacheControl :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePipelineCreationCacheControlFeaturesEXT)
#endif
deriving instance Show PhysicalDevicePipelineCreationCacheControlFeaturesEXT

instance ToCStruct PhysicalDevicePipelineCreationCacheControlFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePipelineCreationCacheControlFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (pipelineCreationCacheControl))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePipelineCreationCacheControlFeaturesEXT where
  peekCStruct p = do
    pipelineCreationCacheControl <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePipelineCreationCacheControlFeaturesEXT
             (bool32ToBool pipelineCreationCacheControl)


instance Storable PhysicalDevicePipelineCreationCacheControlFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePipelineCreationCacheControlFeaturesEXT where
  zero = PhysicalDevicePipelineCreationCacheControlFeaturesEXT
           zero


type EXT_PIPELINE_CREATION_CACHE_CONTROL_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_SPEC_VERSION"
pattern EXT_PIPELINE_CREATION_CACHE_CONTROL_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PIPELINE_CREATION_CACHE_CONTROL_SPEC_VERSION = 3


type EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME = "VK_EXT_pipeline_creation_cache_control"

-- No documentation found for TopLevel "VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME"
pattern EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME = "VK_EXT_pipeline_creation_cache_control"

