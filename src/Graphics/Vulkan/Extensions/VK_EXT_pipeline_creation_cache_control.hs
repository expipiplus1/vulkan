{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_pipeline_creation_cache_control  ( pattern ERROR_PIPELINE_COMPILE_REQUIRED_EXT
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
import Graphics.Vulkan.Core10.Enums.Result (Result(PIPELINE_COMPILE_REQUIRED_EXT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT))
-- No documentation found for TopLevel "VK_ERROR_PIPELINE_COMPILE_REQUIRED_EXT"
pattern ERROR_PIPELINE_COMPILE_REQUIRED_EXT = PIPELINE_COMPILE_REQUIRED_EXT


-- | VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT - Structure
-- describing whether pipeline cache control can be supported by an
-- implementation
--
-- = Members
--
-- The members of the
-- 'PhysicalDevicePipelineCreationCacheControlFeaturesEXT' structure
-- describe the following features:
--
-- = Description
--
-- If the 'PhysicalDevicePipelineCreationCacheControlFeaturesEXT' structure
-- is included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDevicePipelineCreationCacheControlFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of
-- 'Graphics.Vulkan.Core10.Device.DeviceCreateInfo' to enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePipelineCreationCacheControlFeaturesEXT = PhysicalDevicePipelineCreationCacheControlFeaturesEXT
  { -- | @pipelineCreationCacheControl@ indicates that the implementation
    -- supports:
    --
    -- -   The following /can/ be used in @Vk*PipelineCreateInfo@::@flags@:
    --
    --     -   'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT'
    --
    --     -   'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT'
    --
    -- -   The following /can/ be used in
    --     'Graphics.Vulkan.Core10.PipelineCache.PipelineCacheCreateInfo'::@flags@:
    --
    --     -   'Graphics.Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT'
    pipelineCreationCacheControl :: Bool }
  deriving (Typeable)
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

