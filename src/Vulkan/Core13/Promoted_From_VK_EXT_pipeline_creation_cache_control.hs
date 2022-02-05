{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_pipeline_creation_cache_control"
module Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_cache_control  ( PhysicalDevicePipelineCreationCacheControlFeatures(..)
                                                                           , PipelineCacheCreateFlagBits(..)
                                                                           , PipelineCacheCreateFlags
                                                                           , StructureType(..)
                                                                           , Result(..)
                                                                           , PipelineCreateFlagBits(..)
                                                                           , PipelineCreateFlags
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES))
import Vulkan.Core10.Enums.PipelineCacheCreateFlagBits (PipelineCacheCreateFlagBits(..))
import Vulkan.Core10.Enums.PipelineCacheCreateFlagBits (PipelineCacheCreateFlags)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlagBits(..))
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDevicePipelineCreationCacheControlFeatures - Structure
-- describing whether pipeline cache control can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDevicePipelineCreationCacheControlFeatures' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDevicePipelineCreationCacheControlFeatures' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_creation_cache_control VK_EXT_pipeline_creation_cache_control>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePipelineCreationCacheControlFeatures = PhysicalDevicePipelineCreationCacheControlFeatures
  { -- | #extension-features-pipelineCreationCacheControl#
    -- @pipelineCreationCacheControl@ indicates that the implementation
    -- supports:
    --
    -- -   The following /can/ be used in @Vk*PipelineCreateInfo@::@flags@:
    --
    --     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT'
    --
    --     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT'
    --
    -- -   The following /can/ be used in
    --     'Vulkan.Core10.PipelineCache.PipelineCacheCreateInfo'::@flags@:
    --
    --     -   'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT'
    pipelineCreationCacheControl :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePipelineCreationCacheControlFeatures)
#endif
deriving instance Show PhysicalDevicePipelineCreationCacheControlFeatures

instance ToCStruct PhysicalDevicePipelineCreationCacheControlFeatures where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePipelineCreationCacheControlFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (pipelineCreationCacheControl))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePipelineCreationCacheControlFeatures where
  peekCStruct p = do
    pipelineCreationCacheControl <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePipelineCreationCacheControlFeatures
             (bool32ToBool pipelineCreationCacheControl)

instance Storable PhysicalDevicePipelineCreationCacheControlFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePipelineCreationCacheControlFeatures where
  zero = PhysicalDevicePipelineCreationCacheControlFeatures
           zero

