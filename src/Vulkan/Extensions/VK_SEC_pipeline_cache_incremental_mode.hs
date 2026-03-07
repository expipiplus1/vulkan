{-# language CPP #-}
-- | = Name
--
-- VK_SEC_pipeline_cache_incremental_mode - device extension
--
-- = VK_SEC_pipeline_cache_incremental_mode
--
-- [__Name String__]
--     @VK_SEC_pipeline_cache_incremental_mode@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     638
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Chris Hambacher
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_SEC_pipeline_cache_incremental_mode] @chambacher%0A*Here describe the issue or question you have about the VK_SEC_pipeline_cache_incremental_mode extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-06-24
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Chris Hambacher, Samsung
--
--     -   Mohan Maiya, Samsung
--
--     -   Brandon Schade, Samsung
--
-- == Description
--
-- This extension allows layered implementations such as ANGLE to modify
-- the default behavior of VkPipelineCache to return only the incremental
-- data from the previous call to vkGetPipelineCacheData. Application
-- developers should avoid using this extension.
--
-- There is currently no specification language written for this extension.
-- The links to APIs defined by the extension are to stubs that only
-- include generated content such as API declarations and implicit valid
-- usage statements.
--
-- This extension is only intended for use in specific embedded
-- environments with known implementation details, and is therefore
-- undocumented.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePipelineCacheIncrementalModeFeaturesSEC'
--
-- == New Enum Constants
--
-- -   'SEC_PIPELINE_CACHE_INCREMENTAL_MODE_EXTENSION_NAME'
--
-- -   'SEC_PIPELINE_CACHE_INCREMENTAL_MODE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CACHE_INCREMENTAL_MODE_FEATURES_SEC'
--
-- == Stub API References
--
-- There is currently no specification language written for this type. This
-- section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_SEC_pipeline_cache_incremental_mode
-- > typedef struct VkPhysicalDevicePipelineCacheIncrementalModeFeaturesSEC {
-- >     VkStructureType    sType;
-- >     void*              pNext;
-- >     VkBool32           pipelineCacheIncrementalMode;
-- > } VkPhysicalDevicePipelineCacheIncrementalModeFeaturesSEC;
--
-- === Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDevicePipelineCacheIncrementalModeFeaturesSEC-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CACHE_INCREMENTAL_MODE_FEATURES_SEC'
--
-- == Version History
--
-- -   Revision 1, 2025-06-24 (Chris Hambacher)
--
--     -   Initial specification
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_SEC_pipeline_cache_incremental_mode Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_SEC_pipeline_cache_incremental_mode  ( PhysicalDevicePipelineCacheIncrementalModeFeaturesSEC(..)
                                                                 , SEC_PIPELINE_CACHE_INCREMENTAL_MODE_SPEC_VERSION
                                                                 , pattern SEC_PIPELINE_CACHE_INCREMENTAL_MODE_SPEC_VERSION
                                                                 , SEC_PIPELINE_CACHE_INCREMENTAL_MODE_EXTENSION_NAME
                                                                 , pattern SEC_PIPELINE_CACHE_INCREMENTAL_MODE_EXTENSION_NAME
                                                                 ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CACHE_INCREMENTAL_MODE_FEATURES_SEC))
-- | VkPhysicalDevicePipelineCacheIncrementalModeFeaturesSEC - Stub
-- description of VkPhysicalDevicePipelineCacheIncrementalModeFeaturesSEC
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_SEC_pipeline_cache_incremental_mode VK_SEC_pipeline_cache_incremental_mode>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePipelineCacheIncrementalModeFeaturesSEC = PhysicalDevicePipelineCacheIncrementalModeFeaturesSEC
  { -- No documentation found for Nested "VkPhysicalDevicePipelineCacheIncrementalModeFeaturesSEC" "pipelineCacheIncrementalMode"
    pipelineCacheIncrementalMode :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePipelineCacheIncrementalModeFeaturesSEC)
#endif
deriving instance Show PhysicalDevicePipelineCacheIncrementalModeFeaturesSEC

instance ToCStruct PhysicalDevicePipelineCacheIncrementalModeFeaturesSEC where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePipelineCacheIncrementalModeFeaturesSEC{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CACHE_INCREMENTAL_MODE_FEATURES_SEC)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (pipelineCacheIncrementalMode))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CACHE_INCREMENTAL_MODE_FEATURES_SEC)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePipelineCacheIncrementalModeFeaturesSEC where
  peekCStruct p = do
    pipelineCacheIncrementalMode <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePipelineCacheIncrementalModeFeaturesSEC
             (bool32ToBool pipelineCacheIncrementalMode)

instance Storable PhysicalDevicePipelineCacheIncrementalModeFeaturesSEC where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePipelineCacheIncrementalModeFeaturesSEC where
  zero = PhysicalDevicePipelineCacheIncrementalModeFeaturesSEC
           zero


type SEC_PIPELINE_CACHE_INCREMENTAL_MODE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_SEC_PIPELINE_CACHE_INCREMENTAL_MODE_SPEC_VERSION"
pattern SEC_PIPELINE_CACHE_INCREMENTAL_MODE_SPEC_VERSION :: forall a . Integral a => a
pattern SEC_PIPELINE_CACHE_INCREMENTAL_MODE_SPEC_VERSION = 1


type SEC_PIPELINE_CACHE_INCREMENTAL_MODE_EXTENSION_NAME = "VK_SEC_pipeline_cache_incremental_mode"

-- No documentation found for TopLevel "VK_SEC_PIPELINE_CACHE_INCREMENTAL_MODE_EXTENSION_NAME"
pattern SEC_PIPELINE_CACHE_INCREMENTAL_MODE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern SEC_PIPELINE_CACHE_INCREMENTAL_MODE_EXTENSION_NAME = "VK_SEC_pipeline_cache_incremental_mode"

