{-# language CPP #-}
-- | = Name
--
-- VK_SEC_amigo_profiling - device extension
--
-- == VK_SEC_amigo_profiling
--
-- [__Name String__]
--     @VK_SEC_amigo_profiling@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     486
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Ralph Potter <<data:image/png;base64, GitLab>>r_potter
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-07-29
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Ralph Potter, Samsung
--
--     -   Sangrak Oh, Samsung
--
--     -   Jinku Kang, Samsung
--
-- == Description
--
-- This extension is intended to communicate information from layered API
-- implementations such as ANGLE to internal proprietary system schedulers.
-- It has no behavioural implications beyond enabling more intelligent
-- behaviour from the system scheduler.
--
-- Application developers should avoid using this extension. It is
-- documented solely for the benefit of tools and layer developers, who may
-- need to manipulate @pNext@ chains that include these structures.
--
-- Note
--
-- There is currently no specification language written for this extension.
-- The links to APIs defined by the extension are to stubs that only
-- include generated content such as API declarations and implicit valid
-- usage statements.
--
-- Note
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
--     -   'PhysicalDeviceAmigoProfilingFeaturesSEC'
--
-- -   Extending 'Vulkan.Core10.Queue.SubmitInfo':
--
--     -   'AmigoProfilingSubmitInfoSEC'
--
-- == New Enum Constants
--
-- -   'SEC_AMIGO_PROFILING_EXTENSION_NAME'
--
-- -   'SEC_AMIGO_PROFILING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_AMIGO_PROFILING_SUBMIT_INFO_SEC'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_AMIGO_PROFILING_FEATURES_SEC'
--
-- == Stub API References
--
-- There is currently no specification language written for this type. This
-- section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_SEC_amigo_profiling
-- > typedef struct VkPhysicalDeviceAmigoProfilingFeaturesSEC {
-- >     VkStructureType    sType;
-- >     void*              pNext;
-- >     VkBool32           amigoProfiling;
-- > } VkPhysicalDeviceAmigoProfilingFeaturesSEC;
--
-- === Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceAmigoProfilingFeaturesSEC-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_AMIGO_PROFILING_FEATURES_SEC'
--
-- There is currently no specification language written for this type. This
-- section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_SEC_amigo_profiling
-- > typedef struct VkAmigoProfilingSubmitInfoSEC {
-- >     VkStructureType    sType;
-- >     const void*        pNext;
-- >     uint64_t           firstDrawTimestamp;
-- >     uint64_t           swapBufferTimestamp;
-- > } VkAmigoProfilingSubmitInfoSEC;
--
-- === Valid Usage (Implicit)
--
-- -   #VUID-VkAmigoProfilingSubmitInfoSEC-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_AMIGO_PROFILING_SUBMIT_INFO_SEC'
--
-- == Version History
--
-- -   Revision 1, 2022-07-29 (Ralph Potter)
--
--     -   Initial specification
--
-- == See Also
--
-- 'AmigoProfilingSubmitInfoSEC', 'PhysicalDeviceAmigoProfilingFeaturesSEC'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_SEC_amigo_profiling Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_SEC_amigo_profiling  ( PhysicalDeviceAmigoProfilingFeaturesSEC(..)
                                                 , AmigoProfilingSubmitInfoSEC(..)
                                                 , SEC_AMIGO_PROFILING_SPEC_VERSION
                                                 , pattern SEC_AMIGO_PROFILING_SPEC_VERSION
                                                 , SEC_AMIGO_PROFILING_EXTENSION_NAME
                                                 , pattern SEC_AMIGO_PROFILING_EXTENSION_NAME
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
import Data.Word (Word64)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_AMIGO_PROFILING_SUBMIT_INFO_SEC))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_AMIGO_PROFILING_FEATURES_SEC))
-- | VkPhysicalDeviceAmigoProfilingFeaturesSEC - Stub description of
-- VkPhysicalDeviceAmigoProfilingFeaturesSEC
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_SEC_amigo_profiling VK_SEC_amigo_profiling>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceAmigoProfilingFeaturesSEC = PhysicalDeviceAmigoProfilingFeaturesSEC
  { -- No documentation found for Nested "VkPhysicalDeviceAmigoProfilingFeaturesSEC" "amigoProfiling"
    amigoProfiling :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceAmigoProfilingFeaturesSEC)
#endif
deriving instance Show PhysicalDeviceAmigoProfilingFeaturesSEC

instance ToCStruct PhysicalDeviceAmigoProfilingFeaturesSEC where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceAmigoProfilingFeaturesSEC{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_AMIGO_PROFILING_FEATURES_SEC)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (amigoProfiling))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_AMIGO_PROFILING_FEATURES_SEC)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceAmigoProfilingFeaturesSEC where
  peekCStruct p = do
    amigoProfiling <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceAmigoProfilingFeaturesSEC
             (bool32ToBool amigoProfiling)

instance Storable PhysicalDeviceAmigoProfilingFeaturesSEC where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceAmigoProfilingFeaturesSEC where
  zero = PhysicalDeviceAmigoProfilingFeaturesSEC
           zero


-- | VkAmigoProfilingSubmitInfoSEC - Stub description of
-- VkAmigoProfilingSubmitInfoSEC
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_SEC_amigo_profiling VK_SEC_amigo_profiling>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AmigoProfilingSubmitInfoSEC = AmigoProfilingSubmitInfoSEC
  { -- No documentation found for Nested "VkAmigoProfilingSubmitInfoSEC" "firstDrawTimestamp"
    firstDrawTimestamp :: Word64
  , -- No documentation found for Nested "VkAmigoProfilingSubmitInfoSEC" "swapBufferTimestamp"
    swapBufferTimestamp :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AmigoProfilingSubmitInfoSEC)
#endif
deriving instance Show AmigoProfilingSubmitInfoSEC

instance ToCStruct AmigoProfilingSubmitInfoSEC where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AmigoProfilingSubmitInfoSEC{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_AMIGO_PROFILING_SUBMIT_INFO_SEC)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (firstDrawTimestamp)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (swapBufferTimestamp)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_AMIGO_PROFILING_SUBMIT_INFO_SEC)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    f

instance FromCStruct AmigoProfilingSubmitInfoSEC where
  peekCStruct p = do
    firstDrawTimestamp <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    swapBufferTimestamp <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    pure $ AmigoProfilingSubmitInfoSEC
             firstDrawTimestamp swapBufferTimestamp

instance Storable AmigoProfilingSubmitInfoSEC where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AmigoProfilingSubmitInfoSEC where
  zero = AmigoProfilingSubmitInfoSEC
           zero
           zero


type SEC_AMIGO_PROFILING_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_SEC_AMIGO_PROFILING_SPEC_VERSION"
pattern SEC_AMIGO_PROFILING_SPEC_VERSION :: forall a . Integral a => a
pattern SEC_AMIGO_PROFILING_SPEC_VERSION = 1


type SEC_AMIGO_PROFILING_EXTENSION_NAME = "VK_SEC_amigo_profiling"

-- No documentation found for TopLevel "VK_SEC_AMIGO_PROFILING_EXTENSION_NAME"
pattern SEC_AMIGO_PROFILING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern SEC_AMIGO_PROFILING_EXTENSION_NAME = "VK_SEC_amigo_profiling"

