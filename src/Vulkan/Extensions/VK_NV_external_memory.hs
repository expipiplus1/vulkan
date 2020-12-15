{-# language CPP #-}
-- | = Name
--
-- VK_NV_external_memory - device extension
--
-- == VK_NV_external_memory
--
-- [__Name String__]
--     @VK_NV_external_memory@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     57
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_NV_external_memory_capabilities@
--
-- [__Deprecation state__]
--
--     -   /Deprecated/ by @VK_KHR_external_memory@ extension
--
--         -   Which in turn was /promoted/ to
--             <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NV_external_memory:%20&body=@cubanismo%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-08-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   James Jones, NVIDIA
--
--     -   Carsten Rohde, NVIDIA
--
-- == Description
--
-- Applications may wish to export memory to other Vulkan instances or
-- other APIs, or import memory from other Vulkan instances or other APIs
-- to enable Vulkan workloads to be split up across application module,
-- process, or API boundaries. This extension enables applications to
-- create exportable Vulkan memory objects such that the underlying
-- resources can be referenced outside the Vulkan instance that created
-- them.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo':
--
--     -   'ExternalMemoryImageCreateInfoNV'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'ExportMemoryAllocateInfoNV'
--
-- == New Enum Constants
--
-- -   'NV_EXTERNAL_MEMORY_EXTENSION_NAME'
--
-- -   'NV_EXTERNAL_MEMORY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV'
--
-- == Issues
--
-- 1) If memory objects are shared between processes and APIs, is this
-- considered aliasing according to the rules outlined in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-memory-aliasing Memory Aliasing>
-- section?
--
-- __RESOLVED__: Yes, but strict exceptions to the rules are added to allow
-- some forms of aliasing in these cases. Further, other extensions may
-- build upon these new aliasing rules to define specific support usage
-- within Vulkan for imported native memory objects, or memory objects from
-- other APIs.
--
-- 2) Are new image layouts or metadata required to specify image layouts
-- and layout transitions compatible with non-Vulkan APIs, or with other
-- instances of the same Vulkan driver?
--
-- __RESOLVED__: No. Separate instances of the same Vulkan driver running
-- on the same GPU should have identical internal layout semantics, so
-- applictions have the tools they need to ensure views of images are
-- consistent between the two instances. Other APIs will fall into two
-- categories: Those that are Vulkan compatible (a term to be defined by
-- subsequent interopability extensions), or Vulkan incompatible. When
-- sharing images with Vulkan incompatible APIs, the Vulkan image must be
-- transitioned to the
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' layout before
-- handing it off to the external API.
--
-- Note this does not attempt to address cross-device transitions, nor
-- transitions to engines on the same device which are not visible within
-- the Vulkan API. Both of these are beyond the scope of this extension.
--
-- == Examples
--
-- >     // TODO: Write some sample code here.
--
-- == Version History
--
-- -   Revision 1, 2016-08-19 (James Jones)
--
--     -   Initial draft
--
-- = See Also
--
-- 'ExportMemoryAllocateInfoNV', 'ExternalMemoryImageCreateInfoNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_external_memory Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_external_memory  ( ExternalMemoryImageCreateInfoNV(..)
                                                , ExportMemoryAllocateInfoNV(..)
                                                , NV_EXTERNAL_MEMORY_SPEC_VERSION
                                                , pattern NV_EXTERNAL_MEMORY_SPEC_VERSION
                                                , NV_EXTERNAL_MEMORY_EXTENSION_NAME
                                                , pattern NV_EXTERNAL_MEMORY_EXTENSION_NAME
                                                , ExternalMemoryHandleTypeFlagBitsNV(..)
                                                , ExternalMemoryHandleTypeFlagsNV
                                                ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Vulkan.Extensions.VK_NV_external_memory_capabilities (ExternalMemoryHandleTypeFlagsNV)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV))
import Vulkan.Extensions.VK_NV_external_memory_capabilities (ExternalMemoryHandleTypeFlagBitsNV(..))
import Vulkan.Extensions.VK_NV_external_memory_capabilities (ExternalMemoryHandleTypeFlagsNV)
-- | VkExternalMemoryImageCreateInfoNV - Specify that an image may be backed
-- by external memory
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagsNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExternalMemoryImageCreateInfoNV = ExternalMemoryImageCreateInfoNV
  { -- | @handleTypes@ is zero, or a bitmask of
    -- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagBitsNV'
    -- specifying one or more external memory handle types.
    --
    -- #VUID-VkExternalMemoryImageCreateInfoNV-handleTypes-parameter#
    -- @handleTypes@ /must/ be a valid combination of
    -- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagBitsNV'
    -- values
    handleTypes :: ExternalMemoryHandleTypeFlagsNV }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalMemoryImageCreateInfoNV)
#endif
deriving instance Show ExternalMemoryImageCreateInfoNV

instance ToCStruct ExternalMemoryImageCreateInfoNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalMemoryImageCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagsNV)) (handleTypes)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ExternalMemoryImageCreateInfoNV where
  peekCStruct p = do
    handleTypes <- peek @ExternalMemoryHandleTypeFlagsNV ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagsNV))
    pure $ ExternalMemoryImageCreateInfoNV
             handleTypes

instance Storable ExternalMemoryImageCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalMemoryImageCreateInfoNV where
  zero = ExternalMemoryImageCreateInfoNV
           zero


-- | VkExportMemoryAllocateInfoNV - Specify memory handle types that may be
-- exported
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagsNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExportMemoryAllocateInfoNV = ExportMemoryAllocateInfoNV
  { -- | @handleTypes@ is a bitmask of
    -- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagBitsNV'
    -- specifying one or more memory handle types that /may/ be exported.
    -- Multiple handle types /may/ be requested for the same allocation as long
    -- as they are compatible, as reported by
    -- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.getPhysicalDeviceExternalImageFormatPropertiesNV'.
    --
    -- #VUID-VkExportMemoryAllocateInfoNV-handleTypes-parameter# @handleTypes@
    -- /must/ be a valid combination of
    -- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagBitsNV'
    -- values
    handleTypes :: ExternalMemoryHandleTypeFlagsNV }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportMemoryAllocateInfoNV)
#endif
deriving instance Show ExportMemoryAllocateInfoNV

instance ToCStruct ExportMemoryAllocateInfoNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportMemoryAllocateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagsNV)) (handleTypes)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ExportMemoryAllocateInfoNV where
  peekCStruct p = do
    handleTypes <- peek @ExternalMemoryHandleTypeFlagsNV ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagsNV))
    pure $ ExportMemoryAllocateInfoNV
             handleTypes

instance Storable ExportMemoryAllocateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportMemoryAllocateInfoNV where
  zero = ExportMemoryAllocateInfoNV
           zero


type NV_EXTERNAL_MEMORY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_SPEC_VERSION"
pattern NV_EXTERNAL_MEMORY_SPEC_VERSION :: forall a . Integral a => a
pattern NV_EXTERNAL_MEMORY_SPEC_VERSION = 1


type NV_EXTERNAL_MEMORY_EXTENSION_NAME = "VK_NV_external_memory"

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME"
pattern NV_EXTERNAL_MEMORY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_EXTERNAL_MEMORY_EXTENSION_NAME = "VK_NV_external_memory"

