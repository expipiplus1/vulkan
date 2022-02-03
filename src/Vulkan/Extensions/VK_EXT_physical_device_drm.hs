{-# language CPP #-}
-- | = Name
--
-- VK_EXT_physical_device_drm - device extension
--
-- == VK_EXT_physical_device_drm
--
-- [__Name String__]
--     @VK_EXT_physical_device_drm@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     354
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Simon Ser
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_physical_device_drm] @emersion%0A<<Here describe the issue or question you have about the VK_EXT_physical_device_drm extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-06-09
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Simon Ser
--
-- == Description
--
-- This extension provides new facilities to query DRM properties for
-- physical devices, enabling users to match Vulkan physical devices with
-- DRM nodes on Linux.
--
-- Its functionality closely overlaps with
-- @EGL_EXT_device_drm@<https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_physical_device_drm-fn1 1>^.
-- Unlike the EGL extension, this extension does not expose a string
-- containing the name of the device file and instead exposes device minor
-- numbers.
--
-- DRM defines multiple device node types. Each physical device may have
-- one primary node and one render node associated. Physical devices may
-- have no primary node (e.g. if the device does not have a display
-- subsystem), may have no render node (e.g. if it is a software rendering
-- engine), or may have neither (e.g. if it is a software rendering engine
-- without a display subsystem).
--
-- To query DRM properties for a physical device, chain
-- 'PhysicalDeviceDrmPropertiesEXT' to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceDrmPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PHYSICAL_DEVICE_DRM_EXTENSION_NAME'
--
-- -   'EXT_PHYSICAL_DEVICE_DRM_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DRM_PROPERTIES_EXT'
--
-- == References
--
-- 1.  #VK_EXT_physical_device_drm-fn1#
--     <https://www.khronos.org/registry/EGL/extensions/EXT/EGL_EXT_device_drm.txt EGL_EXT_device_drm>
--
-- == Version History
--
-- -   Revision 1, 2021-06-09
--
--     -   First stable revision
--
-- == See Also
--
-- 'PhysicalDeviceDrmPropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_physical_device_drm Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_physical_device_drm  ( PhysicalDeviceDrmPropertiesEXT(..)
                                                     , EXT_PHYSICAL_DEVICE_DRM_SPEC_VERSION
                                                     , pattern EXT_PHYSICAL_DEVICE_DRM_SPEC_VERSION
                                                     , EXT_PHYSICAL_DEVICE_DRM_EXTENSION_NAME
                                                     , pattern EXT_PHYSICAL_DEVICE_DRM_EXTENSION_NAME
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
import Data.Int (Int64)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DRM_PROPERTIES_EXT))
-- | VkPhysicalDeviceDrmPropertiesEXT - Structure containing DRM information
-- of a physical device
--
-- = Description
--
-- If the 'PhysicalDeviceDrmPropertiesEXT' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- These are properties of the DRM information of a physical device.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_physical_device_drm VK_EXT_physical_device_drm>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDrmPropertiesEXT = PhysicalDeviceDrmPropertiesEXT
  { -- | @hasPrimary@ is a boolean indicating whether the physical device has a
    -- DRM primary node.
    hasPrimary :: Bool
  , -- | @hasRender@ is a boolean indicating whether the physical device has a
    -- DRM render node.
    hasRender :: Bool
  , -- | @primaryMajor@ is the DRM primary node major number, if any.
    primaryMajor :: Int64
  , -- | @primaryMinor@ is the DRM primary node minor number, if any.
    primaryMinor :: Int64
  , -- | @renderMajor@ is the DRM render node major number, if any.
    renderMajor :: Int64
  , -- | @renderMinor@ is the DRM render node minor number, if any.
    renderMinor :: Int64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDrmPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceDrmPropertiesEXT

instance ToCStruct PhysicalDeviceDrmPropertiesEXT where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDrmPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DRM_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (hasPrimary))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (hasRender))
    poke ((p `plusPtr` 24 :: Ptr Int64)) (primaryMajor)
    poke ((p `plusPtr` 32 :: Ptr Int64)) (primaryMinor)
    poke ((p `plusPtr` 40 :: Ptr Int64)) (renderMajor)
    poke ((p `plusPtr` 48 :: Ptr Int64)) (renderMinor)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DRM_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Int64)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Int64)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Int64)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Int64)) (zero)
    f

instance FromCStruct PhysicalDeviceDrmPropertiesEXT where
  peekCStruct p = do
    hasPrimary <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    hasRender <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    primaryMajor <- peek @Int64 ((p `plusPtr` 24 :: Ptr Int64))
    primaryMinor <- peek @Int64 ((p `plusPtr` 32 :: Ptr Int64))
    renderMajor <- peek @Int64 ((p `plusPtr` 40 :: Ptr Int64))
    renderMinor <- peek @Int64 ((p `plusPtr` 48 :: Ptr Int64))
    pure $ PhysicalDeviceDrmPropertiesEXT
             (bool32ToBool hasPrimary) (bool32ToBool hasRender) primaryMajor primaryMinor renderMajor renderMinor

instance Storable PhysicalDeviceDrmPropertiesEXT where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDrmPropertiesEXT where
  zero = PhysicalDeviceDrmPropertiesEXT
           zero
           zero
           zero
           zero
           zero
           zero


type EXT_PHYSICAL_DEVICE_DRM_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_PHYSICAL_DEVICE_DRM_SPEC_VERSION"
pattern EXT_PHYSICAL_DEVICE_DRM_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PHYSICAL_DEVICE_DRM_SPEC_VERSION = 1


type EXT_PHYSICAL_DEVICE_DRM_EXTENSION_NAME = "VK_EXT_physical_device_drm"

-- No documentation found for TopLevel "VK_EXT_PHYSICAL_DEVICE_DRM_EXTENSION_NAME"
pattern EXT_PHYSICAL_DEVICE_DRM_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PHYSICAL_DEVICE_DRM_EXTENSION_NAME = "VK_EXT_physical_device_drm"

