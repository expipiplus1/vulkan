{-# language CPP #-}
-- | = Name
--
-- VK_KHR_maintenance7 - device extension
--
-- = VK_KHR_maintenance7
--
-- [__Name String__]
--     @VK_KHR_maintenance7@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     563
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_maintenance7] @zmike%0A*Here describe the issue or question you have about the VK_KHR_maintenance7 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_maintenance7.adoc VK_KHR_maintenance7>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-01-30
--
-- [__Interactions and External Dependencies__; __Contributors__]
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Pan Gao, Huawei
--
--     -   Tobias Hector, AMD
--
--     -   Jon Leech, Khronos
--
--     -   Daniel Story, Nintendo
--
--     -   Shahbaz Youssefi, Google
--
--     -   Yiwei Zhang, Google
--
--     -   Matthew Netsch, Qualcomm
--
-- == Description
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance7 VK_KHR_maintenance7>
-- adds a collection of minor features, none of which would warrant an
-- entire extension of their own.
--
-- The proposed new features are as follows:
--
-- -   Add a property query to determine if a framebuffer writes to depth
--     or stencil aspect does not trigger a write access in the sibling
--     aspect. For example, this allows sampling stencil aspect as a
--     texture while rendering to the sibling depth attachment and
--     vice-versa given appropriate image layouts.
--
-- -   Add a way to query information regarding the underlying devices in
--     environments where the Vulkan implementation is provided through
--     layered implementations. For example, running on Mesa\/Venus, driver
--     ID is returned as
--     'Vulkan.Core12.Enums.DriverId.DRIVER_ID_MESA_VENUS', but it can be
--     necessary to know what the real driver under the hood is. The new
--     'PhysicalDeviceLayeredApiPropertiesKHR' structure can be used to
--     gather information regarding layers underneath the top-level
--     physical device.
--
-- -   Promote
--     'Vulkan.Extensions.VK_EXT_nested_command_buffer.RENDERING_CONTENTS_INLINE_BIT_EXT'
--     and
--     'Vulkan.Extensions.VK_EXT_nested_command_buffer.SUBPASS_CONTENTS_INLINE_AND_SECONDARY_COMMAND_BUFFERS_EXT'
--     to KHR
--
-- -   Add a limit to report the maximum total count of dynamic uniform
--     buffers and dynamic storage buffers that can be included in a
--     pipeline layout.
--
-- -   Require that for an unsigned integer query, the 32-bit result value
--     /must/ be equal to the 32 least significant bits of the equivalent
--     64-bit result value.
--
-- -   Add query for robust access support when using fragment shading rate
--     attachments
--
-- == New Structures
--
-- -   'PhysicalDeviceLayeredApiPropertiesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMaintenance7FeaturesKHR'
--
-- -   Extending 'PhysicalDeviceLayeredApiPropertiesKHR':
--
--     -   'PhysicalDeviceLayeredApiVulkanPropertiesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceLayeredApiPropertiesListKHR'
--
--     -   'PhysicalDeviceMaintenance7PropertiesKHR'
--
-- == New Enums
--
-- -   'PhysicalDeviceLayeredApiKHR'
--
-- == New Enum Constants
--
-- -   'KHR_MAINTENANCE_7_EXTENSION_NAME'
--
-- -   'KHR_MAINTENANCE_7_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core13.Enums.RenderingFlagBits.RenderingFlagBits':
--
--     -   'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_CONTENTS_INLINE_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_PROPERTIES_LIST_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_VULKAN_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_7_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_7_PROPERTIES_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.SubpassContents.SubpassContents':
--
--     -   'Vulkan.Core10.Enums.SubpassContents.SUBPASS_CONTENTS_INLINE_AND_SECONDARY_COMMAND_BUFFERS_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2024-01-30 (Jon Leech)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_maintenance7 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_maintenance7  ( PhysicalDeviceMaintenance7FeaturesKHR(..)
                                              , PhysicalDeviceMaintenance7PropertiesKHR(..)
                                              , PhysicalDeviceLayeredApiPropertiesListKHR(..)
                                              , PhysicalDeviceLayeredApiPropertiesKHR(..)
                                              , PhysicalDeviceLayeredApiVulkanPropertiesKHR(..)
                                              , PhysicalDeviceLayeredApiKHR( PHYSICAL_DEVICE_LAYERED_API_VULKAN_KHR
                                                                           , PHYSICAL_DEVICE_LAYERED_API_D3D12_KHR
                                                                           , PHYSICAL_DEVICE_LAYERED_API_METAL_KHR
                                                                           , PHYSICAL_DEVICE_LAYERED_API_OPENGL_KHR
                                                                           , PHYSICAL_DEVICE_LAYERED_API_OPENGLES_KHR
                                                                           , ..
                                                                           )
                                              , KHR_MAINTENANCE_7_SPEC_VERSION
                                              , pattern KHR_MAINTENANCE_7_SPEC_VERSION
                                              , KHR_MAINTENANCE_7_EXTENSION_NAME
                                              , pattern KHR_MAINTENANCE_7_EXTENSION_NAME
                                              ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Data.ByteString (packCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.APIConstants (MAX_PHYSICAL_DEVICE_NAME_SIZE)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceProperties2)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_PROPERTIES_LIST_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_VULKAN_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_7_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_7_PROPERTIES_KHR))
-- | VkPhysicalDeviceMaintenance7FeaturesKHR - Structure describing whether
-- the implementation supports maintenance7 functionality
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance7FeaturesKHR' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceMaintenance7FeaturesKHR' /can/ also be used in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance7 VK_KHR_maintenance7>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance7FeaturesKHR = PhysicalDeviceMaintenance7FeaturesKHR
  { -- | #features-maintenance7# @maintenance7@ indicates that the implementation
    -- supports the following:
    --
    -- -   The
    --     'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_CONTENTS_INLINE_BIT_KHR'
    --     and
    --     'Vulkan.Core10.Enums.SubpassContents.SUBPASS_CONTENTS_INLINE_AND_SECONDARY_COMMAND_BUFFERS_KHR'
    --     flags /can/ be used to record commands in render pass instances both
    --     inline and in secondary command buffers executed with
    --     'Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands' for dynamic
    --     rendering and legacy render passes respectively.
    --
    -- -   Querying information regarding the underlying devices in
    --     environments where the Vulkan implementation is provided through
    --     layered implementations. This is done by chaining
    --     'PhysicalDeviceLayeredApiPropertiesListKHR' to
    --     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'.
    --
    -- -   New limits which indicate the maximum total count of dynamic uniform
    --     buffers and dynamic storage buffers that /can/ be included in a
    --     pipeline layout.
    --
    -- -   32-bit timestamp queries /must/ wrap on overflow
    --
    -- -   A property that indicates whether a fragment shading rate attachment
    --     can have a size that is too small to cover a specified render area.
    --
    -- -   A property that indicates support for writing to one aspect of a
    --     depth\/stencil attachment without performing a read-modify-write
    --     operation on the other aspect
    maintenance7 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance7FeaturesKHR)
#endif
deriving instance Show PhysicalDeviceMaintenance7FeaturesKHR

instance ToCStruct PhysicalDeviceMaintenance7FeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance7FeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_7_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (maintenance7))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_7_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMaintenance7FeaturesKHR where
  peekCStruct p = do
    maintenance7 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMaintenance7FeaturesKHR
             (bool32ToBool maintenance7)

instance Storable PhysicalDeviceMaintenance7FeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance7FeaturesKHR where
  zero = PhysicalDeviceMaintenance7FeaturesKHR
           zero


-- | VkPhysicalDeviceMaintenance7PropertiesKHR - Structure describing various
-- implementation-defined properties introduced with VK_KHR_maintenance7
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance7PropertiesKHR' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance7 VK_KHR_maintenance7>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance7PropertiesKHR = PhysicalDeviceMaintenance7PropertiesKHR
  { -- | #limits-robustFragmentShadingRateAttachmentAccess#
    -- @robustFragmentShadingRateAttachmentAccess@ indicates whether the scaled
    -- size of a fragment shading rate attachment /can/ be less than the size
    -- of the render area. If @robustFragmentShadingRateAttachmentAccess@ is
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', the size of the attachment
    -- multiplied by the texel size /must/ be greater than or equal to the size
    -- of the render area. If it is 'Vulkan.Core10.FundamentalTypes.TRUE' and
    -- the fragment shading rate attachment was created with
    -- 'Vulkan.Core10.ImageView.ImageSubresourceRange'::@baseMipLevel@ equal to
    -- 0, the scaled size /can/ be smaller than the render area, and shading
    -- rates for missing texels are defined by
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-texel-replacement texel replacement for invalid texels>.
    robustFragmentShadingRateAttachmentAccess :: Bool
  , -- | #limits-separateDepthStencilAttachmentAccess#
    -- @separateDepthStencilAttachmentAccess@ indicates support for writing to
    -- one aspect of a depth\/stencil attachment without performing
    -- read-modify-write operations on the other aspect. If this property is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE', writes to one aspect /must/ not
    -- result in read-modify-write operations on the other aspect. If
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', writes to one aspect /may/
    -- result in writes to the other aspect as defined by
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-load-operations render pass load operations>,
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-store-operations render pass store operations>
    -- and
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-resolve-operations render pass resolve operations>.
    separateDepthStencilAttachmentAccess :: Bool
  , -- | #limits-maxDescriptorSetTotalUniformBuffersDynamic#
    -- @maxDescriptorSetTotalUniformBuffersDynamic@ is the maximum total count
    -- of dynamic uniform buffers that /can/ be included in a pipeline layout.
    -- Descriptors with a type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
    -- count against this limit. Only descriptors in descriptor set layouts
    -- created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit. See
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-uniformbufferdynamic>.
    maxDescriptorSetTotalUniformBuffersDynamic :: Word32
  , -- | #limits-maxDescriptorSetTotalStorageBuffersDynamic#
    -- @maxDescriptorSetTotalStorageBuffersDynamic@ is the maximum total count
    -- of dynamic storage buffers that /can/ be included in a pipeline layout.
    -- Descriptors with a type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
    -- count against this limit. Only descriptors in descriptor set layouts
    -- created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit. See
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storagebufferdynamic>.
    maxDescriptorSetTotalStorageBuffersDynamic :: Word32
  , -- | #limits-maxDescriptorSetTotalBuffersDynamic#
    -- @maxDescriptorSetTotalBuffersDynamic@ is the maximum total count of
    -- dynamic uniform buffers and storage buffers that /can/ be included in a
    -- pipeline layout. Descriptors with a type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
    -- or
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
    -- count against this limit. Only descriptors in descriptor set layouts
    -- created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit.
    maxDescriptorSetTotalBuffersDynamic :: Word32
  , -- | #limits-maxDescriptorSetUpdateAfterBindTotalUniformBuffersDynamic#
    -- @maxDescriptorSetUpdateAfterBindTotalUniformBuffersDynamic@ is similar
    -- to @maxDescriptorSetTotalUniformBuffersDynamic@ but counts descriptors
    -- from descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindTotalUniformBuffersDynamic :: Word32
  , -- | #limits-maxDescriptorSetUpdateAfterBindTotalStorageBuffersDynamic#
    -- @maxDescriptorSetUpdateAfterBindTotalStorageBuffersDynamic@ is similar
    -- to @maxDescriptorSetTotalStorageBuffersDynamic@ but counts descriptors
    -- from descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindTotalStorageBuffersDynamic :: Word32
  , -- | #limits-maxDescriptorSetUpdateAfterBindTotalBuffersDynamic#
    -- @maxDescriptorSetUpdateAfterBindTotalBuffersDynamic@ is similar to
    -- @maxDescriptorSetTotalBuffersDynamic@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set. While an application /can/ allocate dynamic storage buffer
    -- descriptors from a pool created with the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT',
    -- bindings for these descriptors /must/ not be present in any descriptor
    -- set layout that includes bindings created with
    -- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'.
    maxDescriptorSetUpdateAfterBindTotalBuffersDynamic :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance7PropertiesKHR)
#endif
deriving instance Show PhysicalDeviceMaintenance7PropertiesKHR

instance ToCStruct PhysicalDeviceMaintenance7PropertiesKHR where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance7PropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_7_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (robustFragmentShadingRateAttachmentAccess))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (separateDepthStencilAttachmentAccess))
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxDescriptorSetTotalUniformBuffersDynamic)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (maxDescriptorSetTotalStorageBuffersDynamic)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (maxDescriptorSetTotalBuffersDynamic)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindTotalUniformBuffersDynamic)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindTotalStorageBuffersDynamic)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindTotalBuffersDynamic)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_7_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceMaintenance7PropertiesKHR where
  peekCStruct p = do
    robustFragmentShadingRateAttachmentAccess <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    separateDepthStencilAttachmentAccess <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    maxDescriptorSetTotalUniformBuffersDynamic <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    maxDescriptorSetTotalStorageBuffersDynamic <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    maxDescriptorSetTotalBuffersDynamic <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindTotalUniformBuffersDynamic <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindTotalStorageBuffersDynamic <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindTotalBuffersDynamic <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    pure $ PhysicalDeviceMaintenance7PropertiesKHR
             (bool32ToBool robustFragmentShadingRateAttachmentAccess)
             (bool32ToBool separateDepthStencilAttachmentAccess)
             maxDescriptorSetTotalUniformBuffersDynamic
             maxDescriptorSetTotalStorageBuffersDynamic
             maxDescriptorSetTotalBuffersDynamic
             maxDescriptorSetUpdateAfterBindTotalUniformBuffersDynamic
             maxDescriptorSetUpdateAfterBindTotalStorageBuffersDynamic
             maxDescriptorSetUpdateAfterBindTotalBuffersDynamic

instance Storable PhysicalDeviceMaintenance7PropertiesKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance7PropertiesKHR where
  zero = PhysicalDeviceMaintenance7PropertiesKHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceLayeredApiPropertiesListKHR - Structure describing
-- layered implementations underneath the Vulkan physical device
--
-- = Description
--
-- If @pLayeredApis@ is @NULL@, then the number of layered implementations
-- that are underneath the top-most Vulkan physical device (i.e. the one
-- returned by
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2')
-- is returned in @layeredApiCount@. Otherwise, @layeredApiCount@ /must/ be
-- set by the application to the number of elements in the @pLayeredApis@
-- array, and on return the variable is overwritten with the number of
-- values actually written to @pLayeredApis@. If the value of
-- @layeredApiCount@ is less than the number of layered implementations
-- underneath the Vulkan physical device, at most @layeredApiCount@ values
-- will be written to @pLayeredApis@. An implementation that is not a layer
-- will return 0 in @layeredApiCount@.
--
-- In the presence of multiple layered implementations, each element of
-- @pLayeredApis@ corresponds to an API implementation that is implemented
-- on top of the API at the previous index. If there are layered
-- implementations underneath a non-Vulkan implementation, they may not be
-- visible in this query as the corresponding APIs may lack such a query.
--
-- If the 'PhysicalDeviceLayeredApiPropertiesListKHR' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceLayeredApiPropertiesListKHR-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_PROPERTIES_LIST_KHR'
--
-- -   #VUID-VkPhysicalDeviceLayeredApiPropertiesListKHR-pLayeredApis-parameter#
--     If @layeredApiCount@ is not @0@, and @pLayeredApis@ is not @NULL@,
--     @pLayeredApis@ /must/ be a valid pointer to an array of
--     @layeredApiCount@ 'PhysicalDeviceLayeredApiPropertiesKHR' structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance7 VK_KHR_maintenance7>,
-- 'PhysicalDeviceLayeredApiPropertiesKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceLayeredApiPropertiesListKHR = PhysicalDeviceLayeredApiPropertiesListKHR
  { -- | @layeredApiCount@ is an integer related to the number of layered
    -- implementations underneath the Vulkan physical device, as described
    -- below.
    layeredApiCount :: Word32
  , -- | @pLayeredApis@ is a pointer to an array of
    -- 'PhysicalDeviceLayeredApiPropertiesKHR' in which information regarding
    -- the layered implementations underneath the Vulkan physical device are
    -- returned.
    layeredApis :: Ptr (PhysicalDeviceLayeredApiPropertiesKHR '[])
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceLayeredApiPropertiesListKHR)
#endif
deriving instance Show PhysicalDeviceLayeredApiPropertiesListKHR

instance ToCStruct PhysicalDeviceLayeredApiPropertiesListKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceLayeredApiPropertiesListKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_PROPERTIES_LIST_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (layeredApiCount)
    poke ((p `plusPtr` 24 :: Ptr (Ptr (PhysicalDeviceLayeredApiPropertiesKHR _)))) (layeredApis)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_PROPERTIES_LIST_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PhysicalDeviceLayeredApiPropertiesListKHR where
  peekCStruct p = do
    layeredApiCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pLayeredApis <- peek @(Ptr (PhysicalDeviceLayeredApiPropertiesKHR _)) ((p `plusPtr` 24 :: Ptr (Ptr (PhysicalDeviceLayeredApiPropertiesKHR _))))
    pure $ PhysicalDeviceLayeredApiPropertiesListKHR
             layeredApiCount pLayeredApis

instance Storable PhysicalDeviceLayeredApiPropertiesListKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceLayeredApiPropertiesListKHR where
  zero = PhysicalDeviceLayeredApiPropertiesListKHR
           zero
           zero


-- | VkPhysicalDeviceLayeredApiPropertiesKHR - Structure describing a single
-- layered implementation underneath the Vulkan physical device
--
-- = Description
--
-- If @layeredAPI@ is 'PHYSICAL_DEVICE_LAYERED_API_VULKAN_KHR', additional
-- Vulkan-specific information can be queried by including the
-- 'PhysicalDeviceLayeredApiVulkanPropertiesKHR' structure in the @pNext@
-- chain. Otherwise if such a structure is included in the @pNext@ chain,
-- it is ignored.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceLayeredApiPropertiesKHR-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_PROPERTIES_KHR'
--
-- -   #VUID-VkPhysicalDeviceLayeredApiPropertiesKHR-pNext-pNext# @pNext@
--     /must/ be @NULL@ or a pointer to a valid instance of
--     'PhysicalDeviceLayeredApiVulkanPropertiesKHR'
--
-- -   #VUID-VkPhysicalDeviceLayeredApiPropertiesKHR-sType-unique# The
--     @sType@ value of each struct in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance7 VK_KHR_maintenance7>,
-- 'PhysicalDeviceLayeredApiKHR',
-- 'PhysicalDeviceLayeredApiPropertiesListKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceLayeredApiPropertiesKHR (es :: [Type]) = PhysicalDeviceLayeredApiPropertiesKHR
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @vendorID@ is a unique identifier for the vendor of the layered
    -- implementation.
    vendorID :: Word32
  , -- | @deviceID@ is a unique identifier for the layered implementation among
    -- devices available from the vendor.
    deviceID :: Word32
  , -- | @layeredAPI@ is a 'PhysicalDeviceLayeredApiKHR' specifying the API
    -- implemented by the layered implementation.
    layeredAPI :: PhysicalDeviceLayeredApiKHR
  , -- | @deviceName@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_PHYSICAL_DEVICE_NAME_SIZE' @char@
    -- containing a null-terminated UTF-8 string which is the name of the
    -- device.
    deviceName :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceLayeredApiPropertiesKHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PhysicalDeviceLayeredApiPropertiesKHR es)

instance Extensible PhysicalDeviceLayeredApiPropertiesKHR where
  extensibleTypeName = "PhysicalDeviceLayeredApiPropertiesKHR"
  setNext PhysicalDeviceLayeredApiPropertiesKHR{..} next' = PhysicalDeviceLayeredApiPropertiesKHR{next = next', ..}
  getNext PhysicalDeviceLayeredApiPropertiesKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PhysicalDeviceLayeredApiPropertiesKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PhysicalDeviceLayeredApiVulkanPropertiesKHR = Just f
    | otherwise = Nothing

instance ( Extendss PhysicalDeviceLayeredApiPropertiesKHR es
         , PokeChain es ) => ToCStruct (PhysicalDeviceLayeredApiPropertiesKHR es) where
  withCStruct x f = allocaBytes 288 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceLayeredApiPropertiesKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_PROPERTIES_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (vendorID)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (deviceID)
    lift $ poke ((p `plusPtr` 24 :: Ptr PhysicalDeviceLayeredApiKHR)) (layeredAPI)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 28 :: Ptr (FixedArray MAX_PHYSICAL_DEVICE_NAME_SIZE CChar))) (deviceName)
    lift $ f
  cStructSize = 288
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_PROPERTIES_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr PhysicalDeviceLayeredApiKHR)) (zero)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 28 :: Ptr (FixedArray MAX_PHYSICAL_DEVICE_NAME_SIZE CChar))) (mempty)
    lift $ f

instance ( Extendss PhysicalDeviceLayeredApiPropertiesKHR es
         , PeekChain es ) => FromCStruct (PhysicalDeviceLayeredApiPropertiesKHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    vendorID <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    deviceID <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    layeredAPI <- peek @PhysicalDeviceLayeredApiKHR ((p `plusPtr` 24 :: Ptr PhysicalDeviceLayeredApiKHR))
    deviceName <- packCString (lowerArrayPtr ((p `plusPtr` 28 :: Ptr (FixedArray MAX_PHYSICAL_DEVICE_NAME_SIZE CChar))))
    pure $ PhysicalDeviceLayeredApiPropertiesKHR
             next vendorID deviceID layeredAPI deviceName

instance es ~ '[] => Zero (PhysicalDeviceLayeredApiPropertiesKHR es) where
  zero = PhysicalDeviceLayeredApiPropertiesKHR
           ()
           zero
           zero
           zero
           mempty


-- | VkPhysicalDeviceLayeredApiVulkanPropertiesKHR - Structure describing
-- physical device properties of a layered Vulkan implementation underneath
-- the Vulkan physical device
--
-- = Description
--
-- The implementation /must/ zero-fill the contents of
-- @properties.properties.limits@ and
-- @properties.properties.sparseProperties@.
--
-- == Valid Usage
--
-- -   #VUID-VkPhysicalDeviceLayeredApiVulkanPropertiesKHR-pNext-10011#
--     Only
--     'Vulkan.Core12.Promoted_From_VK_KHR_driver_properties.PhysicalDeviceDriverProperties'
--     and
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceIDProperties'
--     are allowed in the @pNext@ chain of @properties@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceLayeredApiVulkanPropertiesKHR-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_VULKAN_PROPERTIES_KHR'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance7 VK_KHR_maintenance7>,
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceLayeredApiVulkanPropertiesKHR = PhysicalDeviceLayeredApiVulkanPropertiesKHR
  { -- | @properties@ is a
    -- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
    -- in which properties of the underlying layered Vulkan implementation are
    -- returned.
    properties :: SomeStruct PhysicalDeviceProperties2 }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceLayeredApiVulkanPropertiesKHR)
#endif
deriving instance Show PhysicalDeviceLayeredApiVulkanPropertiesKHR

instance ToCStruct PhysicalDeviceLayeredApiVulkanPropertiesKHR where
  withCStruct x f = allocaBytes 856 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceLayeredApiVulkanPropertiesKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_VULKAN_PROPERTIES_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeSomeCStruct (forgetExtensions ((p `plusPtr` 16 :: Ptr (PhysicalDeviceProperties2 _)))) (properties) . ($ ())
    lift $ f
  cStructSize = 856
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_VULKAN_PROPERTIES_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeSomeCStruct (forgetExtensions ((p `plusPtr` 16 :: Ptr (PhysicalDeviceProperties2 _)))) ((SomeStruct zero)) . ($ ())
    lift $ f

instance FromCStruct PhysicalDeviceLayeredApiVulkanPropertiesKHR where
  peekCStruct p = do
    properties <- peekSomeCStruct (forgetExtensions ((p `plusPtr` 16 :: Ptr (PhysicalDeviceProperties2 _))))
    pure $ PhysicalDeviceLayeredApiVulkanPropertiesKHR
             properties

instance Zero PhysicalDeviceLayeredApiVulkanPropertiesKHR where
  zero = PhysicalDeviceLayeredApiVulkanPropertiesKHR
           (SomeStruct zero)


-- | VkPhysicalDeviceLayeredApiKHR - API implemented by the layered
-- implementation
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance7 VK_KHR_maintenance7>,
-- 'PhysicalDeviceLayeredApiPropertiesKHR'
newtype PhysicalDeviceLayeredApiKHR = PhysicalDeviceLayeredApiKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'PHYSICAL_DEVICE_LAYERED_API_VULKAN_KHR' - the device implements the
-- Vulkan API.
pattern PHYSICAL_DEVICE_LAYERED_API_VULKAN_KHR = PhysicalDeviceLayeredApiKHR 0

-- | 'PHYSICAL_DEVICE_LAYERED_API_D3D12_KHR' - the device implements the
-- D3D12 API.
pattern PHYSICAL_DEVICE_LAYERED_API_D3D12_KHR = PhysicalDeviceLayeredApiKHR 1

-- | 'PHYSICAL_DEVICE_LAYERED_API_METAL_KHR' - the device implements the
-- Metal API.
pattern PHYSICAL_DEVICE_LAYERED_API_METAL_KHR = PhysicalDeviceLayeredApiKHR 2

-- | 'PHYSICAL_DEVICE_LAYERED_API_OPENGL_KHR' - the device implements the
-- OpenGL API.
pattern PHYSICAL_DEVICE_LAYERED_API_OPENGL_KHR = PhysicalDeviceLayeredApiKHR 3

-- | 'PHYSICAL_DEVICE_LAYERED_API_OPENGLES_KHR' - the device implements the
-- OpenGL ES API.
pattern PHYSICAL_DEVICE_LAYERED_API_OPENGLES_KHR = PhysicalDeviceLayeredApiKHR 4

{-# COMPLETE
  PHYSICAL_DEVICE_LAYERED_API_VULKAN_KHR
  , PHYSICAL_DEVICE_LAYERED_API_D3D12_KHR
  , PHYSICAL_DEVICE_LAYERED_API_METAL_KHR
  , PHYSICAL_DEVICE_LAYERED_API_OPENGL_KHR
  , PHYSICAL_DEVICE_LAYERED_API_OPENGLES_KHR ::
    PhysicalDeviceLayeredApiKHR
  #-}

conNamePhysicalDeviceLayeredApiKHR :: String
conNamePhysicalDeviceLayeredApiKHR = "PhysicalDeviceLayeredApiKHR"

enumPrefixPhysicalDeviceLayeredApiKHR :: String
enumPrefixPhysicalDeviceLayeredApiKHR = "PHYSICAL_DEVICE_LAYERED_API_"

showTablePhysicalDeviceLayeredApiKHR :: [(PhysicalDeviceLayeredApiKHR, String)]
showTablePhysicalDeviceLayeredApiKHR =
  [
    ( PHYSICAL_DEVICE_LAYERED_API_VULKAN_KHR
    , "VULKAN_KHR"
    )
  ,
    ( PHYSICAL_DEVICE_LAYERED_API_D3D12_KHR
    , "D3D12_KHR"
    )
  ,
    ( PHYSICAL_DEVICE_LAYERED_API_METAL_KHR
    , "METAL_KHR"
    )
  ,
    ( PHYSICAL_DEVICE_LAYERED_API_OPENGL_KHR
    , "OPENGL_KHR"
    )
  ,
    ( PHYSICAL_DEVICE_LAYERED_API_OPENGLES_KHR
    , "OPENGLES_KHR"
    )
  ]

instance Show PhysicalDeviceLayeredApiKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixPhysicalDeviceLayeredApiKHR
      showTablePhysicalDeviceLayeredApiKHR
      conNamePhysicalDeviceLayeredApiKHR
      (\(PhysicalDeviceLayeredApiKHR x) -> x)
      (showsPrec 11)

instance Read PhysicalDeviceLayeredApiKHR where
  readPrec =
    enumReadPrec
      enumPrefixPhysicalDeviceLayeredApiKHR
      showTablePhysicalDeviceLayeredApiKHR
      conNamePhysicalDeviceLayeredApiKHR
      PhysicalDeviceLayeredApiKHR

type KHR_MAINTENANCE_7_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_7_SPEC_VERSION"
pattern KHR_MAINTENANCE_7_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_MAINTENANCE_7_SPEC_VERSION = 1


type KHR_MAINTENANCE_7_EXTENSION_NAME = "VK_KHR_maintenance7"

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_7_EXTENSION_NAME"
pattern KHR_MAINTENANCE_7_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_MAINTENANCE_7_EXTENSION_NAME = "VK_KHR_maintenance7"

