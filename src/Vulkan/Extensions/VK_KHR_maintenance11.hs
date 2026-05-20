{-# language CPP #-}
-- | = Name
--
-- VK_KHR_maintenance11 - device extension
--
-- = VK_KHR_maintenance11
--
-- [__Name String__]
--     @VK_KHR_maintenance11@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     658
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_EXT_mesh_shader
--
--     -   Interacts with VK_EXT_shader_object
--
--     -   Interacts with VK_NV_mesh_shader
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_maintenance11] @zmike%0A*Here describe the issue or question you have about the VK_KHR_maintenance11 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_maintenance11.adoc VK_KHR_maintenance11>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-09-09
--
-- [__Interactions and External Dependencies__; __Contributors__]
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Piers Daniell, NVIDIA
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Caterina Shablia, Collabora
--
-- == Description
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance11 VK_KHR_maintenance11>
-- adds a collection of minor features, none of which would warrant an
-- entire extension of their own.
--
-- The new features are as follows:
--
-- -   Add D3D compatibility for mismatch between @Arrayed@ in shaders and
--     the arrayness of the underlying descriptor when the descriptor
--     contains a single array layer
--
-- -   Clarify the pipeline depth clipping state when the pipeline is
--     created without
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLIP_ENABLE_EXT'
--     being set and the
--     'Vulkan.Extensions.VK_EXT_depth_clip_enable.PipelineRasterizationDepthClipStateCreateInfoEXT'
--     struct is not present
--
-- -   Add
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_INDEPENDENT_SETS_BIT_KHR'
--     to enable shader object functionality to mimic
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT'
--     used for graphics pipeline libraries, including a new pipeline
--     layout creation flag
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_NO_TASK_SHADER_BIT_KHR'
--     to ensure pipeline layouts used with shader objects also created
--     with
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_NO_TASK_SHADER_BIT_EXT'
--     to be compatible
--
-- -   Allow @queueFamilyIndexCount@ of 1 in
--     'Vulkan.Core10.Buffer.BufferCreateInfo',
--     'Vulkan.Core10.Image.ImageCreateInfo',
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.PhysicalDeviceImageDrmFormatModifierInfoEXT',
--     'Vulkan.Extensions.VK_ARM_tensors.TensorCreateInfoARM' when
--     @sharingMode@ is
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT'.
--
-- -   Require @minImageTransferGranularity@ to be (1,1,1) even on
--     transfer-only queues and add @optimalImageTransferGranularity@ queue
--     family property to communicate the performance bump for copies not
--     aligned to the optimal granularity.
--
-- -   When copying between a buffer and an image on a transfer-only queue,
--     do not require @bufferOffset@ to be a multiple of 4.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMaintenance11FeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.QueueFamilyProperties2':
--
--     -   'QueueFamilyOptimalImageTransferGranularityPropertiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_MAINTENANCE_11_EXTENSION_NAME'
--
-- -   'KHR_MAINTENANCE_11_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_ALIAS_SINGLE_LAYER_DESCRIPTOR_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_11_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_OPTIMAL_IMAGE_TRANSFER_GRANULARITY_PROPERTIES_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_shader_object.ShaderCreateFlagBitsEXT':
--
--     -   'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_INDEPENDENT_SETS_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_mesh_shader VK_EXT_mesh_shader>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_mesh_shader VK_NV_mesh_shader>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PipelineLayoutCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_NO_TASK_SHADER_BIT_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2025-09-09 (Mike Blumenkrantz)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_maintenance11 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_maintenance11  ( PhysicalDeviceMaintenance11FeaturesKHR(..)
                                               , QueueFamilyOptimalImageTransferGranularityPropertiesKHR(..)
                                               , KHR_MAINTENANCE_11_SPEC_VERSION
                                               , pattern KHR_MAINTENANCE_11_SPEC_VERSION
                                               , KHR_MAINTENANCE_11_EXTENSION_NAME
                                               , pattern KHR_MAINTENANCE_11_EXTENSION_NAME
                                               , ShaderCreateFlagBitsEXT(..)
                                               , ShaderCreateFlagsEXT
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
import Vulkan.Core10.FundamentalTypes (Extent3D)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_11_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUEUE_FAMILY_OPTIMAL_IMAGE_TRANSFER_GRANULARITY_PROPERTIES_KHR))
import Vulkan.Extensions.VK_EXT_shader_object (ShaderCreateFlagBitsEXT(..))
import Vulkan.Extensions.VK_EXT_shader_object (ShaderCreateFlagsEXT)
-- | VkPhysicalDeviceMaintenance11FeaturesKHR - Structure describing whether
-- the implementation supports maintenance11 functionality
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance11FeaturesKHR' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceMaintenance11FeaturesKHR', it /must/ add an instance of
-- the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structures>]
--
--     -   'Vulkan.Core10.Device.DeviceCreateInfo'
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance11 VK_KHR_maintenance11>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance11FeaturesKHR = PhysicalDeviceMaintenance11FeaturesKHR
  { -- | #features-maintenance11# @maintenance11@ indicates that the
    -- implementation supports the following:
    --
    -- -   'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_ALIAS_SINGLE_LAYER_DESCRIPTOR_BIT_KHR'
    --
    -- -   The depth clipping state of the pipeline is well defined when the
    --     pipeline is created without
    --     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLIP_ENABLE_EXT'
    --     being set and the
    --     'Vulkan.Extensions.VK_EXT_depth_clip_enable.PipelineRasterizationDepthClipStateCreateInfoEXT'
    --     structure is not present
    --
    -- -   'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_INDEPENDENT_SETS_BIT_KHR'
    --
    -- -   Allow @queueFamilyIndexCount@ of 1 in
    --     'Vulkan.Core10.Buffer.BufferCreateInfo',
    --     'Vulkan.Core10.Image.ImageCreateInfo',
    --     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.PhysicalDeviceImageDrmFormatModifierInfoEXT',
    --     'Vulkan.Extensions.VK_ARM_tensors.TensorCreateInfoARM' when
    --     @sharingMode@ is
    --     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT'
    --
    -- -   Require @minImageTransferGranularity@ to be (1,1,1) even on
    --     transfer-only queues and add @optimalImageTransferGranularity@ queue
    --     family property to communicate the performance bump for copies not
    --     aligned to the optimal granularity
    --
    -- -   When copying between a buffer and an image on a transfer-only queue,
    --     do not require @bufferOffset@ to be a multiple of 4
    maintenance11 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance11FeaturesKHR)
#endif
deriving instance Show PhysicalDeviceMaintenance11FeaturesKHR

instance ToCStruct PhysicalDeviceMaintenance11FeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance11FeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_11_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (maintenance11))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_11_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMaintenance11FeaturesKHR where
  peekCStruct p = do
    maintenance11 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMaintenance11FeaturesKHR
             (bool32ToBool maintenance11)

instance Storable PhysicalDeviceMaintenance11FeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance11FeaturesKHR where
  zero = PhysicalDeviceMaintenance11FeaturesKHR
           zero


-- | VkQueueFamilyOptimalImageTransferGranularityPropertiesKHR - Structure
-- specifying the optimal image transfer granularity for a queue family
--
-- = Description
--
-- If this structure is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.QueueFamilyProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceQueueFamilyProperties2',
-- then it is filled with the optimal image transfer granularity for the
-- specified queue family.
--
-- The value returned in @optimalImageTransferGranularity@ has a unit of
-- compressed texel blocks for images having a block-compressed format, and
-- a unit of texels otherwise.
--
-- Possible values of @optimalImageTransferGranularity@ are:
--
-- -   (0,0,0) specifies that an image copy operation is optimal only when
--     copying whole mip levels, i.e. all of the following conditions are
--     met:
--
--     -   The @x@, @y@, and @z@ members of a
--         'Vulkan.Core10.FundamentalTypes.Offset3D' are zero.
--
--     -   The @width@, @height@, and @depth@ members of a
--         'Vulkan.Core10.FundamentalTypes.Extent3D' parameter match the
--         width, height, and depth of the image subresource corresponding
--         to the parameter, respectively.
--
-- -   (Ax, Ay, Az) where Ax, Ay, and Az are all integer powers of two. An
--     image copy operation is optimal when all of the following conditions
--     are met:
--
--     -   @width@ of a 'Vulkan.Core10.FundamentalTypes.Extent3D' parameter
--         is an integer multiple of Ax, or else @x@ + @width@ equals the
--         width of the image subresource corresponding to the parameter.
--
--     -   @height@ of a 'Vulkan.Core10.FundamentalTypes.Extent3D'
--         parameter is an integer multiple of Ay, or else @y@ + @height@
--         equals the height of the image subresource corresponding to the
--         parameter.
--
--     -   @depth@ of a 'Vulkan.Core10.FundamentalTypes.Extent3D' parameter
--         is an integer multiple of Az, or else @z@ + @depth@ equals the
--         depth of the image subresource corresponding to the parameter.
--
--     -   If the format of the image corresponding to the parameters is
--         one of the block-compressed formats then for the purposes of the
--         above calculations the granularity /must/ be scaled up by the
--         compressed texel block dimensions.
--
-- While it is not required that copies are optimal, there may be a
-- performance cost for copies not aligned to
-- @optimalImageTransferGranularity@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkQueueFamilyOptimalImageTransferGranularityPropertiesKHR-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_OPTIMAL_IMAGE_TRANSFER_GRANULARITY_PROPERTIES_KHR'
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structure>]
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.QueueFamilyProperties2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance11 VK_KHR_maintenance11>,
-- 'Vulkan.Core10.FundamentalTypes.Extent3D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data QueueFamilyOptimalImageTransferGranularityPropertiesKHR = QueueFamilyOptimalImageTransferGranularityPropertiesKHR
  { -- | @optimalImageTransferGranularity@ is the optimal granularity for image
    -- copy operations in this queue family.
    optimalImageTransferGranularity :: Extent3D }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (QueueFamilyOptimalImageTransferGranularityPropertiesKHR)
#endif
deriving instance Show QueueFamilyOptimalImageTransferGranularityPropertiesKHR

instance ToCStruct QueueFamilyOptimalImageTransferGranularityPropertiesKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p QueueFamilyOptimalImageTransferGranularityPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_OPTIMAL_IMAGE_TRANSFER_GRANULARITY_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent3D)) (optimalImageTransferGranularity)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_OPTIMAL_IMAGE_TRANSFER_GRANULARITY_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent3D)) (zero)
    f

instance FromCStruct QueueFamilyOptimalImageTransferGranularityPropertiesKHR where
  peekCStruct p = do
    optimalImageTransferGranularity <- peekCStruct @Extent3D ((p `plusPtr` 16 :: Ptr Extent3D))
    pure $ QueueFamilyOptimalImageTransferGranularityPropertiesKHR
             optimalImageTransferGranularity

instance Storable QueueFamilyOptimalImageTransferGranularityPropertiesKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero QueueFamilyOptimalImageTransferGranularityPropertiesKHR where
  zero = QueueFamilyOptimalImageTransferGranularityPropertiesKHR
           zero


type KHR_MAINTENANCE_11_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_11_SPEC_VERSION"
pattern KHR_MAINTENANCE_11_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_MAINTENANCE_11_SPEC_VERSION = 1


type KHR_MAINTENANCE_11_EXTENSION_NAME = "VK_KHR_maintenance11"

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_11_EXTENSION_NAME"
pattern KHR_MAINTENANCE_11_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_MAINTENANCE_11_EXTENSION_NAME = "VK_KHR_maintenance11"

