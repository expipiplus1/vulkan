{-# language CPP #-}
-- | = Name
--
-- VK_EXT_fragment_density_map - device extension
--
-- == VK_EXT_fragment_density_map
--
-- [__Name String__]
--     @VK_EXT_fragment_density_map@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     219
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
--     -   Matthew Netsch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_fragment_density_map:%20&body=@mnetsch%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-09-25
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_fragment_invocation_density.html SPV_EXT_fragment_invocation_density>
--
-- [__Contributors__]
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc.
--
--     -   Robert VanReenen, Qualcomm Technologies, Inc.
--
--     -   Jonathan Wicks, Qualcomm Technologies, Inc.
--
--     -   Tate Hornbeck, Qualcomm Technologies, Inc.
--
--     -   Sam Holmes, Qualcomm Technologies, Inc.
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Pat Brown, NVIDIA
--
--     -   Daniel Rakos, AMD
--
--     -   Piers Daniell, NVIDIA
--
-- == Description
--
-- This extension allows an application to specify areas of the render
-- target where the fragment shader may be invoked fewer times. These
-- fragments are broadcasted out to multiple pixels to cover the render
-- target.
--
-- The primary use of this extension is to reduce workloads in areas where
-- lower quality may not be perceived such as the distorted edges of a lens
-- or the periphery of a user’s gaze.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFragmentDensityMapFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceFragmentDensityMapPropertiesEXT'
--
-- -   Extending 'Vulkan.Core10.Pass.RenderPassCreateInfo',
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.RenderPassCreateInfo2':
--
--     -   'RenderPassFragmentDensityMapCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME'
--
-- -   'EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits':
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits':
--
--     -   'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.ImageLayout.ImageLayout':
--
--     -   'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageViewCreateFlagBits.ImageViewCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageViewCreateFlagBits.IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SamplerCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT'
--
-- == New or Modified Built-In Variables
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-fraginvocationcount FragInvocationCountEXT>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-fragsize FragSizeEXT>
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-FragmentDensityEXT FragmentDensityEXT>
--
-- == Version History
--
-- -   Revision 1, 2018-09-25 (Matthew Netsch)
--
--     -   Initial version
--
-- = See Also
--
-- 'PhysicalDeviceFragmentDensityMapFeaturesEXT',
-- 'PhysicalDeviceFragmentDensityMapPropertiesEXT',
-- 'RenderPassFragmentDensityMapCreateInfoEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_fragment_density_map  ( PhysicalDeviceFragmentDensityMapFeaturesEXT(..)
                                                      , PhysicalDeviceFragmentDensityMapPropertiesEXT(..)
                                                      , RenderPassFragmentDensityMapCreateInfoEXT(..)
                                                      , EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION
                                                      , pattern EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION
                                                      , EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME
                                                      , pattern EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.Pass (AttachmentReference)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT))
-- | VkPhysicalDeviceFragmentDensityMapFeaturesEXT - Structure describing
-- fragment density map features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceFragmentDensityMapFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceFragmentDensityMapFeaturesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceFragmentDensityMapFeaturesEXT' /can/ also be included in
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable the
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentDensityMapFeaturesEXT = PhysicalDeviceFragmentDensityMapFeaturesEXT
  { -- | #features-fragmentDensityMap# @fragmentDensityMap@ specifies whether the
    -- implementation supports render passes with a fragment density map
    -- attachment. If this feature is not enabled and the @pNext@ chain of
    -- 'Vulkan.Core10.Pass.RenderPassCreateInfo' includes a
    -- 'RenderPassFragmentDensityMapCreateInfoEXT' structure,
    -- @fragmentDensityMapAttachment@ /must/ be
    -- 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'.
    fragmentDensityMap :: Bool
  , -- | #features-fragmentDensityMapDynamic# @fragmentDensityMapDynamic@
    -- specifies whether the implementation supports dynamic fragment density
    -- map image views. If this feature is not enabled,
    -- 'Vulkan.Core10.Enums.ImageViewCreateFlagBits.IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT'
    -- /must/ not be included in
    -- 'Vulkan.Core10.ImageView.ImageViewCreateInfo'::@flags@.
    fragmentDensityMapDynamic :: Bool
  , -- | #features-fragmentDensityMapNonSubsampledImages#
    -- @fragmentDensityMapNonSubsampledImages@ specifies whether the
    -- implementation supports regular non-subsampled image attachments with
    -- fragment density map render passes. If this feature is not enabled,
    -- render passes with a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-fragmentdensitymapattachment fragment density map attachment>
    -- /must/ only have
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-subsamplesampler subsampled attachments>
    -- bound.
    fragmentDensityMapNonSubsampledImages :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentDensityMapFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceFragmentDensityMapFeaturesEXT

instance ToCStruct PhysicalDeviceFragmentDensityMapFeaturesEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentDensityMapFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (fragmentDensityMap))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (fragmentDensityMapDynamic))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (fragmentDensityMapNonSubsampledImages))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFragmentDensityMapFeaturesEXT where
  peekCStruct p = do
    fragmentDensityMap <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    fragmentDensityMapDynamic <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    fragmentDensityMapNonSubsampledImages <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceFragmentDensityMapFeaturesEXT
             (bool32ToBool fragmentDensityMap) (bool32ToBool fragmentDensityMapDynamic) (bool32ToBool fragmentDensityMapNonSubsampledImages)

instance Storable PhysicalDeviceFragmentDensityMapFeaturesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentDensityMapFeaturesEXT where
  zero = PhysicalDeviceFragmentDensityMapFeaturesEXT
           zero
           zero
           zero


-- | VkPhysicalDeviceFragmentDensityMapPropertiesEXT - Structure describing
-- fragment density map properties that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceFragmentDensityMapPropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- == Valid Usage (Implicit)
--
-- If the 'PhysicalDeviceFragmentDensityMapPropertiesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits and properties.
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentDensityMapPropertiesEXT = PhysicalDeviceFragmentDensityMapPropertiesEXT
  { -- | #limits-minfragmentdensitytexelsize# @minFragmentDensityTexelSize@ is
    -- the minimum
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#glossary-fragment-density-texel-size fragment density texel size>.
    minFragmentDensityTexelSize :: Extent2D
  , -- | #limits-maxfragmentdensitytexelsize# @maxFragmentDensityTexelSize@ is
    -- the maximum fragment density texel size.
    maxFragmentDensityTexelSize :: Extent2D
  , -- | #limits-fragmentdensityinvocations# @fragmentDensityInvocations@
    -- specifies whether the implementation /may/ invoke additional fragment
    -- shader invocations for each covered sample.
    fragmentDensityInvocations :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentDensityMapPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceFragmentDensityMapPropertiesEXT

instance ToCStruct PhysicalDeviceFragmentDensityMapPropertiesEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentDensityMapPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (minFragmentDensityTexelSize)
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (maxFragmentDensityTexelSize)
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (fragmentDensityInvocations))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFragmentDensityMapPropertiesEXT where
  peekCStruct p = do
    minFragmentDensityTexelSize <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    maxFragmentDensityTexelSize <- peekCStruct @Extent2D ((p `plusPtr` 24 :: Ptr Extent2D))
    fragmentDensityInvocations <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    pure $ PhysicalDeviceFragmentDensityMapPropertiesEXT
             minFragmentDensityTexelSize maxFragmentDensityTexelSize (bool32ToBool fragmentDensityInvocations)

instance Storable PhysicalDeviceFragmentDensityMapPropertiesEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentDensityMapPropertiesEXT where
  zero = PhysicalDeviceFragmentDensityMapPropertiesEXT
           zero
           zero
           zero


-- | VkRenderPassFragmentDensityMapCreateInfoEXT - Structure containing
-- fragment density map attachment for render pass
--
-- = Description
--
-- The fragment density map is read at an implementation-dependent time
-- with the following constraints determined by the attachment’s image view
-- @flags@:
--
-- -   'Vulkan.Core10.Enums.ImageViewCreateFlagBits.IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT'
--     specifies that the fragment density map will be read by the device
--     during
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   'Vulkan.Core10.Enums.ImageViewCreateFlagBits.IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT'
--     specifies that the fragment density map will be read by the host
--     during 'Vulkan.Core10.CommandBuffer.endCommandBuffer' of the primary
--     command buffer that the render pass is recorded into
--
-- -   Otherwise the fragment density map will be read by the host during
--     'Vulkan.Core10.CommandBufferBuilding.cmdBeginRenderPass'
--
-- The fragment density map /may/ additionally be read by the device during
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
-- for any mode.
--
-- If this structure is not present, it is as if
-- @fragmentDensityMapAttachment@ was given as
-- 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'.
--
-- == Valid Usage
--
-- -   #VUID-VkRenderPassFragmentDensityMapCreateInfoEXT-fragmentDensityMapAttachment-02547#
--     If @fragmentDensityMapAttachment@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED',
--     @fragmentDensityMapAttachment@ /must/ be less than
--     'Vulkan.Core10.Pass.RenderPassCreateInfo'::@attachmentCount@
--
-- -   #VUID-VkRenderPassFragmentDensityMapCreateInfoEXT-fragmentDensityMapAttachment-02548#
--     If @fragmentDensityMapAttachment@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED',
--     @fragmentDensityMapAttachment@ /must/ not be an element of
--     'Vulkan.Core10.Pass.SubpassDescription'::@pInputAttachments@,
--     'Vulkan.Core10.Pass.SubpassDescription'::@pColorAttachments@,
--     'Vulkan.Core10.Pass.SubpassDescription'::@pResolveAttachments@,
--     'Vulkan.Core10.Pass.SubpassDescription'::@pDepthStencilAttachment@,
--     or 'Vulkan.Core10.Pass.SubpassDescription'::@pPreserveAttachments@
--     for any subpass
--
-- -   #VUID-VkRenderPassFragmentDensityMapCreateInfoEXT-fragmentDensityMapAttachment-02549#
--     If @fragmentDensityMapAttachment@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', @layout@ /must/ be
--     equal to
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT',
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-VkRenderPassFragmentDensityMapCreateInfoEXT-fragmentDensityMapAttachment-02550#
--     If @fragmentDensityMapAttachment@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED',
--     @fragmentDensityMapAttachment@ /must/ reference an attachment with a
--     @loadOp@ equal to
--     'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_LOAD' or
--     'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_DONT_CARE'
--
-- -   #VUID-VkRenderPassFragmentDensityMapCreateInfoEXT-fragmentDensityMapAttachment-02551#
--     If @fragmentDensityMapAttachment@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED',
--     @fragmentDensityMapAttachment@ /must/ reference an attachment with a
--     @storeOp@ equal to
--     'Vulkan.Core10.Enums.AttachmentStoreOp.ATTACHMENT_STORE_OP_DONT_CARE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderPassFragmentDensityMapCreateInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT'
--
-- -   #VUID-VkRenderPassFragmentDensityMapCreateInfoEXT-fragmentDensityMapAttachment-parameter#
--     @fragmentDensityMapAttachment@ /must/ be a valid
--     'Vulkan.Core10.Pass.AttachmentReference' structure
--
-- = See Also
--
-- 'Vulkan.Core10.Pass.AttachmentReference',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RenderPassFragmentDensityMapCreateInfoEXT = RenderPassFragmentDensityMapCreateInfoEXT
  { -- | @fragmentDensityMapAttachment@ is the fragment density map to use for
    -- the render pass.
    fragmentDensityMapAttachment :: AttachmentReference }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassFragmentDensityMapCreateInfoEXT)
#endif
deriving instance Show RenderPassFragmentDensityMapCreateInfoEXT

instance ToCStruct RenderPassFragmentDensityMapCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassFragmentDensityMapCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AttachmentReference)) (fragmentDensityMapAttachment)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AttachmentReference)) (zero)
    f

instance FromCStruct RenderPassFragmentDensityMapCreateInfoEXT where
  peekCStruct p = do
    fragmentDensityMapAttachment <- peekCStruct @AttachmentReference ((p `plusPtr` 16 :: Ptr AttachmentReference))
    pure $ RenderPassFragmentDensityMapCreateInfoEXT
             fragmentDensityMapAttachment

instance Storable RenderPassFragmentDensityMapCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RenderPassFragmentDensityMapCreateInfoEXT where
  zero = RenderPassFragmentDensityMapCreateInfoEXT
           zero


type EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION"
pattern EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION = 1


type EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME = "VK_EXT_fragment_density_map"

-- No documentation found for TopLevel "VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME"
pattern EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME = "VK_EXT_fragment_density_map"

