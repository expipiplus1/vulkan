{-# language CPP #-}
-- | = Name
--
-- VK_EXT_multisampled_render_to_single_sampled - device extension
--
-- == VK_EXT_multisampled_render_to_single_sampled
--
-- [__Name String__]
--     @VK_EXT_multisampled_render_to_single_sampled@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     377
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_create_renderpass2 VK_KHR_create_renderpass2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_depth_stencil_resolve VK_KHR_depth_stencil_resolve>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_multisampled_render_to_single_sampled] @syoussefi%0A*Here describe the issue or question you have about the VK_EXT_multisampled_render_to_single_sampled extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_multisampled_render_to_single_sampled.adoc VK_EXT_multisampled_render_to_single_sampled>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-04-16
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Shahbaz Youssefi, Google
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Jörg Wagner, Arm
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc.
--
--     -   Jarred Davies, Imagination Technologies
--
-- == Description
--
-- With careful usage of resolve attachments, multisampled image memory
-- allocated with
-- 'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT',
-- @loadOp@ not equal to
-- 'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_LOAD' and
-- @storeOp@ not equal to
-- 'Vulkan.Core10.Enums.AttachmentStoreOp.ATTACHMENT_STORE_OP_STORE', a
-- Vulkan application is able to efficiently perform multisampled rendering
-- without incurring any additional memory penalty on some implementations.
--
-- Under certain circumstances however, the application may not be able to
-- complete its multisampled rendering within a single render pass; for
-- example if it does partial rasterization from frame to frame, blending
-- on an image from a previous frame, or in emulation of
-- GL_EXT_multisampled_render_to_texture. In such cases, the application
-- can use an initial subpass to effectively load single-sampled data from
-- the next subpass’s resolve attachment and fill in the multisampled
-- attachment which otherwise uses @loadOp@ equal to
-- 'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_DONT_CARE'.
-- However, this is not always possible (for example for stencil in the
-- absence of VK_EXT_shader_stencil_export) and has multiple drawbacks.
--
-- Some implementations are able to perform said operation efficiently in
-- hardware, effectively loading a multisampled attachment from the
-- contents of a single sampled one. Together with the ability to perform a
-- resolve operation at the end of a subpass, these implementations are
-- able to perform multisampled rendering on single-sampled attachments
-- with no extra memory or bandwidth overhead. This extension exposes this
-- capability by allowing a framebuffer and render pass to include
-- single-sampled attachments while rendering is done with a specified
-- number of samples.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.FormatProperties2':
--
--     -   'SubpassResolvePerformanceQueryEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDescription2',
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo':
--
--     -   'MultisampledRenderToSingleSampledInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_EXTENSION_NAME'
--
-- -   'EXT_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBPASS_RESOLVE_PERFORMANCE_QUERY_EXT'
--
-- == Issues
--
-- 1) Could the multisampled attachment be initialized through some form of
-- copy?
--
-- __RESOLVED__: No. Some implementations do not support copying between
-- attachments in general, and find expressing this operation through a
-- copy unnatural.
--
-- 2) Another way to achieve this is by introducing a new @loadOp@ to load
-- the contents of the multisampled image from a single-sampled one. Why is
-- this extension preferred?
--
-- __RESOLVED__: Using this extension simplifies the application, as it
-- does not need to manage a secondary lazily-allocated image.
-- Additionally, using this extension leaves less room for error; for
-- example a single mistake in @loadOp@ or @storeOp@ would result in the
-- lazily-allocated image to actually take up memory, and remain so until
-- destruction.
--
-- 3) There is no guarantee that multisampled data between two subpasses
-- with the same number of samples will be retained as the implementation
-- may be forced to split the render pass implicitly for various reasons.
-- Should this extension require that every subpass that uses
-- multisampled-render-to-single-sampled end in an implicit render pass
-- split (which results in a resolve operation)?
--
-- __RESOLVED__: No. Not requiring this allows render passes with multiple
-- multisampled-render-to-single-sampled subpasses to potentially execute
-- more efficiently (though there is no guarantee).
--
-- == Version History
--
-- -   Revision 1, 2021-04-12 (Shahbaz Youssefi)
--
-- == See Also
--
-- 'MultisampledRenderToSingleSampledInfoEXT',
-- 'PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT',
-- 'SubpassResolvePerformanceQueryEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_multisampled_render_to_single_sampled Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled  ( PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT(..)
                                                                       , SubpassResolvePerformanceQueryEXT(..)
                                                                       , MultisampledRenderToSingleSampledInfoEXT(..)
                                                                       , EXT_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_SPEC_VERSION
                                                                       , pattern EXT_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_SPEC_VERSION
                                                                       , EXT_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_EXTENSION_NAME
                                                                       , pattern EXT_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_EXTENSION_NAME
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
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBPASS_RESOLVE_PERFORMANCE_QUERY_EXT))
-- | VkPhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT - Structure
-- describing whether multisampled rendering to single-sampled attachments
-- is supported
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT'
-- structure is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT'
-- /can/ also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_multisampled_render_to_single_sampled VK_EXT_multisampled_render_to_single_sampled>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT = PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT
  { -- | #features-multisampledRenderToSingleSampled#
    -- @multisampledRenderToSingleSampled@ indicates that the implementation
    -- supports multisampled rendering to single-sampled render pass
    -- attachments.
    multisampledRenderToSingleSampled :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT

instance ToCStruct PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (multisampledRenderToSingleSampled))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT where
  peekCStruct p = do
    multisampledRenderToSingleSampled <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT
             (bool32ToBool multisampledRenderToSingleSampled)

instance Storable PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT where
  zero = PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT
           zero


-- | VkSubpassResolvePerformanceQueryEXT - Structure specifying the
-- efficiency of subpass resolve for an attachment with a format
--
-- = Description
--
-- If @optimal@ is 'Vulkan.Core10.FundamentalTypes.FALSE' for a
-- 'Vulkan.Core10.Enums.Format.Format', using a subpass resolve operation
-- on a multisampled attachment with this format can incur additional
-- costs, including additional memory bandwidth usage and a higher memory
-- footprint. If an attachment with such a format is used in a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#subpass-multisampledrendertosinglesampled multisampled-render-to-single-sampled>
-- subpass, the additional memory and memory bandwidth usage can nullify
-- the benefits of using the @VK_EXT_multisampled_render_to_single_sampled@
-- extension.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_multisampled_render_to_single_sampled VK_EXT_multisampled_render_to_single_sampled>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SubpassResolvePerformanceQueryEXT = SubpassResolvePerformanceQueryEXT
  { -- | @optimal@ specifies that a subpass resolve operation is optimally
    -- performed.
    optimal :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubpassResolvePerformanceQueryEXT)
#endif
deriving instance Show SubpassResolvePerformanceQueryEXT

instance ToCStruct SubpassResolvePerformanceQueryEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubpassResolvePerformanceQueryEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_RESOLVE_PERFORMANCE_QUERY_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (optimal))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_RESOLVE_PERFORMANCE_QUERY_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct SubpassResolvePerformanceQueryEXT where
  peekCStruct p = do
    optimal <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ SubpassResolvePerformanceQueryEXT
             (bool32ToBool optimal)

instance Storable SubpassResolvePerformanceQueryEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SubpassResolvePerformanceQueryEXT where
  zero = SubpassResolvePerformanceQueryEXT
           zero


-- | VkMultisampledRenderToSingleSampledInfoEXT - Structure containing info
-- for multisampled rendering to single-sampled attachments in a subpass
--
-- == Valid Usage
--
-- -   #VUID-VkMultisampledRenderToSingleSampledInfoEXT-rasterizationSamples-06878#
--     The value of @rasterizationSamples@ /must/ not be
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkMultisampledRenderToSingleSampledInfoEXT-pNext-06880# If
--     added to the @pNext@ chain of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo',
--     each @imageView@ member of any element of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@,
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment@,
--     or
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment@
--     that is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have a
--     format that supports the sample count specified in
--     @rasterizationSamples@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMultisampledRenderToSingleSampledInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_INFO_EXT'
--
-- -   #VUID-VkMultisampledRenderToSingleSampledInfoEXT-rasterizationSamples-parameter#
--     @rasterizationSamples@ /must/ be a valid
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_multisampled_render_to_single_sampled VK_EXT_multisampled_render_to_single_sampled>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data MultisampledRenderToSingleSampledInfoEXT = MultisampledRenderToSingleSampledInfoEXT
  { -- | @multisampledRenderToSingleSampledEnable@ controls whether multisampled
    -- rendering to single-sampled attachments is performed as described
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#multisampled-render-to-single-sampled below>.
    multisampledRenderToSingleSampledEnable :: Bool
  , -- | @rasterizationSamples@ is a
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' specifying
    -- the number of samples used in rasterization.
    rasterizationSamples :: SampleCountFlagBits
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MultisampledRenderToSingleSampledInfoEXT)
#endif
deriving instance Show MultisampledRenderToSingleSampledInfoEXT

instance ToCStruct MultisampledRenderToSingleSampledInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MultisampledRenderToSingleSampledInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (multisampledRenderToSingleSampledEnable))
    poke ((p `plusPtr` 20 :: Ptr SampleCountFlagBits)) (rasterizationSamples)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr SampleCountFlagBits)) (zero)
    f

instance FromCStruct MultisampledRenderToSingleSampledInfoEXT where
  peekCStruct p = do
    multisampledRenderToSingleSampledEnable <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    rasterizationSamples <- peek @SampleCountFlagBits ((p `plusPtr` 20 :: Ptr SampleCountFlagBits))
    pure $ MultisampledRenderToSingleSampledInfoEXT
             (bool32ToBool multisampledRenderToSingleSampledEnable)
             rasterizationSamples

instance Storable MultisampledRenderToSingleSampledInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MultisampledRenderToSingleSampledInfoEXT where
  zero = MultisampledRenderToSingleSampledInfoEXT
           zero
           zero


type EXT_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_SPEC_VERSION"
pattern EXT_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_SPEC_VERSION = 1


type EXT_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_EXTENSION_NAME = "VK_EXT_multisampled_render_to_single_sampled"

-- No documentation found for TopLevel "VK_EXT_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_EXTENSION_NAME"
pattern EXT_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_EXTENSION_NAME = "VK_EXT_multisampled_render_to_single_sampled"

