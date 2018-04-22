{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview
  ( pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES
  , pattern VK_DEPENDENCY_VIEW_LOCAL_BIT
  , VkPhysicalDeviceMultiviewFeatures(..)
  , VkPhysicalDeviceMultiviewProperties(..)
  , VkRenderPassMultiviewCreateInfo(..)
  ) where

import Data.Int
  ( Int32
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.Pass
  ( VkDependencyFlagBits(..)
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO = VkStructureType 1000053000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES = VkStructureType 1000053001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES = VkStructureType 1000053002
-- | @VK_DEPENDENCY_VIEW_LOCAL_BIT@ specifies that a [subpass has more than
-- one
-- view](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-barriers-subpass-self-dependencies).
pattern VK_DEPENDENCY_VIEW_LOCAL_BIT :: VkDependencyFlagBits
pattern VK_DEPENDENCY_VIEW_LOCAL_BIT = VkDependencyFlagBits 0x00000002
-- | VkPhysicalDeviceMultiviewFeatures - Structure describing multiview
-- features that can be supported by an implementation
--
-- = Members
--
-- The members of the @VkPhysicalDeviceMultiviewFeatures@ structure
-- describe the following features:
--
-- = Description
--
-- -   @multiview@ specifies whether the implementation supports multiview
--     rendering within a render pass. If this feature is not enabled, the
--     view mask of each subpass /must/ always be zero.
--
-- -   @multiviewGeometryShader@ specifies whether the implementation
--     supports multiview rendering within a render pass, with [geometry
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#geometry).
--     If this feature is not enabled, then a pipeline compiled against a
--     subpass with a non-zero view mask /must/ not include a geometry
--     shader.
--
-- -   @multiviewTessellationShader@ specifies whether the implementation
--     supports multiview rendering within a render pass, with
--     [tessellation
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#tessellation).
--     If this feature is not enabled, then a pipeline compiled against a
--     subpass with a non-zero view mask /must/ not include any
--     tessellation shaders.
--
-- If the @VkPhysicalDeviceMultiviewFeatures@ structure is included in the
-- @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- @VkPhysicalDeviceMultiviewFeatures@ /can/ also be used in the @pNext@
-- chain of 'Graphics.Vulkan.Core10.Device.VkDeviceCreateInfo' to enable
-- the features.
--
-- == Valid Usage
--
-- -   If @multiviewGeometryShader@ is enabled then @multiview@ /must/ also
--     be enabled.
--
-- -   If @multiviewTessellationShader@ is enabled then @multiview@ /must/
--     also be enabled.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES@
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDeviceMultiviewFeatures = VkPhysicalDeviceMultiviewFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "multiview"
  vkMultiview :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "multiviewGeometryShader"
  vkMultiviewGeometryShader :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "multiviewTessellationShader"
  vkMultiviewTessellationShader :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceMultiviewFeatures where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMultiviewFeatures <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
                                               <*> peek (ptr `plusPtr` 20)
                                               <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceMultiviewFeatures))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceMultiviewFeatures))
                *> poke (ptr `plusPtr` 16) (vkMultiview (poked :: VkPhysicalDeviceMultiviewFeatures))
                *> poke (ptr `plusPtr` 20) (vkMultiviewGeometryShader (poked :: VkPhysicalDeviceMultiviewFeatures))
                *> poke (ptr `plusPtr` 24) (vkMultiviewTessellationShader (poked :: VkPhysicalDeviceMultiviewFeatures))
-- | VkPhysicalDeviceMultiviewProperties - Structure describing multiview
-- limits that can be supported by an implementation
--
-- = Members
--
-- The members of the @VkPhysicalDeviceMultiviewProperties@ structure
-- describe the following implementation-dependent limits:
--
-- = Description
--
-- -   @maxMultiviewViewCount@ is one greater than the maximum view index
--     that /can/ be used in a subpass.
--
-- -   @maxMultiviewInstanceIndex@ is the maximum valid value of instance
--     index allowed to be generated by a drawing command recorded within a
--     subpass of a multiview render pass instance.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES@
--
-- If the @VkPhysicalDeviceMultiviewProperties@ structure is included in
-- the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDeviceMultiviewProperties = VkPhysicalDeviceMultiviewProperties
  { -- No documentation found for Nested "VkPhysicalDeviceMultiviewProperties" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewProperties" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewProperties" "maxMultiviewViewCount"
  vkMaxMultiviewViewCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewProperties" "maxMultiviewInstanceIndex"
  vkMaxMultiviewInstanceIndex :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceMultiviewProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMultiviewProperties <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceMultiviewProperties))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceMultiviewProperties))
                *> poke (ptr `plusPtr` 16) (vkMaxMultiviewViewCount (poked :: VkPhysicalDeviceMultiviewProperties))
                *> poke (ptr `plusPtr` 20) (vkMaxMultiviewInstanceIndex (poked :: VkPhysicalDeviceMultiviewProperties))
-- | VkRenderPassMultiviewCreateInfo - Structure containing multiview info
-- for all subpasses
--
-- = Description
--
-- When a subpass uses a non-zero view mask, /multiview/ functionality is
-- considered to be enabled. Multiview is all-or-nothing for a render pass
-- - that is, either all subpasses /must/ have a non-zero view mask (though
-- some subpasses /may/ have only one view) or all /must/ be zero.
-- Multiview causes all drawing and clear commands in the subpass to behave
-- as if they were broadcast to each view, where a view is represented by
-- one layer of the framebuffer attachments. All draws and clears are
-- broadcast to each /view index/ whose bit is set in the view mask. The
-- view index is provided in the @ViewIndex@ shader input variable, and
-- color, depth\/stencil, and input attachments all read\/write the layer
-- of the framebuffer corresponding to the view index.
--
-- If the view mask is zero for all subpasses, multiview is considered to
-- be disabled and all drawing commands execute normally, without this
-- additional broadcasting.
--
-- Some implementations /may/ not support multiview in conjunction with
-- [geometry
-- shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-multiview-gs)
-- or [tessellation
-- shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-multiview-tess).
--
-- When multiview is enabled, the @VK_DEPENDENCY_VIEW_LOCAL_BIT@ bit in a
-- dependency /can/ be used to express a view-local dependency, meaning
-- that each view in the destination subpass depends on a single view in
-- the source subpass. Unlike pipeline barriers, a subpass dependency /can/
-- potentially have a different view mask in the source subpass and the
-- destination subpass. If the dependency is view-local, then each view
-- (dstView) in the destination subpass depends on the view dstView +
-- pViewOffsets[dependency] in the source subpass. If there is not such a
-- view in the source subpass, then this dependency does not affect that
-- view in the destination subpass. If the dependency is not view-local,
-- then all views in the destination subpass depend on all views in the
-- source subpass, and the view offset is ignored. A non-zero view offset
-- is not allowed in a self-dependency.
--
-- The elements of @pCorrelationMasks@ are a set of masks of views
-- indicating that views in the same mask /may/ exhibit spatial coherency
-- between the views, making it more efficient to render them concurrently.
-- Correlation masks /must/ not have a functional effect on the results of
-- the multiview rendering.
--
-- When multiview is enabled, at the beginning of each subpass all
-- non-render pass state is undefined. In particular, each time
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBeginRenderPass' or
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdNextSubpass' is
-- called the graphics pipeline /must/ be bound, any relevant descriptor
-- sets or vertex\/index buffers /must/ be bound, and any relevant dynamic
-- state or push constants /must/ be set before they are used.
--
-- A multiview subpass /can/ declare that its shaders will write per-view
-- attributes for all views in a single invocation, by setting the
-- @VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX@ bit in the subpass
-- description. The only supported per-view attributes are position and
-- viewport mask, and per-view position and viewport masks are written to
-- output array variables decorated with @PositionPerViewNV@ and
-- @ViewportMaskPerViewNV@, respectively. If
-- @{html_spec_relative}#VK_NV_viewport_array2@ is not supported and
-- enabled, @ViewportMaskPerViewNV@ /must/ not be used. Values written to
-- elements of @PositionPerViewNV@ and @ViewportMaskPerViewNV@ /must/ not
-- depend on the @ViewIndex@. The shader /must/ also write to an output
-- variable decorated with @Position@, and the value written to @Position@
-- /must/ equal the value written to @PositionPerViewNV@[@ViewIndex@].
-- Similarly, if @ViewportMaskPerViewNV@ is written to then the shader
-- /must/ also write to an output variable decorated with @ViewportMaskNV@,
-- and the value written to @ViewportMaskNV@ /must/ equal the value written
-- to @ViewportMaskPerViewNV@[@ViewIndex@]. Implementations will either use
-- values taken from @Position@ and @ViewportMaskNV@ and invoke the shader
-- once for each view, or will use values taken from @PositionPerViewNV@
-- and @ViewportMaskPerViewNV@ and invoke the shader fewer times. The
-- values written to @Position@ and @ViewportMaskNV@ /must/ not depend on
-- the values written to @PositionPerViewNV@ and @ViewportMaskPerViewNV@,
-- or vice versa (to allow compilers to eliminate the unused outputs). All
-- attributes that do not have *PerViewNV counterparts /must/ not depend on
-- @ViewIndex@.
--
-- Per-view attributes are all-or-nothing for a subpass. That is, all
-- pipelines compiled against a subpass that includes the
-- @VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX@ bit /must/ write
-- per-view attributes to the *PerViewNV[] shader outputs, in addition to
-- the non-per-view (e.g. @Position@) outputs. Pipelines compiled against a
-- subpass that does not include this bit /must/ not include the
-- *PerViewNV[] outputs in their interfaces.
--
-- == Valid Usage
--
-- -   If @subpassCount@ is not zero, @subpassCount@ /must/ be equal to the
--     @subpassCount@ in the @VkRenderPassCreateInfo@ structure at the
--     start of the chain
--
-- -   If @dependencyCount@ is not zero, @dependencyCount@ /must/ be equal
--     to the @dependencyCount@ in the @VkRenderPassCreateInfo@ structure
--     at the start of the chain
--
-- -   Each view index /must/ not be set in more than one element of
--     @pCorrelationMasks@
--
-- -   If an element of @pViewOffsets@ is non-zero, the corresponding
--     'Graphics.Vulkan.Core10.Pass.VkSubpassDependency' structure /must/
--     have different values of @srcSubpass@ and @dstSubpass@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO@
--
-- -   If @subpassCount@ is not @0@, @pViewMasks@ /must/ be a valid pointer
--     to an array of @subpassCount@ @uint32_t@ values
--
-- -   If @dependencyCount@ is not @0@, @pViewOffsets@ /must/ be a valid
--     pointer to an array of @dependencyCount@ @int32_t@ values
--
-- -   If @correlationMaskCount@ is not @0@, @pCorrelationMasks@ /must/ be
--     a valid pointer to an array of @correlationMaskCount@ @uint32_t@
--     values
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkRenderPassMultiviewCreateInfo = VkRenderPassMultiviewCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @subpassCount@ is zero or is the number of subpasses in the render pass.
  vkSubpassCount :: Word32
  , -- | @pViewMasks@ points to an array of @subpassCount@ number of view masks,
  -- where each mask is a bitfield of view indices describing which views
  -- rendering is broadcast to in each subpass, when multiview is enabled. If
  -- @subpassCount@ is zero, each view mask is treated as zero.
  vkPViewMasks :: Ptr Word32
  , -- | @dependencyCount@ is zero or the number of dependencies in the render
  -- pass.
  vkDependencyCount :: Word32
  , -- | @pViewOffsets@ points to an array of @dependencyCount@ view offsets, one
  -- for each dependency. If @dependencyCount@ is zero, each dependency’s
  -- view offset is treated as zero. Each view offset controls which views in
  -- the source subpass the views in the destination subpass depend on.
  vkPViewOffsets :: Ptr Int32
  , -- | @correlationMaskCount@ is zero or a number of correlation masks.
  vkCorrelationMaskCount :: Word32
  , -- | @pCorrelationMasks@ is an array of view masks indicating sets of views
  -- that /may/ be more efficient to render concurrently.
  vkPCorrelationMasks :: Ptr Word32
  }
  deriving (Eq, Show)

instance Storable VkRenderPassMultiviewCreateInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkRenderPassMultiviewCreateInfo <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 24)
                                             <*> peek (ptr `plusPtr` 32)
                                             <*> peek (ptr `plusPtr` 40)
                                             <*> peek (ptr `plusPtr` 48)
                                             <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkRenderPassMultiviewCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkRenderPassMultiviewCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkSubpassCount (poked :: VkRenderPassMultiviewCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPViewMasks (poked :: VkRenderPassMultiviewCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkDependencyCount (poked :: VkRenderPassMultiviewCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPViewOffsets (poked :: VkRenderPassMultiviewCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkCorrelationMaskCount (poked :: VkRenderPassMultiviewCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkPCorrelationMasks (poked :: VkRenderPassMultiviewCreateInfo))
