{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview
  ( withCStructPhysicalDeviceMultiviewFeatures
  , fromCStructPhysicalDeviceMultiviewFeatures
  , PhysicalDeviceMultiviewFeatures(..)
  , withCStructPhysicalDeviceMultiviewProperties
  , fromCStructPhysicalDeviceMultiviewProperties
  , PhysicalDeviceMultiviewProperties(..)
  , withCStructRenderPassMultiviewCreateInfo
  , fromCStructRenderPassMultiviewCreateInfo
  , RenderPassMultiviewCreateInfo(..)
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES
  , pattern VK_DEPENDENCY_VIEW_LOCAL_BIT
  ) where

import Data.Function
  ( (&)
  )
import Data.Int
  ( Int32
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview
  ( VkPhysicalDeviceMultiviewFeatures(..)
  , VkPhysicalDeviceMultiviewProperties(..)
  , VkRenderPassMultiviewCreateInfo(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview
  ( pattern VK_DEPENDENCY_VIEW_LOCAL_BIT
  )



-- | VkPhysicalDeviceMultiviewFeatures - Structure describing multiview
-- features that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewFeatures'
-- structure describe the following features:
--
-- = Description
--
-- -   @multiview@ specifies whether the implementation supports multiview
--     rendering within a render pass. If this feature is not enabled, the
--     view mask of each subpass /must/ always be zero.
--
-- -   @multiviewGeometryShader@ specifies whether the implementation
--     supports multiview rendering within a render pass, with
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#geometry geometry shaders>.
--     If this feature is not enabled, then a pipeline compiled against a
--     subpass with a non-zero view mask /must/ not include a geometry
--     shader.
--
-- -   @multiviewTessellationShader@ specifies whether the implementation
--     supports multiview rendering within a render pass, with
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#tessellation tessellation shaders>.
--     If this feature is not enabled, then a pipeline compiled against a
--     subpass with a non-zero view mask /must/ not include any
--     tessellation shaders.
--
-- If the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewFeatures'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewFeatures'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable the
-- features.
--
-- == Valid Usage
--
-- -   If @multiviewGeometryShader@ is enabled then @multiview@ /must/ also
--     be enabled.
--
-- -   If @multiviewTessellationShader@ is enabled then @multiview@ /must/
--     also be enabled.
--
-- Unresolved directive in VkPhysicalDeviceMultiviewFeatures.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceMultiviewFeatures.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceMultiviewFeatures = PhysicalDeviceMultiviewFeatures
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceMultiviewFeatures" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMultiviewFeatures" "multiview"
  multiview :: Bool
  , -- No documentation found for Nested "PhysicalDeviceMultiviewFeatures" "multiviewGeometryShader"
  multiviewGeometryShader :: Bool
  , -- No documentation found for Nested "PhysicalDeviceMultiviewFeatures" "multiviewTessellationShader"
  multiviewTessellationShader :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceMultiviewFeatures' and
-- marshal a 'PhysicalDeviceMultiviewFeatures' into it. The 'VkPhysicalDeviceMultiviewFeatures' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceMultiviewFeatures :: PhysicalDeviceMultiviewFeatures -> (VkPhysicalDeviceMultiviewFeatures -> IO a) -> IO a
withCStructPhysicalDeviceMultiviewFeatures marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceMultiviewFeatures)) (\pPNext -> cont (VkPhysicalDeviceMultiviewFeatures VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES pPNext (boolToBool32 (multiview (marshalled :: PhysicalDeviceMultiviewFeatures))) (boolToBool32 (multiviewGeometryShader (marshalled :: PhysicalDeviceMultiviewFeatures))) (boolToBool32 (multiviewTessellationShader (marshalled :: PhysicalDeviceMultiviewFeatures)))))

-- | A function to read a 'VkPhysicalDeviceMultiviewFeatures' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceMultiviewFeatures'.
fromCStructPhysicalDeviceMultiviewFeatures :: VkPhysicalDeviceMultiviewFeatures -> IO PhysicalDeviceMultiviewFeatures
fromCStructPhysicalDeviceMultiviewFeatures c = PhysicalDeviceMultiviewFeatures <$> -- Univalued Member elided
                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceMultiviewFeatures)))
                                                                               <*> pure (bool32ToBool (vkMultiview (c :: VkPhysicalDeviceMultiviewFeatures)))
                                                                               <*> pure (bool32ToBool (vkMultiviewGeometryShader (c :: VkPhysicalDeviceMultiviewFeatures)))
                                                                               <*> pure (bool32ToBool (vkMultiviewTessellationShader (c :: VkPhysicalDeviceMultiviewFeatures)))

instance Zero PhysicalDeviceMultiviewFeatures where
  zero = PhysicalDeviceMultiviewFeatures Nothing
                                         False
                                         False
                                         False



-- | VkPhysicalDeviceMultiviewProperties - Structure describing multiview
-- limits that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewProperties'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewProperties'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- Unresolved directive in VkPhysicalDeviceMultiviewProperties.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceMultiviewProperties.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceMultiviewProperties = PhysicalDeviceMultiviewProperties
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceMultiviewProperties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMultiviewProperties" "maxMultiviewViewCount"
  maxMultiviewViewCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMultiviewProperties" "maxMultiviewInstanceIndex"
  maxMultiviewInstanceIndex :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceMultiviewProperties' and
-- marshal a 'PhysicalDeviceMultiviewProperties' into it. The 'VkPhysicalDeviceMultiviewProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceMultiviewProperties :: PhysicalDeviceMultiviewProperties -> (VkPhysicalDeviceMultiviewProperties -> IO a) -> IO a
withCStructPhysicalDeviceMultiviewProperties marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceMultiviewProperties)) (\pPNext -> cont (VkPhysicalDeviceMultiviewProperties VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES pPNext (maxMultiviewViewCount (marshalled :: PhysicalDeviceMultiviewProperties)) (maxMultiviewInstanceIndex (marshalled :: PhysicalDeviceMultiviewProperties))))

-- | A function to read a 'VkPhysicalDeviceMultiviewProperties' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceMultiviewProperties'.
fromCStructPhysicalDeviceMultiviewProperties :: VkPhysicalDeviceMultiviewProperties -> IO PhysicalDeviceMultiviewProperties
fromCStructPhysicalDeviceMultiviewProperties c = PhysicalDeviceMultiviewProperties <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceMultiviewProperties)))
                                                                                   <*> pure (vkMaxMultiviewViewCount (c :: VkPhysicalDeviceMultiviewProperties))
                                                                                   <*> pure (vkMaxMultiviewInstanceIndex (c :: VkPhysicalDeviceMultiviewProperties))

instance Zero PhysicalDeviceMultiviewProperties where
  zero = PhysicalDeviceMultiviewProperties Nothing
                                           zero
                                           zero



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
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-multiview-gs geometry shaders>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-multiview-tess tessellation shaders>.
--
-- When multiview is enabled, the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VK_DEPENDENCY_VIEW_LOCAL_BIT'
-- bit in a dependency /can/ be used to express a view-local dependency,
-- meaning that each view in the destination subpass depends on a single
-- view in the source subpass. Unlike pipeline barriers, a subpass
-- dependency /can/ potentially have a different view mask in the source
-- subpass and the destination subpass. If the dependency is view-local,
-- then each view (dstView) in the destination subpass depends on the view
-- dstView + @pViewOffsets@[dependency] in the source subpass. If there is
-- not such a view in the source subpass, then this dependency does not
-- affect that view in the destination subpass. If the dependency is not
-- view-local, then all views in the destination subpass depend on all
-- views in the source subpass, and the view offset is ignored. A non-zero
-- view offset is not allowed in a self-dependency.
--
-- The elements of @pCorrelationMasks@ are a set of masks of views
-- indicating that views in the same mask /may/ exhibit spatial coherency
-- between the views, making it more efficient to render them concurrently.
-- Correlation masks /must/ not have a functional effect on the results of
-- the multiview rendering.
--
-- When multiview is enabled, at the beginning of each subpass all
-- non-render pass state is undefined. In particular, each time
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginRenderPass' or
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdNextSubpass' is
-- called the graphics pipeline /must/ be bound, any relevant descriptor
-- sets or vertex\/index buffers /must/ be bound, and any relevant dynamic
-- state or push constants /must/ be set before they are used.
--
-- A multiview subpass /can/ declare that its shaders will write per-view
-- attributes for all views in a single invocation, by setting the
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_multiview_per_view_attributes.VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX'
-- bit in the subpass description. The only supported per-view attributes
-- are position and viewport mask, and per-view position and viewport masks
-- are written to output array variables decorated with @PositionPerViewNV@
-- and @ViewportMaskPerViewNV@, respectively. If
-- @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.1-extensions\/html\/vkspec.html#VK_NV_viewport_array2@
-- is not supported and enabled, @ViewportMaskPerViewNV@ /must/ not be
-- used. Values written to elements of @PositionPerViewNV@ and
-- @ViewportMaskPerViewNV@ /must/ not depend on the @ViewIndex@. The shader
-- /must/ also write to an output variable decorated with @Position@, and
-- the value written to @Position@ /must/ equal the value written to
-- @PositionPerViewNV@[@ViewIndex@]. Similarly, if @ViewportMaskPerViewNV@
-- is written to then the shader /must/ also write to an output variable
-- decorated with @ViewportMaskNV@, and the value written to
-- @ViewportMaskNV@ /must/ equal the value written to
-- @ViewportMaskPerViewNV@[@ViewIndex@]. Implementations will either use
-- values taken from @Position@ and @ViewportMaskNV@ and invoke the shader
-- once for each view, or will use values taken from @PositionPerViewNV@
-- and @ViewportMaskPerViewNV@ and invoke the shader fewer times. The
-- values written to @Position@ and @ViewportMaskNV@ /must/ not depend on
-- the values written to @PositionPerViewNV@ and @ViewportMaskPerViewNV@,
-- or vice versa (to allow compilers to eliminate the unused outputs). All
-- attributes that do not have @*PerViewNV@ counterparts /must/ not depend
-- on @ViewIndex@.
--
-- Per-view attributes are all-or-nothing for a subpass. That is, all
-- pipelines compiled against a subpass that includes the
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_multiview_per_view_attributes.VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX'
-- bit /must/ write per-view attributes to the @*PerViewNV[]@ shader
-- outputs, in addition to the non-per-view (e.g. @Position@) outputs.
-- Pipelines compiled against a subpass that does not include this bit
-- /must/ not include the @*PerViewNV[]@ outputs in their interfaces.
--
-- == Valid Usage
--
-- -   Each view index /must/ not be set in more than one element of
--     @pCorrelationMasks@
--
-- Unresolved directive in VkRenderPassMultiviewCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkRenderPassMultiviewCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data RenderPassMultiviewCreateInfo = RenderPassMultiviewCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "RenderPassMultiviewCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassMultiviewCreateInfo" "pViewMasks"
  viewMasks :: Vector Word32
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassMultiviewCreateInfo" "pViewOffsets"
  viewOffsets :: Vector Int32
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassMultiviewCreateInfo" "pCorrelationMasks"
  correlationMasks :: Vector Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkRenderPassMultiviewCreateInfo' and
-- marshal a 'RenderPassMultiviewCreateInfo' into it. The 'VkRenderPassMultiviewCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructRenderPassMultiviewCreateInfo :: RenderPassMultiviewCreateInfo -> (VkRenderPassMultiviewCreateInfo -> IO a) -> IO a
withCStructRenderPassMultiviewCreateInfo marshalled cont = withVec (&) (correlationMasks (marshalled :: RenderPassMultiviewCreateInfo)) (\pPCorrelationMasks -> withVec (&) (viewOffsets (marshalled :: RenderPassMultiviewCreateInfo)) (\pPViewOffsets -> withVec (&) (viewMasks (marshalled :: RenderPassMultiviewCreateInfo)) (\pPViewMasks -> maybeWith withSomeVkStruct (next (marshalled :: RenderPassMultiviewCreateInfo)) (\pPNext -> cont (VkRenderPassMultiviewCreateInfo VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO pPNext (fromIntegral (Data.Vector.length (viewMasks (marshalled :: RenderPassMultiviewCreateInfo)))) pPViewMasks (fromIntegral (Data.Vector.length (viewOffsets (marshalled :: RenderPassMultiviewCreateInfo)))) pPViewOffsets (fromIntegral (Data.Vector.length (correlationMasks (marshalled :: RenderPassMultiviewCreateInfo)))) pPCorrelationMasks)))))

-- | A function to read a 'VkRenderPassMultiviewCreateInfo' and all additional
-- structures in the pointer chain into a 'RenderPassMultiviewCreateInfo'.
fromCStructRenderPassMultiviewCreateInfo :: VkRenderPassMultiviewCreateInfo -> IO RenderPassMultiviewCreateInfo
fromCStructRenderPassMultiviewCreateInfo c = RenderPassMultiviewCreateInfo <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkRenderPassMultiviewCreateInfo)))
                                                                           -- Length valued member elided
                                                                           <*> (Data.Vector.generateM (fromIntegral (vkSubpassCount (c :: VkRenderPassMultiviewCreateInfo))) (peekElemOff (vkPViewMasks (c :: VkRenderPassMultiviewCreateInfo))))
                                                                           -- Length valued member elided
                                                                           <*> (Data.Vector.generateM (fromIntegral (vkDependencyCount (c :: VkRenderPassMultiviewCreateInfo))) (peekElemOff (vkPViewOffsets (c :: VkRenderPassMultiviewCreateInfo))))
                                                                           -- Length valued member elided
                                                                           <*> (Data.Vector.generateM (fromIntegral (vkCorrelationMaskCount (c :: VkRenderPassMultiviewCreateInfo))) (peekElemOff (vkPCorrelationMasks (c :: VkRenderPassMultiviewCreateInfo))))

instance Zero RenderPassMultiviewCreateInfo where
  zero = RenderPassMultiviewCreateInfo Nothing
                                       Data.Vector.empty
                                       Data.Vector.empty
                                       Data.Vector.empty

