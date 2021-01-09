{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_multiview"
module Vulkan.Core11.Promoted_From_VK_KHR_multiview  ( PhysicalDeviceMultiviewFeatures(..)
                                                     , PhysicalDeviceMultiviewProperties(..)
                                                     , RenderPassMultiviewCreateInfo(..)
                                                     , StructureType(..)
                                                     , DependencyFlagBits(..)
                                                     , DependencyFlags
                                                     ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO))
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlagBits(..))
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceMultiviewFeatures - Structure describing multiview
-- features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceMultiviewFeatures' structure describe
-- the following features:
--
-- = Description
--
-- -   #extension-features-multiview# @multiview@ specifies whether the
--     implementation supports multiview rendering within a render pass. If
--     this feature is not enabled, the view mask of each subpass /must/
--     always be zero.
--
-- -   #extension-features-multiview-gs# @multiviewGeometryShader@
--     specifies whether the implementation supports multiview rendering
--     within a render pass, with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#geometry geometry shaders>.
--     If this feature is not enabled, then a pipeline compiled against a
--     subpass with a non-zero view mask /must/ not include a geometry
--     shader.
--
-- -   #extension-features-multiview-tess# @multiviewTessellationShader@
--     specifies whether the implementation supports multiview rendering
--     within a render pass, with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#tessellation tessellation shaders>.
--     If this feature is not enabled, then a pipeline compiled against a
--     subpass with a non-zero view mask /must/ not include any
--     tessellation shaders.
--
-- If the 'PhysicalDeviceMultiviewFeatures' structure is included in the
-- @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceMultiviewFeatures' /can/ also be included in the @pNext@
-- chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable the features.
--
-- == Valid Usage
--
-- -   #VUID-VkPhysicalDeviceMultiviewFeatures-multiviewGeometryShader-00580#
--     If @multiviewGeometryShader@ is enabled then @multiview@ /must/ also
--     be enabled
--
-- -   #VUID-VkPhysicalDeviceMultiviewFeatures-multiviewTessellationShader-00581#
--     If @multiviewTessellationShader@ is enabled then @multiview@ /must/
--     also be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceMultiviewFeatures-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMultiviewFeatures = PhysicalDeviceMultiviewFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "multiview"
    multiview :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "multiviewGeometryShader"
    multiviewGeometryShader :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "multiviewTessellationShader"
    multiviewTessellationShader :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMultiviewFeatures)
#endif
deriving instance Show PhysicalDeviceMultiviewFeatures

instance ToCStruct PhysicalDeviceMultiviewFeatures where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMultiviewFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (multiview))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (multiviewGeometryShader))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (multiviewTessellationShader))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMultiviewFeatures where
  peekCStruct p = do
    multiview <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    multiviewGeometryShader <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    multiviewTessellationShader <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceMultiviewFeatures
             (bool32ToBool multiview) (bool32ToBool multiviewGeometryShader) (bool32ToBool multiviewTessellationShader)

instance Storable PhysicalDeviceMultiviewFeatures where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMultiviewFeatures where
  zero = PhysicalDeviceMultiviewFeatures
           zero
           zero
           zero


-- | VkPhysicalDeviceMultiviewProperties - Structure describing multiview
-- limits that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceMultiviewProperties' structure
-- describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceMultiviewProperties' structure is included in the
-- @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMultiviewProperties = PhysicalDeviceMultiviewProperties
  { -- | #extension-limits-maxMultiviewViewCount# @maxMultiviewViewCount@ is one
    -- greater than the maximum view index that /can/ be used in a subpass.
    maxMultiviewViewCount :: Word32
  , -- | #extension-limits-maxMultiviewInstanceIndex# @maxMultiviewInstanceIndex@
    -- is the maximum valid value of instance index allowed to be generated by
    -- a drawing command recorded within a subpass of a multiview render pass
    -- instance.
    maxMultiviewInstanceIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMultiviewProperties)
#endif
deriving instance Show PhysicalDeviceMultiviewProperties

instance ToCStruct PhysicalDeviceMultiviewProperties where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMultiviewProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxMultiviewViewCount)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxMultiviewInstanceIndex)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceMultiviewProperties where
  peekCStruct p = do
    maxMultiviewViewCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxMultiviewInstanceIndex <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ PhysicalDeviceMultiviewProperties
             maxMultiviewViewCount maxMultiviewInstanceIndex

instance Storable PhysicalDeviceMultiviewProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMultiviewProperties where
  zero = PhysicalDeviceMultiviewProperties
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiview-gs geometry shaders>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiview-tess tessellation shaders>.
--
-- When multiview is enabled, the
-- 'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_VIEW_LOCAL_BIT' bit
-- in a dependency /can/ be used to express a view-local dependency,
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
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBeginRenderPass' or
-- 'Vulkan.Core10.CommandBufferBuilding.cmdNextSubpass' is called the
-- graphics pipeline /must/ be bound, any relevant descriptor sets or
-- vertex\/index buffers /must/ be bound, and any relevant dynamic state or
-- push constants /must/ be set before they are used.
--
-- A multiview subpass /can/ declare that its shaders will write per-view
-- attributes for all views in a single invocation, by setting the
-- 'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX'
-- bit in the subpass description. The only supported per-view attributes
-- are position and viewport mask, and per-view position and viewport masks
-- are written to output array variables decorated with @PositionPerViewNV@
-- and @ViewportMaskPerViewNV@, respectively. If @VK_NV_viewport_array2@ is
-- not supported and enabled, @ViewportMaskPerViewNV@ /must/ not be used.
-- Values written to elements of @PositionPerViewNV@ and
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
-- 'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX'
-- bit /must/ write per-view attributes to the @*PerViewNV[]@ shader
-- outputs, in addition to the non-per-view (e.g. @Position@) outputs.
-- Pipelines compiled against a subpass that does not include this bit
-- /must/ not include the @*PerViewNV[]@ outputs in their interfaces.
--
-- == Valid Usage
--
-- -   #VUID-VkRenderPassMultiviewCreateInfo-pCorrelationMasks-00841# Each
--     view index /must/ not be set in more than one element of
--     @pCorrelationMasks@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderPassMultiviewCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO'
--
-- -   #VUID-VkRenderPassMultiviewCreateInfo-pViewMasks-parameter# If
--     @subpassCount@ is not @0@, @pViewMasks@ /must/ be a valid pointer to
--     an array of @subpassCount@ @uint32_t@ values
--
-- -   #VUID-VkRenderPassMultiviewCreateInfo-pViewOffsets-parameter# If
--     @dependencyCount@ is not @0@, @pViewOffsets@ /must/ be a valid
--     pointer to an array of @dependencyCount@ @int32_t@ values
--
-- -   #VUID-VkRenderPassMultiviewCreateInfo-pCorrelationMasks-parameter#
--     If @correlationMaskCount@ is not @0@, @pCorrelationMasks@ /must/ be
--     a valid pointer to an array of @correlationMaskCount@ @uint32_t@
--     values
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RenderPassMultiviewCreateInfo = RenderPassMultiviewCreateInfo
  { -- | @pViewMasks@ is a pointer to an array of @subpassCount@ view masks,
    -- where each mask is a bitfield of view indices describing which views
    -- rendering is broadcast to in each subpass, when multiview is enabled. If
    -- @subpassCount@ is zero, each view mask is treated as zero.
    viewMasks :: Vector Word32
  , -- | @pViewOffsets@ is a pointer to an array of @dependencyCount@ view
    -- offsets, one for each dependency. If @dependencyCount@ is zero, each
    -- dependency’s view offset is treated as zero. Each view offset controls
    -- which views in the source subpass the views in the destination subpass
    -- depend on.
    viewOffsets :: Vector Int32
  , -- | @pCorrelationMasks@ is a pointer to an array of @correlationMaskCount@
    -- view masks indicating sets of views that /may/ be more efficient to
    -- render concurrently.
    correlationMasks :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassMultiviewCreateInfo)
#endif
deriving instance Show RenderPassMultiviewCreateInfo

instance ToCStruct RenderPassMultiviewCreateInfo where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassMultiviewCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (viewMasks)) :: Word32))
    pPViewMasks' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (viewMasks)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPViewMasks' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (viewMasks)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPViewMasks')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (viewOffsets)) :: Word32))
    pPViewOffsets' <- ContT $ allocaBytesAligned @Int32 ((Data.Vector.length (viewOffsets)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPViewOffsets' `plusPtr` (4 * (i)) :: Ptr Int32) (e)) (viewOffsets)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Int32))) (pPViewOffsets')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (correlationMasks)) :: Word32))
    pPCorrelationMasks' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (correlationMasks)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPCorrelationMasks' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (correlationMasks)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr Word32))) (pPCorrelationMasks')
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct RenderPassMultiviewCreateInfo where
  peekCStruct p = do
    subpassCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pViewMasks <- peek @(Ptr Word32) ((p `plusPtr` 24 :: Ptr (Ptr Word32)))
    pViewMasks' <- generateM (fromIntegral subpassCount) (\i -> peek @Word32 ((pViewMasks `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    dependencyCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pViewOffsets <- peek @(Ptr Int32) ((p `plusPtr` 40 :: Ptr (Ptr Int32)))
    pViewOffsets' <- generateM (fromIntegral dependencyCount) (\i -> peek @Int32 ((pViewOffsets `advancePtrBytes` (4 * (i)) :: Ptr Int32)))
    correlationMaskCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pCorrelationMasks <- peek @(Ptr Word32) ((p `plusPtr` 56 :: Ptr (Ptr Word32)))
    pCorrelationMasks' <- generateM (fromIntegral correlationMaskCount) (\i -> peek @Word32 ((pCorrelationMasks `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ RenderPassMultiviewCreateInfo
             pViewMasks' pViewOffsets' pCorrelationMasks'

instance Zero RenderPassMultiviewCreateInfo where
  zero = RenderPassMultiviewCreateInfo
           mempty
           mempty
           mempty

