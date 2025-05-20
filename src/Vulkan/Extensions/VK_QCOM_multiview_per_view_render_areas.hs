{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_multiview_per_view_render_areas - device extension
--
-- == VK_QCOM_multiview_per_view_render_areas
--
-- [__Name String__]
--     @VK_QCOM_multiview_per_view_render_areas@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     511
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Jeff Leger
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_multiview_per_view_render_areas] @jackohound%0A*Here describe the issue or question you have about the VK_QCOM_multiview_per_view_render_areas extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-01-10
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension interacts with @VK_KHR_dynamic_rendering@
--
--     -   This extension interacts with @VK_QCOM_render_pass_transform@
--
-- [__Contributors__]
--
--     -   Jeff Leger, Qualcomm
--
--     -   Jonathan Tinkham, Qualcomm
--
--     -   Jonathan Wicks, Qualcomm
--
-- == Description
--
-- Certain use cases (e.g., side-by-side VR rendering) use multiview and
-- render to distinct regions of the framebuffer for each view. On some
-- implementations, there may be a performance benefit for providing
-- per-view render areas to the implementation. Such per-view render areas
-- can be used by the implementation to reduce the pixels that are affected
-- by attachment load, store, and multisample resolve operations.
--
-- The extension enables a multiview render pass instance to define
-- per-view render areas. For each view of a multiview render pass
-- instance, only those pixels in the per-view render area are affected by
-- load, store and resolve operations.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM'
--
-- -   Extending 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo',
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo':
--
--     -   'MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_MULTIVIEW_PER_VIEW_RENDER_AREAS_EXTENSION_NAME'
--
-- -   'QCOM_MULTIVIEW_PER_VIEW_RENDER_AREAS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_RENDER_AREAS_RENDER_PASS_BEGIN_INFO_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_RENDER_AREAS_FEATURES_QCOM'
--
-- == Issues
--
-- 1) Do the per-view @renderAreas@ interact with
-- 'Vulkan.Core10.Pass.getRenderAreaGranularity' ?
--
-- __RESOLVED__: There is no change. The granularity returned by
-- 'Vulkan.Core10.Pass.getRenderAreaGranularity' also applies to the
-- per-view @renderAreas@.
--
-- 2) How does this extension interact with
-- @VK_QCOM_render_pass_transform@?
--
-- __RESOLVED__: When @VK_QCOM_render_pass_transform@ is enabled, the
-- application provides render area in non-rotated coordinates which is
-- rotated by the implementation to the rotated coordinate system. When
-- this extension is used in combination with
-- @VK_QCOM_render_pass_transform@, then the @renderArea@ provided in
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@renderArea@,
-- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'::@renderArea@,
-- or
-- 'Vulkan.Extensions.VK_QCOM_render_pass_transform.CommandBufferInheritanceRenderPassTransformInfoQCOM'::@renderArea@
-- is rotated by the implementation. The per-view render areas are not
-- rotated.
--
-- 3) How does this extension interact with
-- @VK_QCOM_multiview_per_view_viewports@
--
-- __RESOLVED__: There is no direct interaction. The per-view viewports and
-- the per-view renderAreas are orthogonal features.
--
-- 4) When a per-view @renderArea@ is specified, must multiview rendering
-- for each view of a multiview render pass be contained within the
-- per-view @renderArea@?
--
-- __RESOLVED__: Yes, and the @VK_QCOM_multiview_per_view_viewports@ may
-- help here since it provides per-view scissors.
--
-- 5) When per-view render areas are specified, what purpose if any do
-- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'::@renderArea@
-- and
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@renderArea@
-- serve?
--
-- __RESOLVED__: The per-view @renderArea@ effectively overrides the
-- per-renderpass @renderArea@. The per-view @renderArea@ defines the
-- regions of the attachments that are effected by load, store, and
-- multisample resolve operations. A valid implementation could ignore the
-- per-renderpass @renderArea@. However, as an aid to the implementation,
-- the application must set the per-renderpass @renderArea@ to an area that
-- is at least as large as the union of all the per-view render areas.
-- Pixels that are within the per-renderpass @renderArea@ but not within
-- any per-view render area must not be affected by load, store, or
-- multisample resolve operations.
--
-- == Version History
--
-- -   Revision 1, 2023-01-10 (Jeff Leger)
--
-- == See Also
--
-- 'MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM',
-- 'PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QCOM_multiview_per_view_render_areas Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_multiview_per_view_render_areas  ( PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM(..)
                                                                  , MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM(..)
                                                                  , QCOM_MULTIVIEW_PER_VIEW_RENDER_AREAS_SPEC_VERSION
                                                                  , pattern QCOM_MULTIVIEW_PER_VIEW_RENDER_AREAS_SPEC_VERSION
                                                                  , QCOM_MULTIVIEW_PER_VIEW_RENDER_AREAS_EXTENSION_NAME
                                                                  , pattern QCOM_MULTIVIEW_PER_VIEW_RENDER_AREAS_EXTENSION_NAME
                                                                  ) where

import Foreign.Marshal.Alloc (allocaBytes)
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
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_RENDER_AREAS_RENDER_PASS_BEGIN_INFO_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_RENDER_AREAS_FEATURES_QCOM))
-- | VkPhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM - Structure
-- describing multiview per view render areas features that can be
-- supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_multiview_per_view_render_areas VK_QCOM_multiview_per_view_render_areas>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM = PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM
  { -- | #features-multiview-per-view-render-areas# @multiviewPerViewRenderAreas@
    -- indicates that the implementation supports multiview per-view render
    -- areas.
    multiviewPerViewRenderAreas :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM)
#endif
deriving instance Show PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM

instance ToCStruct PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_RENDER_AREAS_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (multiviewPerViewRenderAreas))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_RENDER_AREAS_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM where
  peekCStruct p = do
    multiviewPerViewRenderAreas <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM
             (bool32ToBool multiviewPerViewRenderAreas)

instance Storable PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM where
  zero = PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM
           zero


-- | VkMultiviewPerViewRenderAreasRenderPassBeginInfoQCOM - Set the multiview
-- per view render areas for a render pass instance
--
-- = Description
--
-- If @perViewRenderAreaCount@ is not zero, then the elements of
-- @pPerViewRenderAreas@ override the value of
-- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'::@renderArea@
-- or
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@renderArea@
-- and define per-view render areas for the individual views of a multiview
-- render pass. The render area for the view with /view index/ @i@ is
-- specified by @pPerViewRenderAreas@[i].
--
-- The per-view render areas define per-view regions of attachments that
-- are loaded, stored, and resolved according to the @loadOp@, @storeOp@,
-- and @resolveMode@ values of the render pass instance. When per-view
-- render areas are defined, the value of
-- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'::@renderArea@
-- or
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@renderArea@
-- /must/ be set to a render area that includes the union of all per-view
-- render areas, /may/ be used by the implementation for optimizations, but
-- does not affect loads, stores, or resolves.
--
-- If this structure is present and if @perViewRenderAreaCount@ is not
-- zero, then @perViewRenderAreaCount@ /must/ be at least least one greater
-- than the most significant bit set in any any element of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'::@pViewMasks@.
-- or
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@viewMask@
--
-- If this structure is not present or if @perViewRenderAreaCount@ is zero,
-- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'::@renderArea@
-- or
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@renderArea@
-- is used for all views.
--
-- == Valid Usage
--
-- -   #VUID-VkMultiviewPerViewRenderAreasRenderPassBeginInfoQCOM-offset-07861#
--     The @offset.x@ member of any element of @pPerViewRenderAreas@ /must/
--     be greater than or equal to 0
--
-- -   #VUID-VkMultiviewPerViewRenderAreasRenderPassBeginInfoQCOM-offset-07862#
--     The @offset.y@ member of any element of @pPerViewRenderAreas@ /must/
--     be greater than or equal to 0
--
-- -   #VUID-VkMultiviewPerViewRenderAreasRenderPassBeginInfoQCOM-offset-07863#
--     The sum of the @offset.x@ and @extent.width@ members of any element
--     of @pPerViewRenderAreas@ /must/ be less than or equal to
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxFramebufferWidth maxFramebufferWidth>
--
-- -   #VUID-VkMultiviewPerViewRenderAreasRenderPassBeginInfoQCOM-offset-07864#
--     The sum of the @offset.y@ and @extent.height@ members of any element
--     of @pPerViewRenderAreas@ /must/ be less than or equal to
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxFramebufferHeight maxFramebufferHeight>
--
-- -   #VUID-VkMultiviewPerViewRenderAreasRenderPassBeginInfoQCOM-pNext-07865#
--     If this structure is in the @pNext@ chain of
--     'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo' and if the
--     render pass object included an element in
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'::@pViewMasks@
--     that set bit @n@, then @perViewRenderAreaCount@ /must/ be at least
--     equal to @n+1@.
--
-- -   #VUID-VkMultiviewPerViewRenderAreasRenderPassBeginInfoQCOM-pNext-07866#
--     If this structure is in the @pNext@ chain of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'
--     and if
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@viewMask@
--     set bit @n@, then @perViewRenderAreaCount@ /must/ be at least equal
--     to @n+1@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMultiviewPerViewRenderAreasRenderPassBeginInfoQCOM-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_RENDER_AREAS_RENDER_PASS_BEGIN_INFO_QCOM'
--
-- -   #VUID-VkMultiviewPerViewRenderAreasRenderPassBeginInfoQCOM-pPerViewRenderAreas-parameter#
--     If @perViewRenderAreaCount@ is not @0@, @pPerViewRenderAreas@ /must/
--     be a valid pointer to an array of @perViewRenderAreaCount@
--     'Vulkan.Core10.FundamentalTypes.Rect2D' structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_multiview_per_view_render_areas VK_QCOM_multiview_per_view_render_areas>,
-- 'Vulkan.Core10.FundamentalTypes.Rect2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM = MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM
  { -- | @pPerViewRenderAreas@ is a pointer to an array of
    -- 'Vulkan.Core10.FundamentalTypes.Rect2D' structures defining the render
    -- area for each view.
    perViewRenderAreas :: Vector Rect2D }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM)
#endif
deriving instance Show MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM

instance ToCStruct MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_RENDER_AREAS_RENDER_PASS_BEGIN_INFO_QCOM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (perViewRenderAreas)) :: Word32))
    pPPerViewRenderAreas' <- ContT $ allocaBytes @Rect2D ((Data.Vector.length (perViewRenderAreas)) * 16)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPerViewRenderAreas' `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (perViewRenderAreas)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Rect2D))) (pPPerViewRenderAreas')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_RENDER_AREAS_RENDER_PASS_BEGIN_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM where
  peekCStruct p = do
    perViewRenderAreaCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pPerViewRenderAreas <- peek @(Ptr Rect2D) ((p `plusPtr` 24 :: Ptr (Ptr Rect2D)))
    pPerViewRenderAreas' <- generateM (fromIntegral perViewRenderAreaCount) (\i -> peekCStruct @Rect2D ((pPerViewRenderAreas `advancePtrBytes` (16 * (i)) :: Ptr Rect2D)))
    pure $ MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM
             pPerViewRenderAreas'

instance Zero MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM where
  zero = MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM
           mempty


type QCOM_MULTIVIEW_PER_VIEW_RENDER_AREAS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_QCOM_MULTIVIEW_PER_VIEW_RENDER_AREAS_SPEC_VERSION"
pattern QCOM_MULTIVIEW_PER_VIEW_RENDER_AREAS_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_MULTIVIEW_PER_VIEW_RENDER_AREAS_SPEC_VERSION = 1


type QCOM_MULTIVIEW_PER_VIEW_RENDER_AREAS_EXTENSION_NAME = "VK_QCOM_multiview_per_view_render_areas"

-- No documentation found for TopLevel "VK_QCOM_MULTIVIEW_PER_VIEW_RENDER_AREAS_EXTENSION_NAME"
pattern QCOM_MULTIVIEW_PER_VIEW_RENDER_AREAS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_MULTIVIEW_PER_VIEW_RENDER_AREAS_EXTENSION_NAME = "VK_QCOM_multiview_per_view_render_areas"

