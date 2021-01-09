{-# language CPP #-}
-- | = Name
--
-- VK_EXT_blend_operation_advanced - device extension
--
-- == VK_EXT_blend_operation_advanced
--
-- [__Name String__]
--     @VK_EXT_blend_operation_advanced@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     149
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_blend_operation_advanced:%20&body=@jeffbolznv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-06-12
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension adds a number of “advanced” blending operations that
-- /can/ be used to perform new color blending operations, many of which
-- are more complex than the standard blend modes provided by unextended
-- Vulkan. This extension requires different styles of usage, depending on
-- the level of hardware support and the enabled features:
--
-- -   If
--     'PhysicalDeviceBlendOperationAdvancedFeaturesEXT'::@advancedBlendCoherentOperations@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', the new blending
--     operations are supported, but a memory dependency /must/ separate
--     each advanced blend operation on a given sample.
--     'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT'
--     is used to synchronize reads using advanced blend operations.
--
-- -   If
--     'PhysicalDeviceBlendOperationAdvancedFeaturesEXT'::@advancedBlendCoherentOperations@
--     is 'Vulkan.Core10.FundamentalTypes.TRUE', advanced blend operations
--     obey primitive order just like basic blend operations.
--
-- In unextended Vulkan, the set of blending operations is limited, and
-- /can/ be expressed very simply. The
-- 'Vulkan.Core10.Enums.BlendOp.BLEND_OP_MIN' and
-- 'Vulkan.Core10.Enums.BlendOp.BLEND_OP_MAX' blend operations simply
-- compute component-wise minimums or maximums of source and destination
-- color components. The 'Vulkan.Core10.Enums.BlendOp.BLEND_OP_ADD',
-- 'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SUBTRACT', and
-- 'Vulkan.Core10.Enums.BlendOp.BLEND_OP_REVERSE_SUBTRACT' modes multiply
-- the source and destination colors by source and destination factors and
-- either add the two products together or subtract one from the other.
-- This limited set of operations supports many common blending operations
-- but precludes the use of more sophisticated transparency and blending
-- operations commonly available in many dedicated imaging APIs.
--
-- This extension provides a number of new “advanced” blending operations.
-- Unlike traditional blending operations using
-- 'Vulkan.Core10.Enums.BlendOp.BLEND_OP_ADD', these blending equations do
-- not use source and destination factors specified by
-- 'Vulkan.Core10.Enums.BlendFactor.BlendFactor'. Instead, each blend
-- operation specifies a complete equation based on the source and
-- destination colors. These new blend operations are used for both RGB and
-- alpha components; they /must/ not be used to perform separate RGB and
-- alpha blending (via different values of color and alpha
-- 'Vulkan.Core10.Enums.BlendOp.BlendOp').
--
-- These blending operations are performed using premultiplied colors,
-- where RGB colors /can/ be considered premultiplied or non-premultiplied
-- by alpha, according to the @srcPremultiplied@ and @dstPremultiplied@
-- members of 'PipelineColorBlendAdvancedStateCreateInfoEXT'. If a color is
-- considered non-premultiplied, the (R,G,B) color components are
-- multiplied by the alpha component prior to blending. For
-- non-premultiplied color components in the range [0,1], the corresponding
-- premultiplied color component would have values in the range [0 × A, 1 ×
-- A].
--
-- Many of these advanced blending equations are formulated where the
-- result of blending source and destination colors with partial coverage
-- have three separate contributions: from the portions covered by both the
-- source and the destination, from the portion covered only by the source,
-- and from the portion covered only by the destination. The blend
-- parameter 'PipelineColorBlendAdvancedStateCreateInfoEXT'::@blendOverlap@
-- /can/ be used to specify a correlation between source and destination
-- pixel coverage. If set to 'BLEND_OVERLAP_CONJOINT_EXT', the source and
-- destination are considered to have maximal overlap, as would be the case
-- if drawing two objects on top of each other. If set to
-- 'BLEND_OVERLAP_DISJOINT_EXT', the source and destination are considered
-- to have minimal overlap, as would be the case when rendering a complex
-- polygon tessellated into individual non-intersecting triangles. If set
-- to 'BLEND_OVERLAP_UNCORRELATED_EXT', the source and destination coverage
-- are assumed to have no spatial correlation within the pixel.
--
-- In addition to the coherency issues on implementations not supporting
-- @advancedBlendCoherentOperations@, this extension has several
-- limitations worth noting. First, the new blend operations have a limit
-- on the number of color attachments they /can/ be used with, as indicated
-- by
-- 'PhysicalDeviceBlendOperationAdvancedPropertiesEXT'::@advancedBlendMaxColorAttachments@.
-- Additionally, blending precision /may/ be limited to 16-bit
-- floating-point, which /may/ result in a loss of precision and dynamic
-- range for framebuffer formats with 32-bit floating-point components, and
-- in a loss of precision for formats with 12- and 16-bit signed or
-- unsigned normalized integer components.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceBlendOperationAdvancedFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceBlendOperationAdvancedPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo':
--
--     -   'PipelineColorBlendAdvancedStateCreateInfoEXT'
--
-- == New Enums
--
-- -   'BlendOverlapEXT'
--
-- == New Enum Constants
--
-- -   'EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME'
--
-- -   'EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits':
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.BlendOp.BlendOp':
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_BLUE_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_COLORBURN_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_COLORDODGE_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_CONTRAST_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DARKEN_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DIFFERENCE_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DST_ATOP_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DST_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DST_IN_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DST_OUT_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DST_OVER_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_EXCLUSION_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_GREEN_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_HARDLIGHT_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_HARDMIX_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_HSL_COLOR_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_HSL_HUE_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_HSL_LUMINOSITY_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_HSL_SATURATION_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_INVERT_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_INVERT_OVG_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_INVERT_RGB_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_LIGHTEN_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_LINEARBURN_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_LINEARDODGE_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_LINEARLIGHT_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_MINUS_CLAMPED_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_MINUS_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_MULTIPLY_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_OVERLAY_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_PINLIGHT_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_PLUS_CLAMPED_ALPHA_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_PLUS_CLAMPED_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_PLUS_DARKER_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_PLUS_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_RED_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SCREEN_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SOFTLIGHT_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SRC_ATOP_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SRC_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SRC_IN_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SRC_OUT_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SRC_OVER_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_VIVIDLIGHT_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_XOR_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_ZERO_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2017-06-12 (Jeff Bolz)
--
--     -   Internal revisions
--
-- -   Revision 2, 2017-06-12 (Jeff Bolz)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'BlendOverlapEXT', 'PhysicalDeviceBlendOperationAdvancedFeaturesEXT',
-- 'PhysicalDeviceBlendOperationAdvancedPropertiesEXT',
-- 'PipelineColorBlendAdvancedStateCreateInfoEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_blend_operation_advanced Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_blend_operation_advanced  ( PhysicalDeviceBlendOperationAdvancedFeaturesEXT(..)
                                                          , PhysicalDeviceBlendOperationAdvancedPropertiesEXT(..)
                                                          , PipelineColorBlendAdvancedStateCreateInfoEXT(..)
                                                          , BlendOverlapEXT( BLEND_OVERLAP_UNCORRELATED_EXT
                                                                           , BLEND_OVERLAP_DISJOINT_EXT
                                                                           , BLEND_OVERLAP_CONJOINT_EXT
                                                                           , ..
                                                                           )
                                                          , EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION
                                                          , pattern EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION
                                                          , EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME
                                                          , pattern EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME
                                                          ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
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
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT))
-- | VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT - Structure describing
-- advanced blending features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceBlendOperationAdvancedFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceBlendOperationAdvancedFeaturesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceBlendOperationAdvancedFeaturesEXT' /can/ also be included
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- enable the features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceBlendOperationAdvancedFeaturesEXT = PhysicalDeviceBlendOperationAdvancedFeaturesEXT
  { -- | #features-advancedBlendCoherentOperations#
    -- @advancedBlendCoherentOperations@ specifies whether blending using
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operations>
    -- is guaranteed to execute atomically and in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-order primitive order>.
    -- If this is 'Vulkan.Core10.FundamentalTypes.TRUE',
    -- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT'
    -- is treated the same as
    -- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COLOR_ATTACHMENT_READ_BIT',
    -- and advanced blending needs no additional synchronization over basic
    -- blending. If this is 'Vulkan.Core10.FundamentalTypes.FALSE', then memory
    -- dependencies are required to guarantee order between two advanced
    -- blending operations that occur on the same sample.
    advancedBlendCoherentOperations :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceBlendOperationAdvancedFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceBlendOperationAdvancedFeaturesEXT

instance ToCStruct PhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceBlendOperationAdvancedFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (advancedBlendCoherentOperations))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  peekCStruct p = do
    advancedBlendCoherentOperations <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceBlendOperationAdvancedFeaturesEXT
             (bool32ToBool advancedBlendCoherentOperations)

instance Storable PhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  zero = PhysicalDeviceBlendOperationAdvancedFeaturesEXT
           zero


-- | VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT - Structure
-- describing advanced blending limits that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceBlendOperationAdvancedPropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceBlendOperationAdvancedPropertiesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceBlendOperationAdvancedPropertiesEXT = PhysicalDeviceBlendOperationAdvancedPropertiesEXT
  { -- | #limits-advancedBlendMaxColorAttachments#
    -- @advancedBlendMaxColorAttachments@ is one greater than the highest color
    -- attachment index that /can/ be used in a subpass, for a pipeline that
    -- uses an
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operation>.
    advancedBlendMaxColorAttachments :: Word32
  , -- | #limits-advancedBlendIndependentBlend# @advancedBlendIndependentBlend@
    -- specifies whether advanced blend operations /can/ vary per-attachment.
    advancedBlendIndependentBlend :: Bool
  , -- | #limits-advancedBlendNonPremultipliedSrcColor#
    -- @advancedBlendNonPremultipliedSrcColor@ specifies whether the source
    -- color /can/ be treated as non-premultiplied. If this is
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', then
    -- 'PipelineColorBlendAdvancedStateCreateInfoEXT'::@srcPremultiplied@
    -- /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'.
    advancedBlendNonPremultipliedSrcColor :: Bool
  , -- | #limits-advancedBlendNonPremultipliedDstColor#
    -- @advancedBlendNonPremultipliedDstColor@ specifies whether the
    -- destination color /can/ be treated as non-premultiplied. If this is
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', then
    -- 'PipelineColorBlendAdvancedStateCreateInfoEXT'::@dstPremultiplied@
    -- /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'.
    advancedBlendNonPremultipliedDstColor :: Bool
  , -- | #limits-advancedBlendCorrelatedOverlap# @advancedBlendCorrelatedOverlap@
    -- specifies whether the overlap mode /can/ be treated as correlated. If
    -- this is 'Vulkan.Core10.FundamentalTypes.FALSE', then
    -- 'PipelineColorBlendAdvancedStateCreateInfoEXT'::@blendOverlap@ /must/ be
    -- 'BLEND_OVERLAP_UNCORRELATED_EXT'.
    advancedBlendCorrelatedOverlap :: Bool
  , -- | #limits-advancedBlendAllOperations# @advancedBlendAllOperations@
    -- specifies whether all advanced blend operation enums are supported. See
    -- the valid usage of
    -- 'Vulkan.Core10.Pipeline.PipelineColorBlendAttachmentState'.
    advancedBlendAllOperations :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceBlendOperationAdvancedPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceBlendOperationAdvancedPropertiesEXT

instance ToCStruct PhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceBlendOperationAdvancedPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (advancedBlendMaxColorAttachments)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (advancedBlendIndependentBlend))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (advancedBlendNonPremultipliedSrcColor))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (advancedBlendNonPremultipliedDstColor))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (advancedBlendCorrelatedOverlap))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (advancedBlendAllOperations))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  peekCStruct p = do
    advancedBlendMaxColorAttachments <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    advancedBlendIndependentBlend <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    advancedBlendNonPremultipliedSrcColor <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    advancedBlendNonPremultipliedDstColor <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    advancedBlendCorrelatedOverlap <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    advancedBlendAllOperations <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    pure $ PhysicalDeviceBlendOperationAdvancedPropertiesEXT
             advancedBlendMaxColorAttachments (bool32ToBool advancedBlendIndependentBlend) (bool32ToBool advancedBlendNonPremultipliedSrcColor) (bool32ToBool advancedBlendNonPremultipliedDstColor) (bool32ToBool advancedBlendCorrelatedOverlap) (bool32ToBool advancedBlendAllOperations)

instance Storable PhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  zero = PhysicalDeviceBlendOperationAdvancedPropertiesEXT
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPipelineColorBlendAdvancedStateCreateInfoEXT - Structure specifying
-- parameters that affect advanced blend operations
--
-- = Description
--
-- If this structure is not present, @srcPremultiplied@ and
-- @dstPremultiplied@ are both considered to be
-- 'Vulkan.Core10.FundamentalTypes.TRUE', and @blendOverlap@ is considered
-- to be 'BLEND_OVERLAP_UNCORRELATED_EXT'.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineColorBlendAdvancedStateCreateInfoEXT-srcPremultiplied-01424#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-advancedBlendNonPremultipliedSrcColor non-premultiplied source color>
--     property is not supported, @srcPremultiplied@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkPipelineColorBlendAdvancedStateCreateInfoEXT-dstPremultiplied-01425#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-advancedBlendNonPremultipliedDstColor non-premultiplied destination color>
--     property is not supported, @dstPremultiplied@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkPipelineColorBlendAdvancedStateCreateInfoEXT-blendOverlap-01426#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-advancedBlendCorrelatedOverlap correlated overlap>
--     property is not supported, @blendOverlap@ /must/ be
--     'BLEND_OVERLAP_UNCORRELATED_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineColorBlendAdvancedStateCreateInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT'
--
-- -   #VUID-VkPipelineColorBlendAdvancedStateCreateInfoEXT-blendOverlap-parameter#
--     @blendOverlap@ /must/ be a valid 'BlendOverlapEXT' value
--
-- = See Also
--
-- 'BlendOverlapEXT', 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineColorBlendAdvancedStateCreateInfoEXT = PipelineColorBlendAdvancedStateCreateInfoEXT
  { -- | @srcPremultiplied@ specifies whether the source color of the blend
    -- operation is treated as premultiplied.
    srcPremultiplied :: Bool
  , -- | @dstPremultiplied@ specifies whether the destination color of the blend
    -- operation is treated as premultiplied.
    dstPremultiplied :: Bool
  , -- | @blendOverlap@ is a 'BlendOverlapEXT' value specifying how the source
    -- and destination sample’s coverage is correlated.
    blendOverlap :: BlendOverlapEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineColorBlendAdvancedStateCreateInfoEXT)
#endif
deriving instance Show PipelineColorBlendAdvancedStateCreateInfoEXT

instance ToCStruct PipelineColorBlendAdvancedStateCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineColorBlendAdvancedStateCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (srcPremultiplied))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (dstPremultiplied))
    poke ((p `plusPtr` 24 :: Ptr BlendOverlapEXT)) (blendOverlap)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr BlendOverlapEXT)) (zero)
    f

instance FromCStruct PipelineColorBlendAdvancedStateCreateInfoEXT where
  peekCStruct p = do
    srcPremultiplied <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    dstPremultiplied <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    blendOverlap <- peek @BlendOverlapEXT ((p `plusPtr` 24 :: Ptr BlendOverlapEXT))
    pure $ PipelineColorBlendAdvancedStateCreateInfoEXT
             (bool32ToBool srcPremultiplied) (bool32ToBool dstPremultiplied) blendOverlap

instance Storable PipelineColorBlendAdvancedStateCreateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineColorBlendAdvancedStateCreateInfoEXT where
  zero = PipelineColorBlendAdvancedStateCreateInfoEXT
           zero
           zero
           zero


-- | VkBlendOverlapEXT - Enumerant specifying the blend overlap parameter
--
-- = Description
--
-- \'
--
-- +----------------------------------+--------------------------------------------------------------------------------------+
-- | Overlap Mode                     | Weighting Equations                                                                  |
-- +==================================+======================================================================================+
-- | 'BLEND_OVERLAP_UNCORRELATED_EXT' | \[                                              \begin{aligned}                      |
-- |                                  |                                                 p_0(A_s,A_d) & = A_sA_d \\           |
-- |                                  |                                                 p_1(A_s,A_d) & = A_s(1-A_d) \\       |
-- |                                  |                                                 p_2(A_s,A_d) & = A_d(1-A_s) \\       |
-- |                                  |                                               \end{aligned}\]                        |
-- +----------------------------------+--------------------------------------------------------------------------------------+
-- | 'BLEND_OVERLAP_CONJOINT_EXT'     | \[                                              \begin{aligned}                      |
-- |                                  |                                                 p_0(A_s,A_d) & = min(A_s,A_d) \\     |
-- |                                  |                                                 p_1(A_s,A_d) & = max(A_s-A_d,0) \\   |
-- |                                  |                                                 p_2(A_s,A_d) & = max(A_d-A_s,0) \\   |
-- |                                  |                                               \end{aligned}\]                        |
-- +----------------------------------+--------------------------------------------------------------------------------------+
-- | 'BLEND_OVERLAP_DISJOINT_EXT'     | \[                                              \begin{aligned}                      |
-- |                                  |                                                 p_0(A_s,A_d) & = max(A_s+A_d-1,0) \\ |
-- |                                  |                                                 p_1(A_s,A_d) & = min(A_s,1-A_d) \\   |
-- |                                  |                                                 p_2(A_s,A_d) & = min(A_d,1-A_s) \\   |
-- |                                  |                                               \end{aligned}\]                        |
-- +----------------------------------+--------------------------------------------------------------------------------------+
--
-- Advanced Blend Overlap Modes
--
-- = See Also
--
-- 'PipelineColorBlendAdvancedStateCreateInfoEXT'
newtype BlendOverlapEXT = BlendOverlapEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'BLEND_OVERLAP_UNCORRELATED_EXT' specifies that there is no correlation
-- between the source and destination coverage.
pattern BLEND_OVERLAP_UNCORRELATED_EXT = BlendOverlapEXT 0
-- | 'BLEND_OVERLAP_DISJOINT_EXT' specifies that the source and destination
-- coverage are considered to have minimal overlap.
pattern BLEND_OVERLAP_DISJOINT_EXT     = BlendOverlapEXT 1
-- | 'BLEND_OVERLAP_CONJOINT_EXT' specifies that the source and destination
-- coverage are considered to have maximal overlap.
pattern BLEND_OVERLAP_CONJOINT_EXT     = BlendOverlapEXT 2
{-# complete BLEND_OVERLAP_UNCORRELATED_EXT,
             BLEND_OVERLAP_DISJOINT_EXT,
             BLEND_OVERLAP_CONJOINT_EXT :: BlendOverlapEXT #-}

conNameBlendOverlapEXT :: String
conNameBlendOverlapEXT = "BlendOverlapEXT"

enumPrefixBlendOverlapEXT :: String
enumPrefixBlendOverlapEXT = "BLEND_OVERLAP_"

showTableBlendOverlapEXT :: [(BlendOverlapEXT, String)]
showTableBlendOverlapEXT =
  [ (BLEND_OVERLAP_UNCORRELATED_EXT, "UNCORRELATED_EXT")
  , (BLEND_OVERLAP_DISJOINT_EXT    , "DISJOINT_EXT")
  , (BLEND_OVERLAP_CONJOINT_EXT    , "CONJOINT_EXT")
  ]

instance Show BlendOverlapEXT where
  showsPrec = enumShowsPrec enumPrefixBlendOverlapEXT
                            showTableBlendOverlapEXT
                            conNameBlendOverlapEXT
                            (\(BlendOverlapEXT x) -> x)
                            (showsPrec 11)

instance Read BlendOverlapEXT where
  readPrec = enumReadPrec enumPrefixBlendOverlapEXT showTableBlendOverlapEXT conNameBlendOverlapEXT BlendOverlapEXT


type EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION"
pattern EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION = 2


type EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME = "VK_EXT_blend_operation_advanced"

-- No documentation found for TopLevel "VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME"
pattern EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME = "VK_EXT_blend_operation_advanced"

