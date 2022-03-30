{-# language CPP #-}
-- | = Name
--
-- VK_EXT_primitives_generated_query - device extension
--
-- == VK_EXT_primitives_generated_query
--
-- [__Name String__]
--     @VK_EXT_primitives_generated_query@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     383
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_EXT_transform_feedback@
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_primitives_generated_query] @syoussefi%0A<<Here describe the issue or question you have about the VK_EXT_primitives_generated_query extension>> >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_primitives_generated_query.asciidoc VK_EXT_primitives_generated_query>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-01-24
--
-- [__Contributors__]
--
--     -   Shahbaz Youssefi, Google
--
--     -   Piers Daniell, NVIDIA
--
--     -   Jason Ekstrand, Collabora
--
--     -   Jan-Harald Fredriksen, Arm
--
-- == Description
--
-- This extension adds support for a new query type to match OpenGL’s
-- @GL_PRIMITIVES_GENERATED@ to support layering.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PRIMITIVES_GENERATED_QUERY_EXTENSION_NAME'
--
-- -   'EXT_PRIMITIVES_GENERATED_QUERY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.QueryType.QueryType':
--
--     -   'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIMITIVES_GENERATED_QUERY_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2021-06-23 (Shahbaz Youssefi)
--
--     -   Internal revisions
--
-- == Issues
--
-- 1) Can the query from @VK_EXT_transform_feedback@ be used instead?
--
-- __RESOLVED__: No. While the query from VK_EXT_transform_feedback can
-- produce the same results as in this extension, it is only available
-- while transform feedback is active. The OpenGL @GL_PRIMITIVES_GENERATED@
-- query is independent from transform feedback. Emulation through
-- artificial transform feedback is unnecessarily inefficient.
--
-- 2) Can
-- 'Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits.QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT'
-- be used instead?
--
-- __RESOLVED__: It could, but we prefer the extension for simplicity.
-- Vulkan requires that only one query be active at a time. If both the
-- @GL_PRIMITIVES_GENERATED@ and the @GL_CLIPPING_INPUT_PRIMITIVES_ARB@
-- queries need to be simultaneously enabled, emulation of both through
-- 'Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits.QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT'
-- is inconvenient.
--
-- 3) On some hardware, this query cannot be implemented if
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'::@rasterizerDiscardEnable@
-- is enabled. How will this be handled?
--
-- __RESOLVED__: A feature flag is exposed by this extension for this. On
-- said hardware, the GL implementation disables rasterizer-discard and
-- achieves the same effect through other means. It will not be able to do
-- the same in Vulkan due to lack of state information. A feature flag is
-- exposed by this extension so the OpenGL implementation on top of Vulkan
-- would be able to implement a similar workaround.
--
-- 4) On some hardware, this query cannot be implemented for non-zero query
-- indices. How will this be handled?
--
-- __RESOLVED__: A feature flag is exposed by this extension for this. If
-- this feature is not present, the query from @VK_EXT_transform_feedback@
-- can be used to the same effect.
--
-- 5) How is the interaction of this extension with
-- @transformFeedbackRasterizationStreamSelect@ handled?
--
-- __RESOLVED__: Disallowed for non-zero streams. In OpenGL, the
-- rasterization stream is always stream zero.
--
-- == See Also
--
-- 'PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_primitives_generated_query Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_primitives_generated_query  ( PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT(..)
                                                            , EXT_PRIMITIVES_GENERATED_QUERY_SPEC_VERSION
                                                            , pattern EXT_PRIMITIVES_GENERATED_QUERY_SPEC_VERSION
                                                            , EXT_PRIMITIVES_GENERATED_QUERY_EXTENSION_NAME
                                                            , pattern EXT_PRIMITIVES_GENERATED_QUERY_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIMITIVES_GENERATED_QUERY_FEATURES_EXT))
-- | VkPhysicalDevicePrimitivesGeneratedQueryFeaturesEXT - Structure
-- describing support for primitives generated query
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_primitives_generated_query VK_EXT_primitives_generated_query>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT = PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT
  { -- | #features-primitivesGeneratedQuery# @primitivesGeneratedQuery@ indicates
    -- whether the implementation supports the
    -- 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
    -- query type.
    primitivesGeneratedQuery :: Bool
  , -- | #features-primitivesGeneratedQueryWithRasterizerDiscard#
    -- @primitivesGeneratedQueryWithRasterizerDiscard@ indicates whether the
    -- implementation supports this query when
    -- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-discard rasterization discard>
    -- is enabled.
    primitivesGeneratedQueryWithRasterizerDiscard :: Bool
  , -- | #features-primitivesGeneratedQueryWithNonZeroStreams#
    -- @primitivesGeneratedQueryWithNonZeroStreams@ indicates whether the
    -- implementation supports this query with a non-zero index in
    -- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginQueryIndexedEXT'.
    primitivesGeneratedQueryWithNonZeroStreams :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT)
#endif
deriving instance Show PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT

instance ToCStruct PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIMITIVES_GENERATED_QUERY_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (primitivesGeneratedQuery))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (primitivesGeneratedQueryWithRasterizerDiscard))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (primitivesGeneratedQueryWithNonZeroStreams))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIMITIVES_GENERATED_QUERY_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT where
  peekCStruct p = do
    primitivesGeneratedQuery <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    primitivesGeneratedQueryWithRasterizerDiscard <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    primitivesGeneratedQueryWithNonZeroStreams <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT
             (bool32ToBool primitivesGeneratedQuery) (bool32ToBool primitivesGeneratedQueryWithRasterizerDiscard) (bool32ToBool primitivesGeneratedQueryWithNonZeroStreams)

instance Storable PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT where
  zero = PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT
           zero
           zero
           zero


type EXT_PRIMITIVES_GENERATED_QUERY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_PRIMITIVES_GENERATED_QUERY_SPEC_VERSION"
pattern EXT_PRIMITIVES_GENERATED_QUERY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PRIMITIVES_GENERATED_QUERY_SPEC_VERSION = 1


type EXT_PRIMITIVES_GENERATED_QUERY_EXTENSION_NAME = "VK_EXT_primitives_generated_query"

-- No documentation found for TopLevel "VK_EXT_PRIMITIVES_GENERATED_QUERY_EXTENSION_NAME"
pattern EXT_PRIMITIVES_GENERATED_QUERY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PRIMITIVES_GENERATED_QUERY_EXTENSION_NAME = "VK_EXT_primitives_generated_query"

